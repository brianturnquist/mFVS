/*
 * fvsbuild.c -- build helper for the mFVS Make and CMake builds.
 *
 * The original build needed only make, a POSIX shell with its basic file
 * utilities, and the GNU compilers. This helper keeps the dependency
 * preparation and shared-compilation reuse added by O22-O28 inside that
 * boundary: the build compiles it with the C compiler it already requires,
 * and it starts no program except the compilers it is given.
 *
 *   fvsbuild prepare --root DIR --manifest FILE [--native-sources]
 *       Run in a variant build directory before Make reads dependencies.mk.
 *       Stages selected includes, repairs object receipts, records compiler
 *       settings, and writes the bootstrap dependency graph. Prints "ready".
 *
 *   fvsbuild compile --cache-dir DIR [--groups LIST] [--kind KIND]
 *                    [--hardlink] -- COMPILER ARGUMENTS...
 *       Compiler launcher for the audited SQLite, FOFEM and NVEL units.
 *       Identical compilations (same compiler, flags, preprocessed text,
 *       headers and module interfaces) reuse one immutable cache entry.
 *       Each binary still links its own copy of every object.
 *
 *   fvsbuild install-self DESTINATION
 *       Rename this freshly compiled helper over DESTINATION atomically, so
 *       concurrent builds never run a partly written helper.
 */

#if defined(_WIN32)
#define FVS_WINDOWS 1
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#include <fcntl.h>
#include <io.h>
#include <process.h>
#include <sys/utime.h>
#define getpid _getpid
#else
#define _GNU_SOURCE
#define _DARWIN_C_SOURCE
#include <dirent.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <unistd.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

/* ------------------------------------------------------------------ */
/* Memory, strings and lists                                           */

static void die(const char *fmt, ...)
{
  va_list ap;
  fputs("fvsbuild: ", stderr);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  exit(1);
}

static void *xmalloc(size_t n)
{
  void *p = malloc(n ? n : 1);
  if (!p) die("out of memory");
  return p;
}

static void *xrealloc(void *p, size_t n)
{
  p = realloc(p, n ? n : 1);
  if (!p) die("out of memory");
  return p;
}

static char *xstrndup(const char *s, size_t n)
{
  char *p = xmalloc(n + 1);
  memcpy(p, s, n);
  p[n] = 0;
  return p;
}

static char *xstrdup(const char *s) { return xstrndup(s, strlen(s)); }

typedef struct { char *s; size_t n, cap; } Buf;

static void buf_add(Buf *b, const char *s, size_t n)
{
  if (b->n + n + 1 > b->cap) {
    b->cap = (b->n + n + 1) * 2;
    b->s = xrealloc(b->s, b->cap);
  }
  if (n) memcpy(b->s + b->n, s, n);
  b->n += n;
  b->s[b->n] = 0;
}

static void buf_str(Buf *b, const char *s) { buf_add(b, s, strlen(s)); }
static void buf_ch(Buf *b, char c) { buf_add(b, &c, 1); }
static const char *buf_text(Buf *b) { if (!b->s) buf_add(b, "", 0); return b->s; }

static void buf_fmt(Buf *b, const char *fmt, ...)
{
  va_list ap;
  int n;
  char small[256];
  va_start(ap, fmt);
  n = vsnprintf(small, sizeof small, fmt, ap);
  va_end(ap);
  if (n < 0) die("format failed");
  if ((size_t)n < sizeof small) {
    buf_add(b, small, (size_t)n);
  } else {
    char *big = xmalloc((size_t)n + 1);
    va_start(ap, fmt);
    vsnprintf(big, (size_t)n + 1, fmt, ap);
    va_end(ap);
    buf_add(b, big, (size_t)n);
    free(big);
  }
}

typedef struct { char **v; size_t n, cap; } List;

static void list_own(List *l, char *s)
{
  if (l->n == l->cap) {
    l->cap = l->cap ? l->cap * 2 : 16;
    l->v = xrealloc(l->v, l->cap * sizeof *l->v);
  }
  l->v[l->n++] = s;
}

static void list_add(List *l, const char *s) { list_own(l, xstrdup(s)); }

static long list_find(const List *l, const char *s)
{
  size_t i;
  for (i = 0; i < l->n; i++)
    if (!strcmp(l->v[i], s)) return (long)i;
  return -1;
}

static void list_add_unique(List *l, const char *s) { if (list_find(l, s) < 0) list_add(l, s); }

static int compare_strings(const void *a, const void *b)
{
  return strcmp(*(char *const *)a, *(char *const *)b);
}

static void list_sort_unique(List *l)
{
  size_t i, j = 0;
  if (l->n > 1) qsort(l->v, l->n, sizeof *l->v, compare_strings);
  for (i = 0; i < l->n; i++)
    if (!j || strcmp(l->v[j - 1], l->v[i])) l->v[j++] = l->v[i];
  l->n = j;
}

static int has_prefix(const char *s, const char *prefix)
{
  return !strncmp(s, prefix, strlen(prefix));
}

/* ------------------------------------------------------------------ */
/* SHA-256                                                             */

typedef struct { uint32_t h[8]; uint64_t bytes; unsigned char block[64]; size_t used; } Sha;

static const uint32_t SHA_K[64] = {
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2};

#define ROTR(x, n) (((x) >> (n)) | ((x) << (32 - (n))))

static void sha_block(Sha *s, const unsigned char *p)
{
  uint32_t w[64], a, b, c, d, e, f, g, h, t1, t2;
  int i;
  for (i = 0; i < 16; i++)
    w[i] = (uint32_t)p[4 * i] << 24 | (uint32_t)p[4 * i + 1] << 16 | (uint32_t)p[4 * i + 2] << 8 | p[4 * i + 3];
  for (i = 16; i < 64; i++)
    w[i] = (ROTR(w[i - 2], 17) ^ ROTR(w[i - 2], 19) ^ (w[i - 2] >> 10)) + w[i - 7]
         + (ROTR(w[i - 15], 7) ^ ROTR(w[i - 15], 18) ^ (w[i - 15] >> 3)) + w[i - 16];
  a = s->h[0]; b = s->h[1]; c = s->h[2]; d = s->h[3];
  e = s->h[4]; f = s->h[5]; g = s->h[6]; h = s->h[7];
  for (i = 0; i < 64; i++) {
    t1 = h + (ROTR(e, 6) ^ ROTR(e, 11) ^ ROTR(e, 25)) + ((e & f) ^ (~e & g)) + SHA_K[i] + w[i];
    t2 = (ROTR(a, 2) ^ ROTR(a, 13) ^ ROTR(a, 22)) + ((a & b) ^ (a & c) ^ (b & c));
    h = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
  }
  s->h[0] += a; s->h[1] += b; s->h[2] += c; s->h[3] += d;
  s->h[4] += e; s->h[5] += f; s->h[6] += g; s->h[7] += h;
}

static void sha_init(Sha *s)
{
  static const uint32_t start[8] = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
                                    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19};
  memcpy(s->h, start, sizeof start);
  s->bytes = 0;
  s->used = 0;
}

static void sha_update(Sha *s, const void *data, size_t n)
{
  const unsigned char *p = data;
  s->bytes += n;
  while (n) {
    size_t take = 64 - s->used < n ? 64 - s->used : n;
    memcpy(s->block + s->used, p, take);
    s->used += take; p += take; n -= take;
    if (s->used == 64) { sha_block(s, s->block); s->used = 0; }
  }
}

/* Length-prefixed fields keep concatenated identity parts unambiguous. */
static void sha_field(Sha *s, const char *label, const char *value, size_t n)
{
  char size[32];
  sha_update(s, label, strlen(label) + 1);
  snprintf(size, sizeof size, "%lu:", (unsigned long)n);
  sha_update(s, size, strlen(size));
  sha_update(s, value, n);
}

static void sha_final(Sha *s, char hex[65])
{
  unsigned char pad[72] = {0x80};
  uint64_t bits = s->bytes * 8;
  size_t padding = (s->used < 56 ? 56 : 120) - s->used;
  int i;
  for (i = 0; i < 8; i++) pad[padding + i] = (unsigned char)(bits >> (56 - 8 * i));
  sha_update(s, pad, padding + 8);
  for (i = 0; i < 8; i++) snprintf(hex + 8 * i, 9, "%08x", s->h[i]);
}

static void sha_text(const char *text, size_t n, char hex[65])
{
  Sha s;
  sha_init(&s);
  sha_update(&s, text, n);
  sha_final(&s, hex);
}

/* ------------------------------------------------------------------ */
/* Files and paths                                                     */

#ifdef FVS_WINDOWS
typedef struct _stat64 FileStat;
#define file_stat _stat64
#define is_sep(c) ((c) == '/' || (c) == '\\')
#define DEV_NULL "NUL"
#else
typedef struct stat FileStat;
#define file_stat stat
#define is_sep(c) ((c) == '/')
#define DEV_NULL "/dev/null"
#endif

static int is_file(const char *path)
{
  FileStat st;
  return file_stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

static int exists(const char *path)
{
  FileStat st;
  return file_stat(path, &st) == 0;
}

typedef struct { long long atime, mtime, size; unsigned mode; } Times;

static int file_times(const char *path, Times *t)
{
  FileStat st;
  if (file_stat(path, &st)) return -1;
#if defined(FVS_WINDOWS)
  t->atime = (long long)st.st_atime * 1000000000LL;
  t->mtime = (long long)st.st_mtime * 1000000000LL;
#elif defined(__APPLE__)
  t->atime = (long long)st.st_atimespec.tv_sec * 1000000000LL + st.st_atimespec.tv_nsec;
  t->mtime = (long long)st.st_mtimespec.tv_sec * 1000000000LL + st.st_mtimespec.tv_nsec;
#else
  t->atime = (long long)st.st_atim.tv_sec * 1000000000LL + st.st_atim.tv_nsec;
  t->mtime = (long long)st.st_mtim.tv_sec * 1000000000LL + st.st_mtim.tv_nsec;
#endif
  t->size = (long long)st.st_size;
  t->mode = (unsigned)st.st_mode;
  return 0;
}

/* Set access/modification times in nanoseconds; now when both are < 0. */
static int set_times(const char *path, long long atime, long long mtime)
{
#ifdef FVS_WINDOWS
  struct __utimbuf64 u;
  if (atime < 0) return _utime64(path, NULL);
  u.actime = (__time64_t)(atime / 1000000000LL);
  u.modtime = (__time64_t)(mtime / 1000000000LL);
  return _utime64(path, &u);
#else
  struct timespec ts[2];
  if (atime < 0) return utimensat(AT_FDCWD, path, NULL, 0);
  ts[0].tv_sec = (time_t)(atime / 1000000000LL);
  ts[0].tv_nsec = (long)(atime % 1000000000LL);
  ts[1].tv_sec = (time_t)(mtime / 1000000000LL);
  ts[1].tv_nsec = (long)(mtime % 1000000000LL);
  return utimensat(AT_FDCWD, path, ts, 0);
#endif
}

static char *read_file(const char *path, size_t *length)
{
  FILE *f = fopen(path, "rb");
  Buf b = {0};
  char chunk[65536];
  size_t n;
  if (!f) return NULL;
  while ((n = fread(chunk, 1, sizeof chunk, f)) > 0) buf_add(&b, chunk, n);
  if (ferror(f)) { fclose(f); free(b.s); return NULL; }
  fclose(f);
  buf_text(&b);
  if (length) *length = b.n;
  return b.s;
}

static void write_file(const char *path, const char *data, size_t n)
{
  FILE *f = fopen(path, "wb");
  if (!f || (n && fwrite(data, 1, n, f) != n) || fclose(f)) die("cannot write %s", path);
}

static void write_changed(const char *path, const char *text)
{
  size_t n;
  char *old = read_file(path, &n);
  if (!old || n != strlen(text) || memcmp(old, text, n)) write_file(path, text, strlen(text));
  free(old);
}

static int sha_file(const char *path, char hex[65])
{
  FILE *f = fopen(path, "rb");
  Sha s;
  char chunk[65536];
  size_t n;
  if (!f) return -1;
  sha_init(&s);
  while ((n = fread(chunk, 1, sizeof chunk, f)) > 0) sha_update(&s, chunk, n);
  if (ferror(f)) { fclose(f); return -1; }
  fclose(f);
  sha_final(&s, hex);
  return 0;
}

static void remove_file(const char *path)
{
#ifdef FVS_WINDOWS
  _chmod(path, _S_IREAD | _S_IWRITE);
#endif
  remove(path);
}

static char *current_directory(void)
{
  char buffer[8192];
#ifdef FVS_WINDOWS
  if (!_getcwd(buffer, sizeof buffer)) die("cannot read the current directory");
#else
  if (!getcwd(buffer, sizeof buffer)) die("cannot read the current directory");
#endif
  return xstrdup(buffer);
}

static int absolute(const char *path)
{
#ifdef FVS_WINDOWS
  return is_sep(path[0]) || (path[0] && path[1] == ':');
#else
  return path[0] == '/';
#endif
}

static char *join(const char *a, const char *b)
{
  Buf r = {0};
  if (absolute(b) || !*a) return xstrdup(b);
  buf_str(&r, a);
  if (!is_sep(a[strlen(a) - 1])) buf_ch(&r, '/');
  buf_str(&r, b);
  return r.s;
}

/* Like pathlib's resolve(): canonical when the file exists, else absolute. */
static char *resolve(const char *path)
{
#ifdef FVS_WINDOWS
  char *full = _fullpath(NULL, path, 0);
  if (full) return full;
#else
  char *real = realpath(path, NULL);
  if (real) return real;
#endif
  {
    char *cwd = current_directory(), *result = join(cwd, path);
    free(cwd);
    return result;
  }
}

static const char *base_name(const char *path)
{
  const char *p = path + strlen(path);
  while (p > path && !is_sep(p[-1])) p--;
  return p;
}

static char *dir_name(const char *path)
{
  const char *name = base_name(path);
  size_t n = (size_t)(name - path);
  if (!n) return xstrdup(".");
  while (n > 1 && is_sep(path[n - 1])) n--;
  return xstrndup(path, n);
}

/* pathlib's suffix and stem, compared case-insensitively by callers. */
static const char *suffix_of(const char *path)
{
  const char *name = base_name(path), *dot = strrchr(name, '.');
  if (!dot || dot == name || !dot[1]) return "";
  return dot;
}

static char *stem_of(const char *path)
{
  const char *name = base_name(path), *suffix = suffix_of(path);
  return xstrndup(name, *suffix ? (size_t)(suffix - name) : strlen(name));
}

static int suffix_is(const char *path, const char *const *choices)
{
  const char *suffix = suffix_of(path);
  for (; *choices; choices++) {
    const char *a = suffix, *b = *choices;
    while (*a && tolower((unsigned char)*a) == *b) a++, b++;
    if (!*a && !*b) return 1;
  }
  return 0;
}

static void make_dirs(const char *path)
{
  char *copy = xstrdup(path), *p;
  for (p = copy + 1; ; p++) {
    if (*p && !is_sep(*p)) continue;
    {
      char c = *p;
      *p = 0;
#ifdef FVS_WINDOWS
      if (!(p - copy == 2 && copy[1] == ':')) _mkdir(copy);
#else
      mkdir(copy, 0777);
#endif
      *p = c;
    }
    if (!*p) break;
  }
  free(copy);
}

static void remove_tree(const char *path)
{
#ifdef FVS_WINDOWS
  struct _finddata_t entry;
  char *pattern = join(path, "*");
  intptr_t handle = _findfirst(pattern, &entry);
  free(pattern);
  if (handle != -1) {
    do {
      char *child;
      if (!strcmp(entry.name, ".") || !strcmp(entry.name, "..")) continue;
      child = join(path, entry.name);
      if (entry.attrib & _A_SUBDIR) remove_tree(child); else remove_file(child);
      free(child);
    } while (_findnext(handle, &entry) == 0);
    _findclose(handle);
  }
  _rmdir(path);
#else
  DIR *dir = opendir(path);
  struct dirent *entry;
  if (dir) {
    while ((entry = readdir(dir))) {
      FileStat st;
      char *child;
      if (!strcmp(entry->d_name, ".") || !strcmp(entry->d_name, "..")) continue;
      child = join(path, entry->d_name);
      if (!lstat(child, &st) && S_ISDIR(st.st_mode)) remove_tree(child); else remove_file(child);
      free(child);
    }
    closedir(dir);
  }
  rmdir(path);
#endif
}

static char *make_temp_dir(const char *parent, const char *prefix)
{
#ifdef FVS_WINDOWS
  unsigned long seed = (unsigned long)GetTickCount() ^ ((unsigned long)_getpid() << 16);
  int attempt;
  for (attempt = 0; attempt < 1000; attempt++) {
    Buf name = {0};
    char *path;
    seed = seed * 1103515245UL + 12345UL;
    buf_fmt(&name, "%s%08lx", prefix, seed);
    path = join(parent, name.s);
    free(name.s);
    if (!_mkdir(path)) return path;
    free(path);
  }
  die("cannot create a temporary directory in %s", parent);
  return NULL;
#else
  Buf name = {0};
  char *path;
  buf_fmt(&name, "%sXXXXXX", prefix);
  path = join(parent, name.s);
  free(name.s);
  if (!mkdtemp(path)) die("cannot create a temporary directory in %s", parent);
  return path;
#endif
}

/* Replace a file atomically, or rename a directory onto a missing path. */
static int replace_path(const char *from, const char *to)
{
#ifdef FVS_WINDOWS
  /* A virus scanner or indexer may hold a freshly written file open for a
     moment, and MoveFileEx then fails with a sharing violation (bug 71).
     Retry for up to about five seconds before giving up. */
  int attempt;
  for (attempt = 0; attempt < 50; attempt++) {
    if (MoveFileExA(from, to, MOVEFILE_REPLACE_EXISTING)) return 0;
    Sleep(100);
  }
  return rename(from, to);
#else
  return rename(from, to);
#endif
}

/* pathlib's copy2: contents, permission bits and both file times. */
static void copy_file(const char *from, const char *to)
{
  size_t n;
  char *data = read_file(from, &n);
  Times t;
  if (!data || file_times(from, &t)) die("cannot read %s", from);
  remove_file(to);
  write_file(to, data, n);
  free(data);
#ifdef FVS_WINDOWS
  _chmod(to, (t.mode & _S_IWRITE) ? (_S_IREAD | _S_IWRITE) : _S_IREAD);
#else
  chmod(to, t.mode & 07777);
#endif
  set_times(to, t.atime, t.mtime);
}

static int hard_link(const char *from, const char *to)
{
#ifdef FVS_WINDOWS
  (void)from; (void)to;
  return -1;
#else
  return link(from, to);
#endif
}

/* Create a file if needed and set its modification time to now. */
static void touch(const char *path)
{
  FILE *f = fopen(path, "ab");
  if (!f) die("cannot write %s", path);
  fclose(f);
  set_times(path, -1, -1);
}

/* ------------------------------------------------------------------ */
/* Processes                                                           */

#ifdef FVS_WINDOWS
/* Quote one argument for the Microsoft C runtime's command-line parser. */
static char *windows_quote(const char *arg)
{
  Buf b = {0};
  const char *p = arg;
  if (*arg && !strpbrk(arg, " \t\n\v\"")) return xstrdup(arg);
  buf_ch(&b, '"');
  for (;;) {
    size_t slashes = 0, i;
    while (*p == '\\') { slashes++; p++; }
    if (!*p) { for (i = 0; i < 2 * slashes; i++) buf_ch(&b, '\\'); break; }
    if (*p == '"') { for (i = 0; i < 2 * slashes + 1; i++) buf_ch(&b, '\\'); }
    else { for (i = 0; i < slashes; i++) buf_ch(&b, '\\'); }
    buf_ch(&b, *p++);
  }
  buf_ch(&b, '"');
  return b.s;
}
#endif

/*
 * Run a program without a shell. Standard output is inherited, discarded
 * (discard_out) or captured; standard error is inherited unless discarded.
 * Returns the exit status, or -1 if the program could not be started.
 */
static int run(char *const argv[], int discard_out, int discard_err, Buf *capture)
{
  fflush(stdout);
  fflush(stderr);
#ifdef FVS_WINDOWS
  {
    size_t count = 0, i;
    char **quoted;
    int saved_out = -1, saved_err = -1, pipe_fd[2] = {-1, -1}, code;
    intptr_t child;
    while (argv[count]) count++;
    quoted = xmalloc((count + 1) * sizeof *quoted);
    for (i = 0; i < count; i++) quoted[i] = windows_quote(argv[i]);
    quoted[count] = NULL;
    if (capture) {
      if (_pipe(pipe_fd, 65536, _O_BINARY | _O_NOINHERIT)) return -1;
      saved_out = _dup(1);
      _dup2(pipe_fd[1], 1);
      _close(pipe_fd[1]);
    } else if (discard_out) {
      int fd = _open(DEV_NULL, _O_WRONLY);
      saved_out = _dup(1);
      _dup2(fd, 1);
      _close(fd);
    }
    if (discard_err) {
      int fd = _open(DEV_NULL, _O_WRONLY);
      saved_err = _dup(2);
      _dup2(fd, 2);
      _close(fd);
    }
    child = _spawnvp(_P_NOWAIT, argv[0], (const char *const *)quoted);
    if (saved_out >= 0) { _dup2(saved_out, 1); _close(saved_out); }
    if (saved_err >= 0) { _dup2(saved_err, 2); _close(saved_err); }
    for (i = 0; i < count; i++) free(quoted[i]);
    free(quoted);
    if (child == -1) { if (capture) _close(pipe_fd[0]); return -1; }
    if (capture) {
      char chunk[65536];
      int n;
      while ((n = _read(pipe_fd[0], chunk, sizeof chunk)) > 0) buf_add(capture, chunk, (size_t)n);
      _close(pipe_fd[0]);
      buf_text(capture);
    }
    if (_cwait(&code, child, 0) == -1) return -1;
    return code;
  }
#else
  {
    int pipe_fd[2] = {-1, -1}, status;
    pid_t child;
    if (capture && pipe(pipe_fd)) return -1;
    child = fork();
    if (child < 0) return -1;
    if (child == 0) {
      if (capture) {
        dup2(pipe_fd[1], 1);
        close(pipe_fd[0]);
        close(pipe_fd[1]);
      } else if (discard_out) {
        int fd = open(DEV_NULL, O_WRONLY);
        if (fd >= 0) { dup2(fd, 1); close(fd); }
      }
      if (discard_err) {
        int fd = open(DEV_NULL, O_WRONLY);
        if (fd >= 0) { dup2(fd, 2); close(fd); }
      }
      execvp(argv[0], argv);
      _exit(127);
    }
    if (capture) {
      char chunk[65536];
      ssize_t n;
      close(pipe_fd[1]);
      while ((n = read(pipe_fd[0], chunk, sizeof chunk)) != 0) {
        if (n < 0) { if (errno == EINTR) continue; break; }
        buf_add(capture, chunk, (size_t)n);
      }
      close(pipe_fd[0]);
      buf_text(capture);
    }
    while (waitpid(child, &status, 0) < 0)
      if (errno != EINTR) return -1;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    return 128 + WTERMSIG(status);
  }
#endif
}

/* Replace this process with the command (Windows: run it and exit). */
static void exec_command(char *const argv[])
{
  int code;
  fflush(stdout);
  fflush(stderr);
#ifndef FVS_WINDOWS
  execvp(argv[0], argv);
  die("cannot run %s: %s", argv[0], strerror(errno));
#endif
  code = run(argv, 0, 0, NULL);
  if (code < 0) die("cannot run %s", argv[0]);
  exit(code);
}

static int run_list(const List *argv, int discard_out, int discard_err, Buf *capture)
{
  char **v = xmalloc((argv->n + 1) * sizeof *v);
  int code;
  memcpy(v, argv->v, argv->n * sizeof *v);
  v[argv->n] = NULL;
  code = run(v, discard_out, discard_err, capture);
  free(v);
  return code;
}

/* Locate a program as the shell would. */
static char *which(const char *command)
{
  const char *path = getenv("PATH");
#ifdef FVS_WINDOWS
  const char list_sep = ';';
  static const char *const suffixes[] = {"", ".exe", NULL};
#else
  const char list_sep = ':';
  static const char *const suffixes[] = {"", NULL};
#endif
  const char *const *suffix;
  if (strchr(command, '/') || (is_sep('\\') && strchr(command, '\\'))) {
    for (suffix = suffixes; *suffix; suffix++) {
      Buf candidate = {0};
      buf_str(&candidate, command);
      buf_str(&candidate, *suffix);
      if (is_file(candidate.s)) { char *r = resolve(candidate.s); free(candidate.s); return r; }
      free(candidate.s);
    }
    return NULL;
  }
  while (path && *path) {
    const char *end = strchr(path, list_sep);
    size_t n = end ? (size_t)(end - path) : strlen(path);
    char *dir = n ? xstrndup(path, n) : xstrdup(".");
    for (suffix = suffixes; *suffix; suffix++) {
      Buf name = {0};
      char *candidate;
      buf_str(&name, command);
      buf_str(&name, *suffix);
      candidate = join(dir, name.s);
      free(name.s);
#ifdef FVS_WINDOWS
      if (is_file(candidate)) {
#else
      if (is_file(candidate) && !access(candidate, X_OK)) {
#endif
        char *r = resolve(candidate);
        free(candidate);
        free(dir);
        return r;
      }
      free(candidate);
    }
    free(dir);
    path = end ? end + 1 : NULL;
  }
  return NULL;
}

/* POSIX shell-like word splitting, as Python's shlex.split(). */
static void shell_split(const char *s, List *out)
{
  const char *space = " \t\r\n";
  while (*s) {
    Buf word = {0};
    while (*s && strchr(space, *s)) s++;
    if (!*s) break;
    while (*s && !strchr(space, *s)) {
      if (*s == '\'') {
        for (s++; *s && *s != '\''; s++) buf_ch(&word, *s);
        if (*s != '\'') die("unterminated quotation in: %s", s);
        s++;
      } else if (*s == '"') {
        for (s++; *s && *s != '"'; s++) {
          if (*s == '\\' && (s[1] == '"' || s[1] == '\\')) s++;
          buf_ch(&word, *s);
        }
        if (*s != '"') die("unterminated quotation in: %s", s);
        s++;
      } else if (*s == '\\') {
        if (s[1]) buf_ch(&word, s[1]), s += 2; else s++;
      } else {
        buf_ch(&word, *s++);
      }
    }
    list_own(out, xstrdup(buf_text(&word)));
    free(word.s);
  }
}

/* Python's str.split(): runs of ASCII whitespace separate words. */
static void split_words(const char *s, List *out)
{
  while (*s) {
    const char *start;
    while (*s && isspace((unsigned char)*s)) s++;
    start = s;
    while (*s && !isspace((unsigned char)*s)) s++;
    if (s > start) list_own(out, xstrndup(start, (size_t)(s - start)));
  }
}

/* Python's shlex.join(), for diagnostics only. */
static void shell_join(Buf *out, char *const argv[])
{
  int i;
  for (i = 0; argv[i]; i++) {
    const char *a = argv[i], *p;
    int safe = *a != 0;
    if (i) buf_ch(out, ' ');
    for (p = a; *p && safe; p++)
      safe = isalnum((unsigned char)*p) || strchr("_@%+=:,./-", *p);
    if (safe) { buf_str(out, a); continue; }
    buf_ch(out, '\'');
    for (p = a; *p; p++) {
      if (*p == '\'') buf_str(out, "'\"'\"'"); else buf_ch(out, *p);
    }
    buf_ch(out, '\'');
  }
}

/* ------------------------------------------------------------------ */
/* Locks: operating-system locks are released if a build job is killed. */

typedef struct { int fd; } Lock;

static void lock_path(Lock *lock, const char *path)
{
  char *parent = dir_name(path);
  make_dirs(parent);
  free(parent);
#ifdef FVS_WINDOWS
  {
    OVERLAPPED overlap;
    lock->fd = _open(path, _O_RDWR | _O_CREAT | _O_BINARY, _S_IREAD | _S_IWRITE);
    if (lock->fd < 0) die("cannot open lock %s", path);
    memset(&overlap, 0, sizeof overlap);
    if (!LockFileEx((HANDLE)_get_osfhandle(lock->fd), LOCKFILE_EXCLUSIVE_LOCK, 0, 1, 0, &overlap))
      die("cannot lock %s", path);
  }
#else
  lock->fd = open(path, O_RDWR | O_CREAT | O_APPEND, 0666);
  if (lock->fd < 0) die("cannot open lock %s", path);
  while (flock(lock->fd, LOCK_EX))
    if (errno != EINTR) die("cannot lock %s", path);
#endif
}

static void unlock_path(Lock *lock)
{
#ifdef FVS_WINDOWS
  OVERLAPPED overlap;
  memset(&overlap, 0, sizeof overlap);
  UnlockFileEx((HANDLE)_get_osfhandle(lock->fd), 0, 1, 0, &overlap);
  _close(lock->fd);
#else
  flock(lock->fd, LOCK_UN);
  close(lock->fd);
#endif
}

/* ------------------------------------------------------------------ */
/* Compiler identity: binary, version and target, cached by path/size/time. */

static const char *host_platform(void)
{
#if defined(FVS_WINDOWS)
  return "win32";
#elif defined(__APPLE__)
  return "darwin";
#elif defined(__linux__)
  return "linux";
#else
  return "unix";
#endif
}

static char *compiler_identity(const char *command, const char *cache)
{
  char *executable = which(command), key[65], *path, *lock_file, *text;
  Buf seed = {0}, compilers = {0};
  Times t;
  Lock lock;
  if (!executable || file_times(executable, &t)) die("cannot find compiler %s", command);
  buf_fmt(&seed, "%s\n%lld\n%lld", executable, t.mtime, t.size);
  sha_text(seed.s, seed.n, key);
  compilers.s = join(cache, "compilers");
  compilers.n = strlen(compilers.s);
  path = join(compilers.s, key);
  lock_file = xmalloc(strlen(path) + 6);
  sprintf(lock_file, "%s.lock", path);
  {
    char *record = xmalloc(strlen(path) + 6);
    sprintf(record, "%s.txt", path);
    free(path);
    path = record;
  }
  lock_path(&lock, lock_file);
  if (!is_file(path)) {
    Buf version = {0}, target = {0}, record = {0};
    char hash[65];
    char *version_argv[3], *target_argv[3];
    version_argv[0] = (char *)command; version_argv[1] = "--version"; version_argv[2] = NULL;
    target_argv[0] = (char *)command; target_argv[1] = "-dumpmachine"; target_argv[2] = NULL;
    if (run(version_argv, 0, 0, &version) || run(target_argv, 0, 0, &target))
      die("cannot query compiler %s", command);
    while (target.n && strchr(" \t\r\n", target.s[target.n - 1])) target.s[--target.n] = 0;
    if (sha_file(executable, hash)) die("cannot read %s", executable);
    buf_fmt(&record, "path %s\nsha256 %s\ntarget %s\nhost %s\nversion\n%s", executable, hash,
            buf_text(&target), host_platform(), buf_text(&version));
    {
      char *temporary = xmalloc(strlen(path) + 32);
      sprintf(temporary, "%s.%ld.tmp", path, (long)getpid());
      write_file(temporary, record.s, record.n);
      if (replace_path(temporary, path)) die("cannot write %s", path);
      free(temporary);
    }
    free(version.s); free(target.s); free(record.s);
  }
  text = read_file(path, NULL);
  unlock_path(&lock);
  if (!text) die("cannot read %s", path);
  free(executable); free(seed.s); free(compilers.s); free(path); free(lock_file);
  return text;
}

/* ------------------------------------------------------------------ */
/* Fortran and preprocessor text, matching the Python regular expressions. */

/* Python's \s and \w for latin-1 text (Unicode str semantics). */
static int py_space(unsigned char c)
{
  return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == 0x0b || c == 0x0c ||
         (c >= 0x1c && c <= 0x1f) || c == 0x85 || c == 0xa0;
}

static int py_word(unsigned char c)
{
  if (c < 0x80) return isalnum(c) || c == '_';
  return c == 0xaa || c == 0xb2 || c == 0xb3 || c == 0xb5 || c == 0xb9 || c == 0xba ||
         (c >= 0xbc && c <= 0xbe) || (c >= 0xc0 && c != 0xd7 && c != 0xf7);
}

/* Length of keyword if text at p starts with it (ASCII, ignoring case). */
static size_t keyword(const char *t, size_t n, size_t p, const char *word)
{
  size_t i;
  for (i = 0; word[i]; i++)
    if (p + i >= n || tolower((unsigned char)t[p + i]) != word[i]) return 0;
  return i;
}

static size_t skip_space(const char *t, size_t n, size_t p)
{
  while (p < n && py_space((unsigned char)t[p])) p++;
  return p;
}

typedef int (*Matcher)(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end);

/* (?im)^\s*module\s+(?!procedure\b|function\b|subroutine\b)(\w+) */
static int match_module(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end)
{
  static const char *const excluded[] = {"procedure", "function", "subroutine", NULL};
  const char *const *word;
  size_t q = skip_space(t, n, p), k = keyword(t, n, q, "module");
  if (!k) return 0;
  q += k;
  if (q >= n || !py_space((unsigned char)t[q])) return 0;
  q = skip_space(t, n, q);
  for (word = excluded; *word; word++) {
    k = keyword(t, n, q, *word);
    if (k && (q + k >= n || !py_word((unsigned char)t[q + k]))) return 0;
  }
  *from = q;
  while (q < n && py_word((unsigned char)t[q])) q++;
  if (q == *from) return 0;
  *to = *end = q;
  return 1;
}

/* (?im)^\s*use\b\s*(?:,\s*(?:non_)?intrinsic\s*)?(?:::\s*)?(\w+) */
static int match_use(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end)
{
  size_t q = skip_space(t, n, p), k = keyword(t, n, q, "use"), r;
  if (!k) return 0;
  q += k;
  if (q < n && py_word((unsigned char)t[q])) return 0;
  q = skip_space(t, n, q);
  if (q < n && t[q] == ',') {
    r = skip_space(t, n, q + 1);
    r += keyword(t, n, r, "non_");
    k = keyword(t, n, r, "intrinsic");
    if (k) q = skip_space(t, n, r + k);
  }
  if (q + 1 < n && t[q] == ':' && t[q + 1] == ':') q = skip_space(t, n, q + 2);
  *from = q;
  while (q < n && py_word((unsigned char)t[q])) q++;
  if (q == *from) return 0;
  *to = *end = q;
  return 1;
}

/* (?im)^\s*include\s*['"]([^'"]+)['"], optionally after #\s* */
static int match_include_common(const char *t, size_t n, size_t p, int hash, size_t *from, size_t *to,
                                size_t *end)
{
  size_t q = skip_space(t, n, p), k;
  if (hash && q < n && t[q] == '#' && keyword(t, n, skip_space(t, n, q + 1), "include"))
    q = skip_space(t, n, q + 1);
  k = keyword(t, n, q, "include");
  if (!k) return 0;
  q = skip_space(t, n, q + k);
  if (q >= n || (t[q] != '\'' && t[q] != '"')) return 0;
  *from = ++q;
  while (q < n && t[q] != '\'' && t[q] != '"') q++;
  if (q == *from || q >= n) return 0;
  *to = q;
  *end = q + 1;
  return 1;
}

static int match_include(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end)
{
  return match_include_common(t, n, p, 0, from, to, end);
}

static int match_hash_include(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end)
{
  return match_include_common(t, n, p, 1, from, to, end);
}

/* (?m)^#\s+\d+\s+"([^"]+)" -- a preprocessor line marker */
static int match_marker(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end)
{
  size_t q = p;
  if (q >= n || t[q] != '#') return 0;
  q++;
  if (q >= n || !py_space((unsigned char)t[q])) return 0;
  q = skip_space(t, n, q);
  if (q >= n || !isdigit((unsigned char)t[q])) return 0;
  while (q < n && isdigit((unsigned char)t[q])) q++;
  if (q >= n || !py_space((unsigned char)t[q])) return 0;
  q = skip_space(t, n, q);
  if (q >= n || t[q] != '"') return 0;
  *from = ++q;
  while (q < n && t[q] != '"') q++;
  if (q == *from || q >= n) return 0;
  *to = q;
  *end = q + 1;
  return 1;
}

/* (?im)^\s*submodule\b */
static int match_submodule(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end)
{
  size_t q = skip_space(t, n, p), k = keyword(t, n, q, "submodule");
  if (!k || (q + k < n && py_word((unsigned char)t[q + k]))) return 0;
  *from = *to = *end = q + k;
  return 1;
}

/* (?m)^\s*# */
static int match_directive(const char *t, size_t n, size_t p, size_t *from, size_t *to, size_t *end)
{
  size_t q = skip_space(t, n, p);
  if (q >= n || t[q] != '#') return 0;
  *from = *to = *end = q + 1;
  return 1;
}

typedef struct { size_t start, from, to, end; } Match;

/*
 * Python's finditer for a pattern anchored at line starts (re.M): try each
 * line start in turn, and resume after the end of each match.
 */
static size_t find_all(const char *t, size_t n, Matcher m, Match **out)
{
  size_t p = 0, count = 0, cap = 0;
  *out = NULL;
  while (p <= n) {
    Match found;
    if (m(t, n, p, &found.from, &found.to, &found.end)) {
      found.start = p;
      if (count == cap) { cap = cap ? 2 * cap : 16; *out = xrealloc(*out, cap * sizeof **out); }
      (*out)[count++] = found;
      p = found.end;
    }
    while (p < n && t[p] != '\n') p++;
    if (p >= n) break;
    p++;
  }
  return count;
}

static void find_names(const char *t, size_t n, Matcher m, int lower, List *out)
{
  Match *matches;
  size_t count = find_all(t, n, m, &matches), i, j;
  for (i = 0; i < count; i++) {
    char *name = xstrndup(t + matches[i].from, matches[i].to - matches[i].from);
    if (lower)
      for (j = 0; name[j]; j++) name[j] = (char)tolower((unsigned char)name[j]);
    list_own(out, name);
  }
  free(matches);
}

static int has_match(const char *t, size_t n, Matcher m)
{
  Match *matches;
  size_t count = find_all(t, n, m, &matches);
  free(matches);
  return count > 0;
}

/* End of a Fortran string at t[start] (Python: '([^']|'')*'), or -1. */
static long string_end(const char *t, size_t start, size_t stop, char quote)
{
  size_t k = start + 1;
  long last_pair = -1;
  while (k < stop) {
    if (t[k] != quote) { k++; continue; }
    if (k + 1 < stop && t[k + 1] == quote) { last_pair = (long)k; k += 2; continue; }
    return (long)k;
  }
  return last_pair;
}

static int line_break(unsigned char c)
{
  return c == '\n' || c == '\r' || c == 0x0b || c == 0x0c || c == 0x1c || c == 0x1d || c == 0x1e || c == 0x85;
}

/*
 * build_dependencies.cleaned(): drop ! comments outside strings line by line
 * (str.splitlines boundaries, rejoined with \n), then join & continuations.
 */
static char *clean_fortran(const char *t, size_t n, size_t *length)
{
  Buf lines = {0}, joined = {0};
  size_t i = 0, k;
  int first = 1;
  while (i < n) {
    size_t e = i, j;
    while (e < n && !line_break((unsigned char)t[e])) e++;
    if (!first) buf_ch(&lines, '\n');
    first = 0;
    for (j = i; j < e;) {
      if (t[j] == '\'' || t[j] == '"') {
        long close = string_end(t, j, e, t[j]);
        if (close >= 0) { buf_add(&lines, t + j, (size_t)close - j + 1); j = (size_t)close + 1; continue; }
      } else if (t[j] == '!') {
        break;
      }
      buf_ch(&lines, t[j++]);
    }
    if (e >= n) break;
    i = (t[e] == '\r' && e + 1 < n && t[e + 1] == '\n') ? e + 2 : e + 1;
  }
  buf_text(&lines);
  for (k = 0; k < lines.n;) {
    if (lines.s[k] == '&') {
      size_t r = k + 1;
      int newline = 0;
      while (r < lines.n && py_space((unsigned char)lines.s[r])) newline |= lines.s[r++] == '\n';
      if (newline) {
        if (r < lines.n && lines.s[r] == '&') r++;
        k = r;
        continue;
      }
    }
    buf_ch(&joined, lines.s[k++]);
  }
  buf_text(&joined);
  free(lines.s);
  *length = joined.n;
  return joined.s;
}

/* pathlib.Path(name).name for a line-marker path. */
static char *marker_name(const char *path, size_t n)
{
  while (n > 1 && is_sep(path[n - 1])) n--;
  {
    size_t start = n;
    if (n == 1 && is_sep(path[0])) return xstrdup("");
    while (start > 0 && !is_sep(path[start - 1])) start--;
    return xstrndup(path + start, n - start);
  }
}

/* ------------------------------------------------------------------ */
/* prepare                                                             */

typedef struct {
  char *source;      /* resolved manifest path */
  char *local;       /* path compiled by Make */
  int fortran;
  List includes;     /* resolved, sorted as pathlib sorts */
  List modules;      /* provided, lower case */
  List uses;         /* lower case */
} Unit;

typedef struct {
  const char *root;
  List fc, fflags;
  char *cwd;
  List included;     /* collected by expand_fortran */
} Prepare;

/* pathlib ordering: compare path components, not raw strings. */
static int compare_paths(const void *a, const void *b)
{
  const char *x = *(char *const *)a, *y = *(char *const *)b;
  for (;;) {
    size_t i = 0, j = 0;
    int c;
    while (x[i] && x[i] != '/') i++;
    while (y[j] && y[j] != '/') j++;
    c = strncmp(x, y, i < j ? i : j);
    if (c) return c;
    if (i != j) return i < j ? -1 : 1;
    if (!x[i] || !y[j]) return (x[i] != 0) - (y[j] != 0);
    x += i + 1;
    y += j + 1;
  }
}

static int capture_preprocessed(const List *compiler, const List *flags, const char *source, Buf *out,
                                int quiet)
{
  List argv = {0};
  size_t i;
  int code;
  for (i = 0; i < compiler->n; i++) list_own(&argv, compiler->v[i]);
  for (i = 0; i < flags->n; i++) list_own(&argv, flags->v[i]);
  list_own(&argv, "-E");
  list_own(&argv, (char *)source);
  code = run_list(&argv, 0, quiet, out);
  free(argv.v);
  return code;
}

/* build_dependencies.fortran_text(): expand INCLUDEs in native search order. */
static void expand_fortran(Prepare *ctx, const char *path, List *ancestors, Buf *out)
{
  char *source = resolve(path), *raw, *text, *cleaned, *parent;
  size_t n, cleaned_n, i;
  List names = {0};
  Buf preprocessed = {0};
  if (list_find(ancestors, source) >= 0) die("recursive Fortran INCLUDE: %s", source);
  raw = read_file(source, &n);
  if (!raw) die("cannot read %s", source);
  text = raw;
  if (has_match(raw, n, match_directive)) {
    Match *markers;
    size_t count;
    if (capture_preprocessed(&ctx->fc, &ctx->fflags, source, &preprocessed, 0))
      die("cannot preprocess %s", source);
    text = preprocessed.s;
    n = preprocessed.n;
    count = find_all(text, n, match_marker, &markers);
    for (i = 0; i < count; i++) {
      char *name = xstrndup(text + markers[i].from, markers[i].to - markers[i].from);
      if (is_file(name)) {
        char *dependency = resolve(name);
        if (strcmp(dependency, source)) list_add_unique(&ctx->included, dependency);
        free(dependency);
      }
      free(name);
    }
    free(markers);
  }
  cleaned = clean_fortran(text, n, &cleaned_n);
  buf_add(out, cleaned, cleaned_n);
  buf_ch(out, '\n');
  find_names(cleaned, cleaned_n, match_include, 0, &names);
  parent = dir_name(source);
  list_add(ancestors, source);
  for (i = 0; i < names.n; i++) {
    char *candidates[2], *target = NULL, *resolved;
    int c;
    candidates[0] = join(parent, names.v[i]);
    candidates[1] = join(ctx->cwd, names.v[i]);
    for (c = 0; c < 2 && !target; c++)
      if (is_file(candidates[c])) target = candidates[c];
    if (!target) die("missing Fortran INCLUDE %s in %s", names.v[i], source);
    resolved = resolve(target);
    list_add_unique(&ctx->included, resolved);
    if (i) buf_ch(out, '\n');
    expand_fortran(ctx, resolved, ancestors, out);
    free(resolved);
    free(candidates[0]);
    free(candidates[1]);
  }
  free(ancestors->v[--ancestors->n]);
  free(parent);
  free(cleaned);
  free(raw);
  free(preprocessed.s);
  free(source);
}

/* Escape a path for a Make prerequisite list, as make_path() did. */
static void make_path(Buf *out, const char *path)
{
  for (; *path; path++) {
    if (*path == '\\') buf_ch(out, '/');
    else if (*path == '$') buf_str(out, "$$");
    else if (*path == '#') buf_str(out, "\\#");
    else if (*path == ' ') buf_str(out, "\\ ");
    else buf_ch(out, *path);
  }
}

static char *file_hash_or_dash(const char *path)
{
  char hex[65];
  if (!is_file(path) || sha_file(path, hex)) return xstrdup("-");
  return xstrdup(hex);
}

/* Read a list of names, one per line (or a former JSON list of strings). */
static void read_names(const char *path, List *out)
{
  size_t n, i = 0;
  char *text = read_file(path, &n);
  if (!text) return;
  if (n && text[0] == '[') {
    while (i < n) {
      size_t start;
      while (i < n && text[i] != '"') i++;
      start = ++i;
      while (i < n && text[i] != '"') i++;
      if (i < n) list_own(out, xstrndup(text + start, i - start));
      i++;
    }
  } else {
    while (i < n) {
      size_t start = i;
      while (i < n && text[i] != '\n') i++;
      if (i > start) list_own(out, xstrndup(text + start, i - start));
      i++;
    }
  }
  free(text);
}

static int prepare(const char *root, const char *manifest_arg, int native)
{
  static const char *const unit_suffixes[] = {".f90", ".f", ".for", ".c", ".cpp", NULL};
  static const char *const fortran_suffixes[] = {".f90", ".f", ".for", NULL};
  static const char *const scanned_suffixes[] = {".f90", ".f", ".for", ".c", ".cpp", ".h", ".inc", NULL};
  Prepare ctx;
  char *manifest = resolve(manifest_arg), *manifest_dir = dir_name(manifest), *text, *headers_file;
  char *makefile_path, *tool_source, *cache, settings_key[65], helper_tool[65], helper_rules[65];
  List paths = {0}, headers = {0}, names = {0}, scan_old = {0};
  Unit *units;
  size_t unit_count = 0, i, j, n;
  Buf deps = {0}, scan = {0}, link = {0}, link_inputs = {0}, owned = {0};
  List providers_names = {0}, providers_units = {0}, previous_modules = {0};
  const char *value;

  memset(&ctx, 0, sizeof ctx);
  ctx.root = root;
  ctx.cwd = current_directory();

  /* Selected paths, unique by basename. */
  text = read_file(manifest, &n);
  if (!text) die("cannot read %s", manifest);
  split_words(text, &names);
  free(text);
  for (i = 0; i < names.n; i++) {
    char *joined = join(manifest_dir, names.v[i]);
    list_own(&paths, resolve(joined));
    free(joined);
  }
  for (i = 0; i < paths.n; i++)
    for (j = i + 1; j < paths.n; j++)
      if (!strcmp(base_name(paths.v[i]), base_name(paths.v[j])))
        die("duplicate selected basename in %s", manifest);

  /* Converted Fortran includes keep .f90 names but are never compiled. */
  headers_file = join(root, "bin/fortran_includes.txt");
  text = read_file(headers_file, &n);
  if (!text) die("cannot read %s", headers_file);
  {
    List entries = {0};
    split_words(text, &entries);
    for (i = 0; i < entries.n; i++) {
      char *joined = join(root, entries.v[i]);
      list_own(&headers, resolve(joined));
      free(joined);
    }
  }
  free(text);
  units = xmalloc((paths.n + 1) * sizeof *units);
  for (i = 0; i < paths.n; i++) {
    if (list_find(&headers, paths.v[i]) >= 0 || !suffix_is(paths.v[i], unit_suffixes)) continue;
    memset(&units[unit_count], 0, sizeof *units);
    units[unit_count].source = paths.v[i];
    units[unit_count].fortran = suffix_is(paths.v[i], fortran_suffixes);
    units[unit_count].local = native ? xstrdup(paths.v[i]) : xstrdup(base_name(paths.v[i]));
    unit_count++;
  }

  /* Native sources must find the same include the flat view selected. */
  if (native) {
    for (i = 0; i < paths.n; i++) {
      List found = {0};
      char *parent;
      if (!suffix_is(paths.v[i], scanned_suffixes)) continue;
      text = read_file(paths.v[i], &n);
      if (!text) die("cannot read %s", paths.v[i]);
      find_names(text, n, match_hash_include, 0, &found);
      parent = dir_name(paths.v[i]);
      for (j = 0; j < found.n; j++) {
        char *adjacent = join(parent, found.v[j]), a[65], b[65];
        size_t k;
        for (k = 0; k < paths.n; k++)
          if (!strcmp(base_name(paths.v[k]), found.v[j])) break;
        if (k < paths.n && is_file(adjacent) &&
            (sha_file(adjacent, a) || sha_file(paths.v[k], b) || strcmp(a, b)))
          die("native-source lookup would change selected include %s: %s", found.v[j], paths.v[i]);
        free(adjacent);
      }
      free(parent);
      free(text);
    }
  }

  /* Stage regular copies (never links) of includes, or of everything. */
  for (i = 0; i < paths.n; i++) {
    const char *destination = base_name(paths.v[i]);
    Times from, to;
    int is_unit = 0;
    for (j = 0; j < unit_count; j++)
      if (units[j].source == paths.v[i]) is_unit = 1;
    if (native && is_unit) continue;
    if (file_times(paths.v[i], &from)) die("missing selected file %s", paths.v[i]);
    if (file_times(destination, &to) || to.mtime != from.mtime || to.size != from.size)
      copy_file(paths.v[i], destination);
  }
  {
    const char *mode_value = native ? "native\n" : "staged\n";
    char *previous = read_file(".source-mode", NULL);
    if ((previous && strcmp(previous, mode_value)) || !exists(".dependency-format")) {
      /* Old .d files name the previous source view. */
#ifdef FVS_WINDOWS
      {
        struct _finddata_t entry;
        intptr_t handle = _findfirst("*.d", &entry);
        if (handle != -1) {
          do remove_file(entry.name); while (_findnext(handle, &entry) == 0);
          _findclose(handle);
        }
      }
#else
      {
        DIR *dir = opendir(".");
        struct dirent *entry;
        if (dir) {
          while ((entry = readdir(dir))) {
            size_t length = strlen(entry->d_name);
            if (length > 2 && !strcmp(entry->d_name + length - 2, ".d")) remove_file(entry->d_name);
          }
          closedir(dir);
        }
      }
#endif
    }
    free(previous);
    write_changed(".source-mode", mode_value);
    write_changed(".dependency-format", "receipts-v1\n");
  }
  if (native)
    for (j = 0; j < unit_count; j++) remove_file(base_name(units[j].source));

  /* Compile recipes rewrite their receipts (bug 67). An interrupted build can
     still leave an object newer than its receipt; synchronize those here. */
  for (j = 0; j < unit_count; j++) {
    char *stem = stem_of(units[j].source), *object = xmalloc(strlen(stem) + 3);
    char *receipt = xmalloc(strlen(stem) + 9);
    Times o, r;
    sprintf(object, "%s.o", stem);
    sprintf(receipt, "%s.o.stamp", stem);
    if (file_times(object, &o)) remove_file(receipt);
    else if (file_times(receipt, &r) || r.mtime < o.mtime) {
      touch(receipt);
      set_times(receipt, o.mtime, o.mtime);
    }
    free(stem); free(object); free(receipt);
  }

  /* Compiler settings: binary identity, flags and helper revisions. */
  value = getenv("FVS_SHARED_CACHE");
  cache = value && *value ? xstrdup(value) : join(root, "bin/.shared_build");
  makefile_path = join(root, "bin/makefile");
  tool_source = join(root, "bin/fvsbuild.c");
  if (sha_file(tool_source, helper_tool) || sha_file(makefile_path, helper_rules))
    die("cannot read the build rules in %s/bin", root);
  {
    const char *languages[2] = {"fortran", "c"};
    int l;
    for (l = 0; l < 2; l++) {
      List compiler = {0}, flags = {0};
      Buf command = {0}, settings = {0};
      char *identity, file[32];
      const char *prefix = getenv(l ? "CCprf" : "FCprf"), *name = getenv(l ? "CC" : "FC");
      buf_str(&command, prefix ? prefix : "");
      buf_str(&command, name ? name : (l ? "gcc" : "gfortran"));
      shell_split(command.s, &compiler);
      value = getenv(l ? "CFLAGS" : "FFLAGS");
      shell_split(value ? value : "", &flags);
      if (!compiler.n) die("no %s compiler", languages[l]);
      identity = compiler_identity(compiler.v[0], cache);
      buf_str(&settings, "fvsbuild-settings 1\ncompiler\n");
      buf_str(&settings, identity);
      buf_str(&settings, "\nend-compiler\n");
      for (i = 0; i < flags.n; i++) buf_fmt(&settings, "flag %s\n", flags.v[i]);
      buf_fmt(&settings, "helper fvsbuild.c %s\nhelper makefile %s\nnative-sources %d\n", helper_tool,
              helper_rules, native);
      sprintf(file, ".%s.flags", languages[l]);
      write_changed(file, settings.s);
      if (!l) { ctx.fc = compiler; ctx.fflags = flags; }
      free(identity);
      free(command.s);
      free(settings.s);
    }
  }

  /* Reuse a Fortran scan while every input and the settings match by content. */
  {
    char *settings_text = read_file(".fortran.flags", &n);
    if (!settings_text) die("cannot read .fortran.flags");
    sha_text(settings_text, n, settings_key);
    free(settings_text);
  }
  /* Records: unit, input (hash, path), include, module, use, end; tab-separated. */
  text = read_file(".fortran-scan", &n);
  if (text && has_prefix(text, "settings\t") && n > 73 && !strncmp(text + 9, settings_key, 64)) {
    size_t p = 0;
    while (p < n) {
      size_t e = p;
      while (e < n && text[e] != '\n') e++;
      list_own(&scan_old, xstrndup(text + p, e - p));
      p = e + 1;
    }
  }
  free(text);
  buf_fmt(&scan, "settings\t%s\n", settings_key);
  for (j = 0; j < unit_count; j++) {
    Unit *u = &units[j];
    char *key;
    size_t first = 0, last = 0;
    int reuse = 0;
    if (!u->fortran) continue;
    key = resolve(u->local);
    for (i = 0; i < scan_old.n; i++) {
      if (has_prefix(scan_old.v[i], "unit\t") && !strcmp(scan_old.v[i] + 5, key)) {
        first = i;
        for (last = i; last < scan_old.n && strcmp(scan_old.v[last], "end"); last++) continue;
        reuse = last < scan_old.n;
        break;
      }
    }
    /* Every recorded input must still hash the same. */
    for (i = first + 1; reuse && i < last; i++) {
      const char *line = scan_old.v[i], *tab;
      char *hash;
      if (!has_prefix(line, "input\t")) continue;
      tab = strchr(line + 6, '\t');
      if (!tab) { reuse = 0; break; }
      hash = file_hash_or_dash(tab + 1);
      reuse = (size_t)(tab - line - 6) == strlen(hash) && !strncmp(line + 6, hash, strlen(hash));
      free(hash);
    }
    if (reuse) {
      for (i = first; i <= last; i++) {
        const char *line = scan_old.v[i];
        if (has_prefix(line, "include\t")) list_add(&u->includes, line + 8);
        else if (has_prefix(line, "module\t")) list_add(&u->modules, line + 7);
        else if (has_prefix(line, "use\t")) list_add(&u->uses, line + 4);
        buf_fmt(&scan, "%s\n", line);
      }
    } else {
      List ancestors = {0}, inputs = {0};
      Buf expanded = {0};
      ctx.included.n = 0;
      expand_fortran(&ctx, u->local, &ancestors, &expanded);
      for (i = 0; i < ctx.included.n; i++) list_add(&u->includes, ctx.included.v[i]);
      if (u->includes.n > 1) qsort(u->includes.v, u->includes.n, sizeof *u->includes.v, compare_paths);
      find_names(buf_text(&expanded), expanded.n, match_module, 1, &u->modules);
      find_names(expanded.s, expanded.n, match_use, 1, &u->uses);
      buf_fmt(&scan, "unit\t%s\n", key);
      list_add(&inputs, key);
      for (i = 0; i < u->includes.n; i++) list_add(&inputs, u->includes.v[i]);
      for (i = 0; i < inputs.n; i++) {
        char *resolved = resolve(inputs.v[i]), *hash = file_hash_or_dash(inputs.v[i]);
        buf_fmt(&scan, "input\t%s\t%s\n", hash, resolved);
        free(resolved);
        free(hash);
      }
      for (i = 0; i < u->includes.n; i++) buf_fmt(&scan, "include\t%s\n", u->includes.v[i]);
      for (i = 0; i < u->modules.n; i++) buf_fmt(&scan, "module\t%s\n", u->modules.v[i]);
      for (i = 0; i < u->uses.n; i++) buf_fmt(&scan, "use\t%s\n", u->uses.v[i]);
      buf_str(&scan, "end\n");
      free(expanded.s);
    }
    for (i = 0; i < u->modules.n; i++) {
      long k = list_find(&providers_names, u->modules.v[i]);
      if (k >= 0 && providers_units.v[k] != u->source) die("two selected producers of module %s", u->modules.v[i]);
      if (k < 0) { list_add(&providers_names, u->modules.v[i]); list_own(&providers_units, u->source); }
    }
    free(key);
  }
  write_changed(".fortran-scan", buf_text(&scan));

  /* A removed/renamed producer must not leave an interface a later build can
     read. Never prune caller-supplied modules. */
  read_names(".module-providers", &previous_modules);
  if (!previous_modules.n) read_names(".module-providers.json", &previous_modules);
  for (i = 0; i < previous_modules.n; i++) {
    if (list_find(&providers_names, previous_modules.v[i]) < 0) {
      char *module = xmalloc(strlen(previous_modules.v[i]) + 5);
      sprintf(module, "%s.mod", previous_modules.v[i]);
      remove_file(module);
      free(module);
    }
  }
  {
    List sorted = {0};
    for (i = 0; i < providers_names.n; i++) list_add(&sorted, providers_names.v[i]);
    list_sort_unique(&sorted);
    for (i = 0; i < sorted.n; i++) buf_fmt(&owned, "%s\n", sorted.v[i]);
    write_changed(".module-providers", buf_text(&owned));
  }
  remove_file(".module-providers.json");
  remove_file(".fortran-scan.json");

  /* The bootstrap graph; the compiler's .d files record active includes. */
  buf_str(&deps, "# Generated from selected sources; the compiler records active includes in *.d.\n");
  for (j = 0; j < unit_count; j++) {
    Unit *u = &units[j];
    char *stem = stem_of(u->source), *dependency_file = xmalloc(strlen(stem) + 3);
    List uses = {0};
    sprintf(dependency_file, "%s.d", stem);
    buf_fmt(&link_inputs, "%s.o\n", stem);
    make_path(&deps, stem);
    buf_str(&deps, ".o.stamp: ");
    make_path(&deps, u->local);
    buf_str(&deps, u->fortran ? " .fortran.flags" : " .c.flags");
    /* Bootstrap header dependencies disappear once the compiler supplies its
       active include closure. New USEs are always ordered. */
    if (!exists(dependency_file))
      for (i = 0; i < u->includes.n; i++) { buf_ch(&deps, ' '); make_path(&deps, u->includes.v[i]); }
    for (i = 0; i < u->uses.n; i++) list_add(&uses, u->uses.v[i]);
    list_sort_unique(&uses);
    for (i = 0; i < uses.n; i++) {
      long k = list_find(&providers_names, uses.v[i]);
      if (k >= 0 && providers_units.v[k] != u->source) buf_fmt(&deps, " %s.mod", uses.v[i]);
    }
    buf_ch(&deps, '\n');
    for (i = 0; i < u->modules.n; i++) {
      /* .mod is a side effect of one object compile; restore it if missing. */
      int nvel = strstr(u->source, "/volume/NVEL/") != NULL;
      buf_fmt(&deps, "%s.mod: ", u->modules.v[i]);
      make_path(&deps, stem);
      buf_str(&deps, ".o\n\t@test -f $@ || ");
      if (nvel) buf_str(&deps, "$(FVS_NVEL_COMPILE) ");
      buf_fmt(&deps, "$(FCprf)$(FC) $(FFLAGS) $(call FVS_DEPFLAGS,%s.o) -c -o ", stem);
      make_path(&deps, stem);
      buf_str(&deps, ".o ");
      make_path(&deps, u->local);
      buf_ch(&deps, '\n');
    }
    free(stem);
    free(dependency_file);
  }
  /* Preserve manifest order, and detect membership changes even when a
     removed object's timestamp can no longer trigger a relink. */
  write_changed(".link-inputs", buf_text(&link_inputs));
  {
    static const char *const names_link[] = {"LD", "FCprf", "DBLINK", "OSARCH", "SLIBSUFX", "PRGSUFX", NULL};
    const char *const *name;
    buf_str(&link, "fvsbuild-link 1\n");
    for (name = names_link; *name; name++) {
      value = getenv(*name);
      buf_fmt(&link, "%s=%s\n", *name, value ? value : "");
    }
    buf_fmt(&link, "rules=%s\n", helper_rules);
    write_changed(".link.flags", link.s);
  }
  write_changed("dependencies.mk", deps.s);
  puts("ready");
  return 0;
}

/* ------------------------------------------------------------------ */
/* compile                                                             */

/* Values of an option given as "-I dir" or "-Idir". */
static void option_paths(char *const *argv, size_t argc, const char *option, List *out)
{
  size_t i, n = strlen(option);
  for (i = 0; i < argc; i++) {
    if (!strcmp(argv[i], option) && i + 1 < argc) list_add(out, argv[i + 1]);
    else if (!strncmp(argv[i], option, n) && argv[i][n]) list_add(out, argv[i] + n);
  }
}

/*
 * Arguments without output destinations. For the identity, search paths and
 * definitions are also dropped: the actual preprocessed text, resolved
 * headers and input module bytes stand in for them.
 */
static void without_outputs(char *const *argv, size_t argc, int identity, List *out)
{
  static const char *const pairs_identity[] = {"-MF", "-MT", "-MQ", "-I", "-J", "-isystem", "-iquote", "-D", "-U", NULL};
  static const char *const joined_identity[] = {"-MF", "-MT", "-MQ", "-I", "-J", "-D", "-U", NULL};
  static const char *const source_suffixes[] = {".c", ".f90", NULL};
  size_t i;
  for (i = 0; i < argc; i++) {
    const char *a = argv[i];
    const char *const *p;
    int pair = !strcmp(a, "-o"), skip = 0;
    if (identity)
      for (p = pairs_identity; *p; p++) pair |= !strcmp(a, *p);
    if (pair) { i++; continue; }
    if (!strcmp(a, "-c")) continue;
    if (identity && (!strcmp(a, "-MD") || !strcmp(a, "-MMD") || !strcmp(a, "-MP"))) continue;
    if (identity)
      for (p = joined_identity; *p; p++) skip |= has_prefix(a, *p) && strcmp(a, *p);
    if (skip) continue;
    if (identity && suffix_is(a, source_suffixes)) list_add(out, base_name(a));
    else list_add(out, a);
  }
}

static int any_unsupported(char *const *argv, size_t argc)
{
  static const char *const options[] = {"-fprofile", "-fauto-profile", "-fplugin", "-gsplit-dwarf",
                                        "-save-temps", "-fstack-usage", "-ftest-coverage", "--coverage",
                                        "-include-pch", "-fsyntax-only", "@", NULL};
  size_t i;
  const char *const *p;
  for (i = 1; i < argc; i++)
    for (p = options; *p; p++)
      if (has_prefix(argv[i], *p)) return 1;
  return 0;
}

/* Header bytes that expand to the compile time cannot be reused. */
static int volatile_text(const char *t, size_t n)
{
  static const char *const words[] = {"__DATE__", "__TIME__", "__TIMESTAMP__", NULL};
  const char *const *w;
  size_t i;
  for (w = words; *w; w++) {
    size_t length = strlen(*w);
    for (i = 0; i + length <= n; i++) {
      unsigned char before = i ? (unsigned char)t[i - 1] : ' ';
      unsigned char after = i + length < n ? (unsigned char)t[i + length] : ' ';
      if (memcmp(t + i, *w, length)) continue;
      if ((isalnum(before) || before == '_') || (isalnum(after) || after == '_')) continue;
      return 1;
    }
  }
  return 0;
}

/* shared_compile.fortran_inputs(): INCLUDE files and input module bytes. */
static int fortran_inputs(const char *source, char *const *args, size_t argc, List *inputs, List *provides)
{
  List files = {0}, texts = {0}, search = {0}, uses = {0}, queue = {0}, module_dirs = {0}, modules = {0};
  Buf all = {0};
  char *cwd = current_directory();
  size_t i, j;
  int ok = 1;
  option_paths(args, argc, "-I", &search);
  list_own(&queue, resolve(source));
  /* Depth-first, in the order Python's recursive visit() used. */
  {
    List stack = {0};
    list_add(&stack, queue.v[0]);
    while (stack.n && ok) {
      char *path = stack.v[--stack.n], hash[65], *text, *parent;
      size_t n;
      List names = {0};
      if (list_find(&files, path) >= 0) { free(path); continue; }
      if (sha_file(path, hash)) { ok = 0; free(path); break; }
      list_add(&files, path);
      list_add(&files, hash);
      text = read_file(path, &n);
      list_own(&texts, text);
      find_names(text, n, match_include, 0, &names);
      parent = dir_name(path);
      /* visit() recursed immediately; push in reverse to keep that order. */
      {
        List next = {0};
        for (i = 0; i < names.n && ok; i++) {
          List candidates = {0}, hashes = {0};
          char *included = NULL;
          list_own(&candidates, join(parent, names.v[i]));
          for (j = 0; j < search.n; j++) {
            char *dir = search.v[j];
            list_own(&candidates, join(dir, names.v[i]));
          }
          list_own(&candidates, join(cwd, names.v[i]));
          for (j = 0; j < candidates.n; j++) {
            char h[65];
            if (!is_file(candidates.v[j])) continue;
            if (!included) included = candidates.v[j];
            if (!sha_file(candidates.v[j], h)) list_add_unique(&hashes, h);
          }
          if (!included || hashes.n > 1) { ok = 0; break; }
          {
            size_t m;
            char *included_text = read_file(included, &m);
            if (!included_text || has_match(included_text, m, match_directive)) ok = 0;
            free(included_text);
          }
          if (ok) list_own(&next, resolve(included));
        }
        for (i = next.n; i > 0; i--) list_own(&stack, next.v[i - 1]);
        free(next.v);
      }
      free(parent);
      free(path);
    }
    free(stack.v);
  }
  if (ok) {
    for (i = 0; i < texts.n; i++) { if (i) buf_ch(&all, '\n'); buf_str(&all, texts.v[i]); }
    buf_text(&all);
    if (has_match(all.s, all.n, match_submodule)) ok = 0;
  }
  if (ok) {
    find_names(all.s, all.n, match_module, 0, &modules);
    find_names(all.s, all.n, match_use, 0, &uses);
    for (i = 0; i < files.n; i += 2) {
      Buf key = {0};
      buf_fmt(&key, "%s:%s=%s", base_name(files.v[i]), files.v[i + 1], files.v[i + 1]);
      list_own(inputs, key.s);
    }
    list_own(&module_dirs, xstrdup(cwd));
    for (i = 0; i < search.n; i++) list_add(&module_dirs, search.v[i]);
    option_paths(args, argc, "-J", &module_dirs);
    for (i = 0; i < uses.n && ok; i++) {
      char *lower = xstrdup(uses.v[i]), *found = NULL;
      List hashes = {0};
      Buf name = {0}, entry = {0};
      for (j = 0; lower[j]; j++) lower[j] = (char)tolower((unsigned char)lower[j]);
      buf_fmt(&name, "%s.mod", lower);
      for (j = 0; j < module_dirs.n; j++) {
        char *candidate = join(module_dirs.v[j], name.s), h[65];
        if (is_file(candidate)) {
          if (!found) found = xstrdup(candidate);
          if (!sha_file(candidate, h)) list_add_unique(&hashes, h);
        }
        free(candidate);
      }
      if (!found || hashes.n > 1) ok = 0;
      else {
        buf_fmt(&entry, "%s=%s", name.s, hashes.v[0]);
        list_own(inputs, entry.s);
      }
      free(found); free(lower); free(name.s);
    }
    for (i = 0; i < modules.n; i++) {
      char *lower = xstrdup(modules.v[i]);
      Buf name = {0};
      for (j = 0; lower[j]; j++) lower[j] = (char)tolower((unsigned char)lower[j]);
      buf_fmt(&name, "%s.mod", lower);
      list_own(provides, name.s);
      free(lower);
    }
    list_sort_unique(inputs);
  }
  free(cwd);
  free(all.s);
  return ok;
}

static char *tool_digest(const char *argv0)
{
  char hex[65], *self;
#ifdef FVS_WINDOWS
  char module[MAX_PATH + 1];
  DWORD n = GetModuleFileNameA(NULL, module, MAX_PATH);
  module[n] = 0;
  self = xstrdup(module);
  (void)argv0;
#else
  self = strchr(argv0, '/') ? xstrdup(argv0) : which(argv0);
#endif
  if (!self || sha_file(self, hex)) return NULL;
  free(self);
  return xstrdup(hex);
}

/* Publish a complete file; never modify an inode shared with the cache. */
static void install(const char *from, const char *to, int link)
{
  char *parent = dir_name(to), *temporary = xmalloc(strlen(to) + 32);
  make_dirs(parent);
  sprintf(temporary, "%s.%ld.tmp", to, (long)getpid());
  remove_file(temporary);
  if (!link || hard_link(from, temporary)) copy_file(from, temporary);
  if (replace_path(temporary, to)) die("cannot install %s", to);
  remove_file(temporary);
  free(parent);
  free(temporary);
}

static int compile_cached(char **command, size_t argc, const char *cache, const char *kind, int hardlink,
                          const char *argv0)
{
  static const char *const source_suffixes[] = {".c", ".f90", NULL};
  const char *source = NULL, *output = NULL;
  char *identity_text, key[65], source_hash[65], *launcher, *entry, *lock_file;
  List pass = {0}, flags = {0}, headers = {0}, inputs = {0}, modules = {0}, seen = {0};
  Buf preprocessed = {0}, normalized = {0}, stamp = {0};
  Match *markers;
  size_t count, i, last = 0;
  Sha sha;
  Lock lock;
  int valid = 0, code;
  char *module_directory, *metadata, *stored_object;
  for (i = argc; i > 0; i--)
    if (suffix_is(command[i - 1], source_suffixes)) { source = command[i - 1]; break; }
  for (i = 0; i + 1 < argc; i++)
    if (!strcmp(command[i], "-o")) output = command[i + 1];
  /* This also protects old hard links when the compiler or eligibility changes. */
  remove_file(output);
  /* Profiles/plugins are mutable inputs; extra outputs are not audited. */
  if (any_unsupported(command, argc)) return run(command, 0, 0, NULL);
  identity_text = compiler_identity(command[0], cache);
  {
    const char *version = strstr(identity_text, "\nversion\n");
    if (!version || (!strstr(version, "GNU") && !strstr(version, "gcc (")))
      return run(command, 0, 0, NULL);
  }
  /* Keep -MD/-MF/-MT on this pass: a CMake cache hit still needs a fresh
     dependency file naming this consumer's object. */
  list_add(&pass, command[0]);
  without_outputs(command + 1, argc - 1, 0, &pass);
  list_add(&pass, "-E");
  if (run_list(&pass, 0, 1, &preprocessed)) return run(command, 0, 0, NULL);
  count = find_all(preprocessed.s, preprocessed.n, match_marker, &markers);
  for (i = 0; i < count; i++) {
    char *path = xstrndup(preprocessed.s + markers[i].from, markers[i].to - markers[i].from);
    if (is_file(path) && list_find(&seen, path) < 0) {
      size_t n;
      char *bytes = read_file(path, &n), hash[65], *gch = xmalloc(strlen(path) + 5);
      Buf entry_name = {0};
      sprintf(gch, "%s.gch", path);
      if (!bytes || volatile_text(bytes, n) || exists(gch)) {
        free(bytes); free(gch); free(path); free(markers);
        return run(command, 0, 0, NULL);
      }
      sha_text(bytes, n, hash);
      list_add(&seen, path);
      buf_fmt(&entry_name, "%s:%s", base_name(path), hash);
      list_own(&headers, entry_name.s);
      free(bytes);
      free(gch);
    }
    /* Directory names in line markers are not tokens. __FILE__ expansions
       remain in the text, and so still distinguish path-sensitive code. */
    {
      const char *line = preprocessed.s + markers[i].start;
      size_t path_n = markers[i].to - markers[i].from, k;
      const char *p = preprocessed.s + markers[i].from;
      char *replacement = (path_n >= 2 && p[path_n - 1] == '/' && p[path_n - 2] == '/') ?
                          xstrdup("<working-directory>") : marker_name(p, path_n);
      buf_add(&normalized, preprocessed.s + last, markers[i].start - last);
      for (k = 0; line + k < preprocessed.s + markers[i].end;) {
        if (line + k + path_n <= preprocessed.s + markers[i].end && !memcmp(line + k, p, path_n)) {
          buf_str(&normalized, replacement);
          k += path_n;
        } else {
          buf_ch(&normalized, line[k++]);
        }
      }
      last = markers[i].end;
      free(replacement);
    }
    free(path);
  }
  buf_add(&normalized, preprocessed.s + last, preprocessed.n - last);
  buf_text(&normalized);
  free(markers);
  list_sort_unique(&headers);
  launcher = tool_digest(argv0);
  if (!launcher || !source || sha_file(source, source_hash)) return run(command, 0, 0, NULL);
  module_directory = NULL;
  {
    List directories = {0};
    option_paths(command + 1, argc - 1, "-J", &directories);
    module_directory = directories.n ? xstrdup(directories.v[0]) : current_directory();
  }
  if (!strcmp(kind, "nvel") && !fortran_inputs(source, command + 1, argc - 1, &inputs, &modules)) {
    /* A dependency outside the audited set compiles normally. */
    remove_file(output);
    return run(command, 0, 0, NULL);
  }
  without_outputs(command + 1, argc - 1, 1, &flags);
  sha_init(&sha);
  sha_field(&sha, "fvsbuild-compile", "1", 1);
  sha_field(&sha, "compiler", identity_text, strlen(identity_text));
  for (i = 0; i < flags.n; i++) sha_field(&sha, "flag", flags.v[i], strlen(flags.v[i]));
  sha_field(&sha, "source", source_hash, 64);
  sha_field(&sha, "preprocessed", normalized.s, normalized.n);
  for (i = 0; i < headers.n; i++) sha_field(&sha, "header", headers.v[i], strlen(headers.v[i]));
  sha_field(&sha, "launcher", launcher, strlen(launcher));
  sha_field(&sha, "kind", kind, strlen(kind));
  for (i = 0; i < inputs.n; i++) sha_field(&sha, "fortran-input", inputs.v[i], strlen(inputs.v[i]));
  sha_final(&sha, key);
  {
    char *kind_dir = join(cache, kind);
    entry = join(kind_dir, key);
    free(kind_dir);
  }
  lock_file = xmalloc(strlen(entry) + 6);
  sprintf(lock_file, "%s.lock", entry);
  metadata = join(entry, "entry.json");
  stored_object = join(entry, "object.o");
  lock_path(&lock, lock_file);
  {
    char *record = read_file(metadata, NULL), hash[65];
    const char *p = record ? strstr(record, "{\"object\": \"") : NULL;
    valid = p && is_file(stored_object) && !sha_file(stored_object, hash) && !strncmp(p + 12, hash, 64);
    p = valid ? strstr(record, "\"modules\": {") : NULL;
    if (valid && !p) valid = 0;
    if (p) p = strchr(p, '{') + 1;
    while (valid) {
      const char *name_end;
      char *name, *path;
      while (*p == ' ' || *p == ',') p++;
      if (*p == '}') break;
      name_end = *p == '"' ? strchr(p + 1, '"') : NULL;
      if (!name_end || strncmp(name_end, "\": \"", 4) || strlen(name_end + 4) < 65) { valid = 0; break; }
      name = xstrndup(p + 1, (size_t)(name_end - p - 1));
      path = join(entry, name);
      valid = !sha_file(path, hash) && !strncmp(name_end + 4, hash, 64) && name_end[68] == '"';
      p = name_end + 69;
      free(name);
      free(path);
    }
    free(record);
  }
  if (!valid) {
    Buf message = {0}, record = {0};
    char *parent, *scratch, *object_copy, object_hash[65];
    /* A previous cache hit may have installed a hard link here. */
    remove_file(output);
    buf_fmt(&message, "SHARED MISS %s: ", base_name(source));
    shell_join(&message, command);
    puts(message.s);
    fflush(stdout);
    code = run(command, 0, 0, NULL);
    if (code) { unlock_path(&lock); return code; }
    parent = dir_name(entry);
    make_dirs(parent);
    {
      Buf prefix = {0};
      buf_fmt(&prefix, "%s.", key);
      scratch = make_temp_dir(parent, prefix.s);
      free(prefix.s);
    }
    object_copy = join(scratch, "object.o");
    copy_file(output, object_copy);
#ifdef FVS_WINDOWS
    _chmod(object_copy, _S_IREAD | _S_IWRITE);
#else
    chmod(object_copy, 0444);
#endif
    if (sha_file(output, object_hash)) die("cannot read %s", output);
    buf_fmt(&record, "{\"object\": \"%s\", \"modules\": {", object_hash);
    for (i = 0; i < modules.n; i++) {
      char *from = join(module_directory, modules.v[i]), *to = join(scratch, modules.v[i]), hash[65];
      copy_file(from, to);
      if (sha_file(to, hash)) die("cannot read %s", to);
      buf_fmt(&record, "%s\"%s\": \"%s\"", i ? ", " : "", modules.v[i], hash);
      free(from);
      free(to);
    }
    buf_str(&record, "}}");
    {
      char *record_path = join(scratch, "entry.json");
      write_file(record_path, record.s, record.n);
      free(record_path);
    }
    if (exists(entry)) remove_tree(entry);
    if (replace_path(scratch, entry)) { remove_tree(scratch); die("cannot publish %s", entry); }
    free(parent); free(scratch); free(object_copy); free(message.s); free(record.s);
  } else {
    printf("SHARED HIT %s\n", base_name(source));
    fflush(stdout);
    /* GNU's Fortran preprocessing pass does not emit INCLUDE/USE
       dependencies. Recreate this consumer's .d file on a cache hit. */
    if (!strcmp(kind, "nvel")) {
      int has_mf = 0;
      for (i = 1; i < argc; i++) has_mf |= !strcmp(command[i], "-MF");
      if (has_mf) {
        List dependencies = {0};
        list_add(&dependencies, command[0]);
        without_outputs(command + 1, argc - 1, 0, &dependencies);
        list_add(&dependencies, "-M");
        code = run_list(&dependencies, 1, 0, NULL);
        if (code) { unlock_path(&lock); return code; }
      }
    }
  }
  for (i = 0; i < modules.n; i++) {
    char *destination = join(module_directory, modules.v[i]), *cached = join(entry, modules.v[i]);
    char a[65], b[65];
    if (!is_file(destination) || sha_file(destination, a) || sha_file(cached, b) || strcmp(a, b)) {
      install(cached, destination, 0);
      set_times(destination, -1, -1);
    }
    free(destination);
    free(cached);
  }
  {
    int linked = hardlink;
#ifdef FVS_WINDOWS
    linked = 0;
#endif
    install(stored_object, output, linked);
    /* CMake owns object timestamps; its dependency machinery must see this
       consumer's completed compilation as current. */
    if (!linked) set_times(output, -1, -1);
  }
  /* Make retains immutable hard links without changing other consumers'
     object times; its link rules also depend on this local receipt. */
  buf_fmt(&stamp, "%s.stamp", output);
  touch(stamp.s);
  unlock_path(&lock);
  return 0;
}

static int compile_main(int argc, char **argv)
{
  static const char *const c_suffix[] = {".c", NULL};
  static const char *const f90_suffix[] = {".f90", NULL};
  const char *cache = NULL, *groups = "sqlite", *kind = NULL;
  int hardlink = 0, i, has_c = 0, has_o = 0, eligible;
  char **command = NULL, *source = NULL, *resolved_cache;
  size_t command_count = 0;
  for (i = 2; i < argc; i++) {
    if (!strcmp(argv[i], "--cache-dir") && i + 1 < argc) cache = argv[++i];
    else if (!strcmp(argv[i], "--groups") && i + 1 < argc) groups = argv[++i];
    else if (!strcmp(argv[i], "--kind") && i + 1 < argc) kind = argv[++i];
    else if (!strcmp(argv[i], "--hardlink")) hardlink = 1;
    else if (!strcmp(argv[i], "--")) { command = argv + i + 1; command_count = (size_t)(argc - i - 1); break; }
    else { command = argv + i; command_count = (size_t)(argc - i); break; }
  }
  if (!cache || !command_count) die("usage: fvsbuild compile --cache-dir DIR [--groups LIST] [--kind KIND] [--hardlink] -- COMMAND");
  for (i = (int)command_count; i > 0; i--)
    if (suffix_is(command[i - 1], c_suffix) || suffix_is(command[i - 1], f90_suffix)) { source = command[i - 1]; break; }
  for (i = 0; i < (int)command_count; i++) {
    has_c |= !strcmp(command[i], "-c");
    has_o |= !strcmp(command[i], "-o") && i + 1 < (int)command_count;
  }
  if (source && !kind) {
    char *parent = dir_name(source);
    const char *parent_name = base_name(parent);
    if (!strcmp(base_name(source), "sqlite3.c") || !strcmp(base_name(source), "fvsqlite3.c")) kind = "sqlite";
    else if (!strcmp(parent_name, "fofem") && !strcmp(suffix_of(source), ".c")) kind = "fofem";
    else if (!strcmp(parent_name, "NVEL") && suffix_is(source, f90_suffix)) kind = "nvel";
    free(parent);
  }
  eligible = 0;
  if (kind && source && has_c && has_o) {
    /* --groups is a comma-separated list; an empty list disables reuse. */
    const char *g = groups;
    size_t n = strlen(kind);
    while (*g) {
      const char *end = strchr(g, ',');
      size_t length = end ? (size_t)(end - g) : strlen(g);
      if (length == n && !strncmp(g, kind, n)) eligible = 1;
      g = end ? end + 1 : g + length;
    }
  }
  if (!eligible) {
    if (kind && has_c && has_o) {
      for (i = 0; i + 1 < (int)command_count; i++)
        if (!strcmp(command[i], "-o")) remove_file(command[i + 1]);
    }
    command[command_count] = NULL;
    exec_command(command);
  }
  resolved_cache = resolve(cache);
  return compile_cached(command, command_count, resolved_cache, kind, hardlink, argv[0]);
}

/* Atomically replace DESTINATION with this freshly built helper. */
static int install_self(const char *self, const char *destination)
{
#ifdef FVS_WINDOWS
  char module[MAX_PATH + 1];
  DWORD n = GetModuleFileNameA(NULL, module, MAX_PATH);
  module[n] = 0;
  (void)self;
  if (MoveFileExA(module, destination, MOVEFILE_REPLACE_EXISTING)) return 0;
  /* A helper in use cannot be replaced; an existing one is equally current. */
  if (is_file(destination)) return 0;
  die("cannot install %s", destination);
#else
  if (rename(self, destination)) die("cannot install %s: %s", destination, strerror(errno));
#endif
  return 0;
}

int main(int argc, char **argv)
{
  if (argc == 3 && !strcmp(argv[1], "install-self")) return install_self(argv[0], argv[2]);
  if (argc >= 2 && !strcmp(argv[1], "compile")) return compile_main(argc, argv);
  if (argc >= 2 && !strcmp(argv[1], "prepare")) {
    const char *root = NULL, *manifest = NULL;
    int native = 0, i;
    for (i = 2; i < argc; i++) {
      if (!strcmp(argv[i], "--root") && i + 1 < argc) root = argv[++i];
      else if (!strcmp(argv[i], "--manifest") && i + 1 < argc) manifest = argv[++i];
      else if (!strcmp(argv[i], "--native-sources")) native = 1;
      else die("unknown prepare option %s", argv[i]);
    }
    if (!root || !manifest) die("usage: fvsbuild prepare --root DIR --manifest FILE [--native-sources]");
    return prepare(root, manifest, native);
  }
  die("usage: fvsbuild prepare|compile ...");
  return 2;
}
