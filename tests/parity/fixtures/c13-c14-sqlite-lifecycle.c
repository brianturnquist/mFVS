/* Observe statement ownership after a real writer returns. sqlite3_close
 * leaves a busy connection open, so the normal fixture teardown can still
 * finalize its statement and close it. Never close an idle connection here.
 */
#include "sqlite3.h"
#include <stddef.h>
extern sqlite3 *dbset[];

int test_sqlite_lifecycle_(int *dbnum, int *statements)
{
  sqlite3_stmt *statement = NULL;
  *statements = 0;
  while ((statement = sqlite3_next_stmt(dbset[*dbnum], statement)) != NULL)
    ++*statements;
  if (*statements > 0)
    return sqlite3_close(dbset[*dbnum]);
  return SQLITE_OK;
}

/* Harness teardown for the intentionally reproduced orphaned statement. */
int test_sqlite_cleanup_(int *dbnum)
{
  sqlite3_stmt *statement;
  while ((statement = sqlite3_next_stmt(dbset[*dbnum], NULL)) != NULL)
    sqlite3_finalize(statement);
  return SQLITE_OK;
}
