# Notes: If OS is defined, then then we are 64 bit windows, otherwise, we are Linix 
# x86_64 or mac osx x86_64. These two are automatically picked as OS is always defined on windows

ifdef OS
 export WIN = -DWINDOWS
 export SLIBSUFX = .dll
 export PIC =      # never use PIC on this OSARCH type
 export PRGSUFX = .exe 
 ifndef OSARCH
   export OSARCH = w64
 endif 
else
#setting WIN as done below is to enable correct processing of fofem (fire) c code.
 export WIN = -Dunix  
 export SLIBSUFX = .so
 export PRGSUFX = 
 ifdef OSTYPE
  ifeq ($(OSTYPE),linux)
    export OSARCH = l64
    export PIC = -fPIC  # always use PIC on this OSARCH type
  endif
  ifeq ($(OSTYPE),linux-gnu)
    export OSARCH = l64
    export PIC = -fPIC  # always use PIC on this OSARCH type
  endif
 else
  ifeq ($(shell uname),Linux)
    export OSARCH = l64
    export PIC = -fPIC
  endif
 endif
 ifndef OSARCH
   export OSARCH = osx
   export PIC =      # never use PIC on this OSARCH type
   $(info OSARCH: $(OSARCH))
 endif
endif                                                                               

	
# FC is the fortran compiler
# FFLAGS are pasted to fortran
# CC is the c compiler and used for c++ code
# CFLAGS are passed to C, of these CMPgcc is used by the fofem routines and WIN is used
# by fvsSQL.c
# LD is the linker

ifdef mingw64
 export FCprf = x86_64-w64-mingw32-
 export CCprf = x86_64-w64-mingw32-
else
 export FCprf = 
 export CCprf = 
endif

$(info OS: $(OS); OSTYPE: $(OSTYPE); WIN: $(WIN); OSARCH: $(OSARCH); SLIBSUFX: $(SLIBSUFX))
$(info PIC: $(PIC); PRGSUFX: $(PRGSUFX))
$(info mingw64: $(mingw64); FCprf: $(FCprf); CCprf: $(CCprf))

export CC = gcc
export FC = gfortran 
# Keep power rounding fixed by R04; disable optimization with FVS_FOPT=-O0.
# Per-variant settings track this override and invalidate affected receipts.
export FVS_FOPT ?= -O2 -ffp-contract=off
export FFLAGS = $(PIC) $(FVS_FOPT) -g -cpp -DCMP${CC} -Wall -Wno-integer-division -ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace $(if $(filter 1,$(FVS_NATIVE_SOURCES)),-I.)
# C optimization is scoped at the individual object rules below.
export CFLAGS = $(PIC) -DANSI -DCMP${CC} -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION $(WIN) $(if $(filter 1,$(FVS_NATIVE_SOURCES)),-I.)
export LD = gfortran

# Reuse code only; each executable/shared library keeps its own linked state.
export FVS_SHARED_GROUPS ?= sqlite,fofem,nvel
export FVS_NATIVE_SOURCES ?= 1
.DEFAULT_GOAL := all

# --------------------------------------------------------------------------
# Version and build metadata
#
# Derived from git at make invocation time. All fields fall back to
# 'unknown' if git is unavailable or the directory is not a git repo.
#
# A -dirty suffix indicates uncommitted changes in the working tree.
# A -dirty binary should not be used in production or deliverable contexts.
# --------------------------------------------------------------------------

# Define source remote name for your environment. This is the remote that will be used to determine the git org and version.
GIT_REMOTE_NAME := origin

FVS_ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
export FVS_SHARED_CACHE ?= $(FVS_ROOT)/bin/.shared_build
# Dependency preparation and compilation reuse use bin/fvsbuild.c, built with
# the host C compiler. The build needs nothing the original F77 build did not.
FVS_HOST_CC ?= $(CC)
FVS_TOOL_NAME := .build_tool/fvsbuild$(strip $(PRGSUFX))
export FVS_TOOL = $(FVS_ROOT)/bin/$(FVS_TOOL_NAME)
FVS_SHARED_COMPILE = "$(FVS_TOOL)" compile --cache-dir "$(FVS_SHARED_CACHE)" --groups "$(FVS_SHARED_GROUPS)" --kind $(FVS_SHARED_KIND) --hardlink --
FVS_NVEL_COMPILE = "$(FVS_TOOL)" compile --cache-dir "$(FVS_SHARED_CACHE)" --groups "$(FVS_SHARED_GROUPS)" --kind nvel --hardlink --

GIT_REMOTE  := $(shell git -C $(FVS_ROOT) remote get-url $(GIT_REMOTE_NAME) 2>/dev/null || echo unknown)
GIT_ORG     := $(shell u="$(GIT_REMOTE)"; \
                o=$$(echo "$$u" | sed -E 's|.*[:/]([^/]+)/[^/]*(\.git)?$$|\1|'); \
               test -n "$$o" && echo "$$o" || echo unknown)
GIT_VERSION := $(shell git -C $(FVS_ROOT) describe --tags --exact-match 2>/dev/null || \
               git -C $(FVS_ROOT) describe --tags 2>/dev/null || echo unknown)
GIT_HASH    := $(shell git -C $(FVS_ROOT) rev-parse --short HEAD 2>/dev/null || echo unknown)
GIT_DATE    := $(shell git -C $(FVS_ROOT) log -1 --format=%cd \
               --date=format:%Y%m%d 2>/dev/null || echo unknown)
GIT_BRANCH  := $(shell git -C $(FVS_ROOT) rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)
GIT_DIRTY   := $(shell git -C $(FVS_ROOT) status --porcelain 2>/dev/null)

ifneq ($(GIT_DIRTY),)
GIT_VERSION := $(GIT_VERSION)-dirty
GIT_HASH    := $(GIT_HASH)-dirty
$(warning WARNING: working tree has uncommitted changes.)
$(warning WARNING: Version will be marked -dirty.)
$(warning WARNING: Do not use this build in production or deliverable contexts.)
endif

VERSION_FLAGS = \
  -DFVS_GIT_ORG='"$(GIT_ORG)"' \
  -DFVS_GIT_VERSION='"$(GIT_VERSION)"' \
  -DFVS_GIT_HASH='"$(GIT_HASH)"' \
  -DFVS_GIT_DATE='"$(GIT_DATE)"' \
  -DFVS_GIT_BRANCH='"$(GIT_BRANCH)"' \
  -DFVS_GIT_REMOTE='"$(GIT_REMOTE)"'

export FFLAGS += $(VERSION_FLAGS)
# --------------------------------------------------------------------------

# Lists are: FVSprgs (all varaints), CANprgs (Canada variants), USprgs (US variants)

ifndef sourceFileList
export FVSprgs := $(subst _sourceList.txt,,$(shell ls FVS*_sourceList.txt))
$(info FVSprgs: $(FVSprgs))
endif

CANprgs := FVSbc FVSon
$(info CANprgs: $(CANprgs))

USprgs := FVSak FVSbm FVSca FVSci FVScr FVScs FVSec FVSem FVSie FVSkt FVSls FVSnc FVSne FVSoc FVSop FVSpn FVSsn FVSso FVStt FVSut FVSwc FVSws
$(info USprgs: $(USprgs))

# the following variables are not processed until sourceFileList is defined...and that happens
# when the first rule fires (the %.setup rule).
ifdef sourceFileList
 ifndef sourceList
  export sourceFileList
  # Simply expanded: Make re-expands an exported recursive variable for every
  # recipe it starts, so a $(shell) here ran once per compile and, where process
  # creation is slow (Windows/MSYS), left a -j build effectively serial (bug 69).
  export sourceList := $(shell cat $(sourceFileList))
  # Converted includes retain .f90 names but are never compiled alone.
  include_names := $(notdir $(shell cat $(FVS_ROOT)/bin/fortran_includes.txt))
  export includes := $(notdir $(filter %.F77 %.INC %.inc %.h,$(sourceList))) $(filter $(include_names),$(notdir $(sourceList)))
  export mods := $(notdir $(filter %_mod.f90, $(sourceList)))
  export mods_mods := $(addsuffix .mod,$(basename $(mods)))
  export source := $(filter-out $(includes),$(notdir $(filter %.c %.f %.f90 %.F90 %.F %.for %.cpp, $(sourceList))))
  export object := $(addsuffix .o,$(basename $(source)))
 endif
endif

# Targets are: all (All varaints), Canada (Canada variants), US (US variants)
#              clean (removes build directories containing copies of source files and objects)

all : $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

Canada : $(addsuffix $(SLIBSUFX),$(CANprgs)) $(addsuffix $(PRGSUFX),$(CANprgs))

US : $(addsuffix $(SLIBSUFX),$(USprgs)) $(addsuffix $(PRGSUFX),$(USprgs))

clean :
	rm -frv *_buildDir .shared_build .build_tool $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

# Packaging is opt-in. The linker writes stripped release copies from each
# variant's objects, in link order; developer binaries are never changed.
FVS_PACKAGE_VARIANTS ?= $(FVSprgs)
FVS_PACKAGE_DIR ?= $(FVS_ROOT)/bin/release
ifeq ($(OSARCH),osx)
FVS_RELEASE_LDFLAGS ?= -Wl,-S -Wl,-x
else
FVS_RELEASE_LDFLAGS ?= -Wl,--strip-debug
endif
export FVS_PACKAGE_DIR FVS_RELEASE_LDFLAGS
.PHONY: package
package: $(addsuffix .package,$(FVS_PACKAGE_VARIANTS))

ifndef sourceFileList

ifneq ($(FVS_PACKAGE_SYMBOLS),)
$(error FVS_PACKAGE_SYMBOLS was removed: separate symbols need tools the original build never used; keep the developer binaries instead)
endif
ifeq ($(abspath $(FVS_PACKAGE_DIR)),$(abspath $(FVS_ROOT)/bin))
$(error FVS_PACKAGE_DIR must differ from bin/, which holds the developer binaries)
endif

# Separate top-level makes (as check_suite.py runs) may build this at once:
# compile under a name unique to this shell, then let the new helper rename
# itself into place atomically.
FVS_TOOL_TEMP = .build_tool/fvsbuild-$$$$$(strip $(PRGSUFX))
$(FVS_TOOL_NAME): fvsbuild.c
	mkdir -pv .build_tool
	$(FVS_HOST_CC) -O2 -o $(FVS_TOOL_TEMP) fvsbuild.c && $(FVS_TOOL_TEMP) install-self $@

# Both artifacts share one setup; the child tracks selected inputs/settings.
define variantRules
$(1).setup: FORCE $$(FVS_TOOL_NAME)
$(1)$$(SLIBSUFX) $(1)$$(PRGSUFX) : $(1).setup
$(1).package: $(1).setup
	$$(MAKE) --file=../makefile --directory=$(1)_buildDir buildDir=$(1)_buildDir \
	         sourceFileList=../$(1)_sourceList.txt $(1).release
endef
$(foreach prg,$(FVSprgs),$(eval $(call variantRules,$(prg))))

.PHONY: FORCE $(addsuffix .package,$(FVSprgs))
FORCE:

%.setup : 
	mkdir -pv $(basename $@)_buildDir
	$(MAKE) --file=../makefile --directory=$(basename $@)_buildDir \
	               buildDir=$(basename $@)_buildDir \
                 sourceFileList=../$(basename $@)_sourceList.txt $(basename $@).prg
endif

.SUFFIXES :

# Object receipts own compilation; .mod files are compiler side effects.
.PRECIOUS : $(object)

ifdef sourceFileList
variant_name := $(patsubst %_sourceList.txt,%,$(notdir $(sourceFileList)))
fortran_objects := $(addsuffix .o,$(basename $(filter %.f %.f90 %.F90 %.F %.for,$(source))))
c_objects := $(addsuffix .o,$(basename $(filter %.c %.cpp,$(source))))
$(object) $(addsuffix .stamp,$(object)): FVS_SHARED_KIND =
$(object) $(addsuffix .stamp,$(object)): FVS_COMPILE_PREFIX =
FVS_DEPFLAGS = -MMD -MP -MF $(basename $(1)).d -MT $(1).stamp

# Stage and inspect only this variant. The compiler's .d files then track active
# direct/transitive includes, and the bootstrap graph orders every USE producer.
# $(shell) can inherit a parent's target-specific environment on older Make.
# Quote and pass this parse's settings explicitly, including recursive restores.
fvs_quote = '$(subst ','"'"',$(1))'
FVS_PREPARED := $(shell FFLAGS=$(call fvs_quote,$(FFLAGS)) CFLAGS=$(call fvs_quote,$(CFLAGS)) FC=$(call fvs_quote,$(FC)) CC=$(call fvs_quote,$(CC)) FCprf=$(call fvs_quote,$(FCprf)) CCprf=$(call fvs_quote,$(CCprf)) LD=$(call fvs_quote,$(LD)) DBLINK=$(call fvs_quote,$(DBLINK)) "$(FVS_TOOL)" prepare --root "$(FVS_ROOT)" --manifest "$(sourceFileList)" $(if $(filter 1,$(FVS_NATIVE_SOURCES)),--native-sources))
ifneq ($(FVS_PREPARED),ready)
$(error Cannot prepare selected source dependencies)
endif

.PHONY: $(variant_name).prg
$(variant_name).prg: ../$(variant_name)$(PRGSUFX) ../$(variant_name)$(SLIBSUFX)

# Independent link targets converge on the same objects in a parallel build.
# Local receipts also detect immutable shared objects restored from an older cache.
../$(variant_name)$(PRGSUFX): $(object) $(addsuffix .stamp,$(object)) .link-inputs .link.flags
ifeq (${OSARCH},w64)
	$(FCprf)$(LD) -static -o $@ $(object)
else
	$(LD) -o $@ $(object) $(DBLINK)
endif

../$(variant_name)$(SLIBSUFX): $(filter-out main.o,$(object)) $(addsuffix .stamp,$(object)) .link-inputs .link.flags
ifeq (${OSARCH},w64)
	$(FCprf)$(LD) -static -shared -Wl,--export-all-symbols -o $@ $(filter-out main.o,$(object))
else
	$(LD) -shared -o $@ $(filter-out main.o,$(object)) $(DBLINK)
endif

# Release copies: the developer link commands plus FVS_RELEASE_LDFLAGS, which
# omit debug information (and, on macOS, local symbols) as strip did in O28.
release_program = "$(FVS_PACKAGE_DIR)/$(variant_name)$(strip $(PRGSUFX))"
release_library = "$(FVS_PACKAGE_DIR)/$(variant_name)$(strip $(SLIBSUFX))"
.PHONY: $(variant_name).release
$(variant_name).release: $(object) $(addsuffix .stamp,$(object))
	mkdir -pv "$(FVS_PACKAGE_DIR)"
ifeq (${OSARCH},w64)
	$(FCprf)$(LD) -static $(FVS_RELEASE_LDFLAGS) -o $(release_program) $(object)
	$(FCprf)$(LD) -static -shared -Wl,--export-all-symbols $(FVS_RELEASE_LDFLAGS) -o $(release_library) $(filter-out main.o,$(object))
else
	$(LD) $(FVS_RELEASE_LDFLAGS) -o $(release_program) $(object) $(DBLINK)
	$(LD) -shared $(FVS_RELEASE_LDFLAGS) -o $(release_library) $(filter-out main.o,$(object)) $(DBLINK)
endif

# Objects are side effects of receipt recipes; no per-object shell is needed to
# test their presence. Make keeps an object's time from before its order-only
# receipt ran, so each compile rewrites its receipt (a shell redirection, not
# another process). The link rules then relink in the same invocation (bug 67).
# Prepare still repairs receipts left by an interrupted build.
$(object): %.o: | %.o.stamp

$(addsuffix .stamp,$(fortran_objects)): %.o.stamp:
	$(FVS_COMPILE_PREFIX) $(FCprf)$(FC) $(FFLAGS) $(call FVS_DEPFLAGS,$(@:.stamp=)) -c -o $(@:.stamp=) "$<" && echo > $@

$(addsuffix .stamp,$(c_objects)): %.o.stamp:
	$(FVS_COMPILE_PREFIX) $(CCprf)$(CC) $(CFLAGS) $(call FVS_DEPFLAGS,$(@:.stamp=)) -c -o $(@:.stamp=) "$<" && echo > $@

include dependencies.mk
-include $(object:.o=.d)
endif

# SQLite optimization is isolated from Fortran, FOFEM and API glue.
# Depend on the build rules so an existing build picks up changed C flags.
sqlite3.o fvsqlite3.o sqlite3.o.stamp fvsqlite3.o.stamp: CFLAGS := $(CFLAGS) -O2
sqlite3.o fvsqlite3.o sqlite3.o.stamp fvsqlite3.o.stamp: FVS_SHARED_KIND = sqlite
sqlite3.o fvsqlite3.o sqlite3.o.stamp fvsqlite3.o.stamp: FVS_COMPILE_PREFIX = $(FVS_SHARED_COMPILE)

# Only the audited C implementation is shared; Canadian CFIM C++ stays local.
ifdef sourceFileList
fofem_objects := $(addsuffix .o,$(basename $(notdir $(filter ../fire/fofem/%.c,$(sourceList)))))
$(fofem_objects) $(addsuffix .stamp,$(fofem_objects)): FVS_SHARED_KIND = fofem
$(fofem_objects) $(addsuffix .stamp,$(fofem_objects)): FVS_COMPILE_PREFIX = $(FVS_SHARED_COMPILE)
# Whole objects retain DATA/SAVE/COMMON initialization and original link order.
# Fortran database wrappers are deliberately outside this source-path boundary.
nvel_sources := $(notdir $(filter ../volume/NVEL/%.f90,$(sourceList)))
nvel_objects := $(addsuffix .o,$(basename $(nvel_sources)))
nvel_modules := $(addsuffix .mod,$(basename $(filter %_mod.f90,$(nvel_sources))))
$(nvel_objects) $(nvel_modules) $(addsuffix .stamp,$(nvel_objects)): FVS_SHARED_KIND = nvel
$(nvel_objects) $(nvel_modules) $(addsuffix .stamp,$(nvel_objects)): FVS_COMPILE_PREFIX = $(FVS_SHARED_COMPILE)
endif
