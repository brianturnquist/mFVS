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
export FFLAGS = $(PIC) -g -cpp -DCMP${CC} -Wall -Wno-integer-division -ffpe-trap=invalid,zero,underflow,overflow,denormal -fbacktrace 
# Adding optimization (-O2) does not help execution speed much but slows compile time a lot
export CFLAGS = $(PIC) -DANSI -DCMP${CC} -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION $(WIN) 
export LD = gfortran

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
export FVSprgs = $(subst _sourceList.txt,,$(shell ls FVS*_sourceList.txt))
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
  export sourceList = $(shell cat $(sourceFileList))
  # Converted includes retain .f90 names but are never compiled alone.
  include_names := $(notdir $(shell cat $(FVS_ROOT)/bin/fortran_includes.txt))
  export includes = $(notdir $(filter %.inc %.h,$(sourceList))) $(filter $(include_names),$(notdir $(sourceList)))
  export mods = $(notdir $(filter %_mod.f90, $(sourceList)))
  export mods_mods = $(addsuffix .mod,$(basename $(mods)))
  export source = $(filter-out $(includes),$(notdir $(filter %.c %.f90 %.cpp, $(sourceList))))
  export object = $(addsuffix .o,$(basename $(source)))
 endif
endif

# Targets are: all (All varaints), Canada (Canada variants), US (US variants)
#              clean (removes build directories containing copies of source files and objects)

all : $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

Canada : $(addsuffix $(SLIBSUFX),$(CANprgs)) $(addsuffix $(PRGSUFX),$(CANprgs))

US : $(addsuffix $(SLIBSUFX),$(USprgs)) $(addsuffix $(PRGSUFX),$(USprgs))

clean :
	rm -frv *_buildDir $(addsuffix $(SLIBSUFX),$(FVSprgs)) $(addsuffix $(PRGSUFX),$(FVSprgs))

ifndef sourceFileList

# Each variant FVSxx is built from common_sourceList.txt (the files every variant
# compiles) plus FVSxx_sourceList.txt (that variant's own files). The lists are
# joined with awk so that a list without a final newline is still read correctly.
commonList := $(wildcard common_sourceList.txt)

define variantRules
$(1)$$(SLIBSUFX) $(1)$$(PRGSUFX) : $(1)_sourceList.txt $(commonList) $$(shell awk 1 $(commonList) $(1)_sourceList.txt)
	$$(MAKE) --file=makefile $(1).setup
endef
$(foreach prg,$(FVSprgs),$(eval $(call variantRules,$(prg))))

%.setup : 
	mkdir -pv $(basename $@)_buildDir
	awk 1 $(commonList) $(basename $@)_sourceList.txt > $(basename $@)_buildDir/allSources.txt
	cp -p `cat $(basename $@)_buildDir/allSources.txt` $(basename $@)_buildDir
	$(MAKE) --file=../makefile --directory=$(basename $@)_buildDir \
	               buildDir=$(basename $@)_buildDir \
                 sourceFileList=allSources.txt $(basename $@).prg
endif

.SUFFIXES :

.PRECIOUS : $(object) $(mods_mods) 

# FVSVERSION module (base/version.f90) — consumers must compile after version.o
fvsversion.mod : version.o

grohed.o main.o revise.o dbscase.o : fvsversion.mod
 
ifeq (${OSARCH},w64) 
%.prg : $(object) 
	$(FCprf)$(LD) -static -shared -Wl,--export-all-symbols -o $(basename ../$@)$(SLIBSUFX)  $(filter-out  main.o,$(object))
	$(FCprf)$(LD) -static -o $(basename ../$@)$(PRGSUFX) $(object)
else
%.prg : $(object) 
	$(LD) -shared -o $(basename ../$@)$(SLIBSUFX) $(filter-out main.o,$(object)) $(DBLINK) 
	$(LD)         -o $(basename ../$@)$(PRGSUFX)  $(object) $(DBLINK)    
endif

%.mod : %.f90 $(includes)
	$(FCprf)$(FC) $(FFLAGS) -c $<

%.o : %.f90 $(includes) $(mods_mods)
	$(FCprf)$(FC) $(FFLAGS) -c -o $@ $<

%.o : %.c 
	$(CCprf)$(CC) $(CFLAGS) -c -o $@ $<
	
%.o : %.cpp 
	$(CCprf)$(CC) $(CFLAGS) -c -o $@ $<
