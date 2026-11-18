## $Id$
## Revised for VS2026 and Intel oneAPI (ifx)
## April 2026 - Don Robinson (ESSA), Google Gemini
## June 2026 - David Diaz (Vibrant Planet), updated to inject version and build metadata

cmake_minimum_required(VERSION 4.3)

message(STATUS "CMake version: ${CMAKE_VERSION}")

# --- 1. TOOLCHAIN SETUP ---
if (MSVC OR CMAKE_GENERATOR MATCHES "Visual Studio")
    message(STATUS "Building FVS for Windows (Visual Studio/Intel)")
    set(IS_WINDOWS_BUILD TRUE)
elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles" OR CMAKE_GENERATOR MATCHES "Unix Makefiles")
    message(STATUS "Building FVS for GCC environment")
    set(IS_GCC_BUILD TRUE)
endif()

# --- 2. VARIANT PROCESSING LOGIC ---
# Choose variants with -DFVS_VARIANTS="FVSie;FVSpn", or "all".
# Keep the existing BC default when no selection is supplied.
set(FVS_VARIANTS "FVSbc" CACHE STRING "Variants to build, e.g. FVSie;FVSpn, or all")
option(FVS_OPTIMIZE_FORTRAN "Use conservative GNU Fortran optimization by default" ON)
set(FVS_SHARED_GROUPS "sqlite,fofem,nvel" CACHE STRING "Reusable GNU compilation groups; empty disables reuse")
if (FVS_VARIANTS STREQUAL "all")
    file(GLOB tobuild FVS*_sourceList.txt)
else()
    set(tobuild)
    foreach (variant ${FVS_VARIANTS})
        if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${variant}_sourceList.txt")
            message(FATAL_ERROR "No source list for variant ${variant}")
        endif()
        list(APPEND tobuild "${CMAKE_CURRENT_SOURCE_DIR}/${variant}_sourceList.txt")
    endforeach()
endif()
list(SORT tobuild)

set(dirsToProcess)

foreach (sourceList ${tobuild})
    get_filename_component(slfn ${sourceList} NAME)
    string(REPLACE "_sourceList.txt" "" prgName ${slfn})

    # Read and adjust source paths
    file(STRINGS ${sourceList} sourceListStrings NEWLINE_CONSUME)
    string(REPLACE "../" "../../" newsourceList "${sourceListStrings}")

    # Extract the sub-project CMakeLists logic (text after the tag)
    file(READ "${CMAKE_CURRENT_LIST_FILE}" full_content)
    string(REGEX REPLACE ".*##DO NOT REMOVE THIS TAG##" "" sub_cmake_logic "${full_content}")

    # Prep the subdirectory

## default
    set(targetDir "${prgName}_CmakeDir")

    file(MAKE_DIRECTORY ${targetDir})
    file(REMOVE "${targetDir}/CMakeCache.txt") 
    
    # Write the actual files into the sub-directory
    file(WRITE "${targetDir}/CMakeLists.txt" "${sub_cmake_logic}")
    file(WRITE "${targetDir}/${prgName}_sourceList.txt" "${newsourceList}")
    list(APPEND dirsToProcess ${targetDir})
endforeach()

# Execute sub-builds
foreach(toProcess ${dirsToProcess})
    # Pass the default-optimization choice to each generated variant project.
    set(fvsChildOptions "-DFVS_OPTIMIZE_FORTRAN=${FVS_OPTIMIZE_FORTRAN}")
    foreach(setting CMAKE_BUILD_TYPE CMAKE_Fortran_FLAGS CMAKE_Fortran_FLAGS_DEBUG FVS_SHARED_GROUPS FVS_SHARED_CACHE)
        if(DEFINED ${setting})
            list(APPEND fvsChildOptions "-D${setting}=${${setting}}")
        endif()
    endforeach()
    execute_process(COMMAND cmake -G "${CMAKE_GENERATOR}" ${fvsChildOptions} . WORKING_DIRECTORY ${toProcess})
endforeach()

# This 'return' prevents the parent script from executing the code below
return()

##DO NOT REMOVE THIS TAG##
cmake_minimum_required(VERSION 4.3)

# Set Intel LLVM (ifx) as the preferred Fortran compiler for VS2026
if(CMAKE_GENERATOR MATCHES "Visual Studio 18 2026")
    set(CMAKE_GENERATOR_TOOLSET "fortran=ifx" CACHE STRING "" FORCE)
endif()

project(FVS LANGUAGES C CXX Fortran)
option(FVS_OPTIMIZE_FORTRAN "Use conservative GNU Fortran optimization by default" ON)

# --- 3. VERSION AND BUILD METADATA ---
# Derived from git at cmake configure time. All fields fall back to
# 'unknown' if git is unavailable or the directory is not a git repo.
#
# A -dirty suffix indicates uncommitted changes in the working tree.
# A -dirty binary should not be used in production or deliverable contexts.
get_filename_component(FVS_ROOT "${CMAKE_CURRENT_LIST_DIR}/../.." ABSOLUTE)
find_package(Git)

if(Git_FOUND)
    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} remote get-url origin
        OUTPUT_VARIABLE FVS_GIT_REMOTE
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_REMOTE STREQUAL "")
        set(FVS_GIT_REMOTE unknown)
    endif()

    string(REGEX REPLACE ".*/([^/]+)/[^/]*(\\.git)?$" "\\1" FVS_GIT_ORG "${FVS_GIT_REMOTE}")
    if(FVS_GIT_ORG STREQUAL "" OR FVS_GIT_ORG STREQUAL FVS_GIT_REMOTE)
        set(FVS_GIT_ORG unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} describe --tags --exact-match
        OUTPUT_VARIABLE FVS_GIT_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_VERSION STREQUAL "")
        execute_process(
            COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} describe --tags
            OUTPUT_VARIABLE FVS_GIT_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
            RESULT_VARIABLE _fvs_git_rc)
    endif()
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_VERSION STREQUAL "")
        set(FVS_GIT_VERSION unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} rev-parse --short HEAD
        OUTPUT_VARIABLE FVS_GIT_HASH
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_HASH STREQUAL "")
        set(FVS_GIT_HASH unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} log -1 --format=%cd --date=format:%Y%m%d
        OUTPUT_VARIABLE FVS_GIT_DATE
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_DATE STREQUAL "")
        set(FVS_GIT_DATE unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} rev-parse --abbrev-ref HEAD
        OUTPUT_VARIABLE FVS_GIT_BRANCH
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE _fvs_git_rc)
    if(NOT _fvs_git_rc EQUAL 0 OR FVS_GIT_BRANCH STREQUAL "")
        set(FVS_GIT_BRANCH unknown)
    endif()

    execute_process(
        COMMAND ${GIT_EXECUTABLE} -C ${FVS_ROOT} status --porcelain
        OUTPUT_VARIABLE _fvs_git_dirty
        ERROR_QUIET)
    if(NOT _fvs_git_dirty STREQUAL "")
        set(FVS_GIT_VERSION "${FVS_GIT_VERSION}-dirty")
        set(FVS_GIT_HASH "${FVS_GIT_HASH}-dirty")
        message(WARNING "WARNING: working tree has uncommitted changes.")
        message(WARNING "WARNING: Version will be marked -dirty.")
        message(WARNING "WARNING: Do not use this build in production or deliverable contexts.")
    endif()
else()
    set(FVS_GIT_REMOTE unknown)
    set(FVS_GIT_ORG unknown)
    set(FVS_GIT_VERSION unknown)
    set(FVS_GIT_HASH unknown)
    set(FVS_GIT_DATE unknown)
    set(FVS_GIT_BRANCH unknown)
endif()

message(STATUS "FVS_GIT_REMOTE:  ${FVS_GIT_REMOTE}")
message(STATUS "FVS_GIT_ORG:     ${FVS_GIT_ORG}")
message(STATUS "FVS_GIT_VERSION: ${FVS_GIT_VERSION}")
message(STATUS "FVS_GIT_HASH:    ${FVS_GIT_HASH}")
message(STATUS "FVS_GIT_DATE:    ${FVS_GIT_DATE}")
message(STATUS "FVS_GIT_BRANCH:  ${FVS_GIT_BRANCH}")

# --- 4. COMPILER CONFIGURATION ---
# All selected Fortran sources use free form, recognized by their .f90 suffix.
set(CMAKE_VERBOSE_MAKEFILE ON)

if(MSVC)
    add_compile_definitions(ANSI WINDOWS _WINDLL _CRT_SECURE_NO_WARNINGS)
else()
    add_compile_definitions(ANSI CMPgcc)
endif()

# --- 5. SOURCE CATEGORIZATION ---
file(GLOB sl_file FVS*_sourceList.txt)
get_filename_component(slfn ${sl_file} NAME)
string(REPLACE "_sourceList.txt" "" prgName ${slfn})
string(REPLACE "FVS" "" varName ${prgName})

file(STRINGS ${prgName}_sourceList.txt fileList)

set(FortranHeaders "")
set(FsourceFVS "")
set(FsourceMAIN "")
set(CsourceSQL "")
set(CsourceFFE "")
set(includeDirs "")

# Converted include files use .f90 names and remain header-only.
file(STRINGS "${FVS_ROOT}/bin/fortran_includes.txt" FortranIncludePaths)
set(FortranIncludeFiles "")
foreach(header ${FortranIncludePaths})
    get_filename_component(header_abs "${FVS_ROOT}/${header}" ABSOLUTE)
    list(APPEND FortranIncludeFiles "${header_abs}")
endforeach()

foreach(fn ${fileList})
    string(STRIP "${fn}" fn)
    get_filename_component(pname ${fn} PATH)
    get_filename_component(fname ${fn} NAME)
    string(TOLOWER "${fn}" fn_low)
    get_filename_component(source_abs "${fn}" ABSOLUTE BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

    # Split Headers: Fortran vs C
    if(fn_low MATCHES "\\.(f77|inc)$" OR source_abs IN_LIST FortranIncludeFiles)
        list(APPEND FortranHeaders "${fn}")
        set(is_header TRUE)
#    elseif(fn_low MATCHES "\\.h$")
#        list(APPEND CHeaders "${fn}")
#        set(is_header TRUE)
    else()
        set(is_header FALSE)
    endif()

    if(is_header)
        if(pname MATCHES "/${varName}$")
            list(INSERT includeDirs 0 "${pname}")
        else()
            list(APPEND includeDirs "${pname}")
        endif()
    # Fortran Source
    elseif(fn_low MATCHES "\\.(f|for|f90)$")
        if(fname STREQUAL "main.f90")
            list(APPEND FsourceMAIN "${fn}")
        else()
            list(APPEND FsourceFVS "${fn}")
        endif()
    # C/C++ Source
    elseif(fn_low MATCHES "\\.(c|cpp)$")
        if(fname MATCHES "(sqlite3|fvsqlite3|apisubsc)\\.c")
            list(APPEND CsourceSQL "${fn}")
        else()
            list(APPEND CsourceFFE "${fn}")
        endif()
    endif()
endforeach()

if(includeDirs)
    list(REMOVE_DUPLICATES includeDirs)
    list(REVERSE includeDirs)
endif()

# --- 6. PROPERTY ASSIGNMENT  ---

# Apply Fortran-specific indexing to .f77 and .inc
if(FortranHeaders)
    set_source_files_properties(${FortranHeaders} PROPERTIES 
        HEADER_FILE_ONLY TRUE
        LANGUAGE Fortran
    )
    source_group("Include Files" FILES ${FortranHeaders})
endif()

# Standard C indexing for .h files
#if(CHeaders)
#    set_source_files_properties(${CHeaders} PROPERTIES
#			 HEADER_FILE_ONLY TRUE
#		)
#    source_group("C Headers" FILES ${CHeaders})
#endif()

## Combine lists for target definition
#set(AllHeaders ${FortranHeaders} ${CHeaders})

# --- 7. TARGET DEFINITIONS ---
include_directories(BEFORE ${includeDirs} "${CMAKE_CURRENT_BINARY_DIR}")

add_library(FVSsql SHARED ${CsourceSQL})
# Compiler-launcher reuse preserves these per-variant libraries and their state.
# Respect an explicitly supplied launcher (for example a caller's own cache).
set(FVS_SHARED_GROUPS "sqlite,fofem,nvel" CACHE STRING "Reusable GNU compilation groups; empty disables reuse")
set(FVS_SHARED_CACHE "${FVS_ROOT}/bin/.shared_build" CACHE PATH "Disposable shared object cache")
# The launcher is bin/fvsbuild.c, compiled here with the project's GNU C
# compiler: reuse needs nothing the original build did not. It stays in place
# with reuse disabled, to remove read-only cached copies before compiling.
# Cross-compiled projects cannot run it, so they compile without reuse.
set(fvsLauncher "")
if(CMAKE_C_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_CROSSCOMPILING)
    set(fvsBuildTool "${CMAKE_CURRENT_BINARY_DIR}/fvsbuild${CMAKE_EXECUTABLE_SUFFIX}")
    set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS "${FVS_ROOT}/bin/fvsbuild.c")
    execute_process(COMMAND "${CMAKE_C_COMPILER}" -O2 -o "${fvsBuildTool}" "${FVS_ROOT}/bin/fvsbuild.c"
        RESULT_VARIABLE fvsToolResult OUTPUT_VARIABLE fvsToolOutput ERROR_VARIABLE fvsToolOutput)
    if(fvsToolResult EQUAL 0)
        set(fvsLauncher "${fvsBuildTool};compile;--cache-dir;${FVS_SHARED_CACHE};--groups;${FVS_SHARED_GROUPS};--")
    else()
        message(WARNING "Cannot compile bin/fvsbuild.c; building without compilation reuse.\n${fvsToolOutput}")
    endif()
endif()
if(fvsLauncher AND NOT CMAKE_C_COMPILER_LAUNCHER)
    set_property(TARGET FVSsql PROPERTY C_COMPILER_LAUNCHER "${fvsLauncher}")
endif()
# Match the scoped GNU Make SQLite optimization in unoptimized configurations.
# Release/size configurations keep their existing caller-selected flags.
foreach(sqliteSource ${CsourceSQL})
    get_filename_component(sqliteName ${sqliteSource} NAME)
    if(sqliteName STREQUAL "sqlite3.c" OR sqliteName STREQUAL "fvsqlite3.c")
        set_property(SOURCE "${sqliteSource}" APPEND PROPERTY COMPILE_OPTIONS
            "$<$<AND:$<COMPILE_LANG_AND_ID:C,GNU>,$<OR:$<CONFIG:Debug>,$<CONFIG:>>>:-O2>")
    endif()
endforeach()

add_library(FVSfofem SHARED ${CsourceFFE})
if(fvsLauncher AND NOT CMAKE_C_COMPILER_LAUNCHER)
    set_property(TARGET FVSfofem PROPERTY C_COMPILER_LAUNCHER "${fvsLauncher}")
endif()
add_library(FVS_${varName} SHARED ${FsourceFVS} ${FortranHeaders})
if(fvsLauncher AND CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_LAUNCHER)
    set_property(TARGET FVS_${varName} PROPERTY Fortran_COMPILER_LAUNCHER "${fvsLauncher}")
endif()
add_executable(${prgName} ${FsourceMAIN} ${FortranHeaders})

# Match Make in configurations without an optimization choice of their own.
# An explicit -O switch or a Release/size configuration keeps its own flags.
# OFF leaves empty/Debug builds unoptimized for source-level debugging.
if(FVS_OPTIMIZE_FORTRAN AND NOT CMAKE_Fortran_FLAGS MATCHES "(^|[ \t])-O[^ \t]*")
    set(fvsDefaultConfigs "$<CONFIG:>")
    if(NOT CMAKE_Fortran_FLAGS_DEBUG MATCHES "(^|[ \t])-O[^ \t]*")
        set(fvsDefaultConfigs "$<OR:$<CONFIG:>,$<CONFIG:Debug>>")
    endif()
    foreach(fortranTarget FVS_${varName} ${prgName})
        target_compile_options(${fortranTarget} PRIVATE
            "$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,${fvsDefaultConfigs}>:-O2>"
            "$<$<AND:$<COMPILE_LANG_AND_ID:Fortran,GNU>,${fvsDefaultConfigs}>:-ffp-contract=off>")
    endforeach()
endif()

# --- 8. TARGET PROPERTIES & LINKING ---
set_property(DIRECTORY PROPERTY VS_STARTUP_PROJECT ${prgName})

set_target_properties(FVS_${varName} PROPERTIES 
    Fortran_PREPROCESS ON
    WINDOWS_EXPORT_ALL_SYMBOLS TRUE
    LINKER_LANGUAGE Fortran
)
target_compile_definitions(FVS_${varName} PRIVATE
    "FVS_GIT_ORG=\"${FVS_GIT_ORG}\""
    "FVS_GIT_VERSION=\"${FVS_GIT_VERSION}\""
    "FVS_GIT_HASH=\"${FVS_GIT_HASH}\""
    "FVS_GIT_DATE=\"${FVS_GIT_DATE}\""
    "FVS_GIT_BRANCH=\"${FVS_GIT_BRANCH}\""
    "FVS_GIT_REMOTE=\"${FVS_GIT_REMOTE}\""
)
set_target_properties(${prgName} PROPERTIES 
    Fortran_PREPROCESS ON
    LINKER_LANGUAGE Fortran
)

if(MSVC OR CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    target_compile_options(FVS_${varName} PRIVATE "/MP")
    target_compile_options(${prgName} PRIVATE "/MP")
endif()

target_link_libraries(FVS_${varName} PRIVATE FVSsql FVSfofem -pthread -ldl)
target_link_libraries(${prgName} PRIVATE FVS_${varName} -pthread -ldl)