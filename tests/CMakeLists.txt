set(PFUNIT_DIR "/home/adrian/.local/lib/pFUnit/build/installed/PFUNIT-4.19/cmake")
find_package(PFUNIT REQUIRED)
message(STATUS "PFUNIT found at: ${PFUNIT_DIR}")
message(STATUS "PFUNIT version: ${PFUNIT_VERSION}")

set(PROJ_SRC_FILES_EXEC_MAIN ${PROJ_SRC_FILES})
list(FILTER PROJ_SRC_FILES_EXEC_MAIN EXCLUDE REGEX ".*main.f90")

# create libarary for code in ../sources/
add_library (sut STATIC ${PROJ_SRC_FILES_EXEC_MAIN})

# list test files
set(TEST_SOURCES
	${CMAKE_CURRENT_SOURCE_DIR}/test_body_mod.pf
)

# ass test target
add_pfunit_ctest(tests
	SOURCES ${TEST_SOURCES}
	LINK_LIBRARIES sut
	EXTRA_ARGS --verbose
)
