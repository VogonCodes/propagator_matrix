cmake_minimum_required(VERSION 3.23)
project(PropagatorMatrixViscoelasticLove
	LANGUAGES Fortran)
enable_language(Fortran)

set(SRC_DIR "${PROJECT_SOURCE_DIR}/src")
set(PROJ_SRC_FILES
	"${SRC_DIR}/constants.f90"
	"${SRC_DIR}/body_mod.f90"
	"${SRC_DIR}/fileio.f90"
	"${SRC_DIR}/helpers.f90"
	"${SRC_DIR}/propagator.f90"
	"${SRC_DIR}/main.f90"
)

find_package(LAPACK REQUIRED)

add_executable("main" "${PROJ_SRC_FILES}")
target_link_libraries(main ${LAPACK_LIBRARIES})
target_include_directories(main PRIVATE ${LAPACK_INCLUDE_DIRS})

######### Testing #########
enable_testing()
add_subdirectory(tests)
