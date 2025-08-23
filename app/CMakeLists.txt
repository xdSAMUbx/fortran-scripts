include(${CMAKE_SOURCE_DIR}/config/fflags.cmake)

add_executable(main main.f90)

target_compile_options(main PUBLIC ${FFLAGS_COMMON})

target_link_libraries(main PUBLIC 
    geoutils 
    linalg
    BLAS::BLAS
    LAPACK::LAPACK
)