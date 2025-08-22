include(${CMAKE_SOURCE_DIR}/config/fflags.cmake)

add_executable(main main.f90)

target_compile_options(main PUBLIC ${FFLAGS_COMMON})

target_link_libraries(main PUBLIC 
    geoutils 
    linalg
    BLAS::BLAS
    LAPACK::LAPACK
    /usr/lib/x86_64-linux-gnu/open-coarrays/openmpi/lib/libcaf_mpi.a
    /usr/lib/x86_64-linux-gnu/libopenmpi.so # Esta es la nueva línea
    /usr/lib/x86_64-linux-gnu/libopenblas.so
)