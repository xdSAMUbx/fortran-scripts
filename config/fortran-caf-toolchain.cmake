# fortran-caf-toolchain.cmake
set(CMAKE_Fortran_COMPILER "/usr/bin/caf" CACHE FILEPATH "Coarray Fortran compiler")
set(BLAS_LIBRARIES "/usr/lib/x86_64-linux-gnu/libblas.so" CACHE FILEPATH "BLAS library")
set(LAPACK_LIBRARIES "/usr/lib/x86_64-linux-gnu/liblapack.so" CACHE FILEPATH "LAPACK library")