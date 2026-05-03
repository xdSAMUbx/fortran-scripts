add_executable(timer timer.f90)
# Solo esto — no add_executable para el mismo archivo
add_library(legendre_lib legendre.f90)

add_executable(stokes stokes.f90)
target_link_libraries(stokes PRIVATE legendre_lib)
add_executable(grid grids.f90)

add_subdirectory(arrays)
add_subdirectory(matrix)