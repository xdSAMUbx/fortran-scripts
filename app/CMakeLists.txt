add_executable(main main.f90)
target_compile_options(main PUBLIC -O2 -Wall)
target_link_libraries(main PUBLIC geoutils linalg)