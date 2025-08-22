add_executable(main main.f90)

add_custom_target(run
    COMMAND cafrun -np 5 $<TARGET_FILE:main>
    DEPENDS main
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Ejecutando con cafrun..."
)

target_compile_options(main PUBLIC -O2 -Wall)

target_link_libraries(main PUBLIC 
    geoutils 
    linalg
    BLAS::BLAS
    LAPACK::LAPACK
)