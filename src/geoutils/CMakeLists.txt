# Genera la carpeta para procesar los módulos de Fortran
include(${CMAKE_SOURCE_DIR}/config/config.cmake)

# Llama a la función primero
processFypp(${CMAKE_CURRENT_SOURCE_DIR} GENERATED_SOURCES)

# Ahora sí crea la librería con las fuentes procesadas
add_library(geoutils STATIC ${GENERATED_SOURCES})

target_link_libraries(geoutils PUBLIC
    LAPACK::LAPACK
    BLAS::BLAS
)