# Preprocesando fypp por si hay en la carpeta
processFypp(${CMAKE_CURRENT_SOURCE_DIR} GENERATED_SOURCES)

add_library(linalg STATIC ${GENERATED_SOURCES})

# Apunta a las librerias básicas como LAPACK y BLAS
target_link_libraries(linalg PUBLIC
    LAPACK::LAPACK
    BLAS::BLAS
)