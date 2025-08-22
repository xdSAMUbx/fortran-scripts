# Genera la carpeta para procesar los binarios de geoutils
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

# Preprocesando fypp por si hay en la carpeta
processFypp(${CMAKE_CURRENT_SOURCE_DIR} GENERATED_SOURCES)

add_library(linalg STATIC ${GENERATED_SOURCES})