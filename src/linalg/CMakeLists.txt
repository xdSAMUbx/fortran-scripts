# Genera la carpeta para procesar los binarios de geoutils
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

add_library(linalg STATIC eye.fypp pruebalinal.f90)