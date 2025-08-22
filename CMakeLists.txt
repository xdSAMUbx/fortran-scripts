cmake_minimum_required(VERSION 3.2)
project(myProject VERSION 0.0.1 LANGUAGES Fortran)

# Genera dependencia en los modulos para compilar con ninja
set(CMAKE_Fortran_PREPROCESS ON)

# Carpetas de binarios
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin") # Binarios

# Genera la carpeta para procesar los módulos de Fortran
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

# Librerias necesarias para el proyecto
find_package(LAPACK REQUIRED)
find_package(BLAS REQUIRED)

# Permitiendo la depuración
set(CMAKE_BUILD_TYPE Debug)

add_subdirectory(src)
add_subdirectory(app)