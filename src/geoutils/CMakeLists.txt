#option(COARR "Execute files for coarrays functionalities" OFF)
# Genera la carpeta para procesar los binarios de geoutils
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

add_library(geoutils STATIC point.fypp pruebageo.f90)

#if(COARR)
# target_compile_options(objetivo no main PRIVATE -fcoarray=single)
#endif()