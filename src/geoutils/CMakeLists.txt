#option(COARR "Execute files for coarrays functionalities" OFF)
# Genera la carpeta para procesar los módulos de Fortran
set(CMAKE_Fortran_MODOUT_FLAG ${CMAKE_CURRENT_BINARY_DIR}/mod)
include(${CMAKE_SOURCE_DIR}/config/config.cmake)

# Llama a la función primero
processFypp(${CMAKE_CURRENT_SOURCE_DIR} GENERATED_SOURCES)

# Ahora sí crea la librería con las fuentes procesadas
add_library(geoutils STATIC ${GENERATED_SOURCES})

target_link_libraries(geoutils PUBLIC
    LAPACK::LAPACK
    BLAS::BLAS
)
#if(COARR)
# target_compile_options(objetivo no main PRIVATE -fcoarray=single)
#endif()