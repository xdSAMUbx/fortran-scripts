#option(COARR "Execute files for coarrays functionalities" OFF)
# Genera la carpeta para procesar los módulos de Fortran
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

# Obtiene todos los archivos FYPP
file(GLOB FYPP_FILES "*.fypp")

# Creamos una lista para almacenar los nombres de los archivos .f90 generados
set(GENERATED_SOURCES "")

foreach(FYPP_FILE IN LISTS FYPP_FILES)
    # Se obtiene el archivo sin extensión
    get_filename_component(BASE_NAME "${FYPP_FILE}" NAME_WE)
    get_filename_component(SRC_DIR "${FYPP_FILE}" DIRECTORY)

    # Archivo de salida en el mismo directorio del .fypp
    set(GENERATED_F90_FILE "${BASE_NAME}.f90")
    set(OUTPUT_PATH "${SRC_DIR}/${GENERATED_F90_FILE}")

    add_custom_command(
        OUTPUT "${OUTPUT_PATH}"
        COMMAND fypp "${FYPP_FILE}" "${OUTPUT_PATH}"
        DEPENDS "${FYPP_FILE}"
        WORKING_DIRECTORY ${SRC_DIR}
        COMMENT "Procesando ${FYPP_FILE} a ${GENERATED_F90_FILE}"
        VERBATIM
    )

    list(APPEND GENERATED_SOURCES "${OUTPUT_PATH}")
endforeach()

add_library(geoutils STATIC ${GENERATED_SOURCES})
#if(COARR)
# target_compile_options(objetivo no main PRIVATE -fcoarray=single)
#endif()