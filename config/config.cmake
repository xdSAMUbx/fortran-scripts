# Esta función permite procesar todos los fypp de las carpetas creadas inicialmente
function(processFypp currentSrcDir outVar)
    # Obtiene todos los archivos FYPP en el directorio especificado
    file(GLOB FYPP_FILES "${currentSrcDir}/*.fypp")

    # Si no hay archivos fypp, termina
    if(NOT FYPP_FILES)
        # Devolvemos vacío para evitar error
        set(${outVar} "" PARENT_SCOPE)
        return()
    endif()
    
    set(GENERATED_F90_FILES "")
    # Itera sobre los archivos y genera los comandos personalizados
    foreach(FYPP_FILE IN LISTS FYPP_FILES)
        # Se obtiene el archivo sin extensión y la ruta de origen
        get_filename_component(BASE_NAME "${FYPP_FILE}" NAME_WE)

        # Se define la ruta de salida del archivo .f90
        set(GENERATED_F90_FILE "${currentSrcDir}/${BASE_NAME}.f90")

        add_custom_command(
            OUTPUT "${GENERATED_F90_FILE}"
            COMMAND fypp "${FYPP_FILE}" "${GENERATED_F90_FILE}"
            DEPENDS "${FYPP_FILE}"
            WORKING_DIRECTORY "${currentSrcDir}"
            COMMENT "Procesando ${FYPP_FILE} a ${BASE_NAME}.f90"
            VERBATIM
        )
        list(APPEND GENERATED_F90_FILES "${GENERATED_F90_FILE}")
    endforeach()
    # Devuelve la lista al caller
    set(${outVar} "${GENERATED_F90_FILES}" PARENT_SCOPE)
endfunction()

