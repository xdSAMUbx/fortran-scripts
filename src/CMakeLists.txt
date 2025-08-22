option(BUILD_LIBRARIES "Build the src libraries" ON)

if(BUILD_LIBRARIES)
    add_subdirectory(geoutils)
    add_subdirectory(linalg)
endif()