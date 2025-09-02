cmake_minimum_required(VERSION 3.20)

project(thale
    VERSION 0.1.0
    LANGUAGES C
)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

set(SOURCES
    source/main.c
)

add_executable(${PROJECT_NAME} ${SOURCES})

target_include_directories(${PROJECT_NAME} PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/source/config.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/config.h
    @ONLY
)

option(ENABLE_WARNINGS "Enable extra warnings" ON)

if (ENABLE_WARNINGS)
    if (MSVC)
        target_compile_options(${PROJECT_NAME} PRIVATE /w4 /permissive-)
    else()
        target_compile_options(${PROJECT_NAME} PRIVATE
            -Wall -Wpedantic -Wextra  
            -Wwrite-strings -Wredundant-decls -Wdisabled-optimization 
            -Wdouble-promotion -Wmissing-declarations -Wconversion 
            -Wstrict-overflow=2 -Wvla
            -fstack-protector-all
        )
    endif()
endif()

option(ENABLE_SANITIZERS "Enable sanitizers (Debug only)" OFF)

if (ENABLE_SANITIZERS AND CMAKE_BUILD_TYPE STREQUAL "Debug")
    if (UNIX AND NOT APPLE)
        target_compile_options(${PROJECT_NAME} PRIVATE -fsanitize=address,undefined)
        target_link_options(${PROJECT_NAME} PRIVATE -fsanitize=address,undefined)
    elseif (APPLE)
        target_compile_options(${PROJECT_NAME} PRIVATE -fsanitize=address)
        target_link_options(${PROJECT_NAME} PRIVATE -fsanitize=address)
    endif()
endif()

target_compile_definitions(${PROJECT_NAME} PRIVATE
    $<$<PLATFORM_ID:Windows>:WINDOWS_PLATFORM>
    $<$<PLATFORM_ID:Darwin>:MACOS_PLATFORM>
    $<$<PLATFORM_ID:Linux>:LINUX_PLATFORM>
)

include(CPack)