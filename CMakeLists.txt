cmake_minimum_required(VERSION 3.1.0)

set(PROJECT_NAME nanovg)

project(${PROJECT_NAME})

set(SRC_DIR "./src")

if(NANOVG_METAL_IMPLEMENTATION)
file(GLOB SRCS ${SRC_DIR}/*.cpp ${SRC_DIR}/*.m)
else()
file(GLOB SRCS ${SRC_DIR}/*.cpp)
endif()

file(GLOB HDRS ${SRC_DIR}/*.h)

add_library(${PROJECT_NAME} ${SRCS})

target_compile_options(nanovg PRIVATE "-fobjc-arc")
set_target_properties(
    nanovg
    PROPERTIES
    XCODE_ATTRIBUTE_CLANG_ENABLE_OBJC_ARC YES
)

install(TARGETS ${PROJECT_NAME}
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include/${PROJECT_NAME}
)


install(FILES ${HDRS} DESTINATION include/${PROJECT_NAME}/)
