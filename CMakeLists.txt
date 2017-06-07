cmake_minimum_required(VERSION 2.6)

project(xfp)

add_subdirectory(rdparse)
include_directories(${CMAKE_SOURCE_DIR}/rdparse/include)

set(FLAGS "-g" "-O0" "-Wall" "-Wno-maybe-uninitialized" "-Werror" "-pedantic" "-std=c11" "-D_POSIX_C_SOURCE=200809L" "-D_GNU_SOURCE")

file(GLOB SOURCES "src/*.c")

foreach(FILE ${SOURCES})
  if("${FILE}" MATCHES "src/test.c" OR "${FILE}" MATCHES "src/repl.c")
    list(REMOVE_ITEM SOURCES "${FILE}")
  endif()
endforeach()


include_directories("rdparse")

add_executable(xfp ${SOURCES} "src/test.c")
target_compile_options(xfp PRIVATE ${FLAGS})
target_link_libraries (xfp rdparse)

add_executable(repl ${SOURCES} "src/repl.c")
target_compile_options(repl PRIVATE ${FLAGS})
target_link_libraries (repl rdparse readline)
