cmake_minimum_required(VERSION 2.8.12)
project(swipl-http)

include("../cmake/PrologPackage.cmake")

AC_CHECK_HEADERS(fcntl.h poll.h sys/ioctl.h sys/select.h unistd.h)
AC_CHECK_FUNCS(el_cursor el_wset poll)

configure_file(config.h.cmake config.h)

swipl_plugin(
    http_stream
    C_SOURCES http_stream.c
    PL_LIB_SUBDIR http
    PL_LIBS http_stream.pl)
