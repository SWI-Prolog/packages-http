cmake_minimum_required(VERSION 2.8.12)
project(swipl-http)

include("../cmake/PrologPackage.cmake")

AC_CHECK_HEADERS(fcntl.h poll.h sys/ioctl.h sys/select.h unistd.h)
AC_CHECK_FUNCS(el_cursor el_wset poll)

configure_file(config.h.cmake config.h)
configure_file(jquery.pl.in jquery.pl)

set(HTTP_PL_FILES
    html_write.pl http_client.pl http_header.pl
    http_sgml_plugin.pl mimepack.pl mimetype.pl dcg_basics.pl
    thread_httpd.pl xpce_httpd.pl inetd_httpd.pl http_wrapper.pl
    http_open.pl http_proxy.pl http_session.pl http_error.pl
    http_parameters.pl http_dispatch.pl http_authenticate.pl http_log.pl
    http_path.pl http_hook.pl html_head.pl http_exception.pl
    http_dirindex.pl http_server_files.pl http_pwp.pl http_host.pl
    http_openid.pl js_write.pl js_grammar.pl http_cookie.pl http_files.pl
    http_cors.pl http_unix_daemon.pl yadis.pl ax.pl html_quasiquotations.pl
    http_load.pl http_multipart_plugin.pl http_digest.pl)

set(HTTP_EXAMPLE_FILES
    README.md
    demo_body.pl demo_client.pl demo_threads.pl
    demo_xpce.pl calc.pl demo_files.pl demo_pwp.pl demo_openid.pl
    demo_daemon.pl upstart-script.conf systemd-script.service)
prepend(HTTP_EXAMPLE_FILES examples/ ${HTTP_EXAMPLE_FILES})

swipl_plugin(
    http
    PL_LIB_SUBDIR http
    PL_LIBS ${HTTP_PL_FILES})

swipl_plugin(
    http_stream
    C_SOURCES http_stream.c
    PL_LIB_SUBDIR http
    PL_LIBS http_stream.pl)
swipl_plugin(
    json
    C_SOURCES json.c
    PL_LIB_SUBDIR http
    PL_LIBS json.pl json_convert.pl http_json.pl)
swipl_plugin(
    websocket
    C_SOURCES websocket.c
    PL_LIB_SUBDIR http
    PL_LIBS websocket.pl hub.pl)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/jquery.pl
	DESTINATION ${SWIPL_INSTALL_LIBRARY}/http)

swipl_examples(${HTTP_EXAMPLE_FILES})

test_libs(cgi_stream http json multipart proxy websocket
	  PACKAGES clib sgml plunit ssl
	  PARENT_LIB)
