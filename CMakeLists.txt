cmake_minimum_required(VERSION 2.8.12)
project(swipl-http)

include("../cmake/PrologPackage.cmake")

if(NOT DEFINED JQUERYDIR)
  message("-- Searching for jQuery")
  if(NOT CMAKE_CROSSCOMPILING)
    set(JQUERYFILE jquery.min.js)
    find_path(JQUERYDIR
	      ${JQUERYFILE}
	      PATHS /usr/share/javascript/jquery
	      NO_DEFAULT_PATH)
  endif()

  if(JQUERYDIR AND EXISTS ${JQUERYDIR}/${JQUERYFILE})
    message("-- Using system jQuery from ${JQUERYDIR}/${JQUERYFILE}")
  else()
    message("-- No system jquery.js; using our own")
    set(JQUERYDIR web/js)
    set(JQUERYFILE jquery-1.11.3.min.js)
  endif()

  set(JQUERYDIR  ${JQUERYDIR}  CACHE INTERNAL "Directory for jquery.js")
  set(JQUERYFILE ${JQUERYFILE} CACHE INTERNAL "File for jquery.js")
endif()

configure_file(config.h.cmake config.h)
configure_file(jquery.pl.in jquery.pl)

set(HTTP_PLATFORM_FILES)
if(UNIX)
  set(HTTP_PLATFORM_FILES http_unix_daemon.pl)
endif()

set(HTTP_PL_FILES
    html_write.pl http_client.pl http_header.pl
    http_sgml_plugin.pl mimepack.pl mimetype.pl dcg_basics.pl
    thread_httpd.pl xpce_httpd.pl inetd_httpd.pl http_wrapper.pl
    http_open.pl http_proxy.pl http_session.pl http_error.pl
    http_parameters.pl http_dispatch.pl http_authenticate.pl http_log.pl
    http_path.pl http_hook.pl html_head.pl http_exception.pl
    http_dirindex.pl http_server_files.pl http_pwp.pl http_host.pl
    http_openid.pl js_write.pl js_grammar.pl http_cookie.pl http_files.pl
    http_cors.pl yadis.pl ax.pl html_quasiquotations.pl
    http_load.pl http_multipart_plugin.pl http_digest.pl README.md
    ${HTTP_PLATFORM_FILES})

set(HTTP_EXAMPLE_FILES
    README.md
    demo_body.pl demo_client.pl demo_threads.pl
    calc.pl demo_files.pl demo_pwp.pl demo_openid.pl
    demo_daemon.pl upstart-script.conf systemd-script.service
    linux-init-script demo_login.pl demo_rest.pl)
prepend(HTTP_EXAMPLE_FILES examples/ ${HTTP_EXAMPLE_FILES})

set(PWP_FILES
    context.pwp index.pwp pwp1.pwp pwp2.pwp pwp3.pwp pwp4.pwp
    pwp5.pwp pwp6.pwp pwp7.pwp pwp8.pwp pwpdb.pl)
prepend(PWP_FILES examples/pwp/ ${PWP_FILES})

set(ICONS back.png compressed.png c.png folder.png generic.png
    layout.png openid-logo-square.png openid-logo-tiny.png)
prepend(ICONS web/icons/ ${ICONS})

swipl_plugin(
    http_libs
    PL_LIB_SUBDIR http
    PL_LIBS ${HTTP_PL_FILES})

swipl_plugin(
    http_stream
    C_SOURCES http_stream.c THREADED
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
swipl_plugin(
    http_css
    PL_LIB_SUBDIR http/web/css
    PL_LIBS web/css/dirindex.css web/css/openid.css)
swipl_plugin(
    http_icons
    PL_LIB_SUBDIR http/web/icons
    PL_LIBS ${ICONS})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/jquery.pl
	DESTINATION ${SWIPL_INSTALL_LIBRARY}/http)
if(NOT IS_ABSOLUTE ${JQUERYDIR})
  install_src(plugin_http_jquery
	      FILES ${JQUERYDIR}/${JQUERYFILE}
	      DESTINATION ${SWIPL_INSTALL_LIBRARY}/http/web/js)
endif()

add_custom_target(http)
add_dependencies(http http_libs http_stream json websocket http_css http_icons)

swipl_examples(${HTTP_EXAMPLE_FILES})
swipl_examples(${PWP_FILES}
	       SUBDIR pwp)

test_libs(cgi_stream http json multipart proxy websocket
	  PACKAGES clib sgml plunit ssl zlib
	  PARENT_LIB)

if(INSTALL_TESTS)
  install(FILES examples/demo_body.pl
	  DESTINATION ${INSTALL_TESTS_DIR}/packages/http/examples)
endif()

pkg_doc(http
	LIBSUBDIR
	    http
	SOURCES
	    json.md figs/httpserver.pdf figs/httpserver.png
	SOURCE post.md --lib=http/html_write
	SECTION
	    websocket.pl hub.pl
	SUBSECTION
	    http_log.pl http_path.pl html_head.pl http_session.pl
	    http_dispatch.pl http_dirindex.pl http_pwp.pl
	    http_open.pl http_host.pl http_openid.pl js_write.pl
	    http_files.pl http_cors.pl mimepack.pl
	    http_authenticate.pl http_client.pl
	    http_header.pl http_digest.pl
	IMAGES httpserver.png
	SUBSUBSECTION
	    ${HTTP_PLATFORM_FILES}
        DEPENDS ssl)
