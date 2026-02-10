cmake_minimum_required(VERSION 3.10)
project(swipl-http)

include("../cmake/PrologPackage.cmake")

if(MULTI_THREADED)
set(SWIPL_WITH_HTTP_SERVER ON)
endif()
if(NOT EMSCRIPTEN)
set(SWIPL_WITH_HTTP_CLIENT ON)
endif()

#
# HTTP SERVER CONFIG
#

if(SWIPL_WITH_HTTP_SERVER)
if(NOT DEFINED JQUERYDIR)
  message("-- Searching for jQuery")
  if(NOT CMAKE_CROSSCOMPILING)
    set(JQUERYFILE jquery.min.js)
    find_path(JQUERYDIR
	      ${JQUERYFILE}
	      PATHS /usr/share/javascript/jquery
	      NO_DEFAULT_PATH)
    # Deal with snap creation
    string(REPLACE "-sdk/current/" "/current/" JQUERYDIR "${JQUERYDIR}")
  endif()

  if(JQUERYDIR AND EXISTS ${JQUERYDIR}/${JQUERYFILE})
    message("-- Using system jQuery from ${JQUERYDIR}/${JQUERYFILE}")
  else()
    message("-- No system jquery.js; using our own")
    set(JQUERYDIR web/js)
    set(JQUERYFILE jquery-3.6.0.min.js)
  endif()

  set(JQUERYDIR  ${JQUERYDIR}  CACHE INTERNAL "Directory for jquery.js")
  set(JQUERYFILE ${JQUERYFILE} CACHE INTERNAL "File for jquery.js")
endif()

configure_file(jquery.pl.in jquery.pl)

set(HTTP_PLATFORM_FILES)
if(UNIX)
  set(HTTP_PLATFORM_FILES http_unix_daemon.pl)
endif()

set(HTTP_SERVER_PL_FILES
    http_parameters.pl http_dispatch.pl http_authenticate.pl http_log.pl
    http_hook.pl http_exception.pl http_wrapper.pl
    http_dirindex.pl http_server_files.pl http_pwp.pl http_host.pl
    http_cookie.pl http_files.pl http_server_health.pl
    http_cors.pl yadis.pl ax.pl http_load.pl http_multipart_plugin.pl
    http_digest.pl http_redis_plugin.pl graphql.pl http_openid.pl
    thread_httpd.pl http_server.pl http_dyn_workers.pl http_session.pl
    README.md htmx.pl ${HTTP_PLATFORM_FILES})

set(HTTP_EXAMPLE_FILES
    README.md
    demo_body.pl demo_client.pl demo_threads.pl
    calc.pl demo_files.pl demo_pwp.pl demo_openid.pl
    demo_daemon.pl upstart-script.conf systemd-script.service
    linux-init-script demo_login.pl demo_rest.pl demo_hello.pl)
prepend(HTTP_EXAMPLE_FILES examples/ ${HTTP_EXAMPLE_FILES})

set(PWP_FILES
    context.pwp index.pwp pwp1.pwp pwp2.pwp pwp3.pwp pwp4.pwp
    pwp5.pwp pwp6.pwp pwp7.pwp pwp8.pwp pwpdb.pl)
prepend(PWP_FILES examples/pwp/ ${PWP_FILES})

set(ICONS back.png compressed.png c.png folder.png generic.png
    layout.png openid-logo-square.png openid-logo-tiny.png)
prepend(ICONS web/icons/ ${ICONS})

endif(SWIPL_WITH_HTTP_SERVER)

configure_file(config.h.cmake config.h)

set(HTML_PL_FILES
    html_write.pl html_decl.pl html_quasiquotations.pl term_html.pl html_head.pl
    http_path.pl mimetype.pl)

set(HTTP_CLIENT_PL_FILES
    http_client.pl http_header.pl js_write.pl
    http_sgml_plugin.pl mimepack.pl dcg_basics.pl
    http_open.pl http_proxy.pl http_error.pl)

swipl_plugin(
    html_libs
    PL_LIB_SUBDIR http
    PL_LIBS ${HTML_PL_FILES})
swipl_plugin(
    http_stream
    C_SOURCES http_stream.c THREADED
    PL_LIB_SUBDIR http
    PL_LIBS http_stream.pl)

test_libs(term_html
	  PACKAGES clib sgml http)

add_custom_target(http)
add_dependencies(http html_libs http_stream)

if(SWIPL_WITH_HTTP_CLIENT)
swipl_plugin(
    http_client_libs
    PL_LIB_SUBDIR http
    PL_LIBS ${HTTP_CLIENT_PL_FILES})

add_dependencies(http http_client_libs)
endif()

if(SWIPL_WITH_HTTP_SERVER)
swipl_plugin(
    http_server_libs
    PL_LIB_SUBDIR http
    PL_LIBS ${HTTP_SERVER_PL_FILES})

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

install_prolog_src(FILES ${CMAKE_CURRENT_BINARY_DIR}/jquery.pl
		   DESTINATION ${SWIPL_INSTALL_LIBRARY}/http)
install_qlfs(${SWIPL_INSTALL_LIBRARY}/http
	     ${CMAKE_CURRENT_BINARY_DIR}/jquery.pl)
if(NOT IS_ABSOLUTE ${JQUERYDIR})
  install_src(plugin_http_jquery
	      FILES ${JQUERYDIR}/${JQUERYFILE}
	      DESTINATION ${SWIPL_INSTALL_LIBRARY}/http/web/js)
endif()

add_dependencies(http http_server_libs websocket http_css http_icons)

swipl_examples(${HTTP_EXAMPLE_FILES})
swipl_examples(${PWP_FILES}
	       SUBDIR pwp)

test_libs(cgi_stream http multipart proxy websocket graphql
	  PACKAGES clib sgml plunit ssl zlib
	  PARENT_LIB)
endif(SWIPL_WITH_HTTP_SERVER)

has_package(ssl HAVE_SSL_PACKAGE)
if(HAVE_SSL_PACKAGE)
  set(doc_depends ssl)
endif()
has_package(zlib HAVE_ZLIB_PACKAGE)
if(HAVE_ZLIB_PACKAGE)
  list(APPEND doc_depends zlib)
endif()

if(SWIPL_WITH_HTTP_SERVER)
  set(HTTP_SECTION_FILES
    websocket.pl hub.pl)
endif()

set(HTTP_PLDOC_FILES
    js_write.pl http_path.pl html_head.pl
    http_open.pl mimepack.pl http_client.pl http_header.pl)
if(SWIPL_WITH_HTTP_SERVER)
  set(HTTP_PLDOC_FILES ${HTTP_PLDOC_FILES}
      http_log.pl http_dispatch.pl http_dirindex.pl http_digest.pl
      http_session.pl http_openid.pl http_dyn_workers.pl http_pwp.pl
      http_files.pl http_cors.pl http_authenticate.pl http_host.pl
      http_server_health.pl htmx.pl)
endif()

pkg_doc(http
	LIBSUBDIR
	    http
	SOURCE
	    post.md --lib=http/html_write
	DEPENDS
	    ${doc_depends}
	SECTION
	    ${HTTP_SECTION_FILES}
	SUBSECTION
	    ${HTTP_PLDOC_FILES}
	SUBSUBSECTION
	    ${HTTP_PLATFORM_FILES})
