/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2015, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <config.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

static IOFUNCTIONS cgi_functions;

typedef struct chunked_trailer
{ struct chunked_trailer *next;
  atom_t key;
  atom_t value;
} chunked_trailer;

typedef struct chunked_metadata
{ term_t	    chunk_ext;		/* Chunk extensions */
  chunked_trailer  *trailer;		/* Reply trailer fields */
  chunked_trailer  *trailer_tail;
} chunked_metadata;

typedef struct chunked_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *chunked_stream;	/* Stream I'm handle of */
  int		    close_parent;	/* close parent on close */
  int		    eof_seen;		/* We saw end-of-file */
  IOENC		    parent_encoding;	/* Saved encoding of parent */
  size_t	    avail;		/* data available */
  chunked_metadata *metadata;		/* Trailer and chunk extensions */
} chunked_context;

typedef enum
{ CGI_HDR  = 0,
  CGI_DATA,
  CGI_DISCARDED
} cgi_state;

typedef struct cgi_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *cgi_stream;		/* Stream I'm handle of */
  IOENC		    parent_encoding;	/* Saved encoding of parent */
  chunked_metadata *metadata;		/* Chunked extensions support */
					/* Prolog attributes */
  module_t	    module;		/* Calling module */
  record_t	    hook;		/* Hook called on action */
  record_t	    request;		/* Associated request term */
  record_t	    header;		/* Associated reply header term */
  atom_t	    transfer_encoding;	/* Current transfer encoding */
  atom_t	    connection;		/* Keep alive? */
  atom_t	    method;		/* method of the request */
					/* state */
  cgi_state	    state;		/* Current state */
					/* data buffering */
  size_t	    data_offset;	/* Start of real data */
  char		   *data;		/* Buffered data */
  size_t	    datasize;		/* #bytes buffered */
  size_t	    dataallocated;	/* #bytes allocated */
  size_t	    chunked_written;	/* #bytes written in chunked encoding */
  int64_t	    id;			/* Identifier */
  unsigned int	    magic;		/* CGI_MAGIC */
} cgi_context;


static atom_t ATOM_chunked;		/* chunked */

#include "http_error.c"
#include "http_chunked.c"
#include "cgi_stream.c"
#include "stream_range.c"
#include "multipart.c"

install_t
install_http_stream(void)
{ init_errors();
  install_http_chunked();
  install_cgi_stream();
  install_stream_range();
  install_multipart();
}
