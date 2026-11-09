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

#define O_DEBUG 1

static int debuglevel = 0;

#ifdef O_DEBUG
#define DEBUG(n, g) if ( debuglevel >= n ) g
#else
#define DEBUG(n, g) (void)0
#endif

#ifdef O_DEBUG
static foreign_t
http_stream_debug(term_t level)
{ return PL_get_integer(level, &debuglevel);
}
#endif

#include "http_chunked.c"
#include "cgi_stream.c"
#include "stream_range.c"
#include "multipart.c"

/** http_read_line(+Stream, -Line, +MaxLength) is det.
 *
 * Read a line as a code list from Stream, removing the line terminator
 * (LF or CRLF).  Unify Line with `end_of_file` if Stream is at end of
 * file.  Raise resource_error(max_line_length) if the line is longer
 * than MaxLength characters.  Used to read HTTP header lines.
 */

static foreign_t
http_read_line(term_t stream, term_t line, term_t max)
{ IOSTREAM *s;
  size_t maxlen, len = 0, allocated = 256;
  int fast[256];
  int *buf = fast;
  int rc = FALSE;

  if ( !PL_get_size_ex(max, &maxlen) ||
       !PL_get_stream(stream, &s, SIO_INPUT) )
    return FALSE;

  for(;;)
  { int c = Sgetcode(s);

    if ( c == EOF )
    { if ( Sferror(s) || PL_exception(0) )
	goto out;
      if ( len == 0 )
      { rc = PL_unify_atom_chars(line, "end_of_file");
	goto out;
      }
      break;
    }
    if ( c == '\n' )
    { if ( len > 0 && buf[len-1] == '\r' )
	len--;
      break;
    }
    if ( len >= maxlen + 1 )		/* allow for CR */
    { rc = PL_resource_error("max_line_length");
      goto out;
    }
    if ( len == allocated )
    { size_t na = allocated*2;
      int *nb;

      if ( buf == fast )
      { if ( (nb = malloc(na*sizeof(int))) )
	  memcpy(nb, buf, len*sizeof(int));
      } else
      { nb = realloc(buf, na*sizeof(int));
      }
      if ( !nb )
      { rc = PL_resource_error("memory");
	goto out;
      }
      buf = nb;
      allocated = na;
    }
    buf[len++] = c;
  }

  if ( len > maxlen )
  { rc = PL_resource_error("max_line_length");
  } else
  { term_t l = PL_new_term_ref();
    term_t h = PL_new_term_ref();

    rc = PL_put_nil(l);
    while(rc && len > 0)
    { rc = ( PL_put_integer(h, buf[--len]) &&
	     PL_cons_list(l, h, l) );
    }
    rc = rc && PL_unify(line, l);
  }

out:
  if ( buf != fast )
    free(buf);
  return PL_release_stream(s) && rc;
}

install_t
install_http_stream(void)
{
#ifdef O_DEBUG
  PL_register_foreign("http_stream_debug", 1, http_stream_debug, 0);
#endif
  PL_register_foreign("http_read_line", 3, http_read_line, 0);
  install_http_chunked();
  install_cgi_stream();
  install_stream_range();
  install_multipart();
}
