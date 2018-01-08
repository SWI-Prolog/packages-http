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

#define O_DEBUG 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <errno.h>

#define MAXHDR 1024			/* max size of chink header line */

static atom_t ATOM_close_parent;	/* close_parent(Bool) */
static atom_t ATOM_max_chunk_size;	/* max_chunk_size(Int) */


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef struct chunked_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *chunked_stream;	/* Stream I'm handle of */
  int		    close_parent;	/* close parent on close */
  int		    eof_seen;		/* We saw end-of-file */
  IOENC		    parent_encoding;	/* Saved encoding of parent */
  size_t	    avail;		/* data available */
} chunked_context;


static chunked_context*
alloc_chunked_context(IOSTREAM *s)
{ chunked_context *ctx = PL_malloc(sizeof(*ctx));

  if ( ctx )
  { memset(ctx, 0, sizeof(*ctx));
    ctx->stream       = s;
    ctx->close_parent = FALSE;
  }

  return ctx;
}


static void
free_chunked_context(chunked_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);

  PL_free(ctx);
}


		 /*******************************
		 *	    CHUNKED I/O		*
		 *******************************/

static ssize_t				/* decode */
chunked_read(void *handle, char *buf, size_t size)
{ chunked_context *ctx = handle;

  if ( ctx->eof_seen )
    return 0;

  for(;;)
  { if ( ctx->avail > 0 )			/* data waiting */
    { size_t  max_rd = ctx->avail < size ? ctx->avail : size;
      ssize_t rc;

      if ( (rc = Sfread(buf, sizeof(char), max_rd, ctx->stream)) > 0 )
      { ctx->avail -= rc;

	if ( ctx->avail == 0 )
	{ if ( Sgetc(ctx->stream) != '\r' ||
	       Sgetc(ctx->stream) != '\n' )
	  { Sseterr(ctx->chunked_stream, 0, "Chunk not followed by \\r\\n");
	    return -1;
	  }
	}

	return rc;
      } else if ( rc == 0 )
      { Sseterr(ctx->chunked_stream, 0, "Unexpected EOF in chunked data");
	return -1;
      } else
      { return -1;
      }
    } else
    { char hdr[MAXHDR];
      char *s;


      if ( (s = Sfgets(hdr, sizeof(hdr), ctx->stream)) )
      { char *ehdr;
	long len;

	errno = 0;
	len = strtol(hdr, &ehdr, 16);
	if ( errno || len < 0 )
	{ Sseterr(ctx->chunked_stream, 0, "Bad chunk length");
	  return -1;
	}
	if ( len == 0 )
	{ do
	  { s = Sfgets(hdr, sizeof(hdr), ctx->stream);
	  } while ( s && strcmp(s, "\r\n") != 0 );
	  if ( s )
	  { ctx->eof_seen = TRUE;
	    return 0;
	  }
	  Sseterr(ctx->chunked_stream, 0, "Bad end-of-stream");
	  return -1;
	}
	ctx->avail = len;
	/*continue*/
      } else
      { if ( !Sferror(ctx->stream) )
	  Sseterr(ctx->chunked_stream, 0, "Unexpected EOF in chunked header");
	return -1;
      }
    }
  }
}


static ssize_t				/* encode */
chunked_write(void *handle, char *buf, size_t size)
{ chunked_context *ctx = handle;

  if ( Sfprintf(ctx->stream, "%x\r\n", size) >= 0 &&
       Sfwrite(buf, sizeof(char), size, ctx->stream) == size &&
       Sfprintf(ctx->stream, "\r\n") >= 0 )
    return size;

  return -1;
}


static int
chunked_control(void *handle, int op, void *data)
{ chunked_context *ctx = handle;

  switch(op)
  { case SIO_FLUSHOUTPUT:
    case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
      if ( ctx->stream->functions->control )
	return (*ctx->stream->functions->control)(ctx->stream->handle, op, data);
      return -1;
  }
}


static int
chunked_close(void *handle)
{ chunked_context *ctx = handle;
  int rc = 0;

  DEBUG(1, Sdprintf("chunked_close() ...\n"));

  if ( (ctx->chunked_stream->flags & SIO_OUTPUT) )
  { if ( Sfprintf(ctx->stream, "0\r\n\r\n") < 0 )
      rc = -1;
  }

  ctx->stream->encoding = ctx->parent_encoding;

  if ( ctx->close_parent )
  { IOSTREAM *parent = ctx->stream;
    int rc2;

    free_chunked_context(ctx);
    rc2 = Sclose(parent);
    if ( rc == 0 )
      rc = rc2;
  } else
  { free_chunked_context(ctx);
  }

  return rc;
}


static IOFUNCTIONS chunked_functions =
{ chunked_read,
  chunked_write,
  NULL,					/* seek */
  chunked_close,
  chunked_control,			/* zcontrol */
  NULL,					/* seek64 */
};


		 /*******************************
		 *	 PROLOG CONNECTION	*
		 *******************************/

#define COPY_FLAGS (SIO_INPUT|SIO_OUTPUT| \
		    SIO_TEXT| \
		    SIO_REPXML|SIO_REPPL|\
		    SIO_RECORDPOS)

static foreign_t
pl_http_chunked_open(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  chunked_context *ctx;
  IOSTREAM *s = NULL, *s2 = NULL;
  int close_parent = FALSE;
  int max_chunk_size = 0;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return type_error(head, "option");
    _PL_get_arg(1, head, arg);

    if ( name == ATOM_max_chunk_size )
    { if ( !get_int_ex(arg, &max_chunk_size) )
	return FALSE;
      if ( max_chunk_size <= 0 )
	return domain_error(arg, "positive_integer");
    } else if ( name == ATOM_close_parent )
    { if ( !get_bool_ex(arg, &close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */
  if ( !(ctx = alloc_chunked_context(s)) )
    goto error;
  ctx->close_parent = close_parent;

  if ( !(s2 = Snew(ctx,
		   (s->flags&COPY_FLAGS)|SIO_FBUF,
		   &chunked_functions))	)
    goto error;

  if ( max_chunk_size > 0 )
  { char *buf = PL_malloc(max_chunk_size);
    Ssetbuffer(s2, buf, max_chunk_size);
  }

  s2->encoding = s->encoding;
  ctx->parent_encoding = s->encoding;
  s->encoding = ENC_OCTET;
  ctx->chunked_stream = s2;
  if ( PL_unify_stream(new, s2) )
  { Sset_filter(s, s2);
    PL_release_stream(s);

    return TRUE;
  }

error:
  if ( s )
    PL_release_stream(s);
  if ( s2 )
  { ctx->close_parent = FALSE;
    Sclose(s2);
  } else if ( ctx )
    free_chunked_context(ctx);

  return FALSE;
}


		 /*******************************
		 *	       INSTALL		*
		 *******************************/

static void
install_http_chunked()
{ ATOM_close_parent   = PL_new_atom("close_parent");
  ATOM_max_chunk_size = PL_new_atom("max_chunk_size");

  PL_register_foreign("http_chunked_open",  3, pl_http_chunked_open,  0);
}
