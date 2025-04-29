/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2025, University of Amsterdam
                              VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define O_DEBUG 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <errno.h>

static atom_t ATOM_close_parent;	/* close_parent(Bool) */
static atom_t ATOM_boundary;		/* boundary(String) */

#ifndef DEBUG
static int multipart_debug = 0;
#define DEBUG(l, g) do { if (multipart_debug >= l) {g;} } while(0)
#endif


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

typedef enum state
{ s_start = 0,
  s_start_boundary,
  s_part_data_start,
  s_part_data,
  s_part_data_almost_boundary,
  s_part_data_boundary,
  s_part_data_almost_end,
  s_part_data_end,
  s_part_data_next,
  s_part_data_final_hyphen,
  s_end
} mp_state;

typedef struct multipart_context
{ IOSTREAM	   *stream;		/* Original stream */
  IOSTREAM	   *multipart_stream;	/* Stream I'm handle of */
  int		    close_parent;	/* close parent on close */
  IOENC		    parent_encoding;	/* Saved encoding of parent */
  char		   *boundary;		/* Our boundary */
  size_t	    boundary_length;	/* length of the boundary */
  char		   *lookbehind;		/* Collected part of potential boundary */
  char		   *unprocessed;	/* Could not emit this data now */
  size_t	    unprocessed_len;	/* length of unprocessed data */
  size_t	    index;
  mp_state	    state;		/* current state */
} multipart_context;


static size_t
multipart_parser_execute(multipart_context* p,
			 const char *buf, size_t len,
			 char **out, size_t *out_lenp);

static multipart_context*
alloc_multipart_context(IOSTREAM *s)
{ multipart_context *ctx = malloc(sizeof(*ctx));

  if ( ctx )
  { memset(ctx, 0, sizeof(*ctx));
    ctx->stream       = s;
  }

  return ctx;
}


static void
free_multipart_context(multipart_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);
  else
    PL_release_stream(ctx->stream);

  if ( ctx->boundary )
    free(ctx->boundary);

  free(ctx);
}


		 /*******************************
		 *	    MULTIPART I/O	*
		 *******************************/

static ssize_t
multipart_read(void *handle, char *buf, size_t size)
{ multipart_context *ctx = handle;
  IOSTREAM *in = ctx->stream;
  char    *out = buf;
  size_t  left = size;

  if ( ctx->unprocessed_len )
  { size_t len = ctx->unprocessed_len;

    DEBUG(1, Sdprintf("Unprocessed: %ld\n", (long)len));

    if ( len > size )
      len = size;
    memcpy(buf, ctx->unprocessed, len);
    ctx->unprocessed_len -= len;
    ctx->unprocessed += len;

    return len;
  }

  if ( ctx->state == s_end )
  { return 0;
  } else if ( ctx->state == s_part_data_end )
  { return 0;
  }

  for(;;)
  { if ( in->bufp >= in->limitp )
    { if ( S__fillbuf(in) == EOF )
      { Sseterr(in, SIO_FERR, "Incomplete multipart/form-data");
	return -1;
      }
      in->bufp--;
    }

    do
    { size_t processed;

#if defined(O_DEBUG) && defined(DEBUG_MULTIPART)
      char tmp[10000];
      memcpy(tmp, in->bufp, in->limitp-in->bufp);
      tmp[in->limitp-in->bufp] = 0;

      DEBUG(1, Sdprintf("multipart_parser_execute(%ld bytes: \"%s\")\n",
			(long)(in->limitp-in->bufp), tmp));
#endif

      processed = multipart_parser_execute(ctx,
					   in->bufp, in->limitp-in->bufp,
					   &out, &left);
      DEBUG(1, Sdprintf("processed %ld bytes, state=%d, left=%ld\n",
			(long)processed, ctx->state, (long)left));

      if ( processed == 0 )
      { Sseterr(in, SIO_FERR, "Invalid multipart/form-data");
	return -1;
      }

      in->bufp += processed;
      if ( in->position )		   /* looses line count/position */
      { in->position->byteno += processed;
	in->position->charno += processed; /* assumes no multibyte encoding */
      }
    } while( left > 0 &&
	     in->bufp < in->limitp &&
	     ctx->state != s_end &&
	     ctx->state != s_part_data_end );

    if ( out > buf || ctx->state == s_end || ctx->state == s_part_data_end )
    { DEBUG(1, Sdprintf("Reply %ld bytes\n", (long)(out-buf)));
      return out-buf;
    }
  }
}


static int
multipart_control(void *handle, int op, void *data)
{ multipart_context *ctx = handle;

  switch(op)
  { case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
      if ( ctx->stream->functions->control )
	return (*ctx->stream->functions->control)(ctx->stream->handle, op, data);
      return -1;
  }
}


static int
multipart_close(void *handle)
{ multipart_context *ctx = handle;
  int rc = 0;

  DEBUG(1, Sdprintf("multipart_close() ...\n"));

  ctx->stream->encoding = ctx->parent_encoding;
  if ( ctx->stream->encoding == ENC_OCTET )
    ctx->stream->flags &= ~SIO_TEXT;
  else
    ctx->stream->flags |= SIO_TEXT;

  if ( ctx->close_parent )
  { IOSTREAM *parent = ctx->stream;
    int rc2;

    free_multipart_context(ctx);
    rc2 = Sclose(parent);
    if ( rc == 0 )
      rc = rc2;
  } else
  { free_multipart_context(ctx);
  }

  return rc;
}


static IOFUNCTIONS multipart_functions =
{ multipart_read,
  NULL,
  NULL,					/* seek */
  multipart_close,
  multipart_control,			/* zcontrol */
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
multipart_open(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  multipart_context *ctx;
  IOSTREAM *s, *s2;
  int close_parent = FALSE;
  char *boundary = NULL;
  size_t boundary_len = 0;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);

    if ( name == ATOM_boundary )
    { if ( !PL_get_nchars(arg, &boundary_len, &boundary,
			  CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
	return FALSE;
    } else if ( name == ATOM_close_parent )
    { if ( !PL_get_bool_ex(arg, &close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */
  if ( !(ctx = alloc_multipart_context(s)) )
    return PL_resource_error("memory");
  ctx->close_parent = close_parent;

  if ( boundary )
  { if ( !(ctx->boundary = malloc(boundary_len*2+9)) )
    { free_multipart_context(ctx);
      return PL_resource_error("memory");
    }
    memcpy(ctx->boundary, "--", 2);
    memcpy(ctx->boundary+2, boundary, boundary_len);
    boundary_len += 2;
    ctx->boundary_length = boundary_len;
    ctx->boundary[boundary_len] = 0;
    ctx->lookbehind = ctx->boundary+boundary_len+1;
  }

  if ( !(s2 = Snew(ctx,
		   (s->flags&COPY_FLAGS)|SIO_FBUF,
		   &multipart_functions))	)
  { free_multipart_context(ctx);	/* no memory */

    return FALSE;
  }

  s2->encoding = s->encoding;
  ctx->parent_encoding = s->encoding;
  s->encoding = ENC_OCTET;
  s->flags &= ~SIO_TEXT;
  ctx->multipart_stream = s2;
  if ( PL_unify_stream(new, s2) )
  { Sset_filter(s, s2);
    PL_release_stream(s);

    return TRUE;
  } else if ( PL_exception(0) )
  { return FALSE;
  } else
  { return PL_uninstantiation_error(new);
  }
}


static foreign_t
multipart_open_next(term_t stream)
{ IOSTREAM *s;
  multipart_context *ctx;

  if ( !PL_get_stream_handle(stream, &s) )
    return FALSE;
  if ( s->functions != &multipart_functions )
  { PL_release_stream(s);
    return PL_type_error("multipart_stream", stream);
  }
  ctx = s->handle;

  switch ( ctx->state )
  { case s_part_data_end:
    { ctx->state = s_part_data_next;
      Sclearerr(ctx->multipart_stream);
      ctx->multipart_stream->encoding = ENC_OCTET;
      ctx->multipart_stream->flags &= ~SIO_TEXT;
      return TRUE;
    }
    case s_end:
      return FALSE;
    default:
      return PL_permission_error("open_next", "multi_part_stream", stream);
  }
}


		 /*******************************
		 *	       INSTALL		*
		 *******************************/

static void
install_multipart(void)
{ ATOM_close_parent   = PL_new_atom("close_parent");
  ATOM_boundary       = PL_new_atom("boundary");

  PL_register_foreign("multipart_open",      3, multipart_open,      0);
  PL_register_foreign("multipart_open_next", 1, multipart_open_next, 0);
}


		 /*******************************
		 *				*
		 *******************************/

/* Based on node-formidable by Felix Geisendörfer
 * Igor Afonov - afonov@gmail.com - 2012
 * MIT License - http://www.opensource.org/licenses/mit-license.php
 */

#define LF 10
#define CR 13

static void multipart_log(const char * format, ...)
{
#ifdef DEBUG_MULTIPART
    va_list args;
    va_start(args, format);

    DEBUG(2,
	  { Sdprintf("[HTTP_MULTIPART_PARSER] %s:%d: ", __FILE__, __LINE__);
	    Svdprintf(format, args);
	    Sdprintf("\n");
	  });

    va_end(args);
#endif
}


static size_t
multipart_parser_execute(multipart_context* p,
			 const char *buf, size_t len,
			 char **out, size_t *out_lenp)
{ size_t i = 0;
  size_t mark = 0;
  char c;
  int is_last = 0;

#define	NOTIFY_CB(f) (void)0
#define EMIT_DATA_CB(part_data, data, len)		\
	do { size_t _len = (len);			\
	     char *_data = (char*)(data);		\
	     if	( _len > *out_lenp )			\
	     { p->unprocessed     = &_data[*out_lenp];	\
	       p->unprocessed_len = _len - *out_lenp;	\
	       _len = *out_lenp;			\
	     }						\
	     memcpy(*out, _data, _len);			\
	     (*out) += _len;				\
	     (*out_lenp) -= _len;			\
	} while(0)

  while(i < len && p->unprocessed_len == 0) {
    c = buf[i];
    is_last = (i == (len - 1));
    switch (p->state) {
      case s_start:
	multipart_log("s_start");
        p->index = 0;
        p->state = s_start_boundary;

      /* fallthrough */
      case s_start_boundary:
	multipart_log("s_start_boundary");
        if (p->index == p->boundary_length) {
          if (c != CR) {
            return i;
          }
          p->index++;
          break;
        } else if (p->index == (p->boundary_length + 1)) {
          if (c != LF) {
            return i;
          }
          p->index = 0;
          NOTIFY_CB(part_data_begin);
          p->state = s_part_data_start;
          break;
        }
        if (c != p->boundary[p->index]) {
           /* Skip anything before the first boundary
              RFC-1341 refers to this as the 'preamble' and says we should ignore it */
           p->index = -1;
        }
        p->index++;
        break;

      case s_part_data_start:
	multipart_log("s_part_data_start at %ld", (long)i);
        mark = i;
        p->state = s_part_data;

      /* fallthrough */
      case s_part_data:
        multipart_log("s_part_data");
        if (c == CR) {
            EMIT_DATA_CB(part_data, buf + mark, i - mark);
            mark = i;
            p->state = s_part_data_almost_boundary;
            p->lookbehind[0] = CR;
            break;
        }
        if (is_last)
            EMIT_DATA_CB(part_data, buf + mark, (i - mark) + 1);
        break;

      case s_part_data_almost_boundary:
        multipart_log("s_part_data_almost_boundary");
        if (c == LF) {
            p->state = s_part_data_boundary;
            p->lookbehind[1] = LF;
            p->index = 0;
            break;
        }
        EMIT_DATA_CB(part_data, p->lookbehind, 1);
        p->state = s_part_data;
        mark = i --;
        break;

      case s_part_data_boundary:
        multipart_log("s_part_data_boundary");
        if (p->boundary[p->index] != c) {
          EMIT_DATA_CB(part_data, p->lookbehind, 2 + p->index);
          p->state = s_part_data;
          mark = i --;
          break;
        }
        p->lookbehind[2 + p->index] = c;
        if ((++ p->index) == p->boundary_length) {
            NOTIFY_CB(part_data_end);
            p->state = s_part_data_almost_end;
        }
        break;

      case s_part_data_almost_end:
        multipart_log("s_part_data_almost_end");
        if (c == '-') {
            p->state = s_part_data_final_hyphen;
            break;
        }
        if (c == CR) {
            p->state = s_part_data_end;
	    i++;
        }
        return i;

      case s_part_data_final_hyphen:
        multipart_log("s_part_data_final_hyphen");
        if (c == '-') {
            NOTIFY_CB(body_end);
            p->state = s_end;
            break;
        }
        return i;

      case s_part_data_next:
        multipart_log("s_part_data_next");
        if (c == LF) {
            p->state = s_part_data_start;
            NOTIFY_CB(part_data_begin);
            break;
        }
        return i;

      case s_end:
        multipart_log("s_end: %02X", (int) c);
        break;

      default:
        multipart_log("Multipart parser unrecoverable error");
        return 0;
    }
    ++ i;
  }

  return i;
}
