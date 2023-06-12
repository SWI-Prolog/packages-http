/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2023, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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

#define O_DEBUG 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include <errno.h>

#define MAXHDR 1024			/* max size of chunk header line */

static atom_t ATOM_close_parent;	/* close_parent(Bool) */
static atom_t ATOM_max_chunk_size;	/* max_chunk_size(Int) */


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#define BUFSIZE SIO_BUFSIZE		/* raw I/O buffer */

static chunked_metadata*
alloc_chunked_metadata(void)
{ chunked_metadata *md = PL_malloc(sizeof(*md));

  if ( md )
    memset(md, 0, sizeof(*md));

  return md;
}

static void
free_chunked_metadata(chunked_metadata *md)
{ if ( md )
  { chunked_trailer *tr;

    if ( (tr=md->trailer) )
    { chunked_trailer *next;
      md->trailer = NULL;
      md->trailer_tail = NULL;

      for( ; tr; tr=next )
      { next = tr->next;
	PL_unregister_atom(tr->key);
	PL_unregister_atom(tr->value);
	PL_free(tr);
      }
    }

    PL_free(md);
  }
}


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
  free_chunked_metadata(ctx->metadata);

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
chunked_write_chunk(IOSTREAM *s, const chunked_metadata *md, char *buf, size_t size)
{ int rc = 0;

  if ( md && md->chunk_ext )
  { if ( Sfprintf(s, "%zx", size) < 0 )
      rc = -1;

    term_t tail = PL_copy_term_ref(md->chunk_ext);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();

    while(rc == 0 && PL_get_list_ex(tail, head, tail))
    { char *k, *v;

      if ( !PL_get_arg(1, head, arg) ||
	   !PL_get_chars(arg, &k, CVT_ATOMIC|CVT_EXCEPTION|REP_ISO_LATIN_1) ||
	   !PL_get_arg(2, head, arg) ||
	   !PL_get_chars(arg, &v, CVT_ATOMIC|CVT_EXCEPTION|REP_ISO_LATIN_1) ||
	   Sfprintf(s, ";%s=%s", k, v) < 0 )
      { term_t ex;

	if ( (ex=PL_exception(0)) )
	  Sset_exception(s, ex);
	rc = -1;
      }
    }
    if ( !PL_get_nil_ex(tail) )
    { Sset_exception(s, PL_exception(0));
      rc = -1;
    }

    if ( rc == 0 && Sfprintf(s, "\r\n") < 0 )
      rc = -1;
  } else
  { if ( Sfprintf(s, "%zx\r\n", size) < 0 )
      rc = -1;
  }

  if ( rc == 0 &&
       Sfwrite(buf, sizeof(char), size, s) == size &&
       Sfprintf(s, "\r\n") >= 0 &&
       Sflush(s) >= 0 )
    return size;

  return -1;
}


static ssize_t				/* encode */
chunked_write(void *handle, char *buf, size_t size)
{ chunked_context *ctx = handle;

  return chunked_write_chunk(ctx->stream, ctx->metadata, buf, size);
}


static int
chunked_control(void *handle, int op, void *data)
{ chunked_context *ctx = handle;

  switch(op)
  { case SIO_FLUSHOUTPUT:
    case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    default:
    { IOSTREAM *s;
      IOFUNCTIONS *sf;

      if ( (s=ctx->stream) &&
	   (sf=s->functions) &&
	   s->magic == SIO_MAGIC )
	return (*sf->control)(s->handle, op, data);
      return -1;
    }
  }
}


static int
chunked_write_trailer(IOSTREAM *s, const chunked_metadata *md)
{ int rc = 0;

  if ( !md || !md->trailer)
  { if ( Sfprintf(s, "0\r\n\r\n") < 0 )
      rc = -1;
  } else
  { if ( Sfprintf(s, "0\r\n") >= 0 )
    { chunked_trailer *tr;

      for(tr=md->trailer; tr && rc == 0; tr = tr->next)
      { char *k, *v;

	PL_STRINGS_MARK();
	if ( !PL_atom_mbchars(tr->key, NULL, &k, REP_UTF8) ||
	     !PL_atom_mbchars(tr->value, NULL, &v, REP_UTF8) ||
	     SfprintfX(s, "%Us: %Us\r\n", k, v) < 0 )
	  rc = -1;
	PL_STRINGS_RELEASE();
      }

      if ( Sfprintf(s, "\r\n") < 0 )
	rc = -1;
    } else
      rc = -1;
  }

  if ( rc == 0 )
    rc = Sflush(s);

  return rc;
}

static int
chunked_close(void *handle)
{ chunked_context *ctx = handle;
  int rc = 0;

  DEBUG(1, Sdprintf("chunked_close() ...\n"));

  if ( (ctx->chunked_stream->flags & SIO_OUTPUT) )
    rc = chunked_write_trailer(ctx->stream, ctx->metadata);

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


static int
get_chunked_metadata(term_t t, chunked_metadata **ctx, int silent)
{ IOSTREAM *s;
  int rc = FALSE;

  if ( (rc=PL_get_stream(t, &s, 0)) )
  { chunked_metadata **mdp = NULL;

    if ( s->functions == &chunked_functions )
    { chunked_context *ctx = s->handle;
      mdp = &ctx->metadata;
    } else if ( s->functions == &cgi_functions )
    { cgi_context *ctx = s->handle;
      if ( ctx->transfer_encoding == ATOM_chunked )
	mdp = &ctx->metadata;
    } else
    { if ( !silent )
	PL_domain_error("http_chunked_stream", t);
    }

    if ( mdp )
    { if ( !*mdp )
	*mdp = alloc_chunked_metadata();
      if ( *mdp )
      { *ctx = *mdp;
	rc = TRUE;
      }
    }

    PL_release_stream(s);
  }

  return rc;
}

static foreign_t
http_chunked_flush(term_t Stream, term_t Ext)
{ chunked_metadata *md;
  int rc;

  if ( (rc=get_chunked_metadata(Stream, &md, FALSE)) )
  { IOSTREAM *s;

    md->chunk_ext = Ext;
    if ( PL_get_stream(Stream, &s, 0) )
    { Sflush(s);
      rc = PL_release_stream(s);
    } else
      rc = FALSE;
    md->chunk_ext = 0;
  }

  return rc;
}


static foreign_t
http_chunked_add_trailer(term_t Stream, term_t Key, term_t Value)
{ chunked_metadata *md;
  atom_t key, value;
  chunked_trailer *tr;

  if ( get_chunked_metadata(Stream, &md, FALSE) &&
       PL_get_atom(Key, &key) &&
       PL_get_atom(Value, &value) &&
       (tr=PL_malloc(sizeof(*tr))) )
  { tr->key = key;
    tr->value = value;
    tr->next = NULL;
    PL_register_atom(key);
    PL_register_atom(value);
    if ( md->trailer_tail )
    { md->trailer_tail->next = tr;
    } else
    { md->trailer = md->trailer_tail = tr;
    }

    return TRUE;
  }

  return FALSE;
}


static foreign_t
http_is_chunked(term_t Stream)
{ chunked_metadata *md;

  return get_chunked_metadata(Stream, &md, TRUE);
}


		 /*******************************
		 *	       INSTALL		*
		 *******************************/

static void
install_http_chunked()
{ ATOM_close_parent   = PL_new_atom("close_parent");
  ATOM_max_chunk_size = PL_new_atom("max_chunk_size");

  PL_register_foreign("http_chunked_open",        3, pl_http_chunked_open,     0);
  PL_register_foreign("http_is_chunked",          1, http_is_chunked,          0);
  PL_register_foreign("http_chunked_flush",       2, http_chunked_flush,       0);
  PL_register_foreign("http_chunked_add_trailer", 3, http_chunked_add_trailer, 0);
}
