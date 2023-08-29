/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2017, VU University Amsterdam
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
#include <errno.h>
#include <string.h>
#include <assert.h>

		 /*******************************
		 *	       STREAM		*
		 *******************************/

static atom_t	ATOM_mode;			/* mode */
static atom_t	ATOM_null;			/* null */
static atom_t	ATOM_status;			/* status */
static atom_t	ATOM_subprotocol;		/* subprotocol */
static atom_t	ATOM_server;			/* server */
static atom_t	ATOM_client;			/* client */
static atom_t	ATOM_close_parent;
static atom_t	ATOM_end_of_file;
static atom_t	ATOM_buffer_size;

typedef enum ws_mode
{ WS_CLIENT,
  WS_SERVER
} ws_mode;

typedef enum ws_state
{ WS_IDLE = 0,					/* initial state */
  WS_MSG_STARTED,				/* after ws_start_message/3 */
  WS_MSG_END,					/* At end of message */
  WS_CLOSED					/* close sent/received */
} ws_state;

static atom_t ws_state_names[4];

typedef enum ws_opcode
{ WS_OP_CONTINUE = 0,
  WS_OP_TEXT     = 1,
  WS_OP_BINARY   = 2,
  WS_OP_CLOSE    = 8,
  WS_OP_PING     = 9,
  WS_OP_PONG     = 10
} ws_opcode;


#define WS_MAGIC		0x2da2f562
#define WS_MAX_HEADER_LEN	14

typedef struct ws_context
{ IOSTREAM	*stream;			/* original stream */
  IOSTREAM	*ws_stream;			/* Stream I'm handle of */
  IOENC		 parent_encoding;		/* Saved encoding of parent */
						/* state */
  ws_mode	 mode;				/* WS_CLIENT or WS_SERVER */
  ws_state	 state;				/* current state */
  atom_t	 subprotocol;			/* subprotocol we use */
  unsigned	 close_parent : 1;		/* close parent stream on close */
  unsigned	 fragmented : 1;		/* use fragmented output */
  unsigned	 fin : 1;			/* fin seen */
  unsigned	 masked : 1;			/* data is masked */
  int		 opcode;			/* current opcode */
  int		 rsv;				/* current rsv */
  int		 mask;				/* data mask */
  int64_t	 payload_written;		/* amount of payload written */
  int64_t	 payload_read;			/* amount of payload read */
  int64_t	 payload_len;			/* payload length (reading) */
						/* data buffering */
  char		*data;				/* Buffered data */
  size_t	 datasize;			/* #bytes buffered */
  size_t	 dataallocated;			/* #bytes allocated */

  int		 magic;				/* WS_MAGIC */
} ws_context;


static int ws_header(char *hdr, ws_context *ctx,
		     int fin, int mask, size_t payload_len);


		 /*******************************
		 *	     ALLOC/FREE		*
		 *******************************/

static ws_context*
alloc_ws_context(IOSTREAM *s)
{ ws_context *ctx = PL_malloc(sizeof(*ctx));

  if ( ctx )
  { memset(ctx, 0, sizeof(*ctx));
    ctx->magic  = WS_MAGIC;
    ctx->stream = s;
  }

  return ctx;
}


static void
free_ws_context(ws_context *ctx)
{ if ( ctx->stream->upstream )
    Sset_filter(ctx->stream, NULL);

  if ( ctx->data )
    free(ctx->data);
  if ( ctx->subprotocol )
    PL_unregister_atom(ctx->subprotocol);

  ctx->magic = 0;
  PL_free(ctx);
}


static int
grow_data_buffer(ws_context *ctx, size_t size)
{ size_t newsize;

  if ( ctx->dataallocated == 0 )
    newsize = SIO_BUFSIZE;
  else
    newsize = ctx->dataallocated;

  while(newsize < size)
    newsize *= 2;
  if ( ctx->data )
  { void *p;

    if ( !(p=realloc(ctx->data, newsize)) )
      return -1;
    ctx->data = p;
    ctx->dataallocated = newsize;
  } else
  { if ( !(ctx->data = malloc(newsize)) )
      return -1;

    ctx->dataallocated = newsize;
  }

  return 0;
}


static void
discard_data_buffer(ws_context *ctx)
{ if ( ctx->data )
  { free(ctx->data);
    ctx->data = NULL;
    ctx->datasize = 0;
    ctx->dataallocated = 0;
  }
}


		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

static IOFUNCTIONS ws_functions;

static int
get_ws_stream(term_t t, IOSTREAM **sp, ws_context **ctx, int flags)
{ IOSTREAM *s;

  if ( !PL_get_stream(t, &s, flags) )
    return FALSE;
  if ( s->functions != &ws_functions )
  { PL_release_stream(s);
    return PL_type_error("ws_stream", t),FALSE;
  }

  *sp = s;
  *ctx = s->handle;

  return TRUE;
}


static foreign_t
is_ws_stream(term_t ws)
{ IOSTREAM *s;
  int rc;

  if ( !PL_get_stream(ws, &s, 0) )
    return FALSE;
  rc = (s->functions == &ws_functions);
  PL_release_stream(s);

  return rc;
}


static void
init_state_names(void)
{ ws_state_names[WS_IDLE]        = PL_new_atom("idle");
  ws_state_names[WS_MSG_STARTED] = PL_new_atom("start");
  ws_state_names[WS_MSG_END]     = PL_new_atom("end");
  ws_state_names[WS_CLOSED]      = PL_new_atom("closed");
}

static foreign_t
ws_property(term_t WsStream, term_t property, term_t value)
{ ws_context *ctx;
  IOSTREAM *ws;
  atom_t prop;
  int rc;

  if ( !PL_get_atom_ex(property, &prop) ||
       !get_ws_stream(WsStream, &ws, &ctx, 0) )
    return FALSE;

  if ( prop == ATOM_status )
  { init_state_names();

    rc = PL_unify_atom(value, ws_state_names[ctx->state]);
  } else if ( prop == ATOM_subprotocol )
  { rc = PL_unify_atom(value, ctx->subprotocol);
  } else
    rc = PL_domain_error("websocket_property", property);

  PL_release_stream_noerror(ws);
  return rc;
}


static foreign_t
ws_set(term_t WsStream, term_t property, term_t value)
{ ws_context *ctx;
  IOSTREAM *ws;
  atom_t prop;
  int rc;

  if ( !PL_get_atom_ex(property, &prop) ||
       !get_ws_stream(WsStream, &ws, &ctx, 0) )
    return FALSE;

  if ( prop == ATOM_subprotocol )
  { atom_t a;

    if ( (rc=PL_get_atom_ex(value, &a)) )
    { PL_register_atom(a);
      PL_unregister_atom(ctx->subprotocol);
      ctx->subprotocol = a;
    }
  } else
    rc = PL_domain_error("websocket_property", property);

  PL_release_stream_noerror(ws);
  return rc;
}

		 /*******************************
		 *	      MASK		*
		 *******************************/

static void
apply_mask(char *data, size_t len, size_t offset, int mask)
{ char *s;
  size_t i;
  char msk[4];

  msk[0] = (mask>>24) & 0xff;
  msk[1] = (mask>>16) & 0xff;
  msk[2] = (mask>> 8) & 0xff;
  msk[3] = (mask>> 0) & 0xff;

  for(s=data, i=0; i<len; s++,i++)
  { *s ^= msk[(i+offset)%4];
  }
}



		 /*******************************
		 *	   IO FUNCTIONS		*
		 *******************************/

static int
ws_random(void)
{ static predicate_t pred = 0;
  fid_t fid = PL_open_foreign_frame();
  term_t t0 = PL_new_term_ref();
  int64_t i64;
  int i;

  if ( !pred )
    pred = PL_predicate("ws_mask", 1, "websocket");

  if ( PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, t0) &&
       PL_get_int64(t0, &i64) )
    i = (int)i64;
  else
    i = rand()^(rand()<<15);

  PL_discard_foreign_frame(fid);

  return i;
}


static ssize_t
ws_send_partial(ws_context *ctx, char *buf, size_t size)
{ char hdr[WS_MAX_HEADER_LEN];
  int fin = (ctx->state == WS_MSG_END);
  int hdr_len;
  int mask;
  ssize_t rc = size;

  if ( ctx->mode == WS_CLIENT )
    mask = ws_random();
  else
    mask = 0;

  hdr_len = ws_header(hdr, ctx, fin, mask, size);
  if ( mask != 0 )
    apply_mask(buf, size, 0, mask);

  if ( Sfwrite(hdr, 1, hdr_len, ctx->stream) != hdr_len ||
       Sfwrite(buf, 1, size,    ctx->stream) != size ||
       Sflush(ctx->stream) < 0 )
    rc = -1;				/* get error on our stream? */

  ctx->payload_written += size;

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ws_flush() handler must deal with the   fact  that Sflush() will not
flush the output buffer  if  it  is   empty.  It  does  call the control
function with SIO_FLUSHOUTPUT, so we can emit the message from here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
ws_flush(ws_context *ctx)
{ if ( ctx->fragmented &&
       ctx->payload_written == 0 &&
       ctx->state == WS_MSG_END )
    return (int)ws_send_partial(ctx, NULL, 0);

  return 0;
}


static ssize_t
ws_write(void *handle, char *buf, size_t size)
{ ws_context *ctx = handle;

  if ( ctx->fragmented )
  { return ws_send_partial(ctx, buf, size);
  } else
  { size_t osize = ctx->datasize;

    if ( osize+size > ctx->dataallocated )
    { if ( grow_data_buffer(ctx, osize+size) < 0 )
	return -1;			/* no memory */
    }
    memcpy(&ctx->data[osize], buf, size);
    ctx->datasize = osize+size;

    return size;
  }
}


static int64_t
read_int(IOSTREAM *in, int bytes)
{ int64_t rc = 0;

  while(bytes-- > 0)
  { int c;

    if ( (c = Sgetc(in)) == EOF )
      return -1;
    rc <<= 8;
    rc |= (int64_t)c&0xff;
  }

  return rc;
}


static int
ws_next_header(ws_context *ctx, int c)
{ int fin, rsv, opcode, mask, masked;
  int64_t payload_len;

  fin    = (c & (1<<7)) != 0;
  rsv    = (c >> 4) & 0x7;
  opcode = (c & 0xf);

  if ( (c = Sgetc(ctx->stream)) == EOF )	/* second byte */
    return FALSE;
  masked = (c & (1<<7)) != 0;
  payload_len = (c & 0x7f);

  if ( payload_len == 126 )
  { if ( (payload_len = read_int(ctx->stream, 2)) < 0 )
      return FALSE;
  } else if ( payload_len == 127 )
  { if ( (payload_len = read_int(ctx->stream, 8)) < 0 )
      return FALSE;
  }

  if ( masked )
    mask = (int)read_int(ctx->stream, 4);
  else
    mask = 0;

  ctx->state        = WS_MSG_STARTED;
  ctx->fin          = fin;
  ctx->opcode	    = opcode;
  ctx->rsv          = rsv;
  ctx->mask         = mask;
  ctx->masked       = masked;
  ctx->payload_len  = payload_len;
  ctx->payload_read = 0;

  return TRUE;
}


static int
skip_payload(ws_context *ctx)
{ size_t payload_len = ctx->payload_len;

  while( payload_len-- > 0 )
  { if ( Sgetc(ctx->stream) == EOF )
      return -1;
  }

  return 0;
}


static int
ws_continuation_header(ws_context *ctx)
{ for(;;)
  { int c;

    if ( (c=Sgetc(ctx->stream)) == EOF )
    { Sseterr(ctx->ws_stream, SIO_WARN, "Unterminated websocket message");
      return FALSE;
    }

    if ( !ws_next_header(ctx, c) )
      return FALSE;

    switch( ctx->opcode )
    { case WS_OP_CONTINUE:
	return TRUE;
      case WS_OP_PONG:
	if ( skip_payload(ctx) < 0 )
	  return FALSE;
	continue;
      case WS_OP_PING:
					/* TBD: Reply */
	continue;
      default:
	Sseterr(ctx->ws_stream, SIO_FERR, "WebSocket: unexpected package");
        return FALSE;
    }
  }
}


static ssize_t
ws_read(void *handle, char *buf, size_t bsize)
{ ws_context *ctx = handle;
  int64_t left = ctx->payload_len - ctx->payload_read;
  size_t size;
  ssize_t n;

  if ( ctx->state != WS_MSG_STARTED )
  { Sseterr(ctx->ws_stream, SIO_FERR, "please call ws_read_header/3 first");
    return -1;
  }

continuation:
  left = ctx->payload_len - ctx->payload_read;
  size = (left < (int64_t)bsize ? left : bsize);

  if ( size == 0 )
  { if ( ctx->fin )
    { ctx->state = (ctx->opcode == WS_OP_CLOSE ? WS_CLOSED : WS_MSG_END);
      return 0;					/* signal EOF */
    } else
    { if ( ws_continuation_header(ctx) )
	goto continuation;

      return -1;
    }
  }

  n = Sread_pending(ctx->stream, buf, size, SIO_RP_BLOCK);
  if ( n >= 0 )
  { if ( ctx->masked )
      apply_mask(buf, n, ctx->payload_read, ctx->mask);
    ctx->payload_read += n;
  }

  return n;
}


static int
ws_control(void *handle, int op, void *data)
{ ws_context *ctx = handle;

  if ( ctx->magic != WS_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  switch(op)
  { case SIO_FLUSHOUTPUT:
      return ws_flush(ctx);
    case SIO_GETPENDING:
    { size_t *sp = data;
      IOSTREAM *s = ctx->stream;

      if ( s->bufp < s->limitp )
	*sp = s->limitp - s->bufp;
      else
	*sp = 0;

      return 0;
    }
    default:
      if ( ctx->stream->functions->control )
	return (*ctx->stream->functions->control)(ctx->stream->handle, op, data);
      return -1;
  }
}


static int
ws_close(void *handle)
{ ws_context *ctx = handle;
  int rc = 0;
  IOSTREAM *parent = (ctx->close_parent ? ctx->stream : NULL);

						/* TBD: check sane state? */
  ctx->stream->encoding = ctx->parent_encoding;

  free_ws_context(ctx);
  if ( parent )
    rc = Sclose(parent);

  return rc;
}


static IOFUNCTIONS ws_functions =
{ ws_read,
  ws_write,
  NULL,					/* seek */
  ws_close,
  ws_control,				/* control */
  NULL					/* seek64 */
};



		 /*******************************
		 *	      OPEN		*
		 *******************************/

#define WS_COPY_FLAGS (SIO_INPUT| \
		       SIO_OUTPUT| \
		       SIO_TEXT| \
		       SIO_REPXML|SIO_REPPL|\
		       SIO_RECORDPOS)

static foreign_t
ws_open(term_t org, term_t new, term_t options)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  ws_context *ctx;
  IOSTREAM *s = NULL, *s2 = NULL;
  ws_mode mode = WS_CLIENT;
  int close_parent = TRUE;
  int bufsize = 0;
  atom_t subprotocol = ATOM_null;

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg = PL_new_term_ref();

    if ( !PL_get_name_arity(head, &name, &arity) || arity != 1 )
      return PL_type_error("option", head);
    _PL_get_arg(1, head, arg);

    if ( name == ATOM_mode )
    { atom_t a;

      if ( !PL_get_atom_ex(arg, &a) )
	return FALSE;
      if ( a == ATOM_server )
	mode = WS_SERVER;
      else if ( a == ATOM_client )
	mode = WS_CLIENT;
      else
	return PL_domain_error("mode", arg);
    } else if ( name == ATOM_subprotocol )
    { if ( !PL_get_atom_ex(arg, &subprotocol) )
	return FALSE;
    } else if ( name == ATOM_close_parent )
    { if ( !PL_get_bool_ex(arg, &close_parent) )
	return FALSE;
    } else if ( name == ATOM_buffer_size )
    { if ( !PL_get_integer_ex(arg, &bufsize) )
	return FALSE;
      if ( bufsize < 0 )
	return PL_domain_error("buffer_size", arg);
    }
  }
  if ( !PL_get_nil(tail) )
    return PL_type_error("list", tail);

  if ( !PL_is_variable(new) )
    return PL_uninstantiation_error(new);
  if ( !PL_get_stream_handle(org, &s) )
    return FALSE;			/* Error */

  if ( !(ctx = alloc_ws_context(s)) )
    goto error;

  PL_register_atom(subprotocol);

  ctx->mode = mode;
  ctx->close_parent = close_parent;
  ctx->subprotocol = subprotocol;
  Ssetenc(s, ENC_OCTET, &ctx->parent_encoding);
  if ( !(s2 = Snew(ctx,
		   (s->flags&WS_COPY_FLAGS)|SIO_FBUF,
		   &ws_functions)) )
    goto error;

  ctx->ws_stream = s2;
  if ( bufsize > 0 )
  { Ssetbuffer(s2, NULL, bufsize);
    ctx->fragmented = TRUE;
  }

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
    free_ws_context(ctx);

  return FALSE;
}


static foreign_t
ws_start_message(term_t WsStream, term_t OpCode, term_t RSV)
{ ws_context *ctx;
  IOSTREAM *ws;
  int opcode, rsv;
  int rc = TRUE;

  if ( !PL_get_integer_ex(OpCode, &opcode) ||
       !PL_get_integer_ex(RSV, &rsv) )
    return FALSE;
  if ( opcode < 0 || opcode > 15 )
    return PL_domain_error("opcode", OpCode);
  if ( rsv < 0 || rsv > 7 )
    PL_domain_error("rsv", RSV);

  if ( !get_ws_stream(WsStream, &ws, &ctx, SIO_OUTPUT) )
    return FALSE;

  if ( ctx->state != WS_IDLE )
    rc = PL_permission_error("start_message", "ws_stream", WsStream);

  if ( rc )
  { ctx->opcode          = opcode;
    ctx->rsv		 = rsv;
    ctx->state		 = WS_MSG_STARTED;
    ctx->payload_written = 0;
    switch(opcode)
    { case WS_OP_BINARY:
      case WS_OP_PING:
      case WS_OP_PONG:
      case WS_OP_CLOSE:			/* first two bytes for the code */
	Ssetenc(ctx->ws_stream, ENC_OCTET, NULL);
        break;
      default:
	Ssetenc(ctx->ws_stream, ENC_UTF8, NULL);
    }
  }

					/* stream not released; this is done */
					/* in ws_send/1 */
  return rc;
}


static int
ws_header(char *hdr, ws_context *ctx, int fin, int mask, size_t payload_len)
{ int hdr_len = 2;
  int masked = (mask != 0);
  int opcode = (ctx->payload_written ? WS_OP_CONTINUE : ctx->opcode);

  hdr[0] = ( (fin      << 7) |
	     (ctx->rsv << 4) |
	     (opcode   << 0) );

  if ( payload_len < 126 )
  { hdr[1] = ( (char)(masked<<7) | (char)payload_len );
  } else if ( payload_len < 65536 )
  { hdr[1] = ( (char)(masked<<7) | (char)126 );
    hdr[2] = (payload_len >> 8) & 0xff;
    hdr[3] = (payload_len & 0xff);
    hdr_len += 2;
  } else
  { int i;
    uint64_t plen64 = payload_len;

    hdr[1] = ( (masked<<7) | 127 );
    for(i=0; i<8; i++)
      hdr[2+i] = (plen64 >> ((7-i)*8)) & 0xff;
    hdr_len += 8;
  }
  if ( masked )
  { int i;

    for(i=0; i<4; i++)
      hdr[hdr_len++] = (mask >> ((3-i)*8)) & 0xff;
  }

  return hdr_len;
}


static foreign_t
ws_send(term_t WsStream)
{ ws_context *ctx;
  IOSTREAM *ws;
  int rc = TRUE;

  if ( !get_ws_stream(WsStream, &ws, &ctx, SIO_OUTPUT) )
    return FALSE;
  if ( !PL_release_stream(ws) )		/* release from ws_start_message/3 */
    return PL_release_stream(ws);

  if ( ctx->state != WS_MSG_STARTED )
  { rc = PL_permission_error("send", "ws_stream", WsStream);
    goto out;
  }

  ctx->state = WS_MSG_END;
  if ( Sflush(ctx->ws_stream) < 0 )
  { rc = FALSE;
    goto out;
  }

  if ( !ctx->fragmented )
  { int mask;
    int fin = 1;
    char hdr[WS_MAX_HEADER_LEN];
    int hdr_len;

    if ( ctx->mode == WS_CLIENT )
    { mask = ws_random();
      if ( PL_exception(0) )
      { rc = FALSE;
	goto out;
      }
    } else
    { mask = 0;
    }

    hdr_len = ws_header(hdr, ctx, fin, mask, ctx->datasize);
    if ( mask != 0 )
      apply_mask(ctx->data, ctx->datasize, 0, mask);

    if ( Sfwrite(hdr,       1, hdr_len,       ctx->stream) != hdr_len ||
	 Sfwrite(ctx->data, 1, ctx->datasize, ctx->stream) != ctx->datasize ||
	 Sflush(ctx->stream) < 0 )
      rc = FALSE;

    discard_data_buffer(ctx);
  }

  ctx->state = (ctx->opcode == WS_OP_CONTINUE ? WS_CLOSED : WS_IDLE);

out:
  if ( rc )
    rc = PL_release_stream(ws);
  else
    PL_release_stream(ws);

  return rc;
}


static foreign_t
ws_read_header(term_t WsStream, term_t OpCode, term_t RSV)
{ ws_context *ctx;
  IOSTREAM *ws;
  int rc = TRUE;
  int c;

  if ( !PL_is_variable(OpCode) ) return PL_uninstantiation_error(OpCode);
  if ( !PL_is_variable(RSV)    ) return PL_uninstantiation_error(RSV);

  if ( !get_ws_stream(WsStream, &ws, &ctx, SIO_INPUT) )
    return FALSE;

  if ( ctx->state == WS_MSG_STARTED )
  { while( Sgetc(ctx->ws_stream) != EOF )
      ;					/* read remainder of data */
    if ( ctx->state != WS_MSG_END )
    { rc = FALSE;			/* I/O error */
      goto out;
    }
  }

  if ( ctx->state == WS_MSG_END )	/* reset EOF from previous message */
  { ctx->state = WS_IDLE;
    ctx->ws_stream->flags &= ~(SIO_FEOF|SIO_FEOF2);
  } else if ( ctx->state != WS_IDLE )
  { rc = PL_permission_error("read_header", "ws_stream", WsStream);
    goto out;
  }

  if ( (c = Sgetc(ctx->stream)) == EOF )	/* first byte */
  { return ( PL_unify_atom(OpCode, ATOM_end_of_file) &&
	     PL_release_stream(ws)
	   );
  }

  if ( !ws_next_header(ctx, c) )
    goto out;

  switch(ctx->opcode)
  { case WS_OP_BINARY:
    case WS_OP_PING:
    case WS_OP_PONG:
    case WS_OP_CLOSE:
      Ssetenc(ctx->ws_stream, ENC_OCTET, NULL);
      break;
    default:
      Ssetenc(ctx->ws_stream, ENC_UTF8, NULL);
  }

out:
  if ( rc )
    rc = PL_release_stream(ws);
  else
    PL_release_stream(ws);

  rc = ( rc &&
	 PL_unify_integer(OpCode, ctx->opcode) &&
	 PL_unify_integer(RSV, ctx->rsv)
       );

  return rc;
}


		 /*******************************
		 *	      INSTALL		*
		 *******************************/

install_t
install_websocket(void)
{ ATOM_mode         = PL_new_atom("mode");
  ATOM_null	    = PL_new_atom("null");
  ATOM_status       = PL_new_atom("status");
  ATOM_subprotocol  = PL_new_atom("subprotocol");
  ATOM_server       = PL_new_atom("server");
  ATOM_client       = PL_new_atom("client");
  ATOM_close_parent = PL_new_atom("close_parent");
  ATOM_end_of_file  = PL_new_atom("end_of_file");
  ATOM_buffer_size  = PL_new_atom("buffer_size");

  PL_register_foreign("is_ws_stream",	  1, is_ws_stream,     0);
  PL_register_foreign("ws_property",	  3, ws_property,      0);
  PL_register_foreign("ws_set",		  3, ws_set,           0);
  PL_register_foreign("ws_open",	  3, ws_open,	       0);
  PL_register_foreign("ws_start_message", 3, ws_start_message, 0);
  PL_register_foreign("ws_send",	  1, ws_send,	       0);
  PL_register_foreign("ws_read_header",	  3, ws_read_header,   0);
}
