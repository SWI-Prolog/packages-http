/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2018, University of Amsterdam
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
#include <string.h>


		 /*******************************
		 *	       READ		*
		 *******************************/

#define FAST_BUFFER 256

typedef struct text
{ char *t;
  char *o;
  char *e;
  size_t allocated;
  char buf[FAST_BUFFER];
} text;


static void
init_text(text *t)
{ t->t = t->o = t->buf;
  t->e = t->o + FAST_BUFFER;
}

static void
free_text(text *t)
{ if ( t->t != t->buf )
    free(t->t);
}

static int
put_byte(text *t, int c)
{ if ( t->o < t->e )
  { *t->o++ = c;
  } else
  { size_t cnt = t->o - t->t;

    if ( t->t == t->buf )
    { t->allocated = FAST_BUFFER*2;
      if ( !(t->t = malloc(t->allocated)) )
	return -1;
    } else {
      char *n;
      t->allocated *= 2;
      if ( (n = realloc(t->t, t->allocated)) )
	t->t = n;
      else
	return -1;
    }
    t->o = t->t + cnt;
    t->e = t->t + t->allocated;
    *t->o++ = c;
  }

  return 0;
}



static foreign_t
json_read_number(term_t stream, term_t c0, term_t number)
{ IOSTREAM *in;
  text t;
  int rc = FALSE;
  int c;
  term_t tmp;

  if ( !PL_get_stream(stream, &in, SIO_INPUT) ||
       !PL_get_char_ex(c0, &c, FALSE) )
    return FALSE;

  init_text(&t);
  put_byte(&t, c);

  for(;;)
  { c = Speekcode(in);

    if ( (c >= '0' && c <= '9') ||
	 c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E' )
    { if ( put_byte(&t, c) != 0 )
      { rc = PL_resource_error("memory");
	break;
      }

      (void)Sgetcode(in);
      continue;
    }
    if ( put_byte(&t, 0) != 0 )
    { rc = PL_resource_error("memory");
      break;
    }

    rc = ( (tmp = PL_new_term_ref()) &&
	   PL_put_term_from_chars(tmp, REP_ISO_LATIN_1|CVT_EXCEPTION, t.o-t.t-1, t.t) &&
	   PL_is_number(tmp) &&
	   PL_unify(tmp, number) );

    break;
  }
  free_text(&t);

  if ( !rc && !PL_exception(0) )
    rc = PL_syntax_error("illegal_number", in);

  PL_release_stream(in);

  return rc;
}

static int
is_ws(int c)
{ return ( c == ' ' || c == '\t' || c == '\n' || c == '\r' );
}

static foreign_t
json_skip_ws(term_t stream, term_t c0, term_t next)
{ int c;

  if ( !PL_get_char_ex(c0, &c, TRUE) )
    return FALSE;

  if ( is_ws(c) )
  { IOSTREAM *in;

    if ( !PL_get_stream(stream, &in, SIO_INPUT) )
      return FALSE;

    do
    { c = Sgetcode(in);
    } while ( is_ws(c) );

    PL_release_stream(in);
  }

  return PL_unify_integer(next, c);
}


		 /*******************************
		 *	      WRITE		*
		 *******************************/

#define TRYPUTC(c, s) if ( Sputcode(c, s) < 0 ) { return -1; }

static int
json_put_code(IOSTREAM *out, int pc, int c)
{ static char escape[128];
  static int escape_initialized = FALSE;

  if ( !escape_initialized )
  { memset(escape, 0, sizeof(escape));

    escape['"']  = '"';
    escape['\\'] = '\\';
    escape['\b'] = 'b';
    escape['\f'] = 'f';
    escape['\n'] = 'n';
    escape['\r'] = 'r';
    escape['\t'] = 't';

    escape_initialized = TRUE;
  }

  if ( c < 128 )
  { if ( escape[c] )
    { TRYPUTC('\\', out);
      TRYPUTC(escape[c], out);
    } else if ( c < ' ' )	/* control characters *must* be escaped */
    { TRYPUTC('\\', out);
      if ( Sfprintf(out, "u%04x", c) < 0 )
	return -1;
    } else if ( pc == '<' && c == '/' )	/* Emit </ inside a string as <\/ to */
                                        /* allow safe embedding in html */
    { TRYPUTC('\\', out);
      TRYPUTC('/', out);
    } else
    { TRYPUTC(c, out);
    }
  } else
  { TRYPUTC(c, out);
  }

  return 0;
}

#undef TRYPUTC
#define TRYPUTC(c, s) if ( Sputcode(c, s) < 0 ) { rc = FALSE; goto out; }

#define IS_UTF16_LEAD(c)      ((c) >= 0xD800 && (c) <= 0xDBFF)
#define IS_UTF16_TRAIL(c)     ((c) >= 0xDC00 && (c) <= 0xDFFF)

static inline int
utf16_decode(int lead, int trail)
{ int l = (lead-0xD800) << 10;
  int t = (trail-0xDC00);

  return l+t+0x10000;
}

static inline const wchar_t*
get_wchar(const wchar_t *in, int *chr)
{
#if SIZEOF_WCHAR_T == 2
  int c = *in++;
  if ( IS_UTF16_LEAD(c) && IS_UTF16_TRAIL(in[0]) )
  { *chr = utf16_decode(c, in[0]);
    in++;
  } else
  { *chr = c;
  }
  return in;
#else
  *chr = *in++;
  return in;
#endif
}

static foreign_t
json_write_string(term_t stream, term_t text)
{ IOSTREAM *out;
  char *a;
  pl_wchar_t *w;
  size_t len;
  int rc = TRUE;

  if ( !PL_get_stream(stream, &out, SIO_OUTPUT) )
    return FALSE;

  if ( PL_get_nchars(text, &len, &a, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { const char *ap;
    size_t todo;
    int pc = 0;

    TRYPUTC('"', out);
    for(todo=len, ap=a; todo-- > 0; ap++)
    { int c = *ap&0xff;

      if ( json_put_code(out, pc, c) < 0 )
      { rc = FALSE; goto out;
      }
      pc = c;
    }
    TRYPUTC('"', out);
  } else if ( PL_get_wchars(text, &len, &w, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { const wchar_t *wp=w, *we=w+len;
    int pc=0, c;

    TRYPUTC('"', out);
    while((wp = get_wchar(wp, &c)) <= we)
    { if ( json_put_code(out, pc, c) < 0 )
      { rc = FALSE; goto out;
      }
      pc = c;
    }
    TRYPUTC('"', out);
  } else
  { rc = FALSE;
  }

out:
  PL_release_stream(out);

  return rc;
}


static foreign_t
json_write_indent(term_t stream, term_t indent, term_t tab)
{ int i, t, n;
  IOSTREAM *out;

  if ( !PL_get_integer(indent, &i) ||
       !PL_get_integer(tab, &t) )
    return FALSE;

  if ( PL_get_stream(stream, &out, SIO_OUTPUT) )
  { int rc = TRUE;

    if ( !out->position || out->position->linepos > 0 )
    { TRYPUTC('\n', out);
    }
    for(n=0; n<i/t; n++)
      TRYPUTC('\t', out);
    for(n=0; n<i%t; n++)
      TRYPUTC(' ', out);
out:
    PL_release_stream(out);
    return rc;
  }

  return FALSE;
}



install_t
install_json()
{ PL_register_foreign("json_read_number",  3, json_read_number,  0);
  PL_register_foreign("json_skip_ws",      3, json_skip_ws,      0);
  PL_register_foreign("json_write_string", 2, json_write_string, 0);
  PL_register_foreign("json_write_indent", 3, json_write_indent, 0);
}
