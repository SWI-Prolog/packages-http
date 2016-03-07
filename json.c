/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2016, University of Amsterdam
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
  { const pl_wchar_t *wp;
    size_t todo;
    int pc = 0;

    TRYPUTC('"', out);
    for(todo=len, wp=w; todo-- > 0; wp++)
    { int c = *wp;

      if ( json_put_code(out, pc, c) < 0 )
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
{ PL_register_foreign("json_write_string", 2, json_write_string, 0);
  PL_register_foreign("json_write_indent", 3, json_write_indent, 0);
}
