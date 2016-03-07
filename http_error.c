/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2011, University of Amsterdam
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

#define O_DEBUG 1

static functor_t FUNCTOR_error2;	/* error(Formal, Context) */
static functor_t FUNCTOR_type_error2;	/* type_error(Term, Expected) */
static functor_t FUNCTOR_domain_error2;	/* domain_error(Term, Expected) */
static functor_t FUNCTOR_permission_error3; /* permission_error(Op, Type, Term) */
static functor_t FUNCTOR_existence_error2; /* existence_error(Type, Term) */
static int debuglevel = 0;

#define MKFUNCTOR(name, arity) PL_new_functor(PL_new_atom(name), arity)

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


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static int
type_error(term_t actual, const char *expected)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_type_error2,
		         PL_CHARS, expected,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
domain_error(term_t actual, const char *domain)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_domain_error2,
		         PL_CHARS, domain,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
existence_error(term_t actual, const char *type)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_existence_error2,
		         PL_CHARS, type,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
permission_error(const char *op, const char *objtype, term_t obj)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_permission_error3,
		         PL_CHARS, op,
		         PL_CHARS, objtype,
		         PL_TERM, obj,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
instantiation_error()
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_CHARS, "instantiation_error",
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
get_int_ex(term_t t, int *i)
{ if ( PL_get_integer(t, i) )
    return TRUE;

  return type_error(t, "integer");
}


static int
get_bool_ex(term_t t, int *i)
{ if ( PL_get_bool(t, i) )
    return TRUE;

  return type_error(t, "boolean");
}


static void
init_errors()
{ FUNCTOR_error2            = MKFUNCTOR("error", 2);
  FUNCTOR_type_error2	    = MKFUNCTOR("type_error", 2);
  FUNCTOR_domain_error2     = MKFUNCTOR("domain_error", 2);
  FUNCTOR_existence_error2  = MKFUNCTOR("existence_error", 2);
  FUNCTOR_permission_error3 = MKFUNCTOR("permission_error", 3);

#ifdef O_DEBUG
  PL_register_foreign("http_stream_debug", 1, http_stream_debug, 0);
#endif
}
