/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012 VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

static int
get_payload(term_t Payload, int opcode, char **payload, size_t *payload_len)
{ switch(opcode)
  { case 1:					/* text frame */
      return PL_get_nchars(Payload, payload_len, payload,
			   CVT_ATOM|CVT_STRING|CVT_LIST|REP_UTF8|CVT_EXCEPTION);
    case 2:					/* binary */
      return PL_get_nchars(Payload, payload_len, payload,
			   CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION);
    case 9:					/* ping */
    case 10:					/* pong */
    default:
      return PL_warning("TBD: opcode %d", opcode);
  }
}


static foreign_t
ws_write(term_t out,				/* stream-handle */
	 term_t Fin,				/* boolean */
	 term_t Rsv,				/* extension code 0..7 */
	 term_t OpCode,				/* opcode 0..15 */
	 term_t Mask,				/* Mask (int or 0) */
	 term_t Payload)			/* payload data */
{ IOSTREAM *h;
  char hdr[14];					/* max header */
  int  hdr_len = 2;
  int fin, rsv, opcode, mask;
  int rc = TRUE;
  char *payload;
  size_t payload_len;

  if ( !PL_get_bool_ex(Fin, &fin) ||
       !PL_get_integer_ex(Rsv, &rsv) ||
       !PL_get_integer_ex(OpCode, &opcode) ||
       !PL_get_integer_ex(Mask, &mask) )
    return FALSE;
  if ( rsv < 0 || rsv > 7 )
    return PL_domain_error("rsv", Rsv);
  if ( opcode < 0 || opcode > 15 )
    return PL_domain_error("opcode", OpCode);
  if ( !get_payload(Payload, opcode, &payload, &payload_len) )
    return FALSE;

  if ( !PL_get_stream_handle(out, &h) )
    return FALSE;

  hdr[0] = ( (fin << 7) |
	     (rsv << 4) |
	     opcode );
  if ( payload_len < 126 )
  { hdr[1] = ( ((mask != 0)<<7) | payload_len );
  } else if ( payload_len < 65536 )
  { hdr[1] = ( ((mask != 0)<<7) | 126 );
    hdr[2] = (payload_len >> 8) & 0xff;
    hdr[3] = (payload_len & 0xff);
    hdr_len += 2;
  } else
  { int i;
    uint64_t paylen = payload_len;

    hdr[1] = ( ((mask != 0)<<7) | 127 );
    for(i=0; i<8; i++)
      hdr[1+i] = (paylen >> ((7-i)*8)) & 0xff;
    hdr_len += 8;
  }
  if ( mask )
  { int i;

    for(i=0; i<4; i++)
      hdr[hdr_len++] = (mask >> ((3-i)*8)) & 0xff;
  }
  if ( Sfwrite(hdr, 1, hdr_len, h) != hdr_len ||
       Sfwrite(payload, 1, payload_len, h) != payload_len )
    rc = FALSE;

  if ( rc )
    rc = PL_release_stream(h);
  else
    PL_release_stream(h);

  return rc;
}


install_t
install_websocket()
{ PL_register_foreign("ws_write", 6, ws_write, 0);
}
