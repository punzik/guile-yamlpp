/* src/emitter.h --- declarations of emitter related functions

  Copyright © 2023-2024 Georgios Athanasiou <yorgath@gmail.com>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see
  <https://www.gnu.org/licenses/>.
*/

#ifndef SRC_EMITTER_H
#define SRC_EMITTER_H

#include <libguile.h>
#include "macros.h"


//////////////////////////////////////////////////////////////////////
// Names for the Guile procedures.

SCM_PROC_NAME (SCM_PROC_MAKE_EMITTER,      "make-yaml-emitter")
SCM_PROC_NAME (SCM_PROC_EMITTER_GOOD,      "yaml-emitter-good?")
SCM_PROC_NAME (SCM_PROC_EMITTER_STRING,    "yaml-emitter-string")
SCM_PROC_NAME (SCM_PROC_EMIT_NULL,         "yaml-emit-null!")
SCM_PROC_NAME (SCM_PROC_EMIT_STRING,       "yaml-emit-string!")
SCM_PROC_NAME (SCM_PROC_EMIT_BOOLEAN,      "yaml-emit-boolean!")
SCM_PROC_NAME (SCM_PROC_EMIT_INTEGER,      "yaml-emit-integer!")
SCM_PROC_NAME (SCM_PROC_EMIT_COMMENT,      "yaml-emit-comment!")
SCM_PROC_NAME (SCM_PROC_EMIT_NEWLINE,      "yaml-emit-newline!")
SCM_PROC_NAME (SCM_PROC_BEGIN_DOC,         "yaml-begin-doc!")
SCM_PROC_NAME (SCM_PROC_END_DOC,           "yaml-end-doc!")
SCM_PROC_NAME (SCM_PROC_BEGIN_SEQ,         "yaml-begin-seq!")
SCM_PROC_NAME (SCM_PROC_END_SEQ,           "yaml-end-seq!")
SCM_PROC_NAME (SCM_PROC_BEGIN_MAP,         "yaml-begin-map!")
SCM_PROC_NAME (SCM_PROC_END_MAP,           "yaml-end-map!")
SCM_PROC_NAME (SCM_PROC_EMIT_KEY,          "yaml-emit-key!")
SCM_PROC_NAME (SCM_PROC_EMIT_VALUE,        "yaml-emit-value!")
SCM_PROC_NAME (SCM_PROC_EMIT_ANCHOR,       "yaml-emit-anchor!")
SCM_PROC_NAME (SCM_PROC_EMIT_ALIAS,        "yaml-emit-alias!")
SCM_PROC_NAME (SCM_PROC_SET_STYLE,         "yaml-set-style-1!")
SCM_PROC_NAME (SCM_PROC_SET_STRING_FORMAT, "yaml-set-string-format-1!")
SCM_PROC_NAME (SCM_PROC_SET_BOOL_FORMAT,   "yaml-set-bool-format-1!")
SCM_PROC_NAME (SCM_PROC_SET_INT_BASE,      "yaml-set-int-base!")
SCM_PROC_NAME (SCM_PROC_SET_SEQ_FORMAT,    "yaml-set-seq-format-1!")
SCM_PROC_NAME (SCM_PROC_SET_MAP_FORMAT,    "yaml-set-map-format-1!")
SCM_PROC_NAME (SCM_PROC_SET_INDENT,        "yaml-set-indent!")

//////////////////////////////////////////////////////////////////////


/**
 * Initialize the Guile YAML emitter type.
 */
void
init_emitter_type ();


/**
 * Create a new YAML emitter.
 */
SCM
make_emitter ();


/**
 * Return a Scheme boolean that reveals the state of the YAML emitter.
 */
SCM
emitter_good (SCM emitter);


/**
 * Return a Scheme string from the contents of the YAML emitter.
 */
SCM
emitter_string (SCM emitter);


/**
 * Write the null symbol to the emitter.
 */
SCM
emit_null (SCM emitter);


/**
 * Write a string to the YAML emitter.
 */
SCM
emit_string (SCM emitter, SCM string);


/**
 * Write a boolean value to the YAML emitter.
 */
SCM
emit_boolean (SCM emitter, SCM boolean);


/**
 * Write an exact integer to the YAML emitter.
 *
 * It raises an out-of-range error if the Guile integer is out of the
 * long long range of the platform.
 */
SCM
emit_integer (SCM emitter, SCM integer);


/**
 * Write a comment to the YAML emitter.
 */
SCM
emit_comment (SCM emitter, SCM string);


/**
 * Cause a line break.
 */
SCM
emit_newline (SCM emitter);


/**
 * Begin a YAML document.
 */
SCM
begin_doc (SCM emitter);


/**
 * End a YAML document.
 */
SCM
end_doc (SCM emitter);


/**
 * Begin a YAML sequence.
 */
SCM
begin_seq (SCM emitter);


/**
 * End a YAML sequence.
 */
SCM
end_seq (SCM emitter);


/**
 * Begin a YAML mapping.
 */
SCM
begin_map (SCM emitter);


/**
 * End a YAML mapping.
 */
SCM
end_map (SCM emitter);


/**
 * Direct the emitter to expect a key for a YAML mapping.
 */
SCM
emit_key (SCM emitter);


/**
 * Direct the emitter to expect a value for a YAML mapping.
 */
SCM
emit_value (SCM emitter);


/**
 * Write a YAML anchor.
 */
SCM
emit_anchor (SCM emitter, SCM string);


/**
 * Write a YAML alias.
 */
SCM
emit_alias (SCM emitter, SCM string);


/**
 * Set the style for the next element.
 */
SCM
set_style (SCM emitter, SCM symbol);


/**
 * Set the format to emit strings in.
 */
SCM
set_string_format (SCM emitter, SCM symbol);


/**
 * Set the format to emit booleans in.
 */
SCM
set_bool_format (SCM emitter, SCM symbol);


/**
 * Set the numeral system for emitting integers.
 */
SCM
set_int_base (SCM emitter, SCM symbol);


/**
 * Set the format to emit sequences in.
 */
SCM
set_seq_format (SCM emitter, SCM symbol);


/**
 * Set the format to emit mappings in.
 */
SCM
set_map_format (SCM emitter, SCM symbol);


/**
 * Set the number of indentation spaces.
 */
SCM
set_indent (SCM emitter, SCM length);


#endif
