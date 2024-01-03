/* src/init.cxx --- make functions available to Guile

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

#include <libguile.h>

#include "reader.h"
#include "emitter.h"


//////////////////////////////////////////////////////////////////////


// Must be exported as a C function to interface with Guile.
extern "C" void init ();

/**
 * Initialize the Guile part of the library.
 */
void
init ()
{
  init_node_type ();
  {
    void *subr = reinterpret_cast<void*> (load_node);
    scm_c_define_gsubr (SCM_PROC_LOAD_NODE, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (load_nodes);
    scm_c_define_gsubr (SCM_PROC_LOAD_NODES, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (load_node_from_file);
    scm_c_define_gsubr (SCM_PROC_LOAD_NODE_FROM_FILE, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (load_nodes_from_file);
    scm_c_define_gsubr (SCM_PROC_LOAD_NODES_FROM_FILE, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (yaml_node_type);
    scm_c_define_gsubr (SCM_PROC_NODE_TYPE, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (scalar_value);
    scm_c_define_gsubr (SCM_PROC_SCALAR_VALUE, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (yaml_node_to_list);
    scm_c_define_gsubr (SCM_PROC_NODE_TO_LIST, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (yaml_node_to_alist);
    scm_c_define_gsubr ("yaml-node->alist", 1, 0, 0, subr);
  }

  init_emitter_type ();
  {
    void *subr = reinterpret_cast<void*> (make_emitter);
    scm_c_define_gsubr (SCM_PROC_MAKE_EMITTER, 0, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emitter_good);
    scm_c_define_gsubr (SCM_PROC_EMITTER_GOOD, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emitter_string);
    scm_c_define_gsubr (SCM_PROC_EMITTER_STRING, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_null);
    scm_c_define_gsubr (SCM_PROC_EMIT_NULL, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_string);
    scm_c_define_gsubr (SCM_PROC_EMIT_STRING, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_boolean);
    scm_c_define_gsubr (SCM_PROC_EMIT_BOOLEAN, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_integer);
    scm_c_define_gsubr (SCM_PROC_EMIT_INTEGER, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_comment);
    scm_c_define_gsubr (SCM_PROC_EMIT_COMMENT, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_newline);
    scm_c_define_gsubr (SCM_PROC_EMIT_NEWLINE, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (begin_doc);
    scm_c_define_gsubr (SCM_PROC_BEGIN_DOC, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (end_doc);
    scm_c_define_gsubr (SCM_PROC_END_DOC, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (begin_seq);
    scm_c_define_gsubr (SCM_PROC_BEGIN_SEQ, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (end_seq);
    scm_c_define_gsubr (SCM_PROC_END_SEQ, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (begin_map);
    scm_c_define_gsubr (SCM_PROC_BEGIN_MAP, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (end_map);
    scm_c_define_gsubr (SCM_PROC_END_MAP, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_key);
    scm_c_define_gsubr (SCM_PROC_EMIT_KEY, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_value);
    scm_c_define_gsubr (SCM_PROC_EMIT_VALUE, 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_anchor);
    scm_c_define_gsubr (SCM_PROC_EMIT_ANCHOR, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (emit_alias);
    scm_c_define_gsubr (SCM_PROC_EMIT_ALIAS, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (set_style);
    scm_c_define_gsubr (SCM_PROC_SET_STYLE, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (set_string_format);
    scm_c_define_gsubr (SCM_PROC_SET_STRING_FORMAT, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (set_bool_format);
    scm_c_define_gsubr (SCM_PROC_SET_BOOL_FORMAT, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (set_int_base);
    scm_c_define_gsubr (SCM_PROC_SET_INT_BASE, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (set_seq_format);
    scm_c_define_gsubr (SCM_PROC_SET_SEQ_FORMAT, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (set_map_format);
    scm_c_define_gsubr (SCM_PROC_SET_MAP_FORMAT, 2, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (set_indent);
    scm_c_define_gsubr (SCM_PROC_SET_INDENT, 2, 0, 0, subr);
  }

}
