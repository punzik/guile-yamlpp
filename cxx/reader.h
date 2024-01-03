/* src/reader.h --- declarations of reader related functions

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

#ifndef SRC_READER_H
#define SRC_READER_H

#include <libguile.h>
#include "macros.h"


//////////////////////////////////////////////////////////////////////
// Names for the Guile procedures.

SCM_PROC_NAME (SCM_PROC_LOAD_NODE,            "yaml-load-node")
SCM_PROC_NAME (SCM_PROC_LOAD_NODES,           "yaml-load-nodes")
SCM_PROC_NAME (SCM_PROC_LOAD_NODE_FROM_FILE,  "yaml-load-node-from-file")
SCM_PROC_NAME (SCM_PROC_LOAD_NODES_FROM_FILE, "yaml-load-nodes-from-file")
SCM_PROC_NAME (SCM_PROC_NODE_TYPE,            "yaml-node-type")
SCM_PROC_NAME (SCM_PROC_SCALAR_VALUE,         "yaml-scalar-value")
SCM_PROC_NAME (SCM_PROC_NODE_TO_LIST,         "yaml-node->list")
SCM_PROC_NAME (SCM_PROC_NODE_TO_ALIST,        "yaml-node->alist")

//////////////////////////////////////////////////////////////////////


/**
 * Initialize the Guile YAML node type.
 */
void
init_node_type ();


/**
 * Return a symbol that denotes the type of the YAML node.
 */
SCM
yaml_node_type (SCM node);


/**
 * Return the value stored in a scalar YAML node.  Try to guess an
 * appropriate type if the node is not tagged.
 */
SCM
scalar_value (SCM node);


/**
 * Return the children of a sequence node as a Guile list.
 */
SCM
yaml_node_to_list (SCM node);


/**
 * Return an association list for the map node.
 */
SCM
yaml_node_to_alist (SCM node);


/**
 * Parse a Guile string to a YAML node.
 */
SCM
load_node (SCM string);


/**
 * Parse a Guile string to a list of nodes, one node for each YAML
 * document in the string.
 */
SCM
load_nodes (SCM string);


/**
 * Parse a file to a YAML node.
 */
SCM
load_node_from_file (SCM path);


/**
 * Parse a file to a list of YAML nodes.
 */
SCM
load_nodes_from_file (SCM path);


#endif
