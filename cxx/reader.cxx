/* src/reader.cxx --- allow guile-yamlpp to read YAML documents

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

#include <cstring>
#include <filesystem>
#include <fstream>
#include <vector>

#include <yaml-cpp/yaml.h>
#include <libguile.h>

#include "reader.h"
#include "util.h"


//////////////////////////////////////////////////////////////////////


/**
 * Guile object that represents a node in the YAML doc.
 */
static SCM node_type;


/**
 * Retrieve a pointer to the actual C++ YAML node.
 */
static YAML::Node *
c_node (SCM scm_node)
{
  void *p = scm_foreign_object_ref (scm_node, 0);
  return reinterpret_cast<YAML::Node *> (p);
}


/**
 * Called when the Guile YAML node object is destroyed.
 */
static void
finalize_scm_node (SCM scm_node)
{
  YAML::Node *p = c_node (scm_node);
  delete p;
}


/**
 * Initialize the Guile YAML node type.
 */
static void
init_node_type ()
{
  SCM name = scm_from_utf8_symbol ("yaml-node");
  SCM slot = scm_from_utf8_symbol ("pointer-to-c-yaml-node");
  SCM slots = scm_list_1 (slot);
  auto finalizer = finalize_scm_node;
  node_type = scm_make_foreign_object_type (name, slots, finalizer);
}


/**
 * Create a Guile node that corresponds to a C YAML node.
 */
static SCM
make_scm_node (const YAML::Node &node)
{
  // Copy the node to the heap so that the copy survives after the
  // original node is gone.
  YAML::Node *p_node = new YAML::Node (node);
  return scm_make_foreign_object_1 (node_type, p_node);
}


/**
 * Assert that the Guile object represets a YAML node.
 */
static void
assert_node (SCM obj)
{
  scm_assert_foreign_object_type (node_type, obj);
}


//////////////////////////////////////////////////////////////////////


/**
 * Convert a vector of YAML nodes to a list of Guile nodes.
 */
static SCM
make_scm_nodes (const std::vector<YAML::Node> &nodes)
{
  SCM lst = scm_list_n (SCM_UNDEFINED);
  for (const YAML::Node &node : nodes)
    {
      SCM scm_node = make_scm_node (node);
      lst = scm_cons (scm_node, lst);
    }
  return scm_reverse (lst);
}


//////////////////////////////////////////////////////////////////////


/**
 * Raise a parser error.
 */
static SCM
raise_parser_error (const std::exception &e)
{
  return raise_error ("yaml-parser-error", e);
}


/**
 * Raise a file error.
 */
static SCM
raise_file_error (const std::string &path)
{
  std::string msg = std::string (strerror (errno)) + ": " + path;
  return raise_error ("yaml-file-error", msg);
}


/**
 * Open a YAML file.
 */
static std::ifstream
file_stream (SCM scm_path)
{
  // TODO: File name encoding?
  std::string path = scm_to_cxx_string (scm_path);
  if (!std::filesystem::exists (path))
    raise_file_error (path);
  std::ifstream s (path);
  return s;
}


//////////////////////////////////////////////////////////////////////


/**
 * Convert scalar to boolean.
 */
static SCM
scalar_to_bool (const YAML::Node &node)
{
  bool value = node.as<bool> ();
  return scm_from_bool (value);
}


/**
 * Convert scalar to integer.
 */
static SCM
scalar_to_int (const YAML::Node &node)
{
  int value = node.as<int> ();
  return scm_from_int (value);
}


/**
 * Convert scalar to double.
 */
static SCM
scalar_to_double (const YAML::Node &node)
{
  double value = node.as<double> ();
  return scm_from_double (value);
}


/**
 * Convert scalar to string.
 */
static SCM
scalar_to_string (const YAML::Node &node)
{
  std::string value = node.as<std::string> ();
  // TODO: Must the encoding always be UTF-8?
  return scm_from_utf8_string (value.c_str ());
}


//////////////////////////////////////////////////////////////////////


// Core YAML tags for scalars.

static const char *YAML_TAG_BOOL  = "tag:yaml.org,2002:bool";
static const char *YAML_TAG_FLOAT = "tag:yaml.org,2002:float";
static const char *YAML_TAG_INT   = "tag:yaml.org,2002:int";
static const char *YAML_TAG_STR   = "tag:yaml.org,2002:str";


/**
 * Try to guess the appropriate value of an untagged scalar.
 */
static SCM
guess_scalar_value (const YAML::Node &node)
{
  try
    {
      return scalar_to_int (node);
    }
  catch (YAML::BadConversion &) {}
  try
    {
      return scalar_to_double (node);
    }
  catch (YAML::BadConversion &) {}
  try
    {
      return scalar_to_bool (node);
    }
  catch (YAML::BadConversion &) {}
  // Return a string if all else fails.
  return scalar_to_string (node);
}


//////////////////////////////////////////////////////////////////////
// Implementation of functions exposed to Guile.


/**
 * Return a symbol that denotes the type of the YAML node.
 */
static SCM
yaml_node_type (SCM scm_node)
{
  assert_node (scm_node);
  YAML::Node *node = c_node (scm_node);
  const char *name;
  switch (node->Type ())
    {
    case YAML::NodeType::Null:
      name = "null";
      break;
    case YAML::NodeType::Scalar:
      name = "scalar";
      break;
    case YAML::NodeType::Sequence:
      name = "sequence";
      break;
    case YAML::NodeType::Map:
      name = "map";
      break;
    default:
      name = "undefined";
      break;
    }
  return scm_from_utf8_symbol (name);
}


/**
 * Return the value stored in a scalar YAML node.  Try to guess an
 * appropriate type if the node is not tagged.
 */
static SCM
scalar_value (SCM scm_node)
{
  assert_node (scm_node);
  // TODO: Check if actually a scalar YAML node?
  YAML::Node *node = c_node (scm_node);
  std::string tag = node->Tag ();
  if (tag == YAML_TAG_BOOL)
    return scalar_to_bool (*node);
  if (tag == YAML_TAG_INT)
    return scalar_to_int (*node);
  if (tag == YAML_TAG_FLOAT)
    return scalar_to_double (*node);
  if (tag == YAML_TAG_STR)
    return scalar_to_string (*node);
  if (tag == "?")
    return guess_scalar_value (*node);
  // Last resort.
  return scalar_to_string (*node);
}


/**
 * Return the children of a sequence node as a Guile list.
 */
static SCM
yaml_node_to_list (SCM scm_node)
{
  assert_node (scm_node);
  SCM lst = scm_list_n (SCM_UNDEFINED);
  const YAML::Node *node = c_node (scm_node);
  for (const YAML::Node &child : *node)
    {
      SCM scm_node = make_scm_node (child);
      lst = scm_cons (scm_node, lst);
    }
  return scm_reverse (lst);
}


/**
 * Return an association list for the map node.
 */
static SCM
yaml_node_to_alist (SCM scm_node)
{
  assert_node (scm_node);
  // TODO: Ensure that it's a map?
  SCM lst = scm_list_n (SCM_UNDEFINED);
  const YAML::Node *node = c_node (scm_node);
  for (auto it = node->begin (); it != node->end (); ++it)
    {
      SCM scm_key = make_scm_node (it->first);
      SCM scm_val = make_scm_node (it->second);
      SCM pair = scm_cons (scm_key, scm_val);
      lst = scm_cons (pair, lst);
    }
  return scm_reverse (lst);
}


/**
 * Parse a Guile string to a YAML node.
 */
static SCM
load_node (SCM scm_string)
{
  std::string str = scm_to_cxx_string (scm_string);
  try
    {
      YAML::Node node = YAML::Load (str);
      return make_scm_node (node);
    }
  catch (const YAML::ParserException &e)
    {
      return raise_parser_error (e);
    }
}


/**
 * Parse a Guile string to a list of nodes, one node for each YAML
 * document in the string.
 */
static SCM
load_nodes (SCM scm_string)
{
  std::string str = scm_to_cxx_string (scm_string);
  try
    {
      std::vector<YAML::Node> nodes = YAML::LoadAll (str);
      return make_scm_nodes (nodes);
    }
  catch (const YAML::ParserException &e)
    {
      return raise_parser_error (e);
    }
}


/**
 * Parse a file to a YAML node.
 */
static SCM
load_node_from_file (SCM scm_path)
{
  std::ifstream s = file_stream (scm_path);
  try
    {
      YAML::Node node = YAML::Load (s);
      return make_scm_node (node);
    }
  catch (const std::exception &e)
    {
      return raise_parser_error (e);
    }
}


/**
 * Parse a file to a list of YAML nodes.
 */
static SCM
load_nodes_from_file (SCM scm_path)
{
  std::ifstream s = file_stream (scm_path);
  try
    {
      std::vector<YAML::Node> nodes = YAML::LoadAll (s);
      return make_scm_nodes (nodes);
    }
  catch (const std::exception &e)
    {
      return raise_parser_error (e);
    }
}


//////////////////////////////////////////////////////////////////////


void
init_reader ()
{
  init_node_type ();
  {
    void *subr = reinterpret_cast<void*> (load_node);
    scm_c_define_gsubr ("prim:yaml-load-node", 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (load_nodes);
    scm_c_define_gsubr ("prim:yaml-load-nodes", 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (load_node_from_file);
    scm_c_define_gsubr ("prim:yaml-load-node-from-file", 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (load_nodes_from_file);
    scm_c_define_gsubr ("prim:yaml-load-nodes-from-file", 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (yaml_node_type);
    scm_c_define_gsubr ("prim:yaml-node-type", 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (scalar_value);
    scm_c_define_gsubr ("prim:yaml-scalar-value", 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (yaml_node_to_list);
    scm_c_define_gsubr ("prim:yaml-node->list", 1, 0, 0, subr);
  }
  {
    void *subr = reinterpret_cast<void*> (yaml_node_to_alist);
    scm_c_define_gsubr ("prim:yaml-node->alist", 1, 0, 0, subr);
  }
}
