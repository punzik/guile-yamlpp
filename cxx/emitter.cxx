/* src/emitter.cxx --- allow guile-yamlpp to create YAML documents

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

#include <iostream>
#include <yaml-cpp/yaml.h>

#include "emitter.h"
#include "util.h"


//////////////////////////////////////////////////////////////////////


/**
 * Guile object that represents a YAML emitter.
 */
static SCM emitter_type;


/**
 * Assert that the Guile object represets a YAML emitter.
 */
static void
assert_emitter (SCM obj)
{
  scm_assert_foreign_object_type (emitter_type, obj);
}


/**
 * Retrieve a pointer to the actual C++ YAML emitter.
 */
static YAML::Emitter *
c_emitter (SCM scm_emitter)
{
  assert_emitter (scm_emitter);
  void *p = scm_foreign_object_ref (scm_emitter, 0);
  return reinterpret_cast<YAML::Emitter *> (p);
}


/**
 * Called when the Guile YAML emitter object is destroyed.
 */
static void
finalize_scm_emitter (SCM scm_emitter)
{
  YAML::Emitter *p = c_emitter (scm_emitter);
  delete p;
}


void
init_emitter_type ()
{
  SCM name = scm_from_utf8_symbol ("yaml-emitter");
  SCM slot = scm_from_utf8_symbol ("pointer-to-c-yaml-emitter");
  SCM slots = scm_list_1 (slot);
  auto finalizer = finalize_scm_emitter;
  emitter_type = scm_make_foreign_object_type (name, slots, finalizer);
}


/**
 * Create a Guile YAML emitter that contains a freshly created C YAML
 * emitter.
 */
static SCM
make_scm_emitter ()
{
  YAML::Emitter *p_emitter = new YAML::Emitter ();
  return scm_make_foreign_object_1 (emitter_type, p_emitter);
}


//////////////////////////////////////////////////////////////////////


/**
 * Return the appropriate YAML manipulator for the Scheme symbol.
 *
 * It returns the Auto manipulator if the symbol is unknown.
 */
static typeof (YAML::Auto)
manipulator (SCM scm_symbol)
{
  // General.
  static SCM sym_auto  = scm_from_utf8_symbol ("auto");
  // Strings.
  static SCM sym_squote  = scm_from_utf8_symbol ("single-quoted");
  static SCM sym_dquote  = scm_from_utf8_symbol ("double-quoted");
  static SCM sym_literal = scm_from_utf8_symbol ("literal");
  // Booleans.
  static SCM sym_yes   = scm_from_utf8_symbol ("yes-no-bool");
  static SCM sym_true  = scm_from_utf8_symbol ("true-false-bool");
  static SCM sym_on    = scm_from_utf8_symbol ("on-off-bool");
  static SCM sym_upper = scm_from_utf8_symbol ("upper-case");
  static SCM sym_lower = scm_from_utf8_symbol ("lower-case");
  static SCM sym_camel = scm_from_utf8_symbol ("camel-case");
  static SCM sym_long  = scm_from_utf8_symbol ("long-bool");
  static SCM sym_short = scm_from_utf8_symbol ("short-bool");
  // Integers.
  static SCM sym_dec  = scm_from_utf8_symbol ("dec");
  static SCM sym_hex  = scm_from_utf8_symbol ("hex");
  static SCM sym_oct  = scm_from_utf8_symbol ("oct");
  // Sequences & mappings.
  static SCM sym_block = scm_from_utf8_symbol ("block");
  static SCM sym_flow  = scm_from_utf8_symbol ("flow");
  //
  if (scm_is_eq (scm_symbol, sym_auto))
    return YAML::Auto;
  //
  else if (scm_is_eq (scm_symbol, sym_squote))
    return YAML::SingleQuoted;
  else if (scm_is_eq (scm_symbol, sym_dquote))
    return YAML::DoubleQuoted;
  else if (scm_is_eq (scm_symbol, sym_literal))
    return YAML::Literal;
  //
  else if (scm_is_eq (scm_symbol, sym_yes))
    return YAML::YesNoBool;
  else if (scm_is_eq (scm_symbol, sym_true))
    return YAML::TrueFalseBool;
  else if (scm_is_eq (scm_symbol, sym_on))
    return YAML::OnOffBool;
  else if (scm_is_eq (scm_symbol, sym_upper))
    return YAML::UpperCase;
  else if (scm_is_eq (scm_symbol, sym_lower))
    return YAML::LowerCase;
  else if (scm_is_eq (scm_symbol, sym_camel))
    return YAML::CamelCase;
  else if (scm_is_eq (scm_symbol, sym_long))
    return YAML::LongBool;
  else if (scm_is_eq (scm_symbol, sym_short))
    return YAML::ShortBool;
  //
  else if (scm_is_eq (scm_symbol, sym_dec))
    return YAML::Dec;
  else if (scm_is_eq (scm_symbol, sym_hex))
    return YAML::Hex;
  else if (scm_is_eq (scm_symbol, sym_oct))
    return YAML::Oct;
  //
  else if (scm_is_eq (scm_symbol, sym_block))
    return YAML::Block;
  else if (scm_is_eq (scm_symbol, sym_flow))
    return YAML::Flow;
  //
  else
    {
      // TODO: Should we warn?
      return YAML::Auto;
    }
}

//////////////////////////////////////////////////////////////////////
// Implementation of the functions exposed to Guile.


SCM
make_emitter ()
{
  return make_scm_emitter ();
}


SCM
emitter_good (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  return (emitter-> good ())? SCM_BOOL_T : SCM_BOOL_F;
}


SCM
emitter_string (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  return scm_from_utf8_string (emitter->c_str ());
}


SCM
emit_null (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::Null;
  return SCM_UNDEFINED;
}


SCM
emit_string (SCM scm_emitter, SCM scm_string)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  std::string str = scm_to_cxx_string (scm_string);
  *emitter << str;
  return SCM_UNDEFINED;
}


SCM
emit_boolean (SCM scm_emitter, SCM scm_boolean)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  bool b = (scm_is_true (scm_boolean))? true : false;
  *emitter << b;
  return SCM_UNDEFINED;
}


SCM
emit_integer (SCM scm_emitter, SCM scm_integer)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << scm_to_long_long (scm_integer);
  return SCM_UNDEFINED;
}


SCM
emit_comment (SCM scm_emitter, SCM scm_string)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  std::string str = scm_to_cxx_string (scm_string);
  *emitter << YAML::Comment (str);
  return SCM_UNDEFINED;
}


SCM
emit_newline (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::Newline;
  return SCM_UNDEFINED;
}


SCM
begin_doc (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::BeginDoc;
  return SCM_UNDEFINED;
}


SCM
end_doc (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::EndDoc;
  return SCM_UNDEFINED;
}


SCM
begin_seq (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::BeginSeq;
  return SCM_UNDEFINED;
}


SCM
end_seq (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::EndSeq;
  return SCM_UNDEFINED;
}


SCM
begin_map (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::BeginMap;
  return SCM_UNDEFINED;
}


SCM
end_map (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::EndMap;
  return SCM_UNDEFINED;
}


SCM
emit_key (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::Key;
  return SCM_UNDEFINED;
}


SCM
emit_value (SCM scm_emitter)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << YAML::Value;
  return SCM_UNDEFINED;
}


SCM
emit_anchor (SCM scm_emitter, SCM scm_string)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  std::string str = scm_to_cxx_string (scm_string);
  *emitter << YAML::Anchor (str);
  return SCM_UNDEFINED;
}


SCM
emit_alias (SCM scm_emitter, SCM scm_string)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  std::string str = scm_to_cxx_string (scm_string);
  *emitter << YAML::Alias (str);
  return SCM_UNDEFINED;
}


SCM
set_style (SCM scm_emitter, SCM scm_symbol)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  *emitter << manipulator (scm_symbol);
  return SCM_UNDEFINED;
}


SCM
set_string_format (SCM scm_emitter, SCM scm_symbol)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  auto manip = manipulator (scm_symbol);
  emitter->SetStringFormat(manip);
  return SCM_UNDEFINED;
}


SCM
set_bool_format (SCM scm_emitter, SCM scm_symbol)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  auto manip = manipulator (scm_symbol);
  emitter->SetBoolFormat(manip);
  return SCM_UNDEFINED;
}


SCM
set_int_base (SCM scm_emitter, SCM scm_symbol)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  auto manip = manipulator (scm_symbol);
  emitter->SetIntBase(manip);
  return SCM_UNDEFINED;
}


SCM
set_seq_format (SCM scm_emitter, SCM scm_symbol)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  auto manip = manipulator (scm_symbol);
  emitter->SetSeqFormat(manip);
  return SCM_UNDEFINED;
}


SCM
set_map_format (SCM scm_emitter, SCM scm_symbol)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  auto manip = manipulator (scm_symbol);
  emitter->SetMapFormat(manip);
  return SCM_UNDEFINED;
}


SCM
set_indent (SCM scm_emitter, SCM scm_length)
{
  YAML::Emitter *emitter = c_emitter (scm_emitter);
  size_t length = scm_to_size_t (scm_length);
  *emitter << YAML::Indent (length);
  return SCM_UNDEFINED;
}
