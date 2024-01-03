/* src/util.cxx --- commonly used functions

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

#include <string>
#include <libguile.h>


//////////////////////////////////////////////////////////////////////


SCM
raise_error (const char *key, const std::string &message)
{
  scm_throw (scm_from_utf8_symbol (key),
             scm_list_1 (scm_from_utf8_string (message.c_str ())));
  // Never reached; just a convenient return value for return
  // statements;
  return SCM_BOOL_F;
}


SCM
raise_error (const char *key, const std::exception &e)
{
  return raise_error (key, e.what ());
}


std::string
scm_to_cxx_string (SCM scm_string)
{
  char *buf = scm_to_utf8_string (scm_string);
  std::string str (buf);
  free (buf);
  return str;
}
