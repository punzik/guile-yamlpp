/* src/util.h --- declarations of utility functions

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

#ifndef SRC_UTIL_H
#define SRC_UTIL_H

#include <string>
#include <libguile.h>


//////////////////////////////////////////////////////////////////////


/**
 * Raise a Guile error.
 */
SCM
raise_error (const char *key, const std::string &message);


/**
 * Raise a Guile error from a C++ exception.
 */
SCM
raise_error (const char *key, const std::exception &e);


/**
 * Convert SCM string to C++ string, avoiding memory errors.
 */
std::string
scm_to_cxx_string (SCM string);


#endif
