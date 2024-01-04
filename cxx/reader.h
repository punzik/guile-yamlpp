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


/**
 * Initialize the Guile types and procedures necessary for reading
 * YAML documents.
 */
void
init_reader ();


#endif
