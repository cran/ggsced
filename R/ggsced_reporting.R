##
## Copyright 2026 Shawn Gilroy, Louisiana State University
##
## This file is part of ggsced.
##
## ggsced is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## ggsced is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with ggsced  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.

#' ggsced_output_console
#'
#' Optional output function to support debugging (mainly for testing)
#'
#' @param msg Message to be output to console
#' @param output Boolean to discern whether or not to output message
#'
ggsced_output_console <- function(msg, output) {
  if (output == TRUE) message(msg)
}
