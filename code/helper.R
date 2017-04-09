#######################################################################
# Code to check parameter/control objects
# Copyrigth (C) 2011 Michael Hahsler
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
    

## helper to parse parameter lists with defaults
.nodots <- function(...) {
  l <- list(...)
  if(length(l) > 0L) warning("Unknown arguments: ",
    paste(names(l), "=",l, collapse=", "))
}

.get_parameters <- function(defaults, parameter) {
  defaults <- as.list(defaults)
  parameter <- as.list(parameter)
  
  ## add verbose
  if(is.null(defaults$verbose)) defaults$verbose <- FALSE
  
  if(length(parameter) != 0) {
    o <- pmatch(names(parameter), names(defaults))
    
    ## unknown parameter
    if(any(is.na(o))){
      warning(sprintf(ngettext(length(is.na(o)),
        "Unknown parameter: %s",
        "Unknown parameters: %s"),
        paste(names(parameter)[is.na(o)],
          collapse = ", ")), call. = FALSE, immediate. = TRUE)
      
      cat("Available parameter (with default values):\n")
      #print(defaults)
      cat(rbind(names(defaults)," = ", gsub("\n"," ",as.character(defaults))), 
        sep=c("\t"," ","\n"))
    } 
    
    defaults[o[!is.na(o)]] <- parameter[!is.na(o)]
  }

  if(defaults$verbose) {
    cat("Used parameters:\n")
    #print(defaults)
    cat(rbind(names(defaults)," = ", gsub("\n"," ",as.character(defaults))), 
      sep=c("\t"," ","\n"))
  }
  
  defaults
}

.knn <- function(sim, k) apply(sim, MARGIN=1, FUN=function(x) head(
  order(x, decreasing=TRUE, na.last=TRUE), k))

.BIN_UBCF_param <- list( 
  method = "jaccard", 
  nn = 25, 
  weighted = TRUE,
  sample = FALSE
)

.REAL_UBCF_param <- list( 
  method = "cosine", 
  nn = 25, 
  sample = FALSE,
  ## FIXME: implement weighted = TRUE,
  normalize="center",
  minRating = NA
)

.BIN_IBCF_params <- list(
  k = 30,
  method="Jaccard",
  normalize_sim_matrix = FALSE,
  alpha = 0.5
)

.REAL_IBCF_params <- list(
  k = 30, 
  method="Cosine",
  normalize = "center", 
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  na_as_zero = FALSE,
  minRating = NA
)

