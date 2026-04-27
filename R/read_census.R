#' Read in Census data
#'
#' @param filePaths character vector of file paths pointing to Census tsv files
#' @param years numeric vector with the same length as filePaths of years being
#' read into the census.data object and corresponding to each file path.
#' @param weights character vector with the same length as year corresponding
#' to the name of the weight variable used for each year.
#' @param metaData A census.meta object.  Only questions included in the
#' census.meta object will be included in the census.data object.
#'
#' @return A census.data object.
#'
#' @examples
#' # example code

read_census <- function(filePaths, year, weights, metaData){
  if(length(filePaths) != length(years)){
    stop("The number of files must be the same as the number of years")
  }
  if(length(years) != length(weights)){
    stop("The number of weight variables must be the same as the
         number of years")
  }
  #TODO this will be somewhat similar to the readcensus.R script
}

