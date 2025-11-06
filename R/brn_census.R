#' Create a Burner Response Nodule (BRN) object
#'
#' @param dataLocation file path of the folder containing the Census survey data
#' @param years numeric vector with the same length as filePaths of years being
#' read into the census.data object and corresponding to each file path.
#' @param qid character or numeric vector of either survey question id names or
#' numbers present in the metaData object
#' @param weights character vector with the same length as year corresponding
#' to the name of the weight variable used for each year.
#' @param metaData A census.meta object.  Only questions included in the
#' census.meta object will be included in the census.data object.
#'
#' @return A census.data object.
#'
#' @examples
#' # example code

brn_census <- function(dataLocation, years, qid, weights, metaData){
  if(length(years) != length(weights)){
    stop("The number of weight variables must be the same as the
         number of years")
  }
  if(any(years > 2025)){
    stop("Are you from the future?  We have no data on Burns that have not
         happened yet!")
  }
  if(any(years < 2013)){
    stop("We do not use Census data from before 2013.  That stuff is as
         unreliable as a golf cart from the Org.")
  }
  #TODO this function will read in Census data stored as parquet files
  #     (gotta go fast!). This will be an improvement over the mass warning
  #     messages we used to receive.  We'll share the data as parquet files
  #     unless someone really needs a .tsv file.
  #
  #     The metadata file will be a .json file that contains info on every
  #     question we have ever asked in Census.  This file will need to be
  #     maintained annually and will be included in the burnr package.
}

