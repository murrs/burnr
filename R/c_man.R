#' Concatenate man objects
#'
#' @param ... Two or more man objects
#'
#' @returns 'man' object
#'
#' @export

c.man <- function(...){
  men <- list(...)
  if(length(men) == 1){
    return(men[[1]])
  }
  cmen <- men[[1]]
  for(i in 2:length(men)){
    cmen <- cman.2men(cmen, men[[i]])
  }
  return(cmen)
}

