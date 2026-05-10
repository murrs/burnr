#' Title
#'
#' @param filepath
#'
#' @returns
#' @export
#'
#' @examples
#'

read_man <- function(filepath){
  man.raw <- jsonlite::read_json(filepath)
  man.list <- lapply(man.raw, json_to_man)
  men <- do.call(c, man.list)
  return(men)
}
