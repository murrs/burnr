#' Concatenate man objects
#'
#' @param man1 An object of class 'man'.
#' @param man2 An object of class 'man'.
#'
#' @returns An object of class 'man'.
#' @export

`+.man` <- function(man1, man2){
  c(man1, man2)
}
