#' Table Creation and Cross Tabulation for Weighted Census estimates
#'
#' @param census A brn object.
#' @param formula A one- or two-sided formula.  See details.
#' @param years A numeric vector of years to include in the table.
#' @param type One of "proportion" or "populationCounts".  See details.
#'
#' @details TODO
#'


brn_table <- function(brnObject, formula, years,
                      type = c("proportion", "populationCounts")){
  #TODO  This function will decide what to do based on the types of questions
  #      included in the formula and the type of estimate then call the
  #      appropriate utility function.
}
