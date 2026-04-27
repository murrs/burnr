#' Combine Years of a 'qname' Present in Two 'man' Objects
#'
#' @param qnm A qname value for a Census question. This qname should be present
#' in both mn1 and mn2.
#' @param mn1 A 'man' object to concatenate.
#' @param mn2 A 'man' object to concatenate.
#'
#' @returns A list that can be converted to a 'man' object.

cman.cyears <- function(qnm, mn1, mn2){
  years1 <- sapply(mn1[[qnm]]$years, getElement, "year")
  years2 <- sapply(mn2[[qnm]]$years, getElement, "year")

  out <- mn1[qnm]
  out[[qnm]]$years <- c(mn1[[qnm]]$years, mn2[[qnm]]$years)

  if(!identical(mn1[[qnm]]$notes, mn2[[qnm]]$notes)){
    out[[qnm]]$notes <- c(mn1[[qnm]]$notes, mn2[[qnm]]$notes)
  }
  return(out)
}
