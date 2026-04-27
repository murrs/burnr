#' Concatenate two man objects
#'
#' @param man1 A 'man' object to concatenate.
#' @param man2 A 'man' object to concatenate.
#'
#' @returns A 'man' object.

cman.2men <- function(man1, man2){
  qname1 <- do.call(c, lapply(man1, getElement, "qname"))
  qname2 <- do.call(c, lapply(man2, getElement, "qname"))
  if(!any(qname1 %in% qname2)){
    #Change classes to just list (remove man class) so c with concatenate as
    # lists and not use c.man()
    class(man1) <- "list"
    class(man2) <- "list"
    out <- c(man1, man2)
    class(out) <- c("man", "list")
    return(c(man1, man2))
  }
  else{
    qnameMatch <- intersect(qname1, qname2)
    qnameNoMatch1 <- qname1[!(qname1 %in% qnameMatch)]
    qnameNoMatch2 <- qname2[!(qname2 %in% qnameMatch)]

    out <- lapply(qnameMatch, cman.cyears, man1, man2)
    out <- do.call(c, out)
    # names(out) <- qnameMatch

    if(length(qnameNoMatch1) > 0){
      out <- c(out, man1[qnameNoMatch1])
    }
    if(length(qnameNoMatch2) > 0){
      out <- c(out, man2[qnameNoMatch2])
    }
    class(out) <- c("man", "list")
    return(out)
  }
}
