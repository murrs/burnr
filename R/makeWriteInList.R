#' Make a list of write in responses for a Census question
#'
#' @param writeInResponses Wording for responses that have a write-in field
#' @param writeInTypes Type of write-in response, either "character" or "numeric"
#' @param wname wname for write-in responses.
#'
#' @returns list of write-in response metadata.

makeWriteInList <- function(writeInResponses,
                            writeInTypes,
                            wname){
  if(!is.null(writeInResponses)){
    writeInFlag <- TRUE
    writeInDF <- data.frame(writeInResponses = writeInResponses,
                            writeInTypes = writeInTypes,
                            wname = wname)
  }
  else{
    writeInFlag <- FALSE
    writeInDF <- NULL
  }

  out <- list(writeInFlag = writeInFlag,
              writeInResponses = writeInDF)
  return(out)
}
