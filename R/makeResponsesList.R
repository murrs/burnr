
#' Make list of responses for a Census question
#'
#' @param questionType
#' @param responses
#' @param reportingValues
#' @param catgories
#' @param scaleOptions
#' @param rname
#'
#' @returns
#' @export
#'
#' @examples
makeResponsesList <- function(questionType = c("selectOne", "selectMany",
                                               "selectScale", "textBoxNumeric",
                                               "textBoxCharacter"),
                              responses = NULL,
                              reportingValues = NULL,
                              catgories = NULL,
                              scaleOptions = NULL,
                              rname = NULL){
  if(questionType %in% c("selectOne", "selectMany")){
    out <- data.frame(response = responses,
                      reportingValues = reportingValues,
                      questionType = questionType,
                      rname = rname)
  }
  else if(questionType == "selectScale"){
    out <- list(categories = data.frame(category = categories,
                                       rnames = rnames),
                scale = data.frame(scaleOptions = scaleOptions,
                                   reportingValues = reportingValues))
  }
  else if(questionType == "textBoxNumeric"){
    out <- NULL
  }
  else if(questionType == "textBoxCharacter"){
    out <- NULL
  }
  return(out)
}
