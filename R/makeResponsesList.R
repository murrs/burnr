#' Make list of responses for a Census question
#'
#' @param questionType Type of the question. One of "selectOne",
#'                     "selectMultiple", "selectScale", "textBoxNumeric",
#'                     textBoxCharacter".
#' @param responses Question responses. For "selectOne" and "selectMultiple"
#'                  questions, the text for possible response selections
#'                  as they were provided in the question for that year. For
#'                  "selectScale", the text for each sub-question. For
#'                  "textBox..." questions this should be NULL.
#' @param reportingValues Values used to refer to or store the responses in the
#'                        raw data outputs from Alchemer or other survey tools
#'                        used in prior years. For "selectScale" questions this
#'                        is the values used to store the scale options. For
#'                        "textBox..." questions this should be NULL.
#' @param scaleOptions For "selectScale" questions, the text or numeric values
#'                     for scale options as they were shown on the survey.
#' @param rname Response name. For "selectOne" and "selectMutliple" questions,
#'              the text used to reference the response or sub-question in the
#'              cleaned and weighted data file. For "textBox..." question this
#'              should be NULL.
#'
#' @returns A data.frame or list of possible responses to a Census question

makeResponseList <- function(questionType = c("selectOne", "selectMultiple",
                                               "selectScale", "textBoxNumeric",
                                               "textBoxCharacter"),
                              responses = NULL,
                              reportingValues = NULL,
                              scaleOptions = NULL,
                              rname = NULL){
  if(questionType %in% c("selectOne", "selectMultiple")){
    out <- data.frame(response = responses,
                      reportingValues = reportingValues,
                      questionType = questionType,
                      rname = rname)
  }
  else if(questionType == "selectScale"){
    out <- list(responses = data.frame(response = responses,
                                       rname = rname),
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
