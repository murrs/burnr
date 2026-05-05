#' Metadata And Notes (MAN)
#'
#' @description The function man() creates a metadata and notes (man) object
#' which describes the format of a question from the Black Rock City Census.
#'
#' @param qname Question name. The name used to reference a question for
#'              analysis. This name should be the same across years to enable
#'              trend analysis.
#' @param year The year the question was asked. Integer value with the format
#'             19## or 20##.
#' @param dname Data file name. The name the question is stored under in the
#'              cleaned and weighted data file for Census responses for the
#'              given year. For questions of type 'selectMultiple' prior to
#'              2019, this value will be unique for each option. For years 2019
#'              and later, this will be the same as qname.
#' @param questionText The full text of the question as it was asked.
#' @param questionType Type of the question. One of "selectOne",
#'                     "selectMultiple", "selectScale", "textBoxNumeric",
#'                     textBoxCharacter".
#' @param responses Question responses. For "selectOne" and "selectMultiple"
#'                  questions, the text for possible response selections
#'                  as they were provided in the question for that year. For
#'                  "selectScale", the text for each sub-question. For
#'                  "textBox..." questions this should be NULL.
#' @param rname Response name. For "selectOne" and "selectMutliple" questions,
#'              the alias names to refer tp the response in the cleaned and
#'              weighted data file. For "selectScale" questions this should
#'              be the alias name for the sub questions or categories. For
#'              "textBox..." questions this should be NULL. See details for
#'              style suggestions when creating the rnames when they do not
#'              previously exist.
#' @param scaleOptions For "selectScale" questions, the text or numeric values
#'                     for scale options as they were shown on the survey.
#' @param reportingValues Values used to refer to or store the responses in the
#'                        raw data outputs from Alchemer or other survey tools
#'                        used in prior years. For "selectScale" questions this
#'                        is the values used to store the scale options. For
#'                        "textBox..." questions this should be NULL.
#' @param writeIn Named character vector. Names are the rname values
#'                         that of response options that have an additional
#'                         text box option. Values should either be 'character',
#'                         'numeric' or a regular expression. If a regular
#'                         expression is supplied the entered write-in response
#'                         will return NA if it is not matched by the given
#'                         regular expression.
#' @param notes.qname Notes for a question that are pertinent to all years. This
#'                    will be combined with the same field across all years
#'                    when man objects are concatenated.
#' @param notes.year Notes for a question that are pertinent to the given year.
#'
#' @returns A 'man' object.
#'
#' @details
#' TODO
#'
#'
#' @export
#'
#' @examples
#' # 'selectOne' type question no write-in
#' virgin25 <- man(qname = "virgin",
#'                 year = 2025,
#'                 dname = "virgin",
#'                 questionText = "Was this your first time visiting Black Rock City?",
#'                 questionType = "selectOne",
#'                 responses = c("Yes, it was my first time",
#'                               "No, I have been to Black Rock City before",
#'                               "I did not go to Black Rock City in 2025"),
#'                 rname = c("virgin",
#'                           "notVirgin",
#'                           "didNotGoIn2025"),
#'                 reportingValues = c(1, 2, 3))
#'
#' # 'selectMany' type question with write-in
#' raceEth25 <- man(qname = "raceEth",
#'                  year = 2025,
#'                  dname = "raceEth",
#'                  questionText = "Which category best describes your race or ethnicity?",
#'                  questionType = "selectMultiple",
#'                  responses = c("Native American or Alaska Native",
#'                                "Asian",
#'                                "Black or African American",
#'                                "Native Hawaiian or other Pacific Islander",
#'                                "White/Caucasian",
#'                                "Middle Eastern or North African",
#'                                "Hispanic/Latino (e.g., Mexican, Puerto Rican, Cuban, Colombian)",
#'                                "Other (please specify):"),
#'                  rname = c("nativeAmericanAlaskan",
#'                            "asian",
#'                            "black",
#'                            "nativeHawaiianPacificIslander",
#'                            "white",
#'                            "middleEasternNorthAfrican",
#'                            "hispanic",
#'                            "other"),
#'                 reportingValues = c(2, 1, 5, 8, 4, 7, 4, 6),
#'                 writeIn = c(other = "character"),
#'                 notes.qname = "The options for this question are based on
#'                                the recommendations of the US Census.  The
#'                                options were updated in 2022.")
#'
#' # 'selectScale' type question
#' mediaManagedByBMP25 <- man(qname = "mediaManagedByBMP",
#'                            year = 2025,
#'                            dname = "mediaManagedByBMP",
#'                            questionText = "Which of the following media managed by Burning Man Project do you use to receive news, information, and events related to Burning Man, and to engage in discussion on Burning Man topics?",
#'                            questionType = "selectScale",
#'                            responses = c("Jack Rabbit Speaks (JRS) e-newsletter",
#'                                          "Burning Man website",
#'                                          "Burning Man Journal",
#'                                          "Burning Man Hive",
#'                                          "Social media managed by Burning Man Project (Facebook, Instagram, Twitter/X, YouTube, etc.)",
#'                                          "Eplaya"),
#'                            scaleOptions = c("Never", "Rarely", "Often"),
#'                            reportingValues = c(1, 2, 3),
#'                            rname = c("jackrabbitSpeaks",
#'                                      "burningManWebsite",
#'                                      "burningManJournal",
#'                                      "burningManHive",
#'                                      "socialMedia",
#'                                      "ePlaya"))
#'
#' # Pre-alchemer (2022) example
#' virgin19 <- man(qname = "virgin",
#'                 year = 2019,
#'                 dname = "virgin",
#'                 questionText = "Was this your first time visiting Black Rock City?",
#'                 questionType = "selectOne",
#'                 responses = c("Yes, it was my first time",
#'                               "No, I had been to Black Rock City before",
#'                               "I did not go to Black Rock City in 2019"),
#'                 rname = c("virgin",
#'                           "notVirgin",
#'                           "didNotGoIn2019"),
#'                 reportingValues = c(1, 2, 3))
#'
#' # Concatenate man objects (are many mans a men?)
#' virginMan <- virgin25 + virgin19
#'
man <- function(qname, year, dname, questionText, questionType,
                         responses = NULL, rname = NULL, scaleOptions = NULL,
                         reportingValues = NULL, writeInResponses = NULL,
                         writeInTypes = NULL, wname = NULL, notes.qname = NA,
                         notes.year = NA){

  # Check for valid year input
  Sys.year <- as.numeric(format(Sys.Date(), "%Y"))
  lastBurnYear <- ifelse(Sys.Date() < as.Date(paste0(Sys.year, "-10-01")),
                         Sys.year - 1, Sys.year)
  if(!is.numeric(year) | year < 2013){
    stop(paste0("year should be an integer greater than or equal to 2013 and
                less than or equal to ",lastBurnYear, "."))
  }

  #Check for valid inputs by question type
  if(questionType %in% c("selectOne", "selectMultiple")){
    if(is.null(responses) | !is.character(responses)){
      stop(paste0("A character vector for responses must be provided for ",
                  questionType, " questions."))
    }
    if(is.null(rname) | !is.character(rname)){
      stop(paste0("A character vector for rname must be provided for ",
                  questionType, " questions."))
    }
    if(!is.null(scaleOptions)){
      stop(paste0("scaleOptions should be NULL for ", questionType,
                  " questions."))
    }
    #TODO add type checking conditional on year for reporting values.
    #TODO check reporting values and rnames the same length as responses
    if(!all(names(writeInResponses) %in% rname)){
      notPresent <- names(writeIn)[!(names(writeIn) %in% rname)]
      stop("Values for writeIn must match a value provided in
           rname. ", do.call(paste, c(as.list(notPresent), list(sep = ", "))),
           "are not present in given values for rname.")
    }
    #TODO check writeInResponses for valid inputs

  }
  #TODO check inputs for selectScale
  else if(questionType == "selectScale"){

  }
  #TODO check inputs for textBoxXXXX
  else if(questionType %in% c("textBoxNumeric", "textBoxCharacter")){

  }
  else{
    stop("Question type must be one of 'selectOne', 'selectMultiple', 'selectScale',
         'textBoxNumeric', 'textBoxCharacter'. See ?man for details.")
  }
  responseList <- makeResponseList(questionType = questionType,
                                   responses = responses,
                                   reportingValues = reportingValues,
                                   scaleOptions = scaleOptions,
                                   rname = rname)
  # writeInList <- makeWriteInList(writeInResponses = writeInResponses,
  #                                writeInTypes = writeInTypes,
  #                                wname)

  #Create single year entry for a question
  yearList <- list(list(year = year,
                    dname = dname,
                   questionText = questionText,
                   questionType = questionType,
                   responses = responseList,
                   writeIn = writeIn,
                   notes = notes.year))
  names(yearList) <- paste0("census", year)

  #Create output
  out <- list(list(qname = qname,
                   years = yearList,
                   notes = notes.qname))
  names(out) <- qname
  class(out) <- c("man", "list")
  return(out)
}
