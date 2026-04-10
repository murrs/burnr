
brn_metadata <- function(qname, year, dname, questionText, questionType,
                         responses, rname, reportingValues = NULL,
                         scaleQuestions = NULL, scaleOptions = NULL,
                         writeInResponses = NULL, wname = NULL,
                         notes.qname = NA, notes.year = NA){

  #TODO error checking for inputs by question type




  yearList <- list(year = year,
                   dname = dname,
                   questionText = questionText,
                   questionType = questionType,
                   responses = makeResponsesList(responses,
                                                 reportingValues,
                                                 rname,
                                                 questionType),
                   writeIn = makeWriteInList(writeInResponses,
                                             wname,
                                             questionType),
                   notes = notes.year)


  out <- list(qname = qname,
              years = list(yearList),
              notes = notes.qname)

}
