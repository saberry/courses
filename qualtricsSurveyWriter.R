questions = data.frame(question = c("I enjoy coding.", "To what extent do you hate or love R?", "Done?"), 
                       responseOptions = c("No;Yes", "Strongly hate;Hate;Neither;Love;Strongly love", "No;Maybe;Yes"), 
                       questionID = c("enjoyCode", "hateLoveR", "done"), stringsAsFactors = FALSE)

surveyWriter <- function(completeSurveyDataFrame, question, responseOptions, questionID = NULL, 
                         roSeparator = ";", pageBreakEvery = 0, outputFileName = "surveyOut.txt") {
  
  # sink(outputFileName)
  
  if(pageBreakEvery != 0) {
    
    rowCount <- nrow(completeSurveyDataFrame)
    
    completeSurveyDataFrame$rowID <- 1:rowCount
    
    breakNumbers <- seq(from = pageBreakEvery, to = rowCount, 
                        by = pageBreakEvery) + .1
    
    completeSurveyDataFrame[(rowCount + 1):(rowCount + length(breakNumbers)), ] <- ""
    
    completeSurveyDataFrame$question[(rowCount + 1):(rowCount + length(breakNumbers))] <- "[[PageBreak]]"
    
    completeSurveyDataFrame$rowID[(rowCount + 1):(rowCount + length(breakNumbers))] <- breakNumbers
    
    completeSurveyDataFrame <- completeSurveyDataFrame[order(completeSurveyDataFrame$rowID), ]
  }
  
  numberQuestions = nrow(completeSurveyDataFrame)
  # FIX THE QUESTION DATA FRAME PROBLEMS!
  cat("[[AdvancedFormat]]")
  
  lapply(1:numberQuestions, function(x) {
    browser()
    if(question[x] == "[[PageBreak]]") {
      cat(question[x])
    } else {
      
      if(!(is.null(questionID[x]))) {
        idField <- paste("[[ID:", questionID[x], "]]", sep = "")
      } else {
        idField <- paste("[[ID:", gsub("\\s", "_", question[x]), "]]", sep = "")
      }
      
      responseOptionsFormatted <- paste(unlist(strsplit(responseOptions[x], roSeparator)), collapse = "\n")
      
      questionWrite <- paste("\n", "[[Question:MC]]", 
                             idField,  
                             question[x],  
                             "[[Choices]]",  
                             responseOptionsFormatted, sep = "\n")
      
      cat(questionWrite)
    }
    
  })
  
  sink()
}

debugonce(surveyWriter)

surveyWriter(completeSurveyDataFrame = questions, question = questions$question, 
             responseOptions = questions$responseOptions, questionID = questions$questionID, 
             roSeparator = ";", pageBreakEvery = 2)


seq(from = (pageBreakEvery + 1), to = 15, by = pageBreakEvery)
