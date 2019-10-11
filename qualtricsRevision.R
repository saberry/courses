library(httr)

# username = "sberry5@nd.edu#nd"

surveyID = "SV_9Fjwrg4TKCSg125"

surveyID = "SV_eJmYJFlpzjQ4bv7"

apiToken = "DSfaY34hc6jFdTOHiEns1sFf4IOtfuJbB15O0Xz2"

dataCenter = 'ca1'

getQualtricsData <- function(apiToken, dataCenter, surveyID) {
   
   library(httr)
   
   baseUrl <- paste("https://", 
                    dataCenter, 
                    ".qualtrics.com/API/v3/surveys/",
                    surveyID,
                    "/export-responses", 
                    sep = "")
   
   requestHeaders <- c("Content-Type" = "application/json", 
                       "X-API-TOKEN" = apiToken)
   
   startSurveyDownload = POST(url = baseUrl, 
                              body = list("format" = "csv", 
                                          "compress" = "false"),
                              add_headers(.headers = requestHeaders), 
                              encode = "json")
   
   requestID <- content(startSurveyDownload)$`result`$`progressId`
   
   progressLink <- paste(baseUrl, "/", requestID,
                         sep = "")
   
   # The following gets reran:
   
   fileIDFunction <- function() {
      progressComplete <- GET(url = progressLink,
                              add_headers(.headers = requestHeaders))
      
      
      fileID <- content(progressComplete)$`result`$`fileId`
      
      return(fileID)
   }
   
   fileID <- fileIDFunction()
   
   
   while(is.null(fileID)) {
      fileID <- fileIDFunction()
   }

   downloadLink <- paste(baseUrl, "/",
                         fileID, "/file",
                         sep = "")
   
   downloadData <- GET(url = downloadLink,
                       add_headers(.headers = requestHeaders), progress())
   
   out <- data.table::fread(content(downloadData, as = "text"))
   
   keepVariables <- grep("responseID|^values", names(out), value = TRUE)
   
   out <- out[, keepVariables]
   
   names(out) <- gsub("values\\.", "", names(out))
   
   return(out)
}

debugonce(getQualtricsData)

dataTest = getQualtricsData(apiToken, dataCenter, surveyID)
