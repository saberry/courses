library(httr)

username = "sberry5@nd.edu#nd"

surveyID = "SV_9Fjwrg4TKCSg125"

apiToken = "DSfaY34hc6jFdTOHiEns1sFf4IOtfuJbB15O0Xz2"

fileFormat = "csv"

dataCenter = 'ca1'

# Setting static parameters

baseUrl = paste("https://", 
                dataCenter, 
                ".qualtrics.com/API/v3/surveys/",
                surveyID,
                "/export-responses", 
                sep = "")

getSurveyData = POST(url = baseUrl, 
                     body = list("format" = "csv", 
                                 "compress" = "false"),
                     add_headers(.headers = c("Content-Type" = "application/json", 
                                              "X-API-TOKEN" = apiToken)), 
                     encode = "json")

requestID = content(getSurveyData)$`result`$`progressId`

progressMaker = paste("https://", 
                     dataCenter, 
                     ".qualtrics.com/API/v3/surveys/",
                     surveyID,
                     "/export-responses/",
                     requestID,
                     sep = "")

progressComplete = GET(url = progressMaker,
     add_headers(.headers = c("Content-Type" = "application/json", 
                              "X-API-TOKEN" = apiToken)))


fileID = content(progressComplete)$`result`$`fileId`

dataLink = paste("https://", 
      dataCenter, 
      ".qualtrics.com/API/v3/surveys/",
      surveyID,
      "/export-responses/",
      fileID, "/file",
      sep = "")

downloadData = GET(url = dataLink,
                       add_headers(.headers = c("Content-Type" = "application/json", 
                                                "X-API-TOKEN" = apiToken)))
