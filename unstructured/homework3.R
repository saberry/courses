####################
### Assignment 3 ###
####################

## Package Load ##

library(dplyr)

library(R.utils)

library(tidytext)

library(tidyr)

library(tm)

## Decompression and Conversion##

# The R.utils package has a lot of handy functions, but the 
# gunzip function will allow us to decompress those .gz files
# that we found on Sakai. 

gunzip("C:/Users/sethb/Downloads/reviews_Musical_Instruments_5.json.gz")

gunzip("C:/Users/sethb/Downloads/reviews_Patio_Lawn_and_Garden_5.json.gz")

# Those will decompress into whatever directory they were in.

# Any person would logically assume that those file would be json
# files (the json should be a giveaway). Naturally, though, 
# things need to be difficult. R can only read pure json and if
# you look at those files, they are clearly not pure json. 
# We need to use the following function to read the individual
# lines, add a comman at the end of a line, unlist, and then 
# collapse everything down. After that, we wrap every line with
# brackets, remove the comma between the curly brace and bracket, 
# and finally read as a json.

jsonMaker = function(x) {
  
  initialRead = readLines(x)
  
  unlistedData = paste(unlist(lapply(initialRead, function(x) {
    paste(x, ",", sep = "")
  })), collapse = "")
  
  addingBrackets = paste("[", unlistedData, "]", sep = "")
  
  endLines = gsub("},]", "}]", addingBrackets)
  
  result = jsonlite::fromJSON(endLines)
}

musicalInstruments = jsonMaker("C:/Users/sethb/Downloads/reviews_Musical_Instruments_5.json")

lawnGarden = jsonMaker("C:/Users/sethb/Downloads/reviews_Patio_Lawn_and_Garden_5.json")

## Data Manipulation ##

# Since we will want to perform some classification on these 
# reviews, we will want to add a variable to each data frame 
# that will clearly note what data it is. Since we are going 
# to pass this data onto DataframeSource later, we need to
# adhere to the necessary names. Purely to keep things light,
# I am only going to keep the doc_id and text.

musicalInstruments = musicalInstruments %>% 
  mutate(doc_id = paste(1:nrow(.), "instruments", sep = "_"), 
         text = reviewText) %>% 
  select(doc_id, text)

lawnGarden = lawnGarden %>% 
  mutate(doc_id = paste(1:nrow(.), "lawn", sep = "_"), 
         text = reviewText) %>% 
  select(doc_id, text)

# Now we can put them together:

completeData = rbind(musicalInstruments, lawnGarden)

# At this point, you should do any cleaning that you 
# deem necessary. While we do some below, we have 
# have probably learned that an automatic pass might
# not always yield the best results.

# After binding, we should have 23533 rows and 3 columns.

# Now, we should convert those reviews into a document term matrix.

reviewDFS = DataframeSource(completeData)

reviewDTM = DocumentTermMatrix(Corpus(reviewDFS), 
                               control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                 tolower = TRUE, stopwords = stopwords("SMART"), removePunctuation = TRUE, 
                                 removeNumbers = TRUE, stemming = function(x) textstem::lemmatize_strings(x), 
                                 bounds = list(local = c(10, Inf))))

# While that DTM object would work for many pure text 
# operations, we want it to be able to work everywhere.
# To that end, we can convert it back to a data frame
# with the tidy function from tidytext.

reviewDF = tidytext::tidy(reviewDTM)

# Now, we can spread that data out and drop the row values!

reviewDF = spread(reviewDF, key = term, 
                  value = count) %>% 
  separate(., col = document, into = c("rowID", "reviewType")) %>% 
  dplyr::select(-rowID)

# Let's tidy up those NA values:

reviewDF = mutate_all(reviewDF, funs(ifelse(is.na(.), 0, .)))

# At this point, you are ready to engage in 
# your own classification. Start with the Naive
# Bayes classifier and try at least 1 more.
# For you Naive Bayes classifier, you will
# definitely need to use the kernel smooth --
# the model will fail without it!

# Discuss any additional cleaning that you did
# and how your models did in terms of classifying
# the texts. Do you find any limitations or 
# places for further exploration?


