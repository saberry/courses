############################
### Import, Clean, Merge ###
############################

library(dplyr); library(tidyr)

clientRoster = fread("https://www3.nd.edu/~sberry5/data/fuzzyJoinClients.csv")

compustatSheet = fread("https://www3.nd.edu/~sberry5/data/fuzzyJoinCompustat.csv")


### Let's do some string tidying

# AT&T is a bit of an odd duck -- at one time it was AT&T Corp, but it is now
# AT&T Inc. Let's drop the AT&T Corp rows from the compustat data.

compustatSheet = compustatSheet[-(grep("AT&T CORP", compustatSheet$Company.Name)),]

# Let's convert everything to lower case

clientRoster$cleanName = tolower(clientRoster$TIENAME)

compustatSheet$Company.Name = tolower(compustatSheet$Company.Name)

# Now, let's get rid of some junk (e.g., co, corp, inc). These really do not
# help when it comes time to join these things.

clientRoster$cleanName = gsub(" inc$| corp$| co$| ltd$| -cl .$| cos$| companies$| company$| co inc$| plc$",
                              "", clientRoster$cleanName)

compustatSheet$Company.Name = gsub(" inc$| corp$| co$| ltd$| -cl .$| cos$| companies$| company$| co inc$| plc$",
                              " ", compustatSheet$Company.Name)

clientRoster$cleanName = stringr::str_trim(clientRoster$cleanName)

compustatSheet$Company.Name = stringr::str_trim(compustatSheet$Company.Name)

# We are going to run the same two lines again, just to get any stragglers:

clientRoster$cleanName = gsub(" inc$| corp$| co$| ltd$| -cl .$| cos$| companies$| company$| co inc$| plc$",
                              "", clientRoster$cleanName)

compustatSheet$Company.Name = gsub(" inc$| corp$| co$| ltd$| -cl .$| cos$| companies$| company$| co inc$| plc$",
                                   " ", compustatSheet$Company.Name)

# We also need to get rid of some puncutation; we could do it all in 1 swoop,
# but breaking it up is good for seeing what we did.

compustatSheet$Company.Name = gsub("-", " ", compustatSheet$Company.Name, perl = TRUE)

clientRoster$cleanName = gsub("-", " ", clientRoster$cleanName, perl = TRUE)

compustatSheet$Company.Name = gsub(",", "", compustatSheet$Company.Name, perl = TRUE)

clientRoster$cleanName = gsub(",", "", clientRoster$cleanName, perl = TRUE)

compustatSheet$Company.Name = gsub("&", "", compustatSheet$Company.Name, perl = TRUE)

clientRoster$cleanName = gsub("&", "", clientRoster$cleanName, perl = TRUE)

clientRoster$cleanName = gsub("^the ", "", clientRoster$cleanName, perl = TRUE)

# Recoding Time!

clientRoster$cleanName[grep("^3m .*", clientRoster$cleanName)] = "3m"

clientRoster$cleanName[grep("^20th century fox .*", clientRoster$cleanName)] = "20th century fox"

clientRoster$cleanName[grep("apple computer.", clientRoster$cleanName)] = "apple"

clientRoster$cleanName[grep("^att communications|^att international|^att technologies",
                            clientRoster$cleanName)] = "att"

clientRoster$cleanName[grep("^bristol m.*", clientRoster$cleanName)] = "bristol myers squibb"

clientRoster$cleanName[grep("^citib.*|citic.*", clientRoster$cleanName)] = "citigroup"

clientRoster$cleanName[grep("^dow chemical .*", clientRoster$cleanName)] = "dow chemical"

clientRoster$cleanName[grep("^dow jones", clientRoster$cleanName)] = "dow jones"

clientRoster$cleanName[grep("^du pont de nemours ei ", clientRoster$cleanName)] = "du pont de nemours"

compustatSheet$Company.Name[grep("^du pont .*", compustatSheet$Company.Name)] = "du pont de nemours"

clientRoster$cleanName[grep("^eli lilly", clientRoster$cleanName)] = "lilly (eli)"

clientRoster$cleanName[grep("^fisher price", clientRoster$cleanName)] = "fisher price"

clientRoster$cleanName[grep("^ford motor", clientRoster$cleanName)] = "ford motor"

clientRoster$cleanName[grep("^harley davidson", clientRoster$cleanName)] = "harley davidson"

clientRoster$cleanName[grep("^heinz", clientRoster$cleanName)] = "kraft heinz"

clientRoster$cleanName[grep("^hershey", clientRoster$cleanName)] = "hershey"

clientRoster$cleanName[grep("^hillshire farm$", clientRoster$cleanName)] = "hillshire brands"

clientRoster$cleanName[grep("^hj heinz", clientRoster$cleanName)] = "kraft heinz"

clientRoster$cleanName[grep("^honeywell", clientRoster$cleanName)] = "honeywell international"

clientRoster$cleanName[grep("^johnson johnson", clientRoster$cleanName)] = "johnson johnson"

clientRoster$cleanName[grep("^kraft$|^kraft food.*|^kraft general.*|^kraft refrig.*",
                            clientRoster$cleanName)] = "kraft heinz"

clientRoster$cleanName[grep("^mcdonald_s.*|^mcdonalds*|^mcdonalds restaurants", clientRoster$cleanName)] = "mcdonald's"

clientRoster$cleanName[grep("^morgan jp", clientRoster$cleanName)] = "morgan (j p)"

clientRoster$cleanName[grep("^nabisco", clientRoster$cleanName)] = "nabisco group holdings"

clientRoster$cleanName[grep("parker hannifan", clientRoster$cleanName)] = "parker hannifin"

clientRoster$cleanName[grep("pepsi cola|pepsico|pepsico international", clientRoster$cleanName)] = "pepsico"

clientRoster$cleanName[grep("^pfizer", clientRoster$cleanName)] = "pfizer"

clientRoster$cleanName[grep("^procter", clientRoster$cleanName)] = "procter gamble"

clientRoster$cleanName[grep("^reebok", clientRoster$cleanName)] = "reebok international"

clientRoster$cleanName[grep("^scholastic", clientRoster$cleanName)] = "scholastic"

clientRoster$cleanName[grep("^schwab charles", clientRoster$cleanName)] = "schwab (charles)"

clientRoster$cleanName[grep("^t rowe price", clientRoster$cleanName)] = "price (t. rowe) group"

clientRoster$cleanName[grep("^target stores", clientRoster$cleanName)] = "target"

clientRoster$cleanName[grep("texas insturments", clientRoster$cleanName)] = "texas instruments"

clientRoster$cleanName[grep("the coca cola company", clientRoster$cleanName)] = "coca cola"

clientRoster$cleanName[grep("the gillette company", clientRoster$cleanName)] = "gillette"

clientRoster$cleanName[grep("^time warner", clientRoster$cleanName)] = "time warner"

clientRoster$cleanName[grep("^ag edwards", clientRoster$cleanName)] = "edwards (a g)"

clientRoster$cleanName[grep("allstate insurance", clientRoster$cleanName)] = "allstate"

clientRoster$cleanName[grep("^bank of america|bankamerica", clientRoster$cleanName)] = "bank of america"

clientRoster$cleanName[grep("bank of boston", clientRoster$cleanName)] = "bankboston"

clientRoster$cleanName[grep("barnes noble bookstores", clientRoster$cleanName)] = "barnes noble"

clientRoster$cleanName[grep("allergan hydron", clientRoster$cleanName)] = "allergan"

clientRoster$cleanName[grep("american express publishing",
                            clientRoster$cleanName)] = "american express"

clientRoster$cleanName[grep("avis rent a car system",
                            clientRoster$cleanName)] = "avis budget group"

clientRoster$cleanName[grep("bank of new york",
                            clientRoster$cleanName)] = "bank of new york mellon"

# Let's make sure we do not have any trailing spaces.

clientRoster$cleanName = stringr::str_trim(clientRoster$cleanName)

compustatSheet$Company.Name = stringr::str_trim(compustatSheet$Company.Name)


### Selecting and Reshaping

compustatSheet = compustatSheet %>%
  select(Company.Name, Company.Ticker, Company.Cusip, Company.CIK,
         Company.SIC.Code, Company.NAICS.code) %>%
  unique()

compustatSheet = compustatSheet[!duplicated(compustatSheet$Company.Name), ]


library(fuzzyjoin)

test = stringdist_left_join(clientRoster, compustatSheet, by = c("cleanName" = "Company.Name"),
                     max_dist = .0250, method = "jw", p = .1, distance_col = "stringDistance")

write.csv(test, file = "valueFuzzyMerge/data/joinedData.csv")
