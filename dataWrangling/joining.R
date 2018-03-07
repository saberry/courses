library(dplyr)

matchTable = haven::read_sas("https://www3.nd.edu/~sberry5/data/wciklink_gvkey.sas7bdat") %>% 
  select(gvkey, conm, cik) %>% 
  group_by(gvkey) %>% 
  slice(1L)

companyDat = haven::read_dta("https://www3.nd.edu/~sberry5/data/merge1Company.dta") %>% 
  left_join(., matchTable, by = "gvkey")

companyDat$fyear = as.character(companyDat$fyear)

companyDat$sich = as.character(companyDat$sich)

summary10K = readr::read_csv("https://www3.nd.edu/~sberry5/data/merge3McDonald.csv")

summary10K = summary10K %>% 
  tidyr::separate(., FYE, into = c("year", "month", "day"), sep = c(4, 6)) %>% 
  tidyr::unite(col = FYE, year, month, day, sep = "-") %>% 
  select(CIK, FILING_DATE, FYE, FORM_TYPE, SIC, FFInd, 
         starts_with("N"), -N_Tables, -N_Exhibits)

summary10K$CIK = as.character(summary10K$CIK)

summary10K$SIC = as.character(summary10K$SIC)

summary10K$CIK = as.character(summary10K$CIK)

summary10K$CIK = ifelse(nchar(summary10K$CIK) == 2, paste("00000000", summary10K$CIK, sep = ""), 
                        ifelse(nchar(summary10K$CIK) == 3, paste("0000000", summary10K$CIK, sep = ""), 
                               ifelse(nchar(summary10K$CIK) == 4, paste("000000", summary10K$CIK, sep = ""), 
                                      ifelse(nchar(summary10K$CIK) == 5, paste("00000", summary10K$CIK, sep = ""), 
                                             ifelse(nchar(summary10K$CIK) == 6, paste("0000", summary10K$CIK, sep = ""), 
                                                    ifelse(nchar(summary10K$CIK) == 7, paste("000", summary10K$CIK, sep = ""), 
                                                           summary10K$CIK))))))

hmDat = readr::read_delim("https://www3.nd.edu/~sberry5/data/merge2Hoberg.txt", 
                  delim = "\t", col_names = TRUE)

hmDat$gvkey = as.character(hmDat$gvkey)

hmDat$gvkey = ifelse(nchar(hmDat$gvkey) == 4, paste("00", hmDat$gvkey, sep = ""), 
                     ifelse(nchar(hmDat$gvkey) == 5, paste("0", hmDat$gvkey, sep = ""), 
                     hmDat$gvkey))

hmDat$year = as.character(hmDat$year)

companyDat$datadate = as.character(companyDat$datadate)

companyDat$year = as.character(companyDat$year)

companyDatJoined = left_join(companyDat, summary10K, 
                             by = c("cik" = "CIK", 
                                    "datadate" = "FYE"))

companyDatJoined = left_join(companyDatJoined, hmDat, 
                          by = c("gvkey" = "gvkey", 
                                 "year" = "year"))

write.csv(companyDatJoined, file = "CompanyIDNov2017WithConstraintsFYEMerge.csv", 
          row.names = FALSE, na = "")
