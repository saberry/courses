library(dplyr)

library(simstudy)

def <- defData(varname = "nr", dist = "nonrandom", formula = 7, id = "idnum")

def <- defData(def, varname = "age", dist = "normal", formula = "45", variance = 15)

def <- defData(def, varname = "numberPriorJobs", dist = "poisson", formula = "0.2 * age", 
               link = "identity")

def <- defData(def, varname = "department", formula = "0.3;0.2;0.5", dist = "categorical")

def <- defData(def, varname = "proportion401K", dist = "beta", formula = "0.15*department", variance = 1, 
               link = "logit")

def <- defData(def, varname = "startingSalary", dist = "poisson", formula = "10000 * (0.2 * age)", 
               link = "identity")

def <- defData(def, varname = "separatedNY", dist = "binary", formula = "-3 + department", 
               link = "logit")

sdef <- defSurv(varname = "daysToSeparate", formula = "1.5*separatedNY", scale = "department*50 + (1-department)*25", 
                shape = "department*1 + (1-department)*1.5")

dt <- genData(1000, def)

dt <- genSurv(dt, sdef)

summary(dt)

dt <- dt %>% 
  mutate_all(round)
