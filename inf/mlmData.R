library(lme4)

library(simstudy)

gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 3, 
                      id = "idBuilding")

gen.school <- defData(gen.school, varname = "nDepartments", dist = "noZeroPoisson", 
                      formula = 3)

dtSchool <- genData(8, gen.school)

# dtSchool <- trtAssign(dtSchool, n = 2)

gen.class <- defDataAdd(varname = "c0", dist = "normal", formula = 0, variance = 2)

gen.class <- defDataAdd(gen.class, varname = "nEmployees", dist = "noZeroPoisson", 
                        formula = 20)

dtClass <- genCluster(dtSchool, "idBuilding", numIndsVar = "nDepartments", level1ID = "idDepartment")

dtClass <- addColumns(gen.class, dtClass)

gen.student <- defDataAdd(varname = "Manager", dist = "binary", 
                          formula = 0.5)

gen.student <- defDataAdd(gen.student, varname = "age", dist = "uniform", 
                          formula = "19.5; 72")

gen.student <- defDataAdd(gen.student, varname = "performanceScore", dist = "normal", 
                          formula = "50 - 5*Manager + s0 + c0 + 8  - age*.35", variance = 2)

dtStudent <- genCluster(dtClass, cLevelVar = "idDepartment", numIndsVar = "nEmployees", 
                        level1ID = "idEmployee")

dtStudent <- addColumns(gen.student, dtStudent)

dtStudent <- dplyr::select(dtStudent, -s0, -c0)

write.csv(dtStudent, "inf/performanceReviewData.csv", row.names = FALSE)

summary(lm(performanceScore ~ trtGrp + Manager + age, data = dtStudent))

intMod <- lmer(performanceScore ~ trtGrp + 1 + (1|idDepartment), data = dtStudent)

summary(intMod)

riMod = lmer(performanceScore ~ trtGrp + Manager + (1|idDepartment), data = dtStudent)

randomEffects <- lmer(performanceScore ~ trtGrp + Manager + (age|idBuilding), data = dtStudent)

nestedMod = lmer(performanceScore ~ trtGrp + Manager + (1|idBuilding/idDepartment), data = dtStudent)

summary(riMod)
