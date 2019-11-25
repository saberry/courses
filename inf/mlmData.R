library(lme4)

library(simstudy)

gen.school <- defData(varname = "s0", dist = "normal", formula = 0, variance = 3, 
                      id = "idSchool")

gen.school <- defData(gen.school, varname = "nClasses", dist = "noZeroPoisson", 
                      formula = 3)

dtSchool <- genData(8, gen.school)

dtSchool <- trtAssign(dtSchool, n = 2)

gen.class <- defDataAdd(varname = "c0", dist = "normal", formula = 0, variance = 2)

gen.class <- defDataAdd(gen.class, varname = "nStudents", dist = "noZeroPoisson", 
                        formula = 20)

dtClass <- genCluster(dtSchool, "idSchool", numIndsVar = "nClasses", level1ID = "idClass")

dtClass <- addColumns(gen.class, dtClass)

gen.student <- defDataAdd(varname = "Male", dist = "binary", 
                          formula = 0.5)
gen.student <- defDataAdd(gen.student, varname = "age", dist = "uniform", 
                          formula = "9.5; 10.5")
gen.student <- defDataAdd(gen.student, varname = "test", dist = "normal", 
                          formula = "50 - 5*Male + s0 + c0 + 8 * trtGrp", variance = 2)
dtStudent <- genCluster(dtClass, cLevelVar = "idClass", numIndsVar = "nStudents", 
                        level1ID = "idChild")

dtStudent <- addColumns(gen.student, dtStudent)

summary(lm(test ~ trtGrp + Male, data = dtStudent))

riMod = lmer(test ~ trtGrp + Male + (1|idSchool/idClass), data = dtStudent)

summary(riMod)
