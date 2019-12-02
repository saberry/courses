library(ompr.roi)
library(ROI.plugin.glpk)
library(ompr)
library(dplyr)

n <- 35

m <- 7

capacity <- rep.int(5, m)

banMin <- rep.int(2, m)

studentDat <- data.frame(name = paste(sample(LETTERS, n, replace = TRUE), 
                                      sample(LETTERS, n, replace = TRUE)), 
                         major = sample(0:1, n, replace = TRUE), 
                         idNum = 1:n)

summary(as.factor(studentDat$major))

model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:m, j = 1:n, type = "binary") %>%
  set_objective(sum_expr(studentDat[j, "major"] * x[i, j], i = 1:m, j = 1:n), sense = "max") %>%
  add_constraint(sum_expr(x[i, j], i = 1:m) <= capacity[j], j = 1:7) %>% 
  add_constraint(sum_expr(x[i, j], i = 1:n) >= banMin[j], j = 1:m) %>% 
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n)

model$variables

model$objective

model$constraints

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

result$objective_value

matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>% 
  left_join(., studentDat, by = c("i" = "idNum")) %>% 
  select(i, j, name, major)

matching %>% 
  group_by(i) %>% 
  summarize(sum(major))
