library(linprog)

advert <- read.csv("qdm/3. Linear Programming/advertisingData.csv", row.names = 1)

dropDemos <- which(names(advert) == "demographicTotal")

objectiveFunction <- as.matrix(advert["cost", -c(dropDemos)])

dropCost <- which(rownames(advert) == "cost")

constraintMargins <- as.matrix(advert[-c(dropCost), "demographicTotal"])

constraintMatrix <- as.matrix(advert[-c(dropCost), -c(dropDemos)])

res = solveLP(objectiveFunction, constraintMargins, constraintMatrix, 
              maximum = FALSE, const.dir = rep(">=", length(b)))

res$message

C <- c(bbt = 192,
      bones = 108,
      gg = 59,
      glee = 127,
      himym = 145,
      mf = 130,
      ncis = 167,
      office = 191,
      thm = 201) # Objective

b = c(men18 = 80,
      men36 = 56, 
      men55 = 30, 
      women18 = 120, 
      women36 = 56, 
      women55 = 30) # margins

A = rbind(c(4.5, 4.0, 0.2, 2.5, 2.0, 2.0, 3.5, 1.0, 4), 
          c(2.0, 3.0, 0.1, 1.5, 1.5, 1.5, 3.5, 2.5, 3), 
          c(0.5, 0.5, 0.0, 0.5, 1.0, 1.0, 2.0, 1.0, 1), 
          c(4.5, 1.5, 1.5, 3.5, 2.0, 2.0, 2.0, 1.0, 3), 
          c(2.0, 1.5, 0.2, 1.5, 1.5, 1.5, 3.0, 2.5, 3), 
          c(0.5, 0.5, 0.0, 0.5, 1.0, 1.0, 2.0, 1.0, 1)) # constraints

rownames(A) <- names(b)

colnames(A) <- names(C)

res = solveLP(C, b, A, maximum = FALSE, const.dir = rep(">=", length(b)))

res

library(ROI)

constraints = L_constraint(A, rep(">=", 6), b)

model = OP(objective = C, 
           constraints = constraints,
           types = rep.int("I", 9), 
           maximum = FALSE)
model

res = ROI_solve(model, "glpk", verbose = TRUE)

solution(res)

solution(res, "objval")

solution(res, "aux")

library(dplyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

result <- MIPModel() %>%
  add_variable(x[i], i = 1:9, type = "integer", lb = 0) %>%
  set_objective(sum_expr(C[i] * x[i], i = 1:9), sense = "min") %>%
  add_constraint(sum_expr(x[i] * A[j, i], i = 1:9) >= b[j], j = 1:6) 

extract_constraints(result)

out = solve_model(result, with_ROI(solver = "glpk", verbose = TRUE)) 

get_solution(out, x[i])

objective_value(out)

objective_function(result)

solver_status(out)

get_row_duals(out)


### Mixing Drinks

drinks <- data.frame(drink = c("sob", "cosmo"), 
                     profit = c(5, 6))

alcohol <- data.frame(type = c("vodka", "schnapps", 
                               "sec", "juice"), 
                      percent = c(.4, .21, .3, 0), 
                      available = c(1200, 600, 500, 3000), 
                      stringsAsFactors = FALSE)

availableBooze <- c(1200, 600, 500, 3000)

MIPModel() %>% 
  add_variable(x[i], i = 1:2, type = "continuous") %>% 
  set_objective(sum_expr(profit[i] * x[i], i = 1:2), sense = "max") %>% 
  add_constraint(sum_expr(x[1])) %>% 
  add_constraint(x[i] == 6, i = 1:2) %>% 
  add_constraint(sum_expr(x[i] * drinks[i, 2],i = 1:2) <= availableBooze[j], j = 1:4) %>% 
  # solve_model(., with_ROI(solver = "glpk", verbose = TRUE))
  extract_constraints()

  get_solution(out, x[i])
  
  objective_value(out)
  