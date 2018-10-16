# EX 5.4 RedBrand Transshipment Model with transient nodes
#load data
data = read.table(text=
"Origin	Destination	UnitCost
1 2 5
1 3 3
1 4 5
1 5 5
1 6 20
1 7 20
2 1 9
2 3 9
2 4 1
2 5 1
2 6 8
2 7 15
3 1 0.4
3 2 8
3 4 1
3 5 0.5
3 6 10
3 7 12
4 5 1.2
4 6 2
4 7 12
5 4 .8
5 6 2
5 7 12
6 7 1
7 6 7
",header=T)

#import packages
# install.packages("symphony")  (After it loaded the packet in the 1st time, later runs no longer need this line)
library(dplyr)
library(ROI)
library(ROI.plugin.symphony) 
library(ompr)
library(ompr.roi)

#reorganize data as vectors
from = unique(data$Origin)
to = unique(data$Destination)

#setting up costs price matrix, NA values maximized such that solver won't read them
costs = matrix(0, ncol = length(from), nrow = length(to))

for (a in 1:length(data[, 1])) {
  costs[data[a, 1], data[a, 2]] = data[a, 3] 
}

for (y in 1:length(to))
  for (z in 1:length(from))
    if (costs[y, z] == 0)
      costs[y, z] = 99999


#MODEL IS INCOMPLETE, SEE CONSTRAINT ON LINE 66 (changed from <=100 to >=100, no solution if do not change sign)
# Same results obtained on Symphony and Optimx

arcCapacity = 200

#LP Model
model = MIPModel()%>%
  add_variable(x[i, j], i = 1:length(to), j = 1:length(from),
               type="integer", lb = 0, ub = 200) %>%
  add_constraint(((sum_expr(x[i, j], j = 1:length(from), i = 1)) - (sum_expr(x[i , j], i = 1:length(to), j = 1))) <= 200) %>%
  add_constraint(((sum_expr(x[i, j], j = 1:length(from), i = 2)) - (sum_expr(x[i , j], i = 1:length(to), j = 2))) <= 300) %>%
  add_constraint(((sum_expr(x[i, j], j = 1:length(from), i = 3)) - (sum_expr(x[i , j], i = 1:length(to), j = 3))) >= 100) %>%
  add_constraint(((sum_expr(x[i, j], j = 1:length(from), i = 4)) - (sum_expr(x[i , j], i = 1:length(to), j = 4))) == 0) %>%
  add_constraint(((sum_expr(x[i, j], j = 1:length(from), i = 5)) - (sum_expr(x[i , j], i = 1:length(to), j = 5))) == 0) %>%
  add_constraint(((sum_expr(x[i, j], i = 1:length(from), j = 6)) - (sum_expr(x[i , j], j = 1:length(to), i = 6))) >= 400) %>%
  add_constraint(((sum_expr(x[i, j], i = 1:length(from), j = 7)) - (sum_expr(x[i , j], j = 1:length(to), i = 7))) >= 180) %>%
  add_constraint(sum_expr(x[i, j], j = 1:length(to)) <= arcCapacity, i = 1:length(to))%>%
  set_objective(sum_expr(x[i, j] * costs[i, j], i = 1:length(to), j = 1:length(from)), "min")


#solve model
objective = solve_model(model, 
                        with_ROI(solver = "symphony", verbosity = TRUE)) #model solving such that final cost can be seen

result = get_solution(objective, x[i, j])

result = matrix(result[, 4], ncol = length(from), nrow = length(to))

