library(simmer)

# Bank Teller

customer = trajectory("Customer path") %>% 
  set_attribute("start_time", function() {now(bank)}) %>%
  seize("teller") %>% 
  timeout(function() {rnorm(n = 1, mean = 3, sd = .5)}) %>% 
  release("teller")

bank = simmer("bank") %>% 
  add_resource("teller", capacity = 3, queue_size = 8) %>% 
  add_generator("Customer", customer, function() {c(0, rexp(100, 1/2), -1)})

run(bank, until = 120)

result = get_mon_arrivals(bank)

head(result)

mean(result$activity_time[result$finished == TRUE])

sum(result$finished[result$finished == TRUE])

finishers <- result[result$finished == TRUE, ]

finishers$waitingTime <- finishers$end_time - finishers$start_time - finishers$activity_time

mean(finishers$waitingTime)

library(simmer.plot)

get_palette <- scales::brewer_pal(type = "qual", palette = 1)

plot(customer, fill = get_palette)


get_mon_resources(bank)

get_queue_count(bank, "teller")

get_n_generated(bank, "Customer")

get_trajectory(bank, "Customer")


PT_MEAN <- 10.0         # Avg. processing time in minutes
PT_SIGMA <- 2.0         # Sigma of processing time
NUM_MACHINES <- 5      # Number of machines in the machine shop
WEEKS <- 4              # Simulation time in weeks
SIM_TIME <- WEEKS * 7 * 24 * 60  # Simulation time in minutes

# Machine Shop 

set.seed(42)

make_parts <- trajectory("parts") %>% 
  set_attribute("start_time", function() {now(machineShop)}) %>% 
  seize("machine1") %>% 
  timeout(function() {rnorm(n = 1, mean = 5, sd = .5)}) %>% 
  release("machine1") %>% 
  seize("machine2") %>% 
  timeout(function() {rnorm(n = 1, mean = 5, sd = .5)}) %>% 
  release("machine2") %>% 
  seize("machine3") %>% 
  timeout(function() {rnorm(n = 1, mean = 10, sd = 2.5)}) %>% 
  release("machine3")

machineShop <- simmer("machineShop") %>% 
  add_resource("machine1", capacity = 1, queue_size = 0) %>%
  add_resource("machine2", capacity = 1, queue_size = 0) %>% 
  add_resource("machine3", capacity = 1, queue_size = 0) %>% 
  add_generator("part", make_parts, mon = 2, function() {c(0, rexp(1000, 1/5), -1)})  

run(machineShop, 480)

result = get_mon_arrivals(machineShop)

sum(result$finished[result$finished == TRUE])

get_queue_count(bank, "teller")

get_n_generated(machineShop, "part")

get_trajectory(machineShop, "part")


# Call Center

callTimeMean <- 10

callTimeSD <- 2

arrivalRate <- 1/4

simulationTime <- 480

caller <- trajectory("caller") %>% 
  set_attribute("start_time", function() {now(callCenter)}) %>% 
  seize("phoneLine") %>% 
  timeout(function() rnorm(1, callTimeMean, callTimeSD)) %>% 
  release("phoneLine")

callCenter <- simmer("callCenter") %>% 
  add_resource("phoneLine", capacity = 4, queue_size = 6) %>% 
  add_generator("caller", caller, mon = 2, function() {c(0, rexp(1000, arrivalRate), -1)})

run(callCenter, 120)

results <- get_mon_arrivals(callCenter)

results$waitingTime <- results$end_time - results$start_time - results$activity_time

sum(results$finished[results$finished == TRUE]) / length(results$finished)

mean(results$waitingTime)

# Security

arrivalRate <- 1/.5

inspection1WorkingMean <- 1
inspection1WorkingSD <- .1

additionalInspection1WorkingMean <- 5
additionalInspection1WorkingSD <- 1

line2Percentage <- .1

passenger <- trajectory("passenger") %>% 
  set_attribute("start_time", function() {now(airport)}) %>% 
  seize("insp1") %>% 
  timeout(function() rnorm(1, inspection1WorkingMean, inspection1WorkingSD)) %>% 
  release("insp1") %>% 
  branch(function() sample(0:1, 1, prob = c(1 - line2Percentage, line2Percentage)) == 1, 
         continue = TRUE,
         trajectory() %>% 
           seize("insp2") %>% 
           timeout(function() rnorm(1, additionalInspection1WorkingMean, additionalInspection1WorkingSD)) %>% 
           release("insp2")
  )
  
airport <- simmer("airport") %>% 
  add_resource("insp1", capacity = 2, queue_size = Inf) %>% 
  add_resource("insp2", capacity = 2, queue_size = Inf) %>% 
  add_generator("passenger", passenger, mon = 2, function() {c(0, rexp(1000, arrivalRate), -1)})

run(airport, 120)

get_mon_attributes(airport)

resourcesResults <- get_mon_resources(airport)

result <- get_mon_arrivals(airport)

plot(result, metric = "flow_time")

plot(result, metric = "waiting_time")

sum(result$finished[result$finished == TRUE])

(get_mon_resources(airport))$resource

plot(passenger)

plot(resourcesResults, metric = "utilization")

plot(resourcesResults, metric = "usage")


# Bread

breadInventory <- 90

loavesPurchased <- 1

purchaseRate <- 1 / (10/60)

simTime <- 12 * 30

checkoutTime <- 3 / 60

unloadTime <- 10 / 60

customer <- trajectory() %>% 
  branch(function() breadInventory > 0, 
         continue = FALSE, 
         trajectory() %>% 
           seize("checkOut") %>% 
           timeout(function() {
             breadInventory <<- breadInventory - loavesPurchased
             return(checkoutTime)
           }) %>% 
           release("checkOut") %>% 
           log_(function() paste(breadInventory, "left", sep = " "))) %>% 
  leave(prob = 1)

breadTruck <- trajectory() %>% 
  branch(function() (now(store) %% 12) == 0, 
         continue = TRUE, 
         trajectory() %>% 
           timeout(function() {
             breadInventory <<- 90
             return(unloadTime)
           }) %>% 
           log_("delivery")) %>% 
  leave(prob = 1)

store <- simmer("store") %>% 
  add_resource("checkOut", 1) %>% 
  add_generator("breadTruck", breadTruck, at(seq(0, simTime, by = 24))) %>% 
  add_generator("customer", customer, function() c(0, rexp(10000, purchaseRate), -1))

run(store, simTime)

plot(customer)

plot(breadTruck)

results <- get_mon_arrivals(store)

results <- results[which(grepl("^bread", results$name) == FALSE), ]

noBread <- nrow(results[results$finished == FALSE, ])

bread <- nrow(results[results$finished == TRUE, ])

bread / nrow(results)

get_mon_resources(store)

get_mon_attributes(store)
