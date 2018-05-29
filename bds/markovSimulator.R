# You manage 10 teams, named from 1 to 10. Each team name is proportional to the number of people on the team and each team is on a separate floor in the building. 
# 
# You need to visit a team everyday, proportional to the number of people on the team (i.e., you would visit team 10 more often than team 1). 
# 
# At the end of the day, you randomly select whether a proposed move will take you up a floor or down a floor. 
# 
# After randomly selecting up or down, you grab a number of pens that is equal to the number of the current team (for team 10, you would grab 10 pens). 
# 
# Next, you would grab a number of pencils corresponding to the proposed move (your randomly selected proposal would take you up a floor, so starting back at the bottom with team 1). So you would have 10 pens and 1 pencil. 
# 
# If you have more pencils than pens, you will always move to the proposed floor.
# 
# If you have more pens than pencils, you set down a number of pens equal to the number of pencils and put them in a drawer.
# 
# You reach back into the drawer to randomly select a pen or a pencil. 
# 
# Your selection decides where you go -- pen is stay and pencil is go!

markovSim = function(daysRun, startDay) {
  
  
  position = rep(0, daysRun)
  
  current = startDay
  
  for(i in 1:daysRun) {
    position[i] = current
    
    proposal = current + sample(c(-1, 1), size = 1)
    
    if(proposal < 1) proposal = 10
    
    if(proposal > 10) proposal = 1
    
    probabilityMove = proposal / current
    
    current = ifelse(runif(1) < probabilityMove, proposal, current)
    
    # print(paste(position[i], proposal, probabilityMove, current, sep = " -> "))
  }
  
  return(position)
}

test1 = markovSim(1000, 5)

test2 = markovSim(1000, 6)

hist(position)

library(ggplot2)

ggplot() + 
  geom_line(data = as.data.frame(test1), aes(1:length(test1), test1), 
            color = "#ff5500", size = .75) +
  geom_line(data = as.data.frame(test2), aes(1:length(test2), test2), 
            color = "black", size = .75) +
  theme_minimal()
