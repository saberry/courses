mix = function() {
  x = 100
  while(x < 0) {
    x = x - sample(1:10)
    return(x)
  }
}

y = list()

test = function(num) {
  
  num = num - sample(1:10, 1)
  
  if(num < 90) {
    num
  } else {
    num = c(num, num - sample(1:10, 1))
    test(num)
  }
}

shroudData = data.frame(shroudsProduced = floor(rexp(1095, rate = .1)), 
                        employeeCount = sample(15:30, 1095, replace = TRUE), 
                        maxOutsideTempF = as.vector(plyr::raply(3, c(sample(15:32, 91, replace = TRUE), 
                                                                  sample(35:50, 45, replace = TRUE), 
                                                                  sample(50:72, 45, replace = TRUE), 
                                                                  sample(80:95, 92, replace = TRUE), 
                                                                  sample(50:72, 92, replace = TRUE)))), 
                        ceramicMixTons = unlist(replicate(500, c(92, test(95))))[1:1095], 
                        accidentLogged = sample(0:1, 1095, prob = c(.025, .975), replace = TRUE), 
                        kilnMaintNY = rep(c(rep(0, 29), 1), 39)[1:1095], 
                        day = 1:1095
)

summary(lm(shroudsProduced ~ employeeCount, data = shroudData))

summary(glm(shroudsProduced ~ employeeCount, data = shroudData, family = poisson))
