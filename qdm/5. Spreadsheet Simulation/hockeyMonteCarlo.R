timeDat = data.frame(timeLeft = seq(from = 120, to = 0, by = -10), 
                     currentScore = 0, 
                     behind = 0, 
                     goaliePulled = 0, 
                     weScore = 0, 
                     theyScore = 0)

fullStrength = .05/6

ourGoalie = .08/6

theirGoalie = .12/6

score = -1

goaliePull = function(score, pullTime, fullStrength, ourGoalie, theirGoalie) {
  
  timeDat$currentScore[1] = score
  
  timeDat$behind[1] = ifelse(timeDat$currentScore[1] < 0, 1, 0)
  
  timeDat$goaliePulled[1] = ifelse(timeDat$timeLeft[1] == pullTime & timeDat$behind[1] == 1, 
                                   1, 0)
  
  timeDat$weScore[1] = ifelse(timeDat$goaliePulled[1] == 0, 
                              rbinom(1, 1, fullStrength), 
                              rbinom(1, 1, ourGoalie))
  
  timeDat$theyScore[1] = ifelse(timeDat$goaliePulled[1] == 0, 
                                rbinom(1, 1, ourGoalie), 
                                rbinom(1, 1, theirGoalie))
  
  for(i in 2:nrow(timeDat)) {
    timeDat$currentScore[i] = timeDat$currentScore[i - 1] + 
      timeDat$weScore[i - 1] - timeDat$theyScore[i - 1]
    
    timeDat$behind[i] = ifelse(timeDat$currentScore[i] < 0, 1, 0)
    
    timeDat$goaliePulled[i] = ifelse(timeDat$timeLeft[i] <= pullTime & timeDat$behind[i] == 1, 
                                     1, 0)
    
    timeDat$weScore[i] = ifelse(timeDat$goaliePulled[i] == 0, 
                                rbinom(1, 1, fullStrength), 
                                rbinom(1, 1, ourGoalie))
    
    timeDat$theyScore[i] = ifelse(timeDat$goaliePulled[i] == 0, 
                                  rbinom(1, 1, ourGoalie), 
                                  rbinom(1, 1, theirGoalie))
  }
  
  result = if(timeDat$currentScore[nrow(timeDat)] > -1) {
    1
  } else 0
  
  return(list("result" = result))
}

goaliePull(score, pullTime = 120, fullStrength, ourGoalie, theirGoalie)

results = replicate(1000, expr = {
  res = goaliePull(score, pullTime = 120, fullStrength, ourGoalie, theirGoalie)
  
  res[[1]]
})

sum(results[results == 1]) / length(results)


