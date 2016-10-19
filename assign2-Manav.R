# Manav Bhanot

#Excercise 1

# Get the contract matrix
contractMatrix = matrix(
  c(30,20,15,0,0,21,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,20,0,0,41,0,15,0,7,0,0,0,9,0,0,0,0,0,10,0,0,0,0,10,0,25,5,10,0,10,0,10,0,0,10,10,0,0,0,0,80,0,0,0,0,0,0,0,0,0,0,0,20,0,0,0,0,20,0,0,0,0,0,10,0,0,15,0,10,0,0,0,10,10,10,0,15,0,10,10,0,0,15,0,0,7,8,0,0,0,0,9,6,20,0,15,0,3,6,9,0,15,0,11,7,0,10,0,12,0,0,14,0,0,0,0,13,7,5,6,8,4,5,5,5,5,8,4,10,5,5,4,4,2,2,3,3,3,3,2,2,4,4,5,5,10,4,8,5,5,5,5,4,8,6,5,7,10,10,10,10,0,0,0,0,10,0,15,0,0,15,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,0,14,21,0,0,15,20,30,0,0,20,0,8,0,0,15,0,0,41,0,9,0,0,0,0,7,0,0,6,6,6,6,6,6,6,6,8,6,0,6,0,6,0,6,4,6,4,6,5,7,8,6,5,4,5,5,8,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,10,10,10,0,15,0,10,0,0,15,0,20,0,0,0),
  nrow=15,
  ncol=20,
  byrow=TRUE
)

# Take the daily requirement for each city
dailyDemand = c(6.04, 7.15, 9.04, 10.12, 5.80, 5.40, 6.20, 6.06, 7.97, 6.52, 9.05, 5.37, 3.99, 6.69, 5.85, 5.88, 4.86, 5.86, 7.00, 8.30)

# Stores the daily Power Generation by each station
dailyPowerGeneration = matrix(
  c(1,13.60,0.16,1,12.00,0.16,1,5.60,0.14,9,0.76,0.02,6,0.65,0.01,6,1.12,0.02,1,5.53,0.17,3,5.53,0.17,2,7.35,0.12,8,0.78,0.02,11,1.09,0.02,8,0.78,0.02,18,1.45,0.02,6,7.74,0.02,4,0.95,0.01),
  nrow=15,
  ncol=3,
  byrow=TRUE
)

getWorkingGeneratorsOnThisDay = function(stationNumber) {
  return (sample(0:1, dailyPowerGeneration[i][1], TRUE, dailyPowerGeneration[i][3]:1-dailyPowerGeneration[i][3]))
}

daily_power = function() {
  totalPowerGeneratedOnThisDay = 0
  
  # For each station, get the count of working generators on this day by sampling through an indicator variable
  for (i in 1:nrow(dailyPowerGeneration)) {
    X = sample(0:1, dailyPowerGeneration[i][1], TRUE, dailyPowerGeneration[i][3]:1-dailyPowerGeneration[i][3])
    powerGeneratedByThisStation = dailyPowerGeneration[i][2] * X
    totalPowerGeneratedOnThisDay = totalPowerGeneratedOnThisDay + sum(powerGeneratedByThisStation)
  }
  
  return (totalPowerGeneratedOnThisDay)
}

daily_surplus = function() {
  
  totalPowerDemand = sum(dailyDemand)
  totalSupply = daily_power()
  
  return (totalSupply - totalPowerDemand)
  
  # For each station, get the count of working generators on this day by sampling through an indicator variable
  # for (i in 1:nrow(dailyPowerGeneration)) {
  #  X = sample(0:1, dailyPowerGeneration[i][1], TRUE, dailyPowerGeneration[i][3]:1-dailyPowerGeneration[i][3])
  #  powerGeneratedByThisStation = dailyPowerGeneration[i][2] * X
  #  totalPowerGeneratedOnThisDay = totalPowerGeneratedOnThisDay + sum(powerGeneratedByThisStation)
  # }
  
}


# Excercise 2

blackout = function() {
  if (daily_surplus() < 0) {
    return (0)
  } else {
    return (1)
  }
}

estimate_bernoulli <- function(fun, delta, eps) {
  batchSize = 10000
  inc = 10000
  
  s = vector(mode="numeric",10000)
  sampleSpace = vector(mode="numeric", 0)
  
  totalSum = 0
  
  index = 1
  
  repeat {
    sum = 0
    for (i in 1:10000) {
      s[i] = fun()
      sum = sum + s[i]
    }
    totalSum = totalSum + sum
    sampleSpace = c(sampleSpace, s)
    
    z <- qnorm((1 + delta)/2)
    
    se = sqrt(var(sampleSpace)/batchSize)
    
    if ((se * z) <= eps) {
      lambda = totalSum/batchSize
      return (lambda)
    } 
    batchSize = batchSize + inc
  }
}


# Excercise 3

daily_power2 <- function(s, p) {
  X = sample(0:1, dailyPowerGeneration[s][1], TRUE, p:1-p)
  powerGeneratedByThisStation = dailyPowerGeneration[i][2] * X
}

# Excercise 4

