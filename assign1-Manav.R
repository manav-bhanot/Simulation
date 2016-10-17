#Manav Bhanot

#Excercise 1

craps <- function() {
  isFirstRoll <- TRUE
  X <- c(4,5,6,8,9,10)
  repeat {
    DICE_ROLL_ONE <- sample(1:6,1,TRUE,1/6:1/6)
    DICE_ROLL_TWO <- sample(1:6,1,TRUE,1/6:1/6)
    sum <- DICE_ROLL_ONE + DICE_ROLL_TWO
    if (isFirstRoll) {
      if (sum %in% c(7,11)) {
        return(1)
        break
      } else if (sum %in% c(2,3,12)) {
        return(0)
        break
      } 
      isFirstRoll <- FALSE
    } else {
      if (sum %in% X) {
        return(1)
        break
      } else if (sum == 7) {
        return(0)
        break
      }
    }
  }
}


#Excercise 2

f <- function() {
  return (sample(0:1,1,TRUE,1/2:1/2))
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


#Excercise 3

# Calling estimate_bernoulli to estimate the probability of winning a game of craps
# The function craps is passed as the first argument
cat("The probability of winning the craps game is : ",estimate_bernoulli(craps, 0.9, 0.005))

#Excercise 4

# Function to sample a state where the probability of a broken edge(0) is 0.001 and 
# the probability of a non-broken edge(1) is 0.999
sampleAState <- function() {
  return (sample(0:1,1,TRUE))
}

#
indicatorVar <- function(i) {
  if (i == 0) {
    return(0.001)
  }
  else {
    return(0.999)
  }
}

network_reliability <- function() {
  brokenConfigsRequied = 50
  numberOfBrokenConfigurations = 0
  sampleCount = 0
  
  # m = number of edges
  m = 7
  states = vector("numeric", m)
  
  # lambda = probability of the broken network configuration
  lambda = 0
  prob = 1
  
  while (numberOfBrokenConfigurations <= brokenConfigsRequied) {
    for (i in 1:m) {
      states[i] = sampleAState();
    }
    sampleCount = sampleCount + 1
    if ((states[1] == 0 && states[2] == 0) || 
        (states[5] == 0 && states[6] == 0 && states[7] == 0) ||
        (states[2] == 0 && states[3] == 0 && states[5] == 0) || 
        (states[1] == 0 && states[4] == 0 && states[7] == 0) ||
        (states[2] == 0 && states[4] == 0 && states[5] == 0 && states[6] == 0) ||
        (states[1] == 0 && states[3] == 0 && states[6] == 0 && states[7] == 0) || 
        (states[3] == 0 && states[4] == 0 && states[5] == 0 && states[7] == 0)) {
      numberOfBrokenConfigurations = numberOfBrokenConfigurations + 1
      for (i in 1:m) {
        prob = prob * indicatorVar(states[i])
      }
      lambda = lambda + prob 
    }
  }
  
  # variance = E[X^2] - E^2[X]. Note that E[X] = lambda and E[X^2] = lambda
  variance = brokenConfigsRequied*lambda - (lambda)^2
  
  cat("Probability of the broken network configuration is : ")
  print(lambda)
  
  cat("Sample Variance is : ")
  print(variance)
  
  cat("The number of samples needed to reach 50 broken configurations is : ")
  print(sampleCount)
}


#Excercise 5

indicatorVar2 <- function(i) {
  if (i == 0) {
    return(2/7)
  }
  else {
    return(5/7)
  }
}
network_reliability2 <- function() {
  brokenConfigsRequied = 50
  numberOfBrokenConfigurations = 0
  sampleCount = 0
  
  x <- (((0.999*7)/5)^5) * (((0.001*7)/2)^2)
  print(x)
  
  X = vector("numeric", 0)
  
  # m = number of edges
  m = 7
  states = vector("numeric", m)
  
  # lambda = probability of the broken network configuration
  lambda = 0
  prob = 1
  
  while (numberOfBrokenConfigurations <= brokenConfigsRequied) {
    for (i in 1:m) {
      states[i] = sampleAState();
    }
    sampleCount = sampleCount + 1
    X = X + c(0)
    
    if ((states[1] == 0 && states[2] == 0) || 
        (states[5] == 0 && states[6] == 0 && states[7] == 0) ||
        (states[2] == 0 && states[3] == 0 && states[5] == 0) || 
        (states[1] == 0 && states[4] == 0 && states[7] == 0) ||
        (states[2] == 0 && states[4] == 0 && states[5] == 0 && states[6] == 0) ||
        (states[1] == 0 && states[3] == 0 && states[6] == 0 && states[7] == 0) || 
        (states[3] == 0 && states[4] == 0 && states[5] == 0 && states[7] == 0)) {
      
      X = X + c(x)
      
      numberOfBrokenConfigurations = numberOfBrokenConfigurations + 1
      for (i in 1:m) {
        prob = prob * indicatorVar(states[i])
      }
      lambda = lambda + x*prob
    }
  }
  
  variance = (brokenConfigsRequied * (x^2) - sampleCount * (lambda^2)) / (sampleCount - 1)
  
  cat("Probability of the broken network configuration is : ")
  print(lambda)
  
  cat("Sample Variance is : ")
  print(variance)
  
  cat("The number of samples needed to reach 50 broken configurations is : ")
  print(sampleCount)
}
