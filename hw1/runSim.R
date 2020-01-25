## generate 

## parsing command arguments
for (arg in commandArgs(TRUE)) {
  eval(parse(text=arg))
}

## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}

# set seed for random number generation
set.seed(seed)

mseSampAvg = 0
msePrimeAvg = 0

# simulate data
for (r in 1:rep){
  # parse the distribution information
  if (dist == "gaussian"){
    x = rnorm(n)
  } else if (dist == "t1"){
    x = rt(n, df = 1)
  } else if (dist == "t5"){
    x = rt(n, df = 5)
  } else {
    stop("unrecognized distribution")
  }
  
  # compute the primed-indexed average estimator and the classical sample average estimator
  PrimeAvg = estMeanPrimes(x)
  SampAvg = mean(x)
  
  # try two methods
  msePrimeAvg = msePrimeAvg + estMeanPrimes(x)^2
  mseSampAvg = mseSampAvg + mean(x)^2
}

# report the MSE
print(msePrimeAvg / rep)
print(mseSampAvg / rep)