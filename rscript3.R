fn = function(x) {
  return (sin(x) / x)
}

funResult = function(N, min, max) {
  randomData = runif(N, min, max)
  resultData = fn(randomData)
  
  count = mean(resultData)
  
  return (count);
}

funResult(100000, 0, 1)