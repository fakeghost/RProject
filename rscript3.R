fn = function(x) {
  return (sin(x) / x)
}

funResult = function(N, min, max) {
  randomData = runif(N, min, max)
  resultData = fn(randomData)
  
  count = (max - min) * mean(resultData)
  
  return (randomData);
}

funResult(100000, 0, 1)