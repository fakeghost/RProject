# 向量求平方
sum1 = function(vctor) {
  result = 0
  for(i in vctor) {
    result = result + i ^ 2
  }
  
  return (result)
}

# 从一个卡方分布取一部分数据，查看图
df = 5
x = rchisq(10, df)
y = (x / 10) ^ 2 + x
hist(y)

# 从均匀分布取一部分数据，求sin，cos
x = runif(100)
x2 = x ^ 2
x3 = sin(x2)
x4 = cos(x3)
hist(x4)

# 取10000个均值
result = numeric(100000)
for(i in 1:100000) {
  result[i] = sum(runif(100, min = 0, max = 1)) / 100
}
hist(result)

# 计算pi的值
calcKa = function () {
  x = runif(1000000, min = -1, max = 1)
  y = runif(1000000, min = -1, max = 1)
  count = 0;
  pie = 0
  
  for(i in 1:1000000) {
    if(x[i] ^ 2 + y[i] ^ 2 <= 1) {
      count = count + 1
    }
  }
  
  # 向量写法
  m = sum(x ^ 2 + y ^ 2 <= 1)
  
  pie = count / 1000000 * 4
  
  return (pie)
}

# 每次5个点
calcKa = function () {
  x = runif(5, min = -1, max = 1)
  y = runif(5, min = -1, max = 1)
  count = 0;
  pie = 0
  
  for(i in 1:5) {
    if(x[i] ^ 2 + y[i] ^ 2 <= 1) {
      count = count + 1
    }
  }
  
  # 向量写法
  m = sum(x ^ 2 + y ^ 2 <= 1)
  
  pie = count / 5 * 4
  
  return (pie)
}

meanResult = numeric(100000);

for(i in 1:100000) {
  meanResult[i] = calcKa()
}

hist(meanResult)

mean(meanResult)

# 老汤的代码查问题
v = c()
for (i in 1:10000){
  n = 5
  x <- runif(n,min= -1,max = 1)
  y <- runif(n, min = -1 ,max =1)
  m <- sum(x^2+y^2 <= 1)
  pi <- 4*m/n
  # v[i] = pi
  v[i] = pi
}
hist(v)

# 封装成函数
calcKa = function (n) {
  x = runif(n, min = -1, max = 1)
  y = runif(n, min = -1, max = 1)
  count = 0;
  pie = 0
  
  for(i in 1:n) {
    if(x[i] ^ 2 + y[i] ^ 2 <= 1) {
      count = count + 1
    }
  }
  
  # 向量写法
  m = sum(x ^ 2 + y ^ 2 <= 1)
  
  pie = count / n * 4
  
  return (pie)
}

calcPie = function(n, count) {
  meanResult = numeric(count);
  
  for(i in 1:count) {
    meanResult[i] = calcKa(n)
  }
  
  hist(meanResult)
  
  return (mean(meanResult))
}

# 小于平均值就*2，大于就/2
calMean = function() {
  randomData = runif(20, min = -10, max = 10)
  arrangeData = mean(randomData)
  
  for(i in 1:20) {
    if(randomData[i] > arrangeData) {
      randomData[i] = randomData[i] / 2
    } else {
      randomData[i] = randomData[i] * 2
    }
  }
  
  hist(randomData)
}

# 模型开始
library(modelr)
options(na.action = na.warn)
ggplot(sim1, aes(x, y)) + geom_point()

models = tibble(
  x = runif(2500, -20, 40),
  y = runif(2500, -5, 5)
)

ggplot(sim1, aes(x, y)) + geom_abline(aes(intercept = x, slope = y), data = models, alpha = 1/4) + geom_point()

# 模型计算方式，完全没搞懂这里
model1 = function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance = function(mod, data) {
  diff = data$y - model1(mod, data)
  
  sum(diff ^ 2)
}

result = numeric(2500)

x = runif(2500, -20, 40)
y = runif(2500, -5, 5)

for(i in 2500) {
  result[i] = measure_distance(c(x[i], y[i]), sim1)
}

calLine = function() {
  models = tibble(
    x = runif(250000, -20, 40),
    y = runif(250000, -5, 5)
  )
  
  model1 = function(a, data) {
    a[1] + data$x * a[2]
  }
  
  measure_distance = function(mod, data) {
    diff = data$y - model1(mod, data)
    
    return (sum(diff ^ 2))
    
  }
  
  result = numeric(250000)
  
  for(i in 1:250000) {
    result[i] = measure_distance(c(models$x[i], models$y[i]), sim1)
  }
  
  lastIndex = which.min(result)
  
  return (c(models$x[lastIndex], models$y[lastIndex]))
}
