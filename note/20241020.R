# 求f(x1, x2) = (x2 - x1) ^ 2 + (1 - x1) ^ 2的极值
# 暴力计算有问题
calFunc = function(param) {
  result = (param[2] - param[1] ^ 2) ^ 2 + (1 - param[1]) ^ 2
  
  return (result)
}

models = tibble(
  x = runif(2500, -100, 100),
  y = runif(2500, -100, 100)
)

for(i in 1:2500) {
  result[i] = calFunc(c(models$x[i], models$y[i]))
}


maxData = max(result)

minData = min(result)

print (c(maxData, minData))

#分析法
calFuncS = function(param) {
  result = optim(c(0, 0), calFunc)
  
  print (result)
}

# 回归法
calFunB = function() {
  sim1_mod = lm(y ~ x, data = sim1)
  coef(sim1_mod)
}

# 箱线图
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

# hex
ggplot(diamonds, aes(carat, price)) + geom_hex(bins = 30)

# 查看数据线, P值显著度
diamonds2 = diamonds %>% filter(carat <= 2.5) %>% mutate(lprice = log2(price), lcarat = log2(carat))
ggplot(diamonds2, aes(lcarat, lprice)) + geom_hex(bins = 30)
mod_diamond = lm(lprice ~ lcarat, data = diamonds2)

grid = diamonds2 %>% data_grid(carat = seq_range(carat, 20)) %>% mutate(lcarat = log2(carat)) %>% add_predictions(mod_diamond, "lprice") %>% mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + geom_hex(bins = 30) + geom_line(data = grid, color = "red", size = 1)



# 字符串
string1 = "This is a string"
string2 = "To put a change"
str_length(c("a", "R for", NA))

# 字符串组合
test_str = ""

str_c("x", "y")

str_c("x", "y", "z", NA) # 输出NA

str_c("x", c(1,3,4), "y") # 连续拼接

test_str = c("测试123测试", "看看这里", "测试过了")
str_sub(test_str, 1, 2) # 复数倒数

# 正则表达式
str_view(x, '.a.')

# 正则基操
x = c("apple", "banana", "pear")

mean(str_detect(x, "e"))

# 辅音开头
sum(str_count(words, "s"))

# 建立表单
df = tibble(
  word = words,
  i = seq_along(word)
)

df %>% filter(str_detect(words, "x$"))

df = mutate(
  vowels = str_count(word, "[aeiou]"),
  consonants = str_count(word, "[^aeiou]")
)

# 匹配句子
color_vector = c("\\bred\\b", "orange", "yellow", "green", "blue", "purple")
color_match = str_c(color_vector, collapse = "|")
more = sentences[str_count(sentences, color_match) > 1]

# 匹配文章
library(forcats)

# 读文章
textA = readLines("./work/textA.txt")
textB = readLines("./work/textB.txt")
wordA = unlist(strsplit(textA, "\\W+"))
wordB = unlist(strsplit(textB, "\\W+"))

matchBasic = c("may", "maybe", "perhaps", "probably", "likely", "would", "should", "can", "could")
matchWords = 
  
  (matchBasic, collapse = "|")

matchS = sum(str_count(wordA, matchWords))
matchS2 = sum(str_count(wordB, matchWords))

# 求逗号和句号
countA = sum(str_count(textA, "[,\\.]"))
countB = sum(str_count(textB, "[,\\.]"))

countC = sum(str_count(textA, "[\\.]"))
countD = sum(str_count(textA, "[,]"))

# 求停顿
stopA = length(wordA) / countA
stopB = length(wordB) / countB

stopC = length(wordA) / countC
stopD = length(wordA) / countD

