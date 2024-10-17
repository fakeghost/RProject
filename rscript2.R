install.packages("readxl")
library(readxl)

table1 = read_excel("./Work/hw1_a.xlsx")
table2 = read_excel("./Work/hw1_b.xlsx")

# 表1的各种值
arrange1 = mean(table1$Age, na.rm = TRUE)
maxAage1 = max(table1$Age, na.rm = TRUE)
minAage1 = min(table1$Age, na.rm = TRUE)
sdAage1 = sd(table1$Age, na.rm = TRUE)

# 表2的各种值
arrange2 = mean(table2$Age, na.rm = TRUE)
maxAage2 = max(table2$Age, na.rm = TRUE)
minAage2 = min(table2$Age, na.rm = TRUE)
sdAage2 = sd(table2$Age, na.rm = TRUE)

# 合并表单, key为ID
# 第二问
mergeResult = left_join(table1, table2, by="ID")

# 第三问
Income1 = table1 %>% filter(Income > 4000)

Income2 = table2 %>% filter(Is_Default == 1)

result = inner_join(Income1, Income2, by="ID")

# 第四问 (收入越低，员工待的时间越少)
ggplot(mergeResult, aes(x = Income, y = Years_at_Employer)) + geom_point()

# 第五问

# 第六问

# 第七问

# 第八问

# 第九问