library(nycflights13)
library(tidyverse)

delay<- flights %>%
  group_by(dest) %>%
  summarise(
    count=n(),
    dist=mean(distance,na.rm = TRUE),
    delay=mean(arr_delay,na.rm = TRUE)
  )%>%
  filter(count>20, dest!="HNL")
delay
  
##alpha 放在内部只显示效果，赋值不生效，和color类似
ggplot(data = delay)+
  geom_smooth(mapping = aes(x=dist,y=delay),se=FALSE)+
  geom_point(mapping = aes(x=dist,y=delay,size = count),alpha=1/3)
ggplot(data = delay)+
  geom_smooth(mapping = aes(x=dist,y=delay),se=FALSE)+
  geom_point(mapping = aes(x=dist,y=delay,size = count,alpha=1/3))

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

###范围筛选
count_delay<- delay %>%
  mutate(  
    value_group = case_when(  
      count <= 200 ~ "Low",  
      count > 200 & count <= 1000 ~ "Medium",  
      count > 1000 ~ "High"  
    )  
  )  %>%
  group_by(value_group)%>%
  summarise(
    count=n()
  )
count_delay

na_num <- flights %>%
  group_by(dest) %>%
  summarise(
    num=sum(is.na(dep_delay)) 
  )%>%
filter(num>300)
na_num <- arrange(na_num,desc(num))
view(na_num)
#hist() 函数绘制直方图
hist(na_num$num) 
ggplot(data=na_num)+
  geom_point(mapping = aes(x=dest,y=num))
ggplot(data=na_num)+
  geom_bar(mapping = aes(x=dest))

not_cancelled <- flights %>%
   filter(!is.na(dep_delay),!is.na(arr_delay))
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(mean=mean(dep_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )
ggplot(data = delays, mapping = aes(x = delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay,na.rm = TRUE),
    n=n()
  )
ggplot(data=delays,mapping = aes(x=n,y=delay))+
  geom_point(alpha=1/3)

not_cancelled <- flights %>%
  filter(!is.na(dep_delay),!is.na(arr_delay))
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    avg_delay1=mean(arr_delay),
    avg_delay2=mean(arr_delay[arr_delay>0])
  )
#均方误差（又称标准误差，standard deviation，sd）是分散程度的标准度量方式。四分
# 位距 IQR() 和绝对中位差 mad(x) 基本等价，更适合有离群点的情况
not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    distance_sd = sd(distance),
    first = min(dep_time),
    last = max(dep_time),
    #quantile(x, 0.25) 会找出 x 中按从小到大顺序大于前25% 而小于后 75% 的值
    first_25=quantile(dep_time,0.25),
    first_dep=first(dep_time),
    last_dep=last(dep_time)
  )
view(flights)
flights %>%
  group_by(carrier) %>%
  summarise(
    #  distance_mean =min(distance)
    #  distance_mean =max(distance)
    #  distance_mean =mean(distance)
    count=n(),
    distance_sum =sum(distance),
    ave_distance = distance_sum/count
  )%>%
  arrange(desc(count))

#如果想要计算出非缺失值的数量，可以使用 sum(!is.na(x))。要想计算出唯一值的数量，可以使用 n_distinct(x)
not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))
#两次聚合,筛选结果
not_cancelled %>%
  group_by(dest,carrier)%>%
  summarise(
    count=n()
  ) %>%
  group_by(dest)%>%
  summarise(
    count=n()
  )%>%
  arrange(desc(count))

not_cancelled %>%
  group_by(dest,carrier)%>%
  summarise()%>%
  group_by(dest)%>%
  count()%>%
  arrange(-n)

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distinct = length(unique(carrier))) %>% 
  arrange(desc(distinct))

#多个变量分组
daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))
# 如果想要取消分组，并回到未分组的数据继续操作，那么可以使用 ungroup() 函数：
daily %>%
  ungroup() %>% # 不再按日期分组
  summarize(flights = n()) # 所有航班

flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# 根据是否热门分组，计算arr_delay的平均值
popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
mean(popular_dests$arr_delay,na.rm = TRUE)
unpopular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() <= 365)
mean(unpopular_dests$arr_delay,na.rm = TRUE)

# 根据是否热门分组，计算arr_delay的平均值
flights %>% 
  group_by(dest) %>% 
  mutate(gt_365 = if_else(n() > 365, 1, 0)) %>% 
  group_by(gt_365) %>% 
  summarise(mean(arr_delay, na.rm = TRUE))

#airlines：可以根据航空公司的缩写码查到公司全名。
#airports：给出了每个机场的信息，通过 faa 机场编码进行标识
#planes：给出了每架飞机的信息，通过 tailnum 进行标识。
#weather：给出了纽约机场每小时的天气状况。
planes %>%
  count(tailnum) %>%
  filter(n>1)

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
#左连接，右链接right_join，内连接inner_join,全连接 full_join
flights2 %>%
  #select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")
#
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3",
  4, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  5, "y5"
)
left_join(x, y, by = "key")
right_join(x, y, by = "key")
inner_join(x, y, by = "key")
full_join(x, y, by = "key")

flights2 %>%
  left_join(airports, c("dest" = "faa"))

#inner_join(x, y) merge(x, y)
#left_join(x, y) merge(x, y, all.x = TRUE)
#right_join(x, y) merge(x, y, all.y = TRUE)
#full_join(x, y) merge(x, y, all.x = TRUE, all.y = TRUE)

# semi_join(x, y)：保留 x 表中与 y 表中的观测相匹配的所有观测。
# anti_join(x, y)：丢弃 x 表中与 y 表中的观测相匹配的所有观测。

#最受欢迎的前 10 个目的地
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest
#飞往这些目的地的所有航班
flights %>%
  filter(dest %in% top_dest$dest)
flights %>%
  semi_join(top_dest)
right_join(flights, top_dest)

#函数
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df
df$a <- (df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) /
  (max(df$b, na.rm = TRUE) - min(df$b, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) /
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) /
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))
df
range(c(1,3,10,4))

rescale01 <- function(x){
  rng<- range(x,na.rm = TRUE)
  (x-rng[1])/(rng[2]-rng[1])
}
rescale01(df$b)

rescale02 <- function(x,n){
 # rng<- x[1]*x[1]+x[2]*x[2]+x[3]*x[3]
  sum(x^n)
}
rescale02(c(1,2,3,5,6,7),2)

#循环
a=1
for(i in seq(1,10,by=2)){
  a=a*i
}
print(a)

rescale03<-function(x){
  a=0
  for(i in x){
    a=a+i
  }
  print(a)
}
cf<-function(x){
  a=0
  for(i in 1:length(x)){
    a=a+x[i]
  }
  print(a)
}
cf(c(1,2,4,5))




