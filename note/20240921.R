### 20240921上课记录代码
x<- 3*4
seq(1,10)
seq(1,10,2)
x <- "hello world"
y<-seq(1,10,length.out=2)
(y<-seq(1,10,length.out=2))

typeof(letters)
typeof(1:10)
typeof(1)
typeof(1L)
x <-list(1:10)
length(x)
1:10
typeof(list("a","b",1:10))
1:10 %% 3
1:10 %% 3 == 0
c(1:10 %% 3 == 0)
1==1L
(x<-sqrt(2)^2-2)
c(-1,0,1)/0
###安装引入
install.packages("tidyverse")
library(tidyverse)
###散点图
mpg
view(mpg)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
row(mpg)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size=drv, shape=drv))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class, shape = drv, alpha=cyl))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),color="blue")
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy,color="blue"))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy,color=("blue")))
###分面图
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(class ~ drv ~ cyl)
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy,linetype = drv,colour = drv))
###组合图 散点+平滑曲线
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy,linetype = drv,colour = drv))+
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy,colour = drv)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se=FALSE)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(x = displ, y = hwy,linetype = drv),se=FALSE)

###条形图
view(diamonds)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = carat))
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut,colour = cut))
ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut,fill = cut))

demo <- tribble(
  ~a, ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)
ggplot(data = demo) +
  geom_bar(
    mapping = aes(x = a, y = b), stat = "identity"
  )

###条状图比例
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop..,group = 1)
  )
###统计摘要 最小值、最大值、中位数
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin=min,
    fun.ymax=max,
    fun.y=median
  )
ggplot(data = diamonds) +geom_col(mapping = aes(x = cut, y =depth ))
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = color),position = "dodge"
  )
###抖动，显示重叠点
ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"
  )
ggplot(data = mpg,mapping = aes(x = cty, y = hwy)) +
  geom_point()+geom_jitter()
###不同大小显示重叠点
ggplot(data = mpg,mapping = aes(x = cty, y = hwy)) +
  geom_point()+geom_count()

ggplot(data = diamonds) +
  geom_smooth(
    mapping = aes(x = price, y = carat)
  )+
  facet_wrap(~ color)
ggplot(data = diamonds,mapping = aes(x = carat, y = price, color=color))+
  geom_point()+
  geom_smooth(color="blue",se=FALSE)+
  facet_wrap(~ color)

###使用dplyr进行数据转换
install.packages("nycflights13")
library(nycflights13)
library(dplyr)
view(flights)
###缺失值
is.na(flights)
###按值筛选观测（filter()）。
filter(flights,month==1,day==1)  # 多条件并列为且,可用&表示
filter(flights,month==1 & day==1)  # & 表示and
filter(flights,month==1 | daSy==1)  # | 表示or
filter(flights, xor(month==11,month==12))
filter(flights, !(month %in% c(11,12)))
###对行进行重新排序（arrange()）。
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))#使用 desc() 可以按列进行降序排序
df <- tibble(x = c(5, 2, NA))
arrange(df, x)#缺失值总是排在最后
arrange(df, !is.na(x))#缺失值排在最前
###按名称选取变量（select()）。
select(flights, year, month, day)
select(flights, year:day)# 选择“year”和“day”之间的所有列（包括“year”和“day”）
select(flights, -(year:day))# 选择不在“year”和“day”之间的所有列（不包括“year”和“day”）
rename(flights, tail_num = tailnum)#重命名
select(rename(flights, tail_num = tailnum), tail_num)
select(flights, time_hour, air_time, everything())#变量移到数据框开头
###使用现有变量的函数创建新变量（mutate()）。
flights_sml <- select(flights,year:day,ends_with("delay"),distance,air_time)
mutate(flights_sml,gain = arr_delay - dep_delay,speed = distance / air_time * 60)
transmute(flights_sml,gain = arr_delay - dep_delay,speed = distance / air_time * 60)
flights_sml <- mutate(flights,hour,gain = arr_delay - dep_delay,speed = distance / air_time * 60,gain_per_hour=gain/hour)
ggplot(data=flights_sml)+
  geom_smooth(mapping = aes(x=gain_per_hour,y=speed))
ggplot(data=flights_sml)+
  geom_point(mapping = aes(x=hour,y=gain_per_hour))+
  geom_smooth(mapping = aes(x=hour,y=gain_per_hour))
10%/%5
10%%5
###将多个值总结为一个摘要统计量（summarize()）。
summarize(flights, delay = mean(dep_delay, na.rm = TRUE))
summarize(flights, delay = mean(dep_delay))

#分组聚合，分析数据
by_day <- group_by(flights,year,month,day)
summarise(by_day, delay=mean(dep_delay,na.rm=TRUE))


by_dest <- group_by(flights, dest)
(delay <- summarize(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
))
(delay <- filter(delay, count > 20, dest != "HNL"))
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
# 管道
delay<- flights %>%
  group_by(dest) %>%
  summarise(
    count=n(),
    dist =mean(distance,na.rm=TRUE),
    delay = mean(arr_delay, na.rm =TRUE)
  )%>%
  filter(count>20,dest!="HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count,color=count) ) +
  geom_smooth(se = FALSE)
