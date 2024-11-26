install.packages("tidyverse")
install.packages("nycflights13")

library("tidyverse")
library("nycflights13")

# 第一问
flight_arr2hr = flights %>% filter(arr_delay > 120)

# 第二问
top10_dest  = flight_arr2hr %>% group_by(dest) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head(10)

# 第三问
newWeather = weather %>% select(origin:hour, humid, wind_speed)

commonCol = intersect(names(newWeather), names(flight_arr2hr))

flight_weather = left_join(newWeather, flight_arr2hr, by = commonCol)

# 第四问
ggplot(flight_weather, aes(x = wind_speed, y = dep_delay)) + geom_point() + xlim(0, 50) + geom_smooth(se = FALSE) + facet_wrap(~ origin)

# 第五问
flights %>% filter(is.na(dep_time)) %>% group_by(carrier) %>% summarise(count = n())

# 第六问
result = flights %>% group_by(carrier, dest) %>% summarise(count = n())