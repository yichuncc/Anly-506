# Week 6 Practice
# Data transformation

install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

filter(flights, month == 1, day == 1) #subset observations based on selected values
jan1 <- filter(flights, month == 1, day == 1) #use == for floating point numbers

(dec25 <- filter(flights, month == 12, day == 25))

near(sqrt(2) ^ 2,  2)
filter(flights, month == 11 | month == 12) #flights in November or December

nov_dec <- filter(flights, month %in% c(11, 12)) #select every row where x is one of the values in y
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

NA > 5 #represents an unknown value so missing values are “contagious”
NA == NA
x <- NA
y <- NA
x == y
is.na(x) #determine if a value is missing

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)
#filter the missing values

arrange(flights, year, month, day) #filter values by naming orders

arrange(flights, desc(dep_delay))
df <- tibble(x = c(5, 2, NA))
arrange(df, x)

select(flights, year, month, day) #select columns by name specified
select(flights, year:day) #Select all columns between year and day
select(flights, -(year:day)) #Select all columns except those from year to day

rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything())


select(flights, contains("TIME"))


#create a narrower dataset 
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)
#refer to the column created just now
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

#only keep te new vairables
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
) # %/% (integer division) and %% (remainder)

#refer to leading or lagging values
(x <- 1:10)
lag(x)
lead(x)
cumsum(x) #accumulative additions
cummean(x) #accumulative means

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))


summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL") #delays increase with distance up to 750 miles

ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")


flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

#remove missing values
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))
#removee cancelled flights
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10) #flights with the highest average delay

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10) #a scatterplot of number of flights vs. average delay


