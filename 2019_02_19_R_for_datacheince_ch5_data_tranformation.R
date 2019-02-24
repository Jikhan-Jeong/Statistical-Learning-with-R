## 2019_02_19_R_for_datacheince_ch5_data_tranformation
## reference: https://r4ds.had.co.nz/transform.html


getwd()
setwd("C:/rstudio/0_2019_spring/statistical learning/r_data_science")


library(nycflights13)
library(tidyverse)

flights

library(dplyr)
# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).

filter(flights, month ==1, day ==1)
jan1 <- filter(flights, month ==1, day ==1)
view(jan1)

filter(flights, month ==11 | month ==12)
nov_dec <- filter(flights, month %in% c(11,12))

x<- NA
x
is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x >1)
filter(df, is.na(x)| x>1)

## arrange = ordering

arrange(flights, day) # small to big
arrange(flights, desc(day)) # big to small

df <- tibble(x=c(5,2,NA))
arrange(df, x)

## select

select(flights, year, month, day)
select(flights, year:day)
select(flights, -year)

rename(flights, dayss = day)

select(flights, time_hour, everything())


## add new variables with mutate()

flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
                      )
mutate(flights_sml, gain = dep_delay - arr_delay, speed = distance / air_time * 60)
mutate(flights_sml, gain = dep_delay - arr_delay, hours = air_time * 60, gain_per_hour = gain/ hours)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100, # integer division
          minute = dep_time %% 100 # remainder
          )

(x <- 1:10)
lag(x)
lead(x)


x
cumsum(x)
cummean(x)

y <-c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))


## Group summaries with summarise()

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
by_day
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


## combining multiple operations with the pipe

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest, 
                   count = n(),
                   dist  = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
                   )
delay

delay <- filter(delay, count >20, dest != "HNL")


ggplot(data = delay, mapping = aes(x = dist, y = delay)) + geom_point(aes(size = count), alpha = 1/3) + geom_smooth(se = FALSE)


delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count =n(),
    dist = mean(distance, na.rm = TRUE),
    delay =mean(arr_delay, na.rm = TRUE)
  ) %>% filter(count >20, dest !="HNL")
delays


##  x %>% f(y) turns into f(x, y), and x %>% f(y) %>% g(z) turns into g(f(x, y), z)

flights %>%
  group_by(year, month, day) %>%
  summarise(mean= mean(dep_delay))

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

## counts

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(delay = mean(arr_delay)
  )

head(delays,3)

ggplot(data=delays, mapping = aes(x= delay)) + geom_freqpoly(binwidth = 10)


delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay, na.rm=TRUE),
    n=n()
  )

ggplot(data = delays, mapping = aes(x =n, y = delay)) + geom_point(alpha= 1/10)

delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x =n, y = delay)) +
  geom_point(alpha = 1/10)

library(Lahman)

batting <- as_tibble(Lahman::Batting)


batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

head(batters,1)


batters %>%
  filter(ab>100) %>%
  ggplot(mapping = aes(x=ab, y=ba)) +
  geom_point(alpha=1/10) +
  geom_smooth(se = FALSE) 
  
batters %>%
  arrange(desc(ba))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay>0])
  )

## 5.6.4 Useful summary functions
