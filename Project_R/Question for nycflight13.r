## Ask 5 question about nycflights13
## load library
library(nycflights13)
library(dplyr)

## view data before asking question
View(flights)
data("flights")
data("airlines")

## Question 1: What are the top 5 destination from JFK in 2013 
flights %>%
  select(origin, dest) %>%
  filter (origin == "JFK") %>%
  group_by(dest) %>%
  summarise(total_count = n()) %>%
  arrange(desc(total_count)) %>%
  head(5)

## Question 2: How many flights were delayed and what is the average delay from each airport in NYC?
flights %>%
  select(origin, dep_delay) %>%
  filter (dep_delay != "NA" | dep_delay > 0) %>%
  group_by(origin) %>%
  summarise(total_delay_times = n(),
            average_delay_min = mean(dep_delay)) %>%
  arrange(desc(total_delay_times))

## Question 3: What percentage of flights from the top 5 carriers were delayed in 2013?
flights %>%
  select(carrier, dep_delay) %>%
  filter (dep_delay != "NA") %>%
  mutate (delay = if_else(dep_delay > 0, "delay", "no delay")) %>%
  group_by(carrier) %>%
  count(delay) %>%
  mutate(percent_delay = (n/sum(n))*100) %>%
  filter(delay == "delay") %>%
  arrange(desc(percent_delay)) %>%
  head(5) %>%
  select(carrier, percent_delay) %>%
  left_join(airlines) %>%
  select(carrier, name, percent_delay)

## Question 4: What is the average plane speed (miles per hour) for departures from each airport?
flights %>%
  select(origin, air_time, distance) %>%
  filter(air_time != "NA") %>%
  group_by(origin) %>%
  summarise(avg_plane_speed = sum(distance) / (sum(air_time)/60)) %>%
  arrange(desc(avg_plane_speed))

## Question 5: What are the top 5 carriers that have the most flights departing from NYC airports in 2013?
flights %>%
  select(carrier) %>%
  group_by(carrier) %>%
  count(carrier) %>%
  arrange(desc(n)) %>%
  head(5) %>%
  left_join(airlines) %>%
  select(carrier, name, n)
