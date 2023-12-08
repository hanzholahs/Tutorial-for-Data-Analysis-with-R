library(nycflights13)
library(tidyverse)

?nycflights13::flights

# tibble or dataframe

flights


nrow(flights)
ncol(flights)

colnames(flights)
rownames(flights)


# summary(), str(), glimpse()  ----

# function summary untuk mendapatkan basic statistics
summary(flights)
summary(1:100)
summary(c("a", "b", "d"))

# jenis data lainnya di luar primitve data types
flights$time_hour[1:5]
str(flights$time_hour)
str(factor(month.abb)[1])

fivenum(x = 1:100)
summary(object = 1:100)
?fivenum

# str & glimpse

?str # -> lebih fleksible, kurang rapi, pakai class
str(flights)
str(list(a = 1:100, b = 5:10, c = month.abb))

?glimpse # -> lebih rapi, pakai typeof (lebih low level)
glimpse(list(a = 1:100, b = 5:10, c = month.abb))
glimpse(flights)

class(1.23)
typeof(3.42)




# arrange() ----

arrange(flights, dep_time) #ascending order: 0-9, A-Z
arrange(flights, dep_delay) 
arrange(flights, carrier) #ex: g1 vs 1g -> 1g, g1

arrange(flights, desc(dep_time)) #descending order: Z-A, 9-0
arrange(flights, desc(dep_delay))
arrange(flights, desc(carrier))


# pipe operator - tidyverse: %>% 

flights %>%
  arrange(dep_time)

flights %>%
  arrange(dep_time, .data = .)

flights %>%
  arrange(dep_time, .data = select(., dep_time, year))



# pipe operator - base r: |>

flights |> 
  arrange(dep_time)

flights  |> 
  arrange(dep_time, .data = _)

flights |> 
  arrange(dep_time, .data = select(_, dep_time, year))




# select() ----
# mengambil kolom dari df/tibble

# berdasarkan nama kolom
flights |> 
  select(year, month, day, flight, tailnum, origin, dest, dep_time)

# berdasarkan fungsi
flights |> 
  select_if(is.numeric)

flights |> 
  select_if(is.character)

# berdasarkan indeks kolom
flights |> 
  select_at(c(4,7,2,1))



# filter() & slice() ----

# slice untuk mengambil rows berdasarkan index

flights |> 
  slice(1,2,3,100)

flights[c(1,2,3,100), ]

flights |> 
  select_at(c(1,2,3,4)) |> 
  slice(5:10)

flights |> 
  slice(5:10) |> 
  select_at(c(1,2,3,4))

flights[5:10, 1:4]


# filter untuk mengambil rows dengan conditional

flights |> 
  filter(dep_delay < -10, dep_delay > -20)

flights |> 
  filter(dep_delay < -10 & dep_delay > -20)

flights |> 
  filter(dep_delay < -40 | dep_delay > 15 * 60)

flights |> 
  filter(dep_delay > 40, arr_delay < -10)

TRUE & FALSE # and operator: &
TRUE | FALSE # or operator: |




# mutate()

flights_small <- 
  flights |> 
  sample_n(10000) |> 
  select(time_hour, flight, dep_time, arr_time, dep_delay, arr_delay)

# akan membuat kolom baru jika nama kolom tidak ada di aslinya
# akan menggantikan kolom lama jika nama kolom sudah ada
flights_small |> 
  mutate(dep_time_hour = dep_time / 60,
         dep_time = dep_time / 60)

# proses mutasi bergantung pada urutan di fungsi
flights_small |> 
  mutate(dep_time = dep_time / 60,
         dep_time_hour = dep_time / 60)

# summarize()

# mutate -> jenis fungsinya: input berupa vector, output berupa vector

sqrt(1:10)
log10(1:10)


# summarise -> jenis fungsinya: input berupa vector, output berupa scalar

max(1:100)
median(1:43)

median(c(1,2,3,1,3,NA))
median(c(1,2,3,1,3,NA), na.rm = TRUE)



# fungsi summarise

flights_small |> 
  summarise(max_dep_time = max(dep_time, na.rm = TRUE))

flights_small |> 
  summarise(min_arr_delay = min(arr_delay, na.rm = TRUE),
            max_arr_delay = max(arr_delay, na.rm = TRUE),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE))



# group_by()

mtcars <- tibble(mtcars)

unique(mtcars$cyl)

mtcars |> 
  summarise(avg_hp = mean(hp))

mtcars |> 
  group_by(cyl) |> 
  summarise(avg_hp = mean(hp))

mtcars |> 
  group_by(cyl, am) |> 
  summarise(avg_hp = mean(hp))

mtcars |> 
  group_by(am, cyl) |> 
  summarise(avg_hp = mean(hp))

mtcars |> 
  mutate(am = if_else(am == 1, "Manual", "Automatic"))

# mutate akan menduplikasi output fungsi skalar untuk mengisi seluruh baris
mtcars |> 
  mutate(avg_hp = mean(hp)) 

# 
mtcars |> 
  group_by(cyl) |> 
  summarise(avg_hp = mean(hp)) |> 
  arrange(cyl)

mtcars |> 
  group_by(cyl) |> 
  mutate(avg_hp = mean(hp)) |> 
  arrange(cyl)


mtcars |> 
  group_by(cyl) |> 
  mutate(avg_hp = mean(hp),
         deviasi_hp = hp - avg_hp) |> 
  arrange(cyl) |> 
  print(n = Inf)

mtcars |> 
  group_by(cyl) |> 
  mutate(avg_hp = mean(hp),
         deviasi_hp = hp - avg_hp) |> 
  arrange(cyl) |> 
  View()



# variable bawaan R

pi 
month.name
month.abb

# Di R ada beberapa nilai khusus

NA # merepresentasikan data yang hilang
Inf # merepresentasikan nilai infinity
NULL # merepresentasikan ketiadaan data


# Error:
#   mtcars |> 
#     group_by(cyl) %>% 
#     avg_hp = mean(hp)



# join() it with another data frame by matching along a “key” variable. In other words, merge these two data frames together.

inner_join
left_join
right_join
full_join


flights_small <-
  flights |> 
  slice(1:100)

flights_small |> 
  inner_join(airlines, by = "carrier") |> 
  select(time_hour, flight, carrier, name)



flights_small |> 
  inner_join(airports, by = c(origin == faa, dest == faa))

airlines





