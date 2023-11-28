library(tidyverse)

library(ggplot2) # visualisasi
library(dplyr) # data manipulation
library(readr) # data import
library(lubridate) # manipulasi data tanggal
library(forcats) # manipulasi data kategori
library(stringr) # manipulasi data string

library(scales) # tampilan angka

scales::number(432425345, prefix = "Rp", scale = 1e-6,
               suffix = "Jt")

# Tipe data di R ----------------------------------------------------------

# data primitif di R

typeof(4234) # double / float
typeof(13L) # integer

typeof("hello") # character / string
typeof(TRUE) # logical / boolean


# data primitif direpresentasikan dengan array

length(23)
length(c(1,2,3,4))
length("hello") # 1
# python: len("hello") = 5

c("hello", "world", "!!!")
c(TRUE, FALSE)


# konversi data

as.character(123)
as.logical(1)
as.logical(0)
as.double(433L)


# array hanya bisa menyimpan satu tipe data
#   jika lebih, maka type promotion
#   urutannya: logical -> integer -> double -> character

typeof(c(TRUE, 1)) # logical ke double
typeof(c(TRUE, 1L)) # logical ke integer
typeof(c(1, "1", 1., 1L, TRUE)) # all to character


# list

list(1, "1", 1.0, 1L, TRUE)
list(c(1,1,1,1),
     c("1", "2", "3"),
     c(1.0, 1.5, 2.0),
     c(TRUE, FALSE, TRUE))


# variables

x = c(1,2,3,4) # variable assignment
y <- 3:9 # ~ c(3,4,5,6,7,8,9)
z <- seq(3, 9, 1) # ~ c(3,4,5,6,7,8,9)


# vectorized operations, sama seperti `numpy`
#   metodenya namanya "broadcasting"

x + 10
# c(x[1] + 10, x[2] + 10, x[3] + 10, x[4] + 10)

sqrt(x) 
# c(sqrt(x[1]), sqrt(x[2]), sqrt(x[3]), sqrt(x[4]))

y == z
# c(y[1] == z[1], y[2] == z[2], y[3] == z[3], y[4] == z[4])

x + c(10, 1000)
# x[1] + 10, x[2] + 1000, x3 + 10, x4 + 1000

y + c(1,2,3)
# pikir sendiri

# list(1,2,3,4) + 5 # UNCOMMENT THIS TO SEE THE ERROR
# error, list tidak seperti array/vector


# fungsi class vs typeof

class(34)
typeof(34)

class("hello")
typeof("hello")

class(c(2,4,3))
typeof(c(2,4,3))

class(list(1,2,3))
typeof(list(1,2,3))

class(mtcars) # lebih high-level; representasi/abstraksi
typeof(mtcars) # lebih primitif, apa ada di mesinnya

my_date <- as.Date("2023-03-12")

# Perbandingan dengan di python
# - kalau class itu type object
# - kalau typeof itu untuk type data primitif nya
# Class Student():
#   def __init__(self, id, name):
#     self.id = id
#     self.name = name
# 
# udin = Student(id = 123231, "Udin")
# udin ini kelas
# udin.id ini primitif
# udinlname ini primitif juga



# named vector dan named list

penjualan <- c("Jan" = 10, "Feb" = 12, "Mar" = 2)
penjualan

distribusi <- list("Jan" = 10, "Feb" = 12, "Mar" = 2)
distribusi


# indexing list:
#   kalau cuma SINGLE SQUARE BRACKET, outputnya jg list
#   kalau mau dalemnya, pakai DOUBLE SQUARE BRACKETS

distribusi

typeof(distribusi[1])

typeof(distribusi[[1]])


# factors

factor(c(1,2,3,3,4,3,2,2,4,4,3,1,2))

genders <- factor(c("female", "male", "female", "female",
                    "female", "male", "male"))


class(genders)
typeof(genders)

levels(genders)

# kalo di python, factor bisa direpresentasikan sbb.
# ----
# values = [1,1,1,2,1,2]
# labels = {1: "female", 2: "male"}
# data = [labels[i] for i in values]






# Dataframe ---------------------------------------------------------------

# Jenis list khusus

my_data <- list(x = c(1,2,3,4),
                y = c(7,8,9,10))
my_data

as.data.frame(my_data)

# native R data structures:
# - array / vector
# - matrix -> belajar sendiri
# - list
# - data.frame
# - tibble::tibble

tibble(iris)


# attributes

names(my_data)

my_data_as_df <- as.data.frame(my_data)
names(my_data_as_df)
colnames(my_data_as_df)
rownames(my_data_as_df)

colnames(my_data) # tidak bisa untuk list
rownames(my_data) # tidak bisa untuk list



