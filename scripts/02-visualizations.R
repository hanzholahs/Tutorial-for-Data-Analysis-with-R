library(tidyverse)
library(scales)

class(datasets::ChickWeight)

chick <- tibble(datasets::ChickWeight)

# Tipe Data
# Qualitative:
#   - Categorical: gender, negara, kota
#   - Ordinal: Ranking, Besar-Kecil, low-med-hi
# Quantitative:
#   - Discrete: Umur, tahun, jumlah pil obat yang diminum
#   - Continous: Tinggi, Berat badan, waktu.


# scatterplot, lineplot, histogram, boxplot, barplots


# Scatterplot -------------------------------------------------------------

# Input: Dua variable numeric
# Tujuan: melihat relationship antar dua variable

?ggplot

ggplot(data = chick,
       mapping = aes(x = weight, y = Time)) +
  # campuran statistics, scales, geometric
  geom_point()

ggplot(data = chick,
       mapping = aes(x = weight, y = Time, colour = Diet)) +
  # campuran statistics, scales, geometric
  geom_point(mapping = NULL,
             data = NULL) +
  scale_x_continuous(labels = number_format(scale = 1e-3,
                                            suffix = "Kg")) +
  scale_y_continuous(labels = number_format(suffix = "Yr")) +
  scale_colour_viridis_d()

ggplot() +
  # satu layer dalam geom point terdiri dari data, mapping, stats
  geom_point(data = chick,
             mapping = aes(x = Time, y = weight, colour = Diet)) +
  scale_x_continuous(labels = number_format(suffix = "Yr")) +
  scale_y_continuous(labels = number_format(scale = 1e-3,
                                            suffix = "kg")) +
  scale_colour_viridis_d()
  



# Linechart ---------------------------------------------------------------

# input: x -> sequential, y = variabel yang jadi perhatian
# Tujuan: melihat perkembangan satu variable dari waktu ke waktu

chick_2 <- filter(chick, Diet == 2, Chick == 21) 

ggplot(data = chick_2,
       mapping = aes(x = Time, y = weight)) +
  geom_line(colour = "blue")

ggplot(data = chick_2) +
  geom_line(mapping = aes(x = Time, y = weight),
            colour = "blue")

chick_3 <- filter(chick, Diet == 3) 

ggplot(data = chick_3,
       mapping = aes(x = Time, y = weight, group = Chick)) +
  geom_line()

ggplot(data = chick_3,
       mapping = aes(x = Time, y = weight, colour = Chick)) +
  geom_line()



# Histogram ---------------------------------------------------------------

# Input: satu variable numeric
# Tujuan: ingin melihat distribusi (aka nilai umum/centrality,
#   variability, modality, dll)

ggplot(data = chick,
       mapping = aes(x = Time)) +
  geom_histogram()

ggplot(data = chick,
       mapping = aes(x = weight)) +
  geom_histogram() +
  scale_x_log10()

chick_log <- mutate(chick, log_weight = log10(weight))

ggplot(data = chick_log,
       mapping = aes(x = log_weight)) +
  geom_histogram()


# Boxplot -----------------------------------------------------------------

# Input: satu variable numeric
# Tujuan: ingin melihat distribusi (aka nilai umum/centrality,
#   variability, outlier, dll)

ggplot(data = chick,
       mapping = aes(x = weight)) +
  geom_boxplot()



# Barplot -----------------------------------------------------------------

chick
ggplot(data = chick,
       mapping = aes(x = Diet)) +
  geom_bar()


chick_avg_weight <- 
  chick |> 
  group_by(Diet) |> 
  summarise(avg_weight = sum(weight))

ggplot(data = chick_avg_weight,
       mapping = aes(x = Diet, y = avg_weight)) +
  geom_col()



?geom_col
