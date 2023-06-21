# load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(RColorBrewer)

# import ystradgynlais daily rainfall
ystrad_rain <- read.csv("data/ystradgynlais_day_rain.csv") %>%
  mutate(date.time = dmy_hms(date.time),
         daily = as_date(date.time),
         roll.5.rain = rollapplyr(rain.mm, 5, sum, align = "right", fill = NA, partial = TRUE)) %>%
  select(-c("date.time")) # remove unwanted columns

# join ystrad rain with queens
ystrad.rain.join <- left_join(ystrad_rain, queens_swansea, by = "daily")

# plot ystrad rain level vs bacti level
p.ystrad.rain <- ystrad.rain.join %>%
  ggplot(aes(rain.mm, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000, color = "red") +
  geom_hline(yintercept = 4600, color = "orange") +
  geom_hline(yintercept = 230, color = "green")

# plot ystrad rain roll5 vs bacti level
p.ystrad.rain.roll <- ystrad.rain.join %>%
  ggplot(aes(roll.5.rain, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000, color = "red") +
  geom_hline(yintercept = 4600, color = "orange") +
  geom_hline(yintercept = 230, color = "green")

# get a median for ystrad rain
mu.rain <- ystrad_rain %>%
  summarise(rain.median = median(rain.mm))

# plot density histogram of daily rainfall
rain.hist <- ystrad_rain %>%
  ggplot(aes(x=rain.mm, color='#525252', fill='#525252', position="dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 2.0, alpha=0.4, position="identity") +
  geom_density(alpha=0.2) +
  geom_vline(data = mu.rain, aes(xintercept = rain.median, color = "black"), linetype = "dashed") +
  scale_color_manual(values=c('#525252', '#525252'), guide = "none")  +
  scale_fill_manual(values=c('#969696'), guide = "none") +
  labs(x = "Ystradgynlais Daily Rainfall (mm)", y =  "Frequency") +
  theme_classic()

# Open a pdf file
pdf("plots/rain_hist.pdf", height = 3) 

# 2. Create a plot
rain.hist

# Close the pdf file
dev.off()

# quantiles of interest
ystrad_rain %>%
  summarise(rain.quant = quantile(rain.mm, c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE))

rain.ecdf <- ecdf(ystrad_rain$rain.mm)
rain.ecdf(19)
rain.cumulative.ecdf <- ecdf(ystrad_rain$roll.5.rain)

# function to extract period to print - given the sample date
rain_plot_f1 <- function(a){ # a sample date
  ystrad_rain %>%
    filter(daily > ymd(a) -28 & daily < ymd(a) + 3) %>%
    ggplot(aes(daily, rain.mm)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 50)) +
    labs(x = NULL, y = "Ystradgynlais Rainfall (mm)") +
    geom_vline(xintercept = ymd(a), color = "black", linetype = "dashed") +
    theme_minimal() +
    theme(axis.text.x=element_blank())
}

# plot rain level 2021-10-12
rain_21_10_12 <- rain_plot_f1("2021-10-12")

# Open a pdf file
pdf("plots/rain_21_10_12.pdf", height = 2)
rain_21_10_12
dev.off()

# plot rain level 2022-01-11
rain_22_01_11 <- rain_plot_f1("2022-01-11")

# Open a pdf file
pdf("plots/rain_22_01_11.pdf", height = 2)
rain_22_01_11
dev.off()

# plot rain level 2019-09-02
rain_19_09_02 <- rain_plot_f1("2019-09-02")

# Open a pdf file
pdf("plots/rain_19_09_02.pdf", height = 2)
rain_19_09_02
dev.off()

# plot rain level 2022-06-08
rain_22_06_08 <- rain_plot_f1("2022-06-08")

# Open a pdf file
pdf("plots/rain_22_06_08.pdf", height = 2)
rain_22_06_08
dev.off()

# plot rain level 2020-10-14
rain_20_10_14 <- rain_plot_f1("2020-10-14")

# Open a pdf file
pdf("plots/rain_20_10_14.pdf", height = 2)
rain_20_10_14
dev.off()

# plot rain 2022-07-05
rain_22_07_05 <- rain_plot_f1("2022-07-05")

# Open a pdf file
pdf("plots/rain_22_07_05.pdf", height = 2)
rain_22_07_05
dev.off()

# plot rain 2017-05-15
rain_17_05_15 <- rain_plot_f1("2017-05-15")

# Open a pdf file
pdf("plots/rain_17_05_15.pdf", height = 2)
rain_17_05_15
dev.off()

# plot rain 2021-12-13
rain_21_12_13 <- rain_plot_f1("2021-12-13")

# Open a pdf file
pdf("plots/rain_21_12_13.pdf", height = 2)
rain_21_12_13
dev.off()

# plot rain 2021-07-12
rain_21_07_12 <- rain_plot_f1("2021-07-12")

# Open a pdf file
pdf("plots/rain_21_07_12.pdf", height = 2)
rain_21_07_12
dev.off()

# plot rain 2021-08-10
rain_21_08_10 <- rain_plot_f1("2021-08-10")

# Open a pdf file
pdf("plots/rain_21_08_10.pdf", height = 2)
rain_21_08_10
dev.off()

# plot rain 2021-09-06
rain_21_09_06 <- rain_plot_f1("2021-09-06")

# Open a pdf file
pdf("plots/rain_21_09_06.pdf", height = 2)
rain_21_09_06
dev.off()


