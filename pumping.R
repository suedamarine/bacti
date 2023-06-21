# load libraries
library(tidyverse)
library(zoo)
library(openxlsx)
library(lubridate)
library(scales)

# import pumping data
pumping <- read.csv("data/pumping.csv") %>%
  mutate(daily = ymd(daily),
         roll.5.volume = rollapplyr(volume, 5, sum, align = "right", fill = NA, partial = TRUE))

volume.ecdf <- ecdf(pumping$volume)

# plot of pumping regime where data available
pumping.p <- pumping %>%
  ggplot(aes(daily, volume)) +
  geom_line()

# Open a pdf file
pdf("plots/pumping_p.pdf") 

# 2. Create a plot
pumping.p

# Close the pdf file
dev.off()

# select top 10 bacti levels
slice_pumping <-  pumping %>%
  slice_max(volume, n = 10)

skim_combined <- combined %>%
  skim(max.tide.height, pontardawe.height, rain.mm, bacti_level, volume)

# try opening excel and filling merged cells
pumping_2 <- read.xlsx("data/pumping_2.xlsx", fillMergedCells = TRUE, colNames = TRUE) %>%
  mutate(daily = as.Date(Date, origin = "1899-12-30"),
         daily = ymd(daily),
         daily = ymd(daily) - years(1),
         pump_hours = ifelse(is.na(pump_hours), 0, pump_hours),
         pump_hours = pump_hours*24) %>%
  drop_na(pump_hours)

# try opening excel and filling merged cells
pumping_3 <- read.xlsx("data/pumping_3.xlsx", fillMergedCells = TRUE, colNames = TRUE) %>%
  mutate(daily = as.Date(Date, origin = "1899-12-30"),
         daily = ymd(daily),
         pump_hours = ifelse(is.na(pump_hours), 0, pump_hours),
         pump_hours = pump_hours*24) %>%
  drop_na(pump_hours)


pumping_2022 <- read.csv("data/pumping_2022.csv") %>%
  mutate(daily = ymd(daily))

#bind rows
pumping_combined <- bind_rows(pumping_2, pumping_2022, pumping_3) %>%
  select(-c("Date"))



pumping_group <- pumping_combined %>%
  group_by(daily) %>%
  summarise(total_pump_hours = sum(pump_hours)) %>%
  mutate(volume = total_pump_hours * 20736,
         roll.5.volume = rollapplyr(volume, 5, sum, align = "right", fill = NA, partial = TRUE),
         roll.pump.hours = rollapplyr(total_pump_hours, 5, sum, align = "right", fill = NA, partial = TRUE)) %>%
  complete(daily = seq.Date(min(daily), max(daily), by = "day"))

volume.ecdf <- ecdf(pumping_group$volume)
volume.5.ecdf <- ecdf(pumping_group$roll.5.volume)
pump.hours5.ecdf <- ecdf(pumping_group$roll.pump.hours)

# plot points individually - starting with 2021-10-12
pumping_21_10_12 <- pumping_group %>%
  filter(daily > "2021-09-14" & daily < "2021-10-15") %>%
  ggplot(aes(daily, volume)) +
  geom_line() +
  labs(x = "Date", y = expression(paste("Daily Pumping Volume ", m^3))) +
  geom_vline(xintercept = ymd("2021-10-12"), color = "black", linetype = "dashed") +
  theme_minimal()

# Open a pdf file
pdf("plots/pumping_21_10_12.pdf", height = 3)
pumping_21_10_12
dev.off()

# get a median for ystrad rain
mu.pumping <- pumping_group %>%
  summarise(pumping.median = median(total_pump_hours))

pump.hist <- pumping_group %>%
  ggplot(aes(x=total_pump_hours, color='#525252', fill='#969696', position="dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 0.5, alpha=0.4, position="identity") +
  geom_density(alpha=0.2) +
  geom_vline(data = mu.pumping, aes(xintercept = pumping.median), linetype = "dashed") +
  scale_color_manual(values=c('#525252', '#525252'), guide = "none")  +
  scale_fill_manual(values=c('#969696'), guide = "none") +
  labs(x = "Swansea Dock Daily Pumping Hours", y =  "Frequency") +
  theme_classic()

# Open a pdf file
pdf("plots/pump_hist.pdf", height = 3)
pump.hist
dev.off()

# function to extract period to print - given the sample date
pumping_plot_f1 <- function(a){ # a sample date
  pumping_group %>%
    filter(daily > ymd(a) -28 & daily < ymd(a) + 3) %>%
    ggplot(aes(daily, total_pump_hours)) +
    geom_point(aes(size = volume), shape = 1, show.legend = FALSE) +
    scale_size_continuous(range = c(3, 7)) +
    scale_y_continuous(labels = comma, limits = c(0, 12)) +
    labs(x = NULL, y = "Dock Pumping Hours") +
    geom_vline(xintercept = ymd(a), color = "black", linetype = "dashed") +
    theme_minimal()  
}

# plot pumping volume 2021-10-12
pumping_21_10_12 <- pumping_plot_f1("2021-10-12")

# Open a pdf file
pdf("plots/pumping_21_10_12.pdf", height = 2)
pumping_21_10_12
dev.off()

# plot pumping volume 2022-01-11
pumping_22_01_11 <- pumping_plot_f1("2022-01-11")

# Open a pdf file
pdf("plots/pumping_22_01_11.pdf", height = 2)
pumping_22_01_11
dev.off()

# plot pumping volume 2019-09-02
pumping_19_09_02 <- pumping_plot_f1("2019-09-02")

# Open a pdf file
pdf("plots/pumping_19_09_02.pdf", height = 2)
pumping_19_09_02
dev.off()

# plot pumping volume 2022-06-08
pumping_22_06_08 <- pumping_plot_f1("2022-06-08")

# Open a pdf file
pdf("plots/pumping_22_06_08.pdf", height = 2)
pumping_22_06_08
dev.off()

# plot pumping volume 2020-10-14
pumping_20_10_14 <- pumping_plot_f1("2020-10-14")

# Open a pdf file
pdf("plots/pumping_20_10_14.pdf", height = 2)
pumping_20_10_14
dev.off()

# plot pumping 2022-07-05
pumping_22_07_05 <- pumping_plot_f1("2022-07-05")

# Open a pdf file
pdf("plots/pumping_22_07_05.pdf", height = 2)
pumping_22_07_05
dev.off()

# plot pumping 2017-05-15
pumping_17_05_15 <- pumping_plot_f1("2017-05-15")

# Open a pdf file
pdf("plots/pumping_17_05_15.pdf", height = 2)
pumping_17_05_15
dev.off()

# plot pumping 2021-12-13
pumping_21_12_13 <- pumping_plot_f1("2021-12-13")

# Open a pdf file
pdf("plots/pumping_21_12_13.pdf", height = 2)
pumping_21_12_13
dev.off()

# plot pumping 2021-07-12
pumping_21_07_12 <- pumping_plot_f1("2021-07-12")

# Open a pdf file
pdf("plots/pumping_21_07_12.pdf", height = 2)
pumping_21_07_12
dev.off()

# plot pumping 2021-08-10
pumping_21_08_10 <- pumping_plot_f1("2021-08-10")

# Open a pdf file
pdf("plots/pumping_21_08_10.pdf", height = 2)
pumping_21_08_10
dev.off()

# plot pumping 2021-09-06
pumping_21_09_06 <- pumping_plot_f1("2021-09-06")

# Open a pdf file
pdf("plots/pumping_21_09_06.pdf", height = 2)
pumping_21_09_06
dev.off()

