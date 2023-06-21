# load libraries
library(tidyverse)
library(lubridate)
library(zoo)

# import pontardawe max daily level
pontardawe.max.level <- read.csv("data/pontardawe_day_max.csv") %>%
  mutate(date.time = dmy_hms(date.time),
         time.stamp = dmy_hms(time.stamp),
         daily = as_date(date.time),
         roll.5.pd.height = rollapplyr(height.m, 5, max, align = "right", fill = NA, partial = TRUE)) %>%
  rename(pontardawe.height = height.m) %>%
  select(-c("date.time", "time.stamp")) # remove unwanted columns

# import morfa max daily level
morfa.max.level <- read.csv("data/morfa_max_level.csv") %>%
  mutate(date.time = dmy_hms(date.time),
         time.stamp = dmy_hms(time.stamp),
         daily = as_date(date.time),
         roll.5.mo.height = rollapplyr(height.m, 5, max, align = "right", fill = NA, partial = TRUE)) %>%
  rename(morfa.height = height.m) %>%
  select(-c("date.time", "time.stamp"))

# join pontardawe river level with queens
pont.level.join <- left_join(pontardawe.max.level, queens_swansea, by = "daily")

# plot pontardawe level vs bacti level
p.pont.level <- pont.level.join %>%
  ggplot(aes(pontardawe.height, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000, color = "red") +
  geom_hline(yintercept = 4600, color = "orange") +
  geom_hline(yintercept = 230, color = "green")

# plot pontardawe roll5 vs bacti level
p.pont.roll <- pont.level.join %>%
  ggplot(aes(roll.5.pd.height, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000, color = "red") +
  geom_hline(yintercept = 4600, color = "orange") +
  geom_hline(yintercept = 230, color = "green")


# join morfa river level with queens
morfa.level.join <- left_join(morfa.max.level, queens_swansea, by = "daily")

# plot morfa level vs bacti level
p.morfa.level <- morfa.level.join %>%
  ggplot(aes(morfa.height, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000, color = "red") +
  geom_hline(yintercept = 4600, color = "orange") +
  geom_hline(yintercept = 230, color = "green")

# plot morfa roll5 vs bacti level
p.morfa.roll <- morfa.level.join %>%
  ggplot(aes(roll.5.mo.height, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000, color = "red") +
  geom_hline(yintercept = 4600, color = "orange") +
  geom_hline(yintercept = 230, color = "green")

# get a median for pontardawe level
mu.pont <- pontardawe.max.level %>%
  summarise(pont.median = median(pontardawe.height))

# plot a histogram with density histogram for pontardawe level
pd.level.hist <- pontardawe.max.level %>%
  ggplot(aes(x=pontardawe.height, color='#525252', fill='#969696', position="dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 0.1, alpha=0.4, position="identity") +
  geom_density(alpha=0.2) +
  geom_vline(data = mu.pont, aes(xintercept = pont.median, color = "black"), linetype = "dashed") +
  scale_color_manual(values=c('#525252', '#525252'), guide = "none")  +
  scale_fill_manual(values=c('#969696'), guide = "none") +
  labs(x = "Pontardawe River Level (m)", y =  "Frequency") +
  theme_classic()

# Open a pdf file
pdf("plots/pd_level_hist.pdf", height = 3) 

# 2. Create a plot
pd.level.hist

# Close the pdf file
dev.off()

# quantiles of interest
pontardawe.max.level %>%
  summarise(pontardawe.quant = quantile(pontardawe.height, c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE))

river.level.ecdf <- ecdf(pontardawe.max.level$pontardawe.height)
morfa.ecdf <- ecdf(morfa.max.level$morfa.height)

# function to extract period to print - given the sample date
morfa_plot_f1 <- function(a){ # a sample date
  morfa.max.level %>%
    filter(daily > ymd(a) -28 & daily < ymd(a) + 3) %>%
    ggplot(aes(daily, morfa.height)) +
    geom_line() +
    scale_y_continuous(limits = c(3, 6)) +
    labs(x = NULL, y = "Morfa River Level (m)") +
    geom_vline(xintercept = ymd(a), color = "black", linetype = "dashed") +
    theme_minimal() +
    theme(axis.text.x=element_blank())  
}

# plot Morfa level 2021-10-12
morfa_21_10_12 <- morfa_plot_f1("2021-10-12")

# Open a pdf file
pdf("plots/morfa_21_10_12.pdf", height = 2)
morfa_21_10_12
dev.off()

# plot morfa 2022-01-11
morfa_22_01_11 <- morfa_plot_f1("2022-01-11")

# Open a pdf file
pdf("plots/morfa_22_01_11.pdf", height = 2)
morfa_22_01_11
dev.off()

# plot morfa 2019-09-02
morfa_19_09_02 <- morfa_plot_f1("2019-09-02")

# Open a pdf file
pdf("plots/morfa_19_09_02.pdf", height = 2)
morfa_19_09_02
dev.off()

# plot morfa 2022-06-08
morfa_22_06_08 <- morfa_plot_f1("2022-06-08")

# Open a pdf file
pdf("plots/morfa_22_06_08.pdf", height = 2)
morfa_22_06_08
dev.off()

# plot morfa 2020-10-14
morfa_20_10_14 <- morfa_plot_f1("2020-10-14")

# Open a pdf file
pdf("plots/morfa_20_10_14.pdf", height = 2)
morfa_20_10_14
dev.off()

# plot morfa 2022-07-05
morfa_22_07_05 <- morfa_plot_f1("2022-07-05")

# Open a pdf file
pdf("plots/morfa_22_07_05.pdf", height = 2)
morfa_22_07_05
dev.off()

# plot morfa 2017-05-15
morfa_17_05_15 <- morfa_plot_f1("2017-05-15")

# Open a pdf file
pdf("plots/morfa_17_05_15.pdf", height = 2)
morfa_17_05_15
dev.off()

# plot morfa 2021-12-13
morfa_21_12_13 <- morfa_plot_f1("2021-12-13")

# Open a pdf file
pdf("plots/morfa_21_12_13.pdf", height = 2)
morfa_21_12_13
dev.off()

# plot morfa 2021-07-12
morfa_21_07_12 <- morfa_plot_f1("2021-07-12")

# Open a pdf file
pdf("plots/morfa_21_07_12.pdf", height = 2)
morfa_21_07_12
dev.off()

# plot morfa 2021-08-10
morfa_21_08_10 <- morfa_plot_f1("2021-08-10")

# Open a pdf file
pdf("plots/morfa_21_08_10.pdf", height = 2)
morfa_21_08_10
dev.off()

# plot morfa 2021-09-06
morfa_21_09_06 <- morfa_plot_f1("2021-09-06")

# Open a pdf file
pdf("plots/morfa_21_09_06.pdf", height = 2)
morfa_21_09_06
dev.off()

morfa_all <- morfa.max.level %>%
  filter(daily > "2019-09-01" & daily < "2022-02-28") %>%
  ggplot(aes(daily, morfa.height)) +
  geom_line() +
  labs(x = "Date", y = "Maximum tide height (m)") +
  geom_vline(xintercept = ymd(c("2019-09-02", "2020-10-14", "2021-10-12", "2022-01-11")), color = "black", linetype = "dashed") +
  theme_minimal()

# get a median for pontardawe level
mu.morfa <- morfa.max.level %>%
  summarise(morfa.median = median(morfa.height))

# morfa level histogram
morfa.hist <- morfa.max.level %>%
  ggplot(aes(x=morfa.height, color='#525252', fill='#969696', position="dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 0.1, alpha=0.4, position="identity") +
  geom_density(alpha=0.2) +
  scale_color_manual(values=c('#525252', '#525252'), guide = "none")  +
  scale_fill_manual(values=c('#969696'), guide = "none") +
  geom_vline(data = mu.morfa, aes(xintercept = morfa.median, color = "black"), linetype = "dashed") +
  labs(x = "Morfa Max Level (m)", y =  "Frequency") +
  theme_classic()

# Open a pdf file
pdf("plots/morfa_hist.pdf", height = 3)
morfa.hist
dev.off()


# function to extract period to print - given the sample date
pont_plot_f1 <- function(a){ # a sample date
  pontardawe.max.level %>%
    filter(daily > ymd(a) -28 & daily < ymd(a) + 3) %>%
    ggplot(aes(daily, pontardawe.height)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 1.5)) +
    labs(x = NULL, y = "Pontardawe River Level (m)") +
    geom_vline(xintercept = ymd(a), color = "black", linetype = "dashed") +
    theme_minimal() +
    theme(axis.text.x=element_blank())  
}

# plot Pontardawe level 2021-10-12
pont_21_10_12 <- pont_plot_f1("2021-10-12")

# Open a pdf file
pdf("plots/pont_21_10_12.pdf", height = 2)
pont_21_10_12
dev.off()

# plot Pontardawe level 2022-01-11
pont_22_01_11 <- pont_plot_f1("2022-01-11")

# Open a pdf file
pdf("plots/pont_22_01_11.pdf", height = 2)
pont_22_01_11
dev.off()

# plot Pontardawe level 2019-09-02
pont_19_09_02 <- pont_plot_f1("2019-09-02")

# Open a pdf file
pdf("plots/pont_19_09_02.pdf", height = 2)
pont_19_09_02
dev.off()

# plot Pontardawe level 2022-06-08
pont_22_06_08 <- pont_plot_f1("2022-06-08")

# Open a pdf file
pdf("plots/pont_22_06_08.pdf", height = 2)
pont_22_06_08
dev.off()

# plot Pontardawe level 2020-10-14
pont_20_10_14 <- pont_plot_f1("2020-10-14")

# Open a pdf file
pdf("plots/pont_20_10_14.pdf", height = 2)
pont_20_10_14
dev.off()

# plot Pontardawe level 2022-07-05
pont_22_07_05 <- pont_plot_f1("2022-07-05")

# Open a pdf file
pdf("plots/pont_22_07_05.pdf", height = 2)
pont_22_07_05
dev.off()

# plot Pontardawe level 2017-05-15
pont_17_05_15 <- pont_plot_f1("2017-05-15")

# Open a pdf file
pdf("plots/pont_17_05_15.pdf", height = 2)
pont_17_05_15
dev.off()

# plot Pontardawe level 2021-12-13
pont_21_12_13 <- pont_plot_f1("2021-12-13")

# Open a pdf file
pdf("plots/pont_21_12_13.pdf", height = 2)
pont_21_12_13
dev.off()

# plot Pontardawe level 2021-07-12
pont_21_07_12 <- pont_plot_f1("2021-07-12")

# Open a pdf file
pdf("plots/pont_21_07_12.pdf", height = 2)
pont_21_07_12
dev.off()

# plot Pontardawe level 2021-07-12
pont_21_08_10 <- pont_plot_f1("2021-08-10")

# Open a pdf file
pdf("plots/pont_21_08_10.pdf", height = 2)
pont_21_08_10
dev.off()

# plot Pontardawe level 2021-07-12
pont_21_09_06 <- pont_plot_f1("2021-09-06")

# Open a pdf file
pdf("plots/pont_21_09_06.pdf", height = 2)
pont_21_09_06
dev.off()

# plot pontardawe 2022-07-05
pont_22_07_05 <- pont_plot_f1("2022-07-05")

# Open a pdf file
pdf("plots/pont_22_07_05.pdf", height = 2)
pont_22_07_05
dev.off()

# plot pont 2017-05-15
pont_17_05_15 <- pont_plot_f1("2017-05-15")

# Open a pdf file
pdf("plots/pont_17_05_15.pdf", height = 2)
pont_17_05_15
dev.off()

# plot pont 2021-12-13
pont_21_12_13 <- pont_plot_f1("2021-12-13")

# Open a pdf file
pdf("plots/pont_21_12_13.pdf", height = 2)
pont_21_12_13
dev.off()

# plot pont 2021-07-12
pont_21_07_12 <- pont_plot_f1("2021-07-12")

# Open a pdf file
pdf("plots/pont_21_07_12.pdf", height = 2)
pont_21_07_12
dev.off()

# plot pont 2021-08-10
pont_21_08_10 <- pont_plot_f1("2021-08-10")

# Open a pdf file
pdf("plots/pont_21_08_10.pdf", height = 2)
pont_21_08_10
dev.off()

# plot pont 2021-09-06
pont_21_09_06 <- pont_plot_f1("2021-09-06")

# Open a pdf file
pdf("plots/pont_21_09_06.pdf", height = 2)
pont_21_09_06
dev.off()

