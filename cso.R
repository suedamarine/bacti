# load libraries
library(tidyverse)
library(rvest)
library(zoo)
library(graphics)
library(gplots)
library(vcd)
library(ggmosaic)


#import table from welsh water
h <- read_html("https://www.dwrcymru.com/en/our-services/wastewater/river-water-quality/combined-storm-overflows/south-west-wales")
cso_tab <- h %>%
  html_nodes("table")
tab <- cso_tab[[2]] %>%
  html_table %>%
  slice(-1) %>%
  setNames(c("name", "duration", "releases"))

cso_tab_tidy <- tab %>%
  mutate(duration = tolower(duration),
         duration = as.character(duration),
         open = period(cso_tab_tidy$duration))

#import cwmbwrla cso date  
cwmbwrla_cso <- read.csv("data/cwmbwrla_cso.csv") %>%
  mutate(date.time = dmy_hms(date.time))

# remove points outside sensible range and plot 
cwmbwrla_all_points_plot <- cwmbwrla_cso %>%
  filter(X2E47301 <= 500, X2E47301 >= 0) %>%
  ggplot(aes(date.time, X2E47301)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "3330 A5 Screen Level Percent (%)") +
  ggtitle("Cwmbwrla CSO 2E47301") +
  geom_vline(xintercept = as.POSIXct(c("2019-09-06", "2021-11-17")), color = c('red', 'green')) +
  theme_minimal()

pdf("plots/cwmbwrla_all.pdf", height = 3)
cwmbwrla_all_points_plot
dev.off()

# spill level at cwmbwrla is at 100%, filter to observe spills
# subtract 99 to get all time periods considered as a spill
cwmbwrla_spills <- cwmbwrla_cso %>%
  mutate(cwmbwrla_spill_level = X2E47301 - 99,
         cwmbwrla_spill_level = ifelse(cwmbwrla_spill_level < 0, 0, cwmbwrla_spill_level)) %>%
  filter(cwmbwrla_spill_level <= 500)

# calculate the number of instances that a high level occurs per day
cwmbwrla_high_level <- cwmbwrla_spills %>%
  mutate(daily = date(date.time),
         high.level = cwmbwrla_spill_level > 0) %>%
  group_by(daily) %>%
  summarise(high.level.sum = sum(high.level))

# get the mean number of spills per day in three periods
cwmbwrla_mean_spills <- cwmbwrla_high_level %>%
  mutate(period = case_when(daily > "2017-01-21" & daily < "2019-09-06" ~ "Period 1",
                            daily >= "2019-09-06" & daily <= "2021-09-28" ~ "Period 2",
                            daily >= "2021-11-17" & daily <= "2022-08-15" ~ "Period 3"))

# find out how many events occurred (number of days) group by period
events <- cwmbwrla_mean_spills %>%
  mutate(spill = high.level.sum > 0) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(spill_sum = sum(spill),
            spill_n = n()) %>%
  mutate(spill_p = spill_sum / spill_n)

# how many above level readings per period
hl_periods <- cwmbwrla_spills %>%
  mutate(daily = date(date.time),
         level_status = ifelse(cwmbwrla_spill_level > 0, "above", "below"),
         period = case_when(daily > "2017-01-21" & daily < "2019-09-06" ~ "Period 1",
                            daily >= "2019-09-06" & daily <= "2021-09-28" ~ "Period 2",
                            daily >= "2021-11-17" & daily <= "2022-08-15" ~ "Period 3")) %>%
  filter(!is.na(period))


# contingency table
hl_periods_table <- table(hl_periods$period, hl_periods$level_status)
hl_periods_t <- as.data.frame.matrix(hl_periods_table) %>%
  mutate(spill.prop = above / (above + below))

  
# try a mosaic plot using ggplot
periods.mosaic <- hl_periods %>%
  ggplot() +
  geom_mosaic(aes(x = product(period), fill = level_status), offset = 0.02) +
  labs(x = "", y = "Level Status") +
  scale_fill_manual(values = alpha(c("grey75", "grey90"), 0.2)) +
  theme_minimal() +
  theme(legend.position = "None")

pdf("plots/periods_mosaic.pdf", height = 3)
periods.mosaic
dev.off()

# try mosaic plot using https://statsandr.com/blog/chi-square-test-of-independence-in-r/

# chi-square tests
cwmbwrla.chi <- chisq.test(table(hl_periods$period, hl_periods$level_status))

# 24039 / (24039 + 48343) * 100 etc

mu <- cwmbwrla_mean_spills %>% group_by(period) %>% summarize(grp_mean = mean(high.level.sum)) %>% filter(!is.na(period))

cwmbwrla_periods_plot <- cwmbwrla_mean_spills %>%
  filter(!is.na(period)) %>%
  ggplot(aes(high.level.sum, color = period, fill = period, position = "dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 1.0, alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.2, bw = 0.75) +
  scale_y_sqrt() +
  scale_color_manual(values=c("#cccccc","#969696", "#525252"), guide = "none")  +
  scale_fill_manual(name = "",labels = c("Period 1", "Period 2", "Period 3"), values=c("#cccccc","#969696", "#525252")) +
  facet_wrap(~ period, ncol = 1) +
  geom_vline(data = mu, aes(xintercept=grp_mean, colour = period), linetype = "dashed") +
  labs(x = "Number of 'Above Level' Readings per day", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "None")

pdf("plots/cwmbwrla_periods.pdf")
cwmbwrla_periods_plot
dev.off()

cwmbwrla_periods_plot_2 <- cwmbwrla_mean_spills %>%
  filter(!is.na(period)) %>%
  ggplot(aes(high.level.sum, color = period, fill = period, position = "dodge")) +
  geom_density(alpha = 0.2, bw = 0.75)

# plot number of high instances
cwmbwrla_high_plot <- cwmbwrla_high_level %>%
  ggplot(aes(daily, high.level.sum)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Number of 'Above-Level' readings \nper day") +
  geom_vline(xintercept = ymd(c("2019-09-06", "2021-11-17")), color = c('red', 'green')) +
  theme_minimal()

pdf("plots/cwmbwrla_highs.pdf", height = 3)
cwmbwrla_high_plot
dev.off()


# assuming that spill percentage follows a linear function
# sum spill periods occuring on same day to get a metric for daily spill magnitude
cwmbwrla_daily_spills <- cwmbwrla_spills %>%
  mutate(daily = date(date.time)) %>%
  group_by(daily) %>%
  summarise(spill_sum = sum(cwmbwrla_spill_level))

# get a median spill magnitude for whole period
mu.spill.magnitude <- cwmbwrla_daily_spills %>%
  summarise(spill.median = median(spill_sum))

# plot density histogram of daily rainfall
spill.hist <- cwmbwrla_daily_spills %>%
  ggplot(aes(x=spill_sum, color='#525252', fill='#969696', position="dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 100, alpha=0.4, position="identity") +
  geom_density(alpha=0.2) +
  geom_vline(data = mu.spill.magnitude, aes(xintercept = spill.median, color = "black"), linetype = "dashed") +
  scale_color_manual(values=c('#525252', '#525252'), guide = "none")  +
  scale_fill_manual(values=c('#969696'), guide = "none") +
  labs(x = "Cwmbwrla Relative Spill Magnitude", y =  "Frequency") +
  theme_classic()

# Open a pdf file
pdf("plots/spill_hist.pdf", height = 3)
spill.hist
dev.off()


# get the mean number of magnitude metric per day in three periods
cwmbwrla_mean_magnitude <- cwmbwrla_daily_spills %>%
  mutate(period = case_when(daily > "2017-01-21" & daily < "2019-09-06" ~ "Period 1",
                            daily >= "2019-09-06" & daily <= "2021-09-28" ~ "Period 2",
                            daily >= "2021-11-17" & daily <= "2022-08-15" ~ "Period 3"))

mu_magnitude <- cwmbwrla_mean_magnitude %>% group_by(period) %>% summarize(grp_mean = mean(spill_sum)) %>% filter(!is.na(period))

# plot daily spill rate
cwmbwrla_daily_magnitude <- cwmbwrla_daily_spills %>%
  ggplot(aes(daily, spill_sum)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Sum of above-level 15 min intervals \nper day") +
  ggtitle("Daily spill magnitude Cwmbwrla CSO 2E47301") +
  geom_vline(xintercept = ymd(c("2019-09-06", "2021-11-17")), color = c('red', 'green')) +
  theme_minimal()

pdf("plots/cwmbwrla_magnitude.pdf", height = 3)
cwmbwrla_daily_magnitude
dev.off()

# create a five day rolling mean
cwmbwrla_roll_5 <- cwmbwrla_daily_spills %>%
  mutate(cwmbwrla.roll.5 = rollapplyr(spill_sum, 5, sum, partial = TRUE))

# plot the cwmbwrla roll data
cwmbwrla_roll_plot <- cwmbwrla_roll_5 %>%
  ggplot(aes(daily, cwmbwrla.roll.5)) +
  geom_line() +
  labs(x = "Date", y =  "Five day rolling 'magnitude' of spill occurrences") +
  ggtitle("Cwmbwrla CSO 2E47301 Five-Day-Rolling of Spill Magnitude") +
  theme_minimal()

opp_74_bonymaen <- read.csv("data/opp_74_bonymaen.csv") %>%
  mutate(date.time = dmy_hms(date.time))

opp_74_all_points <- opp_74_bonymaen %>%
  filter(X2E80442 >= 0, X2E80442 <= 500) %>%
  ggplot(aes(date.time, X2E80442)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81265 LVL (%)") +
  ggtitle("Opp 74 Bonymaen CSO 2E80442")

pdf("plots/opp_74_all.pdf")
opp_74_all_points
dev.off()

cwmbwrla_daily_spills %>%
  summarise(spill.quant = quantile(spill_sum, c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE))

spill.ecdf <- ecdf(cwmbwrla_daily_spills$spill_sum)
spill.ecdf(19)
roll.spill.ecdf <- ecdf(cwmbwrla_roll_5$cwmbwrla.roll.5)

# function to extract period to print - given the sample date
cwmbwrla_plot_f1 <- function(a){ # a sample date
  cwmbwrla_daily_spills %>%
    filter(daily > ymd(a) -28 & daily < ymd(a) + 3) %>%
    ggplot(aes(daily, spill_sum)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 2000)) +
    labs(x = NULL, y = "Cwmbwrla CSO Spill Metric") +
    geom_vline(xintercept = ymd(a), color = "black", linetype = "dashed") +
    theme_minimal() +
    theme(axis.text.x=element_blank())  
}

# plot cso metric 2021-10-12
cwmbwrla_21_10_12 <- cwmbwrla_plot_f1("2021-10-12")

# Open a pdf file
pdf("plots/cwmbwrla_21_10_12.pdf", height = 2)
cwmbwrla_21_10_12
dev.off()

# plot cso metric 2022-01-11
cwmbwrla_22_01_11 <- cwmbwrla_plot_f1("2022-01-11")

# Open a pdf file
pdf("plots/cwmbwrla_22_01_11.pdf", height = 2)
cwmbwrla_22_01_11
dev.off()

# plot cso metric 2019-09-02
cwmbwrla_19_09_02 <- cwmbwrla_plot_f1("2019-09-02")

# Open a pdf file
pdf("plots/cwmbwrla_19_09_02.pdf", height = 2)
cwmbwrla_19_09_02
dev.off()

# plot csm metric 2022-06-08
cwmbwrla_22_06_08 <- cwmbwrla_plot_f1("2022-06-08")

# Open a pdf file
pdf("plots/cwmbwrla_22_06_08.pdf", height = 2)
cwmbwrla_22_06_08
dev.off()

# plot cso metric 2020-10-14
cwmbwrla_20_10_14 <- cwmbwrla_plot_f1("2020-10-14")

# Open a pdf file
pdf("plots/cwmbwrla_20_10_14.pdf", height = 2)
cwmbwrla_20_10_14
dev.off()

# plot cso 2022-07-05
cwmbwrla_22_07_05 <- cwmbwrla_plot_f1("2022-07-05")

# Open a pdf file
pdf("plots/cwmbwrla_22_07_05.pdf", height = 2)
cwmbwrla_22_07_05
dev.off()

# plot cwmbwrla 2017-05-15
cwmbwrla_17_05_15 <- cwmbwrla_plot_f1("2017-05-15")

# Open a pdf file
pdf("plots/cwmbwrla_17_05_15.pdf", height = 2)
cwmbwrla_17_05_15
dev.off()

# plot cwmbwrla 2021-12-13
cwmbwrla_21_12_13 <- cwmbwrla_plot_f1("2021-12-13")

# Open a pdf file
pdf("plots/cwmbwrla_21_12_13.pdf", height = 2)
cwmbwrla_21_12_13
dev.off()

# plot rain 2021-07-12
cwmbwrla_21_07_12 <- cwmbwrla_plot_f1("2021-07-12")

# Open a pdf file
pdf("plots/cwmbwrla_21_07_12.pdf", height = 2)
cwmbwrla_21_07_12
dev.off()

# plot cwmbwrla 2021-08-10
cwmbwrla_21_08_10 <- cwmbwrla_plot_f1("2021-08-10")

# Open a pdf file
pdf("plots/cwmbwrla_21_08_10.pdf", height = 2)
cwmbwrla_21_08_10
dev.off()

# plot cwmbwrla 2021-09-06
cwmbwrla_21_09_06 <- cwmbwrla_plot_f1("2021-09-06")

# Open a pdf file
pdf("plots/cwmbwrla_21_09_06.pdf", height = 2)
cwmbwrla_21_09_06
dev.off()

