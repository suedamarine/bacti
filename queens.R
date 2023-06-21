# load libraries
library(tidyverse)
library(lubridate)

# import data
queens_d <- read.csv('data/queens_d.csv')

# replace less thann symbol with ""
queens <- queens_d %>%
  mutate(date = ymd(date),
         bacti_level = str_replace_all(bacti_level, "<", ""),
         bacti_level = as.numeric(bacti_level)) %>%
  drop_na() %>%
  filter(bacti_level < 250000) %>%
  rename(daily = date)

# filter for just swansea dock
queens_swansea <- queens %>%
  filter(location == "Swansea Queen Dock")

# write csv to view
write_csv(queens, "tabs/queens.csv")

# plot bacti levels
queens %>%
  filter(location == "Swansea Queen Dock") %>%
  ggplot(aes(daily, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000) +
  geom_hline(yintercept = 4600) +
  geom_hline(yintercept = 230) +
  theme_light()
  
bacti_plot <- queens %>%
  filter(location == "Swansea Queen Dock") %>%
  ggplot(aes(daily, bacti_level)) +
  geom_point(size = 0.25) +
  geom_hline(yintercept = c(230,4600,46000), linetype = 'dashed') +
  labs(x = "Date", y = "E.coli Most Probable Number") +
  geom_vline(xintercept = ymd(c("2019-09-06", "2021-11-17")), color = c('red', 'green')) +
  theme_minimal()

# Open a pdf file
pdf("plots/bacti_plot.pdf", height = 3) 

# 2. Create a plot
bacti_plot

# Close the pdf file
dev.off()

sample.dates <- queens %>%
  filter(location == "Swansea Queen Dock",
         between(daily, as.Date("2020-07-01"), as.Date("2022-01-31"))) %>%
  select(daily)
  
queens %>%
  filter(location == "Swansea Queen Dock") %>%
  ggplot(aes(x = "", y = bacti_level)) +
  geom_point(position = position_jitter(seed = 1, width = 0.2)) +
  geom_boxplot(width = 1, color = "grey", alpha = 0.2)

queens %>%
  filter(location == "Swansea Queen Dock",
         bacti_level < 25000,
         between(daily, as.Date("2020-07-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, bacti_level)) +
  geom_point(size = 0.25) +
  geom_smooth(method = lm) +
  labs(x = "Date", y = "E.coli Most Probable Number") +
  theme_minimal()
