# load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(purrr)
library(broom)
library(grid)
library(gtable)
library(openxlsx)

# merge relavent datasets with queens swansea
combined <- purrr::reduce(list(ilf.tide.max, morfa.max.level, pontardawe.max.level, ystrad_rain, queens_swansea, pumping_group, cwmbwrla_roll_5), dplyr::left_join, by = 'daily')

# try a linear regression model of bacti level and tide
tide.fit <- lm(bacti_level ~ max.tide.height, data = combined)
summary(tide.fit)

# try a linear regression model of bacti level and river level
river.level.fit <- lm(bacti_level ~ pontardawe.height, data = combined)
summary(river.level.fit)

# try a linear regression model of bacti level and rainfall
rainfall.level.fit <- lm(bacti_level ~ rain.mm, data = combined)
summary(rainfall.level.fit)

# try a linear regression model of bacti level and roll.5 cso
cso.fit <- lm(bacti_level ~ cwmbwrla.roll.5, data = combined)
summary(cso.fit)

# try a multiple linear regression of bacti level, tide, river level and rainfall
combined.fit <- lm(bacti_level ~ max.tide.height + pontardawe.height + rain.mm, data = combined)
summary(combined.fit)

# try a multiple linear regression of bacti level, roll5 tide, roll5 river level and roll5 rainfall
roll5.fit <- lm(bacti_level ~ cwmbwrla.roll.5 + roll.5.volume, data = combined)
summary(roll5.fit)


# have a look at the coefficients
summary(roll5.fit)$coefficient

# select top 10 bacti levels for daily data
slice_bacti_1 <-  combined %>%
  slice_max(bacti_level, n = 10) %>%
  rename("Date" = "daily",
         'Max tide' = 'max.tide.height',
         'Max 5-day tide' = 'roll.5.tide',
         'Morfa max level' = 'morfa.height',
         'Morfa 5-day max' = 'roll.5.mo.height',
         'Pontardawe max level' = 'pontardawe.height',
         'Pontardawe 5-day max' = 'roll.5.pd.height',
         'Rainfall' = 'rain.mm',
         'Rainfall 5-day sum' = 'roll.5.rain',
         'Bacti level' = 'bacti_level',
         'Pumping hours' = 'total_pump_hours',
         'Pumping 5-day hours' = 'roll.pump.hours',
         'Day spill' = 'spill_sum',
         'Spill 5-day sum' = 'cwmbwrla.roll.5') %>%
  select(c('Date', 'Bacti level',
           'Max tide',
           'Morfa max level',
           'Pontardawe max level',
           'Rainfall',
           'Day spill',
           'Pumping 5-day hours'))


write_csv(slice_bacti_1, "tabs/slice_bacti_1.csv")

# select top 10 bacti levels for 5-day data
slice_bacti_5 <-  combined %>%
  slice_max(bacti_level, n = 10) %>%
  rename("Date" = "daily",
         'Max tide' = 'max.tide.height',
         'Max 5-day tide' = 'roll.5.tide',
         'Morfa max level' = 'morfa.height',
         'Morfa 5-day max' = 'roll.5.mo.height',
         'Pontardawe max level' = 'pontardawe.height',
         'Pontardawe 5-day max' = 'roll.5.pd.height',
         'Rainfall' = 'rain.mm',
         'Rainfall 5-day sum' = 'roll.5.rain',
         'Bacti level' = 'bacti_level',
         'Pumping hours' = 'total_pump_hours',
         'Pumping 5-day hours' = 'roll.pump.hours',
         'Day spill' = 'spill_sum',
         'Spill 5-day sum' = 'cwmbwrla.roll.5') %>%
  select(c('Date', 'Bacti level',
           'Max 5-day tide', 'Morfa 5-day max',
           'Pontardawe 5-day max',
           'Rainfall 5-day sum','Spill 5-day sum',
           'Pumping 5-day hours'))


write_csv(slice_bacti_5, "tabs/slice_bacti_5.csv")

# select most recent 12 bacti levels for 5-day data
slice_bacti_12 <-  combined %>%
  filter(!is.na(bacti_level)) %>%
  slice_tail(n = 12) %>%
  rename("Date" = "daily",
         'Max tide' = 'max.tide.height',
         'Max 5-day tide' = 'roll.5.tide',
         'Morfa max level' = 'morfa.height',
         'Morfa 5-day max' = 'roll.5.mo.height',
         'Pontardawe max level' = 'pontardawe.height',
         'Pontardawe 5-day max' = 'roll.5.pd.height',
         'Rainfall' = 'rain.mm',
         'Rainfall 5-day sum' = 'roll.5.rain',
         'Bacti level' = 'bacti_level',
         'Pumping volume' = 'volume',
         'Pumping 5-day hours' = 'roll.pump.hours',
         'Day spill' = 'spill_sum',
         'Spill 5-day sum' = 'cwmbwrla.roll.5') %>%
  select(c('Date', 'Bacti level',
           'Max 5-day tide', 'Morfa 5-day max',
           'Pontardawe 5-day max',
           'Rainfall 5-day sum','Spill 5-day sum',
           'Pumping 5-day hours'))


write_csv(slice_bacti_12, "tabs/slice_bacti_12.csv")

# build a summary table for interesting parameters
skim_combined <- combined %>%
  skim(max.tide.height, morfa.height, pontardawe.height, rain.mm, bacti_level, total_pump_hours, spill_sum, cwmbwrla.roll.5)

write_csv(skim_combined, "tabs/skim_combined.csv")  

# find out which rows contain missing values (tide, rain, level)
missing_tide <- combined %>%
  dplyr::filter(is.na(max.tide.height))

write_csv(missing_tide, "tabs/missing_tide.csv")

consol.fit <- lm(bacti_level ~ consolidated, data = combined)
summary(consol.fit)

bacti.lg <- lm(log(bacti_level) ~ sqrt(roll.7.rain), data = combined)
summary(bacti.lg)

# try a principle component analysis
comb.pca <- prcomp(na.omit(combined)[, c(4,7,10,11)], center = TRUE, scale. = TRUE)
summary(comb.pca)

# select a timeframe to plot 
autumn.21 <- combined %>%
  select(daily, max.tide.height, pontardawe.height, rain.mm, bacti_level, cwmbwrla.roll.5) %>%
  filter(between(daily, as.Date("2021-08-01"), as.Date("2022-06-31")))
  
autumn21.bacti <- autumn.21 %>%
  ggplot(aes(daily, bacti_level)) +
  geom_point()

autumn21.tide <- autumn.21 %>%
  ggplot(aes(daily, max.tide.height)) +
  geom_line()

autumn21.pdlevel <- autumn.21 %>%
  ggplot(aes(daily, pontardawe.height)) +
  geom_line()

autumn21.rain <- autumn.21 %>%
  ggplot(aes(daily, rain.mm)) +
  geom_line() +
  scale_x_date(date_breaks = 'month')

grid.newpage()
grid.draw(rbind(ggplotGrob(autumn21.bacti), ggplotGrob(autumn21.rain), size = 'last'))


grid.newpage()
grid.draw(rbind(ggplotGrob(autumn21.bacti), ggplotGrob(autumn21.pdlevel), ggplotGrob(autumn21.tide), size = 'last'))

# reduce combined to min variables and perform ecdf on each variable
combined.red <- combined %>%
  select(c(daily, max.tide.height, pontardawe.height, rain.mm, bacti_level, volume)) %>%
  mutate(tide = tide.ecdf(max.tide.height),
         river = river.level.ecdf(pontardawe.height),
         rain = rain.ecdf(rain.mm),
         volume = volume.ecdf(volume)) %>%
  gather(key = "key", value = "percentile", tide:rain)

combined.bacti.p <- combined.red %>%
  filter(between(daily, as.Date("2021-08-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, bacti_level)) +
  geom_point()

combined.key.p <- combined.red %>%
  filter(between(daily, as.Date("2021-08-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, percentile, color = key)) +
  geom_line()

# filter combined to retrieve only bacti na.rm
combined.filter <- combined %>%
  select(c(daily, max.tide.height, roll.3.tide, pontardawe.height, roll.3.pd.height, rain.mm, roll.3.rain, volume, roll.3.volume, bacti_level)) %>%
filter(between(daily, as.Date("2021-08-01"), as.Date("2022-01-31")),
       !is.na(bacti_level))

write.csv(combined.filter, "tabs/combined_filter.csv")

# reduce combined to min variables and perform ecdf on each roll3 variable
combined.roll <- combined %>%
  select(c(daily, roll.5.tide, roll.5.pd.height, roll.5.rain, roll.5.volume, bacti_level)) %>%
  mutate(tide = tide.ecdf(roll.3.tide),
         river = river.level.ecdf(roll.3.pd.height),
         rain = rain.ecdf(roll.3.rain),
         volume = volume.ecdf(roll.3.volume)) %>%
  gather(key = "key", value = "percentile", tide:volume)

# keep a wide format for easy export to view
combined.roll.wide <- combined %>%
  mutate(tide = tide.ecdf(roll.3.tide),
         river = river.level.ecdf(roll.3.pd.height),
         rain = rain.ecdf(roll.3.rain),
         volume = volume.ecdf(roll.3.volume)) %>%
  select(c(daily, roll.3.tide, tide, roll.3.pd.height, river, roll.3.rain, rain, roll.3.volume, volume, bacti_level)) %>%
  filter(between(daily, as.Date("2021-08-01"), as.Date("2022-04-30")),
         !is.na(bacti_level))

write.csv(combined.roll.wide, "tabs/combined_roll_wide.csv")

combined.roll.p1 <- combined.roll %>%
  filter(between(daily, as.Date("2021-08-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, bacti_level)) +
  geom_point()

# Open a pdf file
pdf("plots/com_roll_bacti.pdf") 

# 2. Create a plot
combined.roll.p1

# Close the pdf file
dev.off()

combined.roll.p2 <- combined.roll %>%
  filter(between(daily, as.Date("2021-08-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, percentile, color = key)) +
  geom_line()

# Open a pdf file
pdf("plots/com_roll_pc.pdf") 

# 2. Create a plot
combined.roll.p2

# Close the pdf file
dev.off()

twin.plot <- combined.roll %>%
  filter(between(daily, as.Date("2021-08-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, bacti_level/ 1000)) +
  geom_point() +
  geom_line(aes(daily, percentile * 100, color = key)) +
  scale_y_continuous("Bacti Level / 1000", sec.axis = sec_axis(~. / 100, name = "Percentile")) +
  geom_vline(xintercept = as.numeric(ymd(c("2021-08-10", "2021-09-06", "2021-10-12", "2021-11-16", "2021-12-13", "2022-01-11"))),
             linetype = 4, color = "black")
  
# Open a pdf file
pdf("plots/twin_plot.pdf") 

# 2. Create a plot
twin.plot

# Close the pdf file
dev.off()

twin.plot2 <- combined.roll %>%
  filter(between(daily, as.Date("2020-07-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, bacti_level/ 1000)) +
  geom_point() +
  geom_line(aes(daily, percentile * 100, color = key)) +
  scale_y_continuous("Bacti Level / 1000", sec.axis = sec_axis(~. / 100, name = "Percentile")) +
  geom_vline(xintercept = as.numeric(ymd(c(sample.dates$daily))),
             linetype = 4, color = "black")

# Open a pdf file
pdf("plots/twin_plot2.pdf", width = 12, height = 4) 

# 2. Create a plot
twin.plot2

# Close the pdf file
dev.off()

# reduce combined to min variables and perform ecdf on each roll3 variable
# reduce combined to min variables and perform ecdf on each roll3 variable
combined.roll2 <- combined %>%
  select(c(daily, roll.3.tide, roll.3.pd.height, roll.3.rain, roll.3.volume, bacti_level)) %>%
  mutate(tide = tide.ecdf(roll.3.tide),
         river = river.level.ecdf(roll.3.pd.height),
         volume = volume.ecdf(roll.3.volume)) %>%
  gather(key = "key", value = "percentile", tide:volume)

twin.plot3 <- combined.roll2 %>%
  filter(between(daily, as.Date("2020-07-01"), as.Date("2022-01-31"))) %>%
  ggplot(aes(daily, bacti_level/ 1000)) +
  geom_point() +
  geom_line(aes(daily, percentile * 100, color = key)) +
  scale_y_continuous("Bacti Level / 1000", sec.axis = sec_axis(~. / 100, name = "Percentile")) +
  geom_vline(xintercept = as.numeric(ymd(c(sample.dates$daily))),
             linetype = 4, color = "black")

# Open a pdf file
pdf("plots/twin_plot3.pdf", width = 12, height = 4) 

# 2. Create a plot
twin.plot3

# Close the pdf file
dev.off()




