# load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(ggridges)

# import mumbles tide guage data
header <- c("cycle", "date", "time", "height", "residual")
mum_tide <- read.table("data/2016MUM.txt", skip = 2, col.names = header)

# remove selected column
mum_tide_clean <- mum_tide %>%
  select(-"cycle") %>%
  mutate(height = str_replace(height, "\\M", ""),
         residual = str_replace(residual, "\\M", ""),
         height = as.numeric(height),
         residual = as.numeric(residual)) %>%
  unite("date.time", date:time, sep = " ", remove = FALSE) %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  select(-c("date", "time"))
  
# import ilfracoome tide guage data
header <- c("cycle", "date", "time", "height", "residual")
ilf.tide <- read.table("data/ilf_tide.txt", skip = 2, col.names = header)

# clean data
ilf.tide.clean <- ilf.tide %>%
  select(-"cycle") %>% #remove cycle column
  mutate(height = str_replace(height, "\\M", ""), # remove unwanted character
         residual = str_replace(residual, "\\M", ""), # remove unwanted character
         height = as.numeric(height), # define as numeric
         residual = as.numeric(residual)) %>% # define as numeric
  unite("date.time", date:time, sep = " ", remove = FALSE) %>% # unite date and time columns
  mutate(date.time = ymd_hms(date.time)) %>% # define as date time format
  filter(height > 5) %>%
  select(-c("date", "time")) # remove unwanted columns

# group by day
ilf.tide.max <- ilf.tide.clean %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.tide.height = max(height)) %>%
  mutate(roll.5.tide = rollapplyr(max.tide.height, 5, max, align = "right", fill = NA, partial = TRUE))

# tide rollmax
ilf.tide.roll <- ilf.tide.max %>%
  mutate(roll.3.tide = rollapplyr(max.tide.height, 3, max, align = "right", fill = NA, partial = TRUE))

# join max tide with queens 
tide.join <- left_join(ilf.tide.max, queens_swansea, by = "daily")

# plot tide height vs bacti level
p.tide <- tide.join %>%
  ggplot(aes(max.tide.height, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10")

# join roll tide with queens
tide.roll.join <- left_join(ilf.tide.roll, queens_swansea, by = "daily")

# plot roll tide vs bacti level
p.roll.tide <- tide.roll.join %>%
  ggplot(aes(roll.3.tide, bacti_level)) +
  geom_point() +
  scale_y_continuous(trans = "log10") +
  geom_hline(yintercept = 18000, color = "red") +
  geom_hline(yintercept = 4600, color = "orange") +
  geom_hline(yintercept = 230, color = "green")

# get a median for tide height
mu.tide <- ilf.tide.max %>%
  summarise(tide.median = median(max.tide.height))

tide.hist <- ilf.tide.max %>%
  filter(max.tide.height > 5.0) %>%
  ggplot(aes(x=max.tide.height, color='#525252', fill='#969696', position="dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 0.1, alpha=0.4, position="identity") +
  geom_density(alpha=0.2) +
  geom_vline(data = mu.tide, aes(xintercept = tide.median, color = "black"), linetype = "dashed") +
  scale_color_manual(values=c('#525252', '#525252'), guide = "none")  +
  scale_fill_manual(values=c('#969696'), guide = "none") +
  labs(x = "Ilfracombe Max Tide Height (m)", y =  "Frequency") +
  theme_classic()

# Open a pdf file
pdf("plots/tide_hist.pdf", height = 3) 

# 2. Create a plot
tide.hist

# Close the pdf file
dev.off()

# quantiles of interest
ilf.tide.max %>%
  summarise(tide.quant = quantile(max.tide.height, c(0.01, 0.05, 0.95, 0.99), na.rm = TRUE))

# ecdf generates a function that you can then use to return percentiles on the data
tide.ecdf <- ecdf(ilf.tide.max$max.tide.height)


