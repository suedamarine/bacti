# load libraries
library(tidyverse)
library(lubridate)

#import opp_74 cso data  
opp_74_cso <- read.csv("data/opp_74_bonymaen.csv") %>%
  mutate(date.time = dmy_hms(date.time))

opp_74_max <- opp_74_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80442), 
            min.level = min(X2E80442)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
opp_74_all_points <- opp_74_cso %>%
  filter(X2E80442 <= 500, X2E80442 >= 0) %>%
  ggplot(aes(date.time, X2E80442)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81265 LVL (%)") +
  ggtitle("Opp 74 Bonymaen Road Swansea CSO 2E80442") +
  theme_minimal()

pdf("plots/opp_74_all.pdf", height = 3)
opp_74_all_points
dev.off()

# remove points outside sensible range and plot 
opp_74_p_max <- opp_74_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81265 LVL (%)") +
  ggtitle("Opp 74 Bonymaen Road Swansea CSO 2E80442") +
  theme_minimal()

pdf("plots/opp_74_p_max.pdf", height = 3)
opp_74_p_max
dev.off()

#import opp_92 cso data  
opp_92_cso <- read.csv("data/opp_92.csv") %>%
  mutate(date.time = dmy_hms(date.time))

opp_92_max <- opp_92_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80560), 
            min.level = min(X2E80560)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
opp_92_all_points <- opp_92_cso %>%
  filter(X2E80560 <= 500, X2E80560 >= 0) %>%
  ggplot(aes(date.time, X2E80560)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81359 LVL (%)") +
  ggtitle("Opp 92 Bonymaen Road Swansea CSO 2E80560") +
  theme_minimal()

pdf("plots/opp_92_all.pdf", height = 3)
opp_92_all_points
dev.off()

# remove points outside sensible range and plot 
opp_92_p_max <- opp_92_max %>%
  filter(level <= 200, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81359 LVL (%)") +
  ggtitle("Opp 92 Bonymaen Road Swansea CSO 2E80560") +
  theme_minimal()

pdf("plots/opp_92_p_max.pdf", height = 3)
opp_92_p_max
dev.off()

#import pentre_100 cso data  
pentre_100_cso <- read.csv("data/pentre_100.csv") %>%
  mutate(date.time = dmy_hms(time.date))

pentre_100_max <- pentre_100_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80434), 
            min.level = min(X2E80434)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
pentre_100_all_points <- pentre_100_cso %>%
  filter(X2E80434 <= 500, X2E80434 >= 0) %>%
  ggplot(aes(date.time, X2E80434)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81257 LVL (%)") +
  ggtitle("Pentre Chwyth Point 100 CSO 2E80434") +
  theme_minimal()

pdf("plots/pentre_100_all.pdf", height = 3)
pentre_100_all_points
dev.off()

# remove points outside sensible range and plot 
pentre_100_p_max <- pentre_100_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81257 LVL (%)") +
  ggtitle("Pentre Chwyth Point 100 CSO 2E80434") +
  theme_minimal()

pdf("plots/pentre_100_p_max.pdf", height = 3)
pentre_100_p_max
dev.off()

#import tawe_989 cso data  
tawe_989_cso <- read.csv("data/tawe_989.csv") %>%
  mutate(date.time = dmy_hms(date.time))

tawe_989_max <- tawe_989_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E14964), 
            min.level = min(X2E14964)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
tawe_989_all_points <- tawe_989_cso %>%
  filter(X2E14964 <= 500, X2E14964 >= 0) %>%
  ggplot(aes(date.time, X2E14964)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Screen Level (Metres)") +
  ggtitle("CSO 989 Tawe Barrage 2E14964") +
  theme_minimal()

pdf("plots/tawe_989_all.pdf", height = 3)
tawe_989_all_points
dev.off()

# remove points outside sensible range and plot 
tawe_989_p_max <- tawe_989_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Screen Level (Metres)") +
  ggtitle("CSO 989 Tawe Barrage 2E14964") +
  theme_minimal()

pdf("plots/tawe_989_p_max.pdf", height = 3)
tawe_989_p_max
dev.off()

#import quay_road cso data  
quay_road_cso <- read.csv("data/quay_road.csv") %>%
  mutate(date.time = dmy_hms(date.time))

quay_road_max <- quay_road_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E87085), 
            min.level = min(X2E87085)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
quay_road_all_points <- quay_road_cso %>%
  filter(X2E87085 <= 500, X2E87085 >= 0) %>%
  ggplot(aes(date.time, X2E87085)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "W52654 Screen LVL (%)") +
  ggtitle("Quay Road Sewage CSO 2E87085") +
  theme_minimal()

pdf("plots/quay_road_all.pdf", height = 3)
quay_road_all_points
dev.off()

# remove points outside sensible range and plot 
quay_road_p_max <- quay_road_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "W52654 Screen LVL (%)") +
  ggtitle("Quay Road Sewage CSO 2E87085") +
  theme_minimal()

pdf("plots/quay_road_p_max.pdf", height = 3)
quay_road_p_max
dev.off()

#import canal_bank cso data  
canal_bank_cso <- read.csv("data/canal_bank.csv") %>%
  mutate(date.time = dmy_hms(date.time))

canal_bank_max <- canal_bank_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E37597), 
            min.level = min(X2E37597)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
canal_bank_all_points <- canal_bank_cso %>%
  filter(X2E37597 <= 150, X2E37597 >= 0) %>%
  ggplot(aes(date.time, X2E37597)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Screen Level Derived (%)") +
  ggtitle("Canal Bank Bridge CSO 2E37579") +
  theme_minimal()

pdf("plots/canal_bank_all.pdf", height = 3)
canal_bank_all_points
dev.off()

# remove points outside sensible range and plot 
canal_bank_p_max <- canal_bank_max %>%
  filter(level <= 150, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Screen Level Derived (%)") +
  ggtitle("Canal Bank Bridge CSO 2E37579") +
  theme_minimal()

pdf("plots/canal_bank_p_max.pdf", height = 3)
canal_bank_p_max
dev.off()

#import nant_gelli cso data  
nant_gelli_cso <- read.csv("data/nant_gelli.csv") %>%
  mutate(date.time = dmy_hms(date.time))

nant_gelli_max <- nant_gelli_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80439), 
            min.level = min(X2E80439)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
nant_gelli_all_points <- nant_gelli_cso %>%
  filter(X2E80439 <= 500, X2E80439 >= 0) %>%
  ggplot(aes(date.time, X2E80439)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81262 Level (%)") +
  ggtitle("Swansea Heol Nant Gelli Point 69A 2E80439") +
  theme_minimal()

pdf("plots/nant_gelli_all.pdf", height = 3)
nant_gelli_all_points
dev.off()

# remove points outside sensible range and plot 
nant_gelli_p_max <- nant_gelli_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81262 Level (%)") +
  ggtitle("Swansea Heol Nant Gelli Point 69A 2E80439") +
  theme_minimal()

pdf("plots/nant_gelli_p_max.pdf", height = 3)
nant_gelli_p_max
dev.off()

# import cwm_level cso data  
cwm_level_cso <- read.csv("data/cwm_level.csv") %>%
  mutate(date.time = dmy_hms(date.time))

cwm_level_max <- cwm_level_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80438), 
            min.level = min(X2E80438)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
cwm_level_all_points <- cwm_level_cso %>%
  filter(X2E80438 <= 500, X2E80438 >= 0) %>%
  ggplot(aes(date.time, X2E80438)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81261 Level (%)") +
  ggtitle("Cwm Level Rd/ Trewyddfa Rd CSO Swansea 2E80438") +
  theme_minimal()

pdf("plots/cwm_level_all.pdf", height = 3)
cwm_level_all_points
dev.off()

# remove points outside sensible range and plot 
cwm_level_p_max <- cwm_level_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81261 Level (%)") +
  ggtitle("Cwm Level Rd/ Trewyddfa Rd CSO Swansea 2E80438") +
  theme_minimal()

pdf("plots/cwm_level_p_max.pdf", height = 3)
cwm_level_p_max
dev.off()

# import llanfelach cso data  
llanfelach_cso <- read.csv("data/llanfelach.csv") %>%
  mutate(date.time = dmy_hms(date.time))

llanfelach_max <- llanfelach_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E47265), 
            min.level = min(X2E47265)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
llanfelach_all_points <- llanfelach_cso %>%
  filter(X2E47265 <= 200, X2E47265 >= 0) %>%
  ggplot(aes(date.time, X2E47265)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "3319 A1 Screen Level (%)") +
  ggtitle("Car Park Llangyfelach Road Brynhyfryd 2E47265") +
  theme_minimal()

pdf("plots/llanfelach_all.pdf", height = 3)
llanfelach_all_points
dev.off()

# remove points outside sensible range and plot 
llanfelach_p_max <- llanfelach_max %>%
  filter(level <= 200, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "3319 A1 Screen Level (%)") +
  ggtitle("Car Park Llangyfelach Road Brynhyfryd 2E47265") +
  theme_minimal()

pdf("plots/llanfelach_p_max.pdf", height = 3)
llanfelach_p_max
dev.off()

# import mynydd cso data  
mynydd_cso <- read.csv("data/mynydd.csv") %>%
  mutate(date.time = dmy_hms(date.time))

mynydd_max <- mynydd_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80324), 
            min.level = min(X2E80324)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
mynydd_all_points <- mynydd_cso %>%
  filter(X2E80324 <= 500, X2E80324 >= 0) %>%
  ggplot(aes(date.time, X2E80324)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81168 Level (%)") +
  ggtitle("Mynydd Garnllwydd CSO 2E80324") +
  theme_minimal()

pdf("plots/mynydd_all.pdf", height = 3)
mynydd_all_points
dev.off()

# remove points outside sensible range and plot 
mynydd_p_max <- mynydd_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81168 Level (%)") +
  ggtitle("Mynydd Garnllwydd CSO 2E80324") +
  theme_minimal()

pdf("plots/mynydd_p_max.pdf", height = 3)
mynydd_p_max
dev.off()

# import gwyrossdd cso data  
gwyrossdd_cso <- read.csv("data/gwyrossdd.csv") %>%
  mutate(date.time = dmy_hms(date.time))

gwyrossdd_max <- gwyrossdd_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80291), 
            min.level = min(X2E80291)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
gwyrossdd_all_points <- gwyrossdd_cso %>%
  filter(X2E80291 <= 500, X2E80291 >= 0) %>%
  ggplot(aes(date.time, X2E80291)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81142 Level (%)") +
  ggtitle("Swansea Heol Gwyross R/O No 61 Pt 52 2E80291") +
  theme_minimal()

pdf("plots/gwyrossdd_all.pdf", height = 3)
gwyrossdd_all_points
dev.off()

# remove points outside sensible range and plot 
gwyrossdd_p_max <- gwyrossdd_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "81142 Level (%)") +
  ggtitle("Swansea Heol Gwyross R/O No 61 Pt 52 2E80291") +
  theme_minimal()

pdf("plots/gwyrossdd_p_max.pdf", height = 3)
gwyrossdd_p_max
dev.off()

# import landore_neath cso data  
landore_neath_cso <- read.csv("data/landore_neath.csv") %>%
  mutate(date.time = dmy_hms(date.time))

landore_neath_max <- landore_neath_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E47235), 
            min.level = min(X2E47235)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
landore_neath_all_points <- landore_neath_cso %>%
  filter(X2E47235 <= 200, X2E47235 >= 0) %>%
  ggplot(aes(date.time, X2E47235)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "3328 A1 Screen Level (%)") +
  ggtitle("Landore Neath Road CSO 2E47235") +
  theme_minimal()

pdf("plots/landore_neath.pdf", height = 3)
landore_neath_all_points
dev.off()

# remove points outside sensible range and plot 
landore_neath_p_max <- landore_neath_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "3328 A1 Screen Level (%)") +
  ggtitle("Landore Neath Road CSO 2E47235") +
  theme_minimal()

pdf("plots/landore_neath_p_max.pdf", height = 3)
landore_neath_p_max
dev.off()

# import eaton_road cso data  
eaton_road_cso <- read.csv("data/eaton_road.csv") %>%
  mutate(date.time = dmy_hms(date.time))

eaton_road_max <- eaton_road_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E80724), 
            min.level = min(X2E80724)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
eaton_road_all_points <- eaton_road_cso %>%
  filter(X2E80724 <= 500, X2E80724 >= 0) %>%
  ggplot(aes(date.time, X2E80724)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Chamber Level (%)") +
  ggtitle("48 Eaton Road CSO Swansea 2E80724") +
  theme_minimal()

pdf("plots/eaton_road.pdf", height = 3)
eaton_road_all_points
dev.off()

# remove points outside sensible range and plot 
eaton_road_p_max <- eaton_road_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "Chamber Level (%)") +
  ggtitle("48 Eaton Road CSO Swansea 2E80724") +
  theme_minimal()

pdf("plots/eaton_road_p_max.pdf", height = 3)
eaton_road_p_max
dev.off()

cwmbwrla_max <- cwmbwrla_cso %>%
  mutate(date.time = ymd_hms(date.time)) %>%
  mutate(daily = as_date(date.time)) %>%
  group_by(daily) %>%
  summarise(max.level = max(X2E47301), 
            min.level = min(X2E47301)) %>%
  gather(key = "status", value = "level", max.level:min.level)

# remove points outside sensible range and plot 
cwmbwrla_p_max <- cwmbwrla_max %>%
  filter(level <= 500, level >= 0) %>%
  ggplot(aes(daily, level)) +
  geom_point(size = 0.25) +
  labs(x = "Date", y = "3330 A5 Screen Level (%)") +
  ggtitle("Cwmbwrla CSO 2E47301") +
  theme_minimal()

pdf("plots/cwmbwrla_p_max.pdf", height = 3)
cwmbwrla_p_max
dev.off()
