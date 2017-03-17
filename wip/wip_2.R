## Add packages
library(lubridate)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(readr)
require(stats)
library(ggplot2)
library(scales)
library(ggpubr)

## Flags
WORKDIR <- ("~/Documents/Repositories/sandbox/")
src <- "src/"

## Load helpers & data
source(paste0(WORKDIR,src,"helpers.R"))
source(paste0(WORKDIR,src,"creating_tbls.R"))

## Add date helpers
sign_ups <- sign_ups %>% rename(date = date_signed_up) %>% ungroup() %>% date.segments()
purchases <- purchases %>% rename(date = date_purchased) %>% ungroup() %>% date.segments()

## Sign Ups Per Month, By Region
monthly_sign_ups <- sign_ups %>%
  group_by(year_month, month, region) %>% summarise(n_sign_ups = n())

monthly_sign_ups_chart <- ggplot(data = monthly_sign_ups,
                                 mapping = aes(x = year_month, y = n_sign_ups, fill = region)) +
  geom_bar(aes(group = region), stat = "identity") +
  labs(title = "New Sign-Ups By Month, By Region",
       x = NULL,
       y = NULL) +
  labs(fill = "Region") +
  scale_x_discrete(labels = c("J","F","M","A","M","J","J","A","S","O","N","D",
                              "J","F","M","A","M","J","J","A","S","O","N","D",
                              "J","F","M","A","M","J","J","A","S","O","N","D",
                              "J","F","M","A","M","J","J","A","S","O","N","D")) +
  theme(plot.title = element_text(hjust = 0, vjust = 0)) +
  theme_bw()
monthly_sign_ups_chart

## Purchases Per Year, By Region
yearly_purchases <- purchases %>%
  group_by(year, region) %>% 
  summarise(cones = n(), scoops = sum(n_scoops)) %>% 
  ungroup %>% gather(metric, value, -year, -region)

yearly_purchases <- ggplot(data = yearly_purchases,
                           mapping = aes(x = year, fill = metric)) +
  geom_bar(aes(y = value, group = metric), stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(0,25000,by=5000), 
                     labels = comma(seq(0,25000,by=5000))) +
  labs(title = "Purchases By Year, By Region",
       x = "Year",
       y = "# of Purchases",
       fill = "Metric") +
  facet_wrap(~region, ncol = 5, scales = "fixed") +
  theme(plot.title = element_text(hjust = 0, vjust = 0)) +
  theme_bw()
yearly_purchases


## Most Popular Flavor & Average Scoops Per Cone
purchases_by_flavor <- purchases %>% group_by(flavor) %>% 
                       summarise(n_cones = n(),
                                 scoops = sum(n_scoops), 
                                 med_scoops = median(n_scoops),
                                 avg_scoops = round(mean(n_scoops),1)) %>% 
  ungroup() %>% 
  mutate(perc_scoop = round(100*scoops/sum(scoops),1),
         perc_cone = round(100*n_cones/sum(n_cones),1))

## Most Popular Flavor
flavor_scoop_cone <- purchases_by_flavor %>% 
  select(flavor,perc_scoop,perc_cone) %>%
  gather(metric, value, -flavor) %>% ungroup()

flavor_scoop_cone_chart <- ggplot(flavor_scoop_cone, aes(x = metric, y = value, fill = flavor)) +
  geom_bar(aes(group = flavor), stat = "identity", position = "dodge") +
  geom_text(aes(y = value +1, label = paste0(round(value,0),"%")), stat = "identity", position = position_dodge(.9)) +
  scale_y_continuous(breaks = seq(0,40, by = 5), labels = paste0(seq(0,40, by = 5),"%")) +
  scale_x_discrete(labels = c("Cones Sold By Flavor","Scoops Sold By Flavor")) +
  labs(title = "Most Popular Flavor",
       y = "Percent of Total Sold",
       x = NULL,
       fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal")
flavor_scoop_cone_chart

## Total Scoops & Scoops Per Cone
cumm_scoops_chart <- ggplot(purchases_by_flavor, aes(x = flavor)) +
  geom_bar(aes(y = scoops), stat = "identity", fill = "#2980B9") +
  geom_text(aes(y = scoops + 2500, label = paste0(comma(scoops),"
scoops")), stat = "identity") +
  geom_label(aes(y = 1300, label = paste0(avg_scoops," per cone")), stat = "identity", fill = "#F7FE0E")  +
  scale_y_continuous(breaks = seq(0,50000, by = 10000), labels = comma(seq(0,50000, by = 10000))) +
  labs(title = "Scoops, By Ice Cream Flavor",
       x = "Flavor",
       y = "Number of Scoops") +
  theme_bw()
cumm_scoops_chart