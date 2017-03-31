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

## Purchases By Gender
purchases_by_gender <- purchases %>%
  group_by(gender) %>% 
  summarise(Cones = n(), Scoops = sum(n_scoops), `Customers` = n_distinct(id)) %>% 
  ungroup %>% gather(metric, value, -gender)

purchases_by_gender <- ggplot(data = purchases_by_gender,
                           mapping = aes(x = metric, fill = gender)) +
  geom_bar(aes(y = value, group = metric), stat = "identity", position = "fill") +
  scale_y_continuous(breaks = seq(0,1,by=.25), 
                     labels = paste0(seq(0,100,by=25),"%")) +
  labs(title = "Purchases By Gender",
       x = NULL,
       y = "% of Purchases",
       fill = "Gender") +
  theme(plot.title = element_text(hjust = 0, vjust = 0)) +
  theme_bw()
purchases_by_gender
