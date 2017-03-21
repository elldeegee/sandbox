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

## Most Popular Flavor (Overall)
flavor_scoop_cone <- popular.flavor(purchases) %>% 
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

## Total Scoops & Scoops Per Cone (Overall)
cumm_scoops <- popular.flavor(purchases)

cumm_scoops_chart <- ggplot(cumm_scoops, aes(x = flavor)) +
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

### By Region & Gender
## Most Popular Flavor (By Region)
flavor_scoop_cone_region <- popular.flavor(purchases, "region") %>% 
  select(flavor,perc_scoop,perc_cone, region) %>%
  gather(metric, value, -flavor, -region) %>% ungroup()

flavor_scoop_cone_region_chart <- ggplot(flavor_scoop_cone_region, aes(x = metric, y = value, fill = flavor)) +
  geom_bar(aes(group = flavor), stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(0,40, by = 5), labels = paste0(seq(0,40, by = 5),"%")) +
  scale_x_discrete(labels = c("Cones Sold","Scoops Sold")) +
  labs(title = "Most Popular Flavor, By Region",
       y = "Percent of Total Sold In Region (%)",
       x = NULL,
       fill = NULL) +
  facet_wrap(~region, ncol = 5, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal")
flavor_scoop_cone_region_chart

## Most Popular Flavor (By Gender)
flavor_scoop_cone_gender <- popular.flavor(purchases, "gender") %>% 
  select(flavor,perc_scoop,perc_cone, gender) %>%
  gather(metric, value, -flavor, -gender) %>% ungroup()

flavor_scoop_cone_gender_chart <- ggplot(flavor_scoop_cone_gender, aes(x = metric, y = value, fill = flavor)) +
  geom_bar(aes(group = flavor), stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(0,40, by = 5), labels = paste0(seq(0,40, by = 5),"%")) +
  scale_x_discrete(labels = c("Cones Sold","Scoops Sold")) +
  labs(title = "Most Popular Flavor, By Gender",
       y = "Percent of Total Sold to Gender (%)",
       x = NULL,
       fill = NULL) +
  facet_wrap(~gender, ncol = 2, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal")
flavor_scoop_cone_gender_chart

## Most Popular Flavor (By Gender - version 2)
flavor_scoop_cone_gender_2 <- purchases %>%
  group_by(flavor,gender) %>%
  summarise(n_cones = n(),
            scoops = sum(n_scoops)) %>% 
  ungroup() %>% group_by(flavor) %>%
  mutate(perc_scoop = round(100*scoops/sum(scoops),1),
         perc_cone = round(100*n_cones/sum(n_cones),1)) %>%
  select(flavor,perc_scoop,perc_cone, gender) %>%
  gather(metric, value, -flavor, -gender) %>% ungroup()

flavor_scoop_cone_gender_chart_2 <- ggplot(flavor_scoop_cone_gender_2, aes(x = metric, y = value, fill = gender)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(labels = c("Cones Sold","Scoops Sold")) +
  labs(title = "Most Popular Flavor, By Region",
       y = "Percent of Total Sold In Region (%)",
       x = NULL,
       fill = NULL) +
  facet_wrap(~flavor, ncol = 4, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal")
flavor_scoop_cone_gender_chart_2
