## Add packages
library(lubridate)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(readr)
require(stats)
library(ggplot2)
library(scales)

## Flags
WORKDIR <- ("~/Documents/Repositories/sandbox/")
src <- "src/"

## Load helpers & data
source(paste0(WORKDIR,src,"helpers.R"))
source(paste0(WORKDIR,src,"creating_tbls.R"))

## Add date helpers
sign_ups <- sign_ups %>% rename(date = date_signed_up) %>% ungroup() %>% date.segments()
purchases <- purchases %>% rename(date = date_purchased) %>% ungroup() %>% date.segments()

## MAP (Monthly Purchases) / YAP (Yearly Purchasers)
stickiness <- purchases %>% group_by(year_month,region) %>% 
  mutate(u_purchasers = n_distinct(id)) %>%
  ungroup() %>% group_by(year,region) %>% 
  summarise(n_purchases = n(), 
            MAP = mean(u_purchasers), 
            YAP = n_distinct(id)) %>% 
  mutate(`MAP/YAP` = round(MAP/YAP,3), 
         purchases_per_id_per_yr = round(n_purchases/YAP,3), 
         MAP = round(MAP, 2))

stickiness_chart <- ggplot(stickiness,
                           aes(x = year, y = `MAP/YAP`)) +
  geom_bar(aes(fill = region),stat = "identity", position = "dodge") +
  labs(title = "Stickiness, By Region",
       x = "Year",
       y = "Monthly Purchases / Yearly Purchasers") +
  labs(fill = "Region") +
  theme(plot.title = element_text(hjust = 0, vjust = 0)) +
  theme_bw()
stickiness_chart