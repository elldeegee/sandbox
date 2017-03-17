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

## Cohort Table Based On Year Registered
yearly_sign_ups <- sign_ups %>%
  group_by(year, region) %>% summarise(n_sign_ups = n())

cohort_purchases <- inner_join(yearly_sign_ups,
                               cohort.purchases(purchases, t_period = "year")) %>% ungroup() %>%
  mutate(per_sign_purchase = round(purchasers_from_cohort/n_sign_ups*100,0),
         n_purchase_per_id = round(purchases_from_cohort/purchasers_from_cohort,2))

cohort_purchases_chart <- ggplot(data = cohort_purchases,
                           mapping = aes(x = year, fill = region)) +
  geom_bar(aes(y = per_sign_purchase, group = region), stat = "identity", show.legend = FALSE) +
  geom_line(aes(y = n_purchase_per_id*5, group = region), stat = "identity") +
  geom_text(aes(y = per_sign_purchase + 3, label = per_sign_purchase, vjust = 0)) +
  geom_text(aes(y = n_purchase_per_id*5+3, label = n_purchase_per_id, vjust = 0)) +
  scale_y_continuous("Percent of Sign Ups Who Purchase In The Same Year (%)",
                     breaks = c(0,25,50,75,100), 
                     labels = c(0,25,50,75,100),
                     sec.axis = sec_axis(~./5,
                                         name = "Purchases Per Consumer")) +
  labs(title = "Cohort Activity, By Region",
       x = "Year") +
  labs(fill = NULL) +
  facet_wrap(~region, ncol = 5, scales = "fixed") +
  theme(plot.title = element_text(hjust = 0, vjust = 0)) +
  theme_bw()
cohort_purchases_chart