## Add packages
library(lubridate)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(readr)
require(stats)

## Flags
WORKDIR <- ("~/Documents/Repositories/sandbox/src/")
set.seed(40)

## Create sign_ups data set
#first names
z_babynames <- read_csv(paste0(WORKDIR,"yob2015.txt"), 
                        col_names = FALSE, 
                        col_types = list(col_character(),col_character(),col_integer())) %>% 
  select(name = X1, gender = X2, number = X3) %>% 
  mutate(gender = if_else(gender == "F", "Female", "Male")) %>% filter(number > 100)

baby_names <-rbind(z_babynames,z_babynames,z_babynames,z_babynames,z_babynames) %>% 
  as.data.frame()

#last names
last_names <- read_csv(paste0(WORKDIR,"lastnames.csv")) %>% 
  filter(Rank <= 5) %>% 
  select(Name)
last_names <- rep_len(last_names$Name, length.out = nrow(baby_names))

# regions
regions <- c("Northville","Southville","Eastville","Westville","Center City")
five_numbers <- sample(1:5)
five_numbers <- c(4,2,5,3,1)
regions <- rep_len(rep(regions,five_numbers), length.out = nrow(baby_names))

# dates
date_born <- rep_len(as.character(sample(seq(as.Date('1950/01/01'), as.Date('1999/12/31'), by="day"), replace = TRUE)),
                     length.out = nrow(baby_names))

date_signed_up <- c(rep_len(as.character(sample(seq(as.Date('2011/01/01'), as.Date('2011/12/31'), by="day"), replace = TRUE)),
                          length.out = nrow(baby_names)*.1),
                    rep_len(as.character(sample(seq(as.Date('2012/01/01'), as.Date('2012/12/31'), by="day"), replace = TRUE)),
                            length.out = nrow(baby_names)*.2),
                    rep_len(as.character(sample(seq(as.Date('2013/01/01'), as.Date('2013/12/31'), by="day"), replace = TRUE)),
                            length.out = nrow(baby_names)*.3),
                    rep_len(as.character(sample(seq(as.Date('2014/01/01'), as.Date('2014/12/31'), by="day"), replace = TRUE)),
                            length.out = nrow(baby_names)*.4))

# create unique identifier
id <- c(1:nrow(baby_names))

# making sign ups table
sign_ups <- cbind(baby_names,data_frame(id, last_names, regions, date_born, date_signed_up)) %>% 
  as.data.frame() %>% ungroup() %>%
  mutate(last_name = as.character(last_names),
         date_born = as.Date(date_born),
         date_signed_up = as.Date(date_signed_up),
         age_at_sign_up = round(as.numeric(date_signed_up - date_born)/365,0)) %>%
  select(id, first_name = name, last_name,gender, region = regions, date_born, date_signed_up, age_at_sign_up) %>% 
  ungroup()

## Creating purchases data set
n_purchases <- rep_len(sample(0:9, 50, replace = TRUE),length.out = nrow(sign_ups))

purchases <- cbind(sign_ups,n_purchases = n_purchases) %>% as.data.frame() %>% filter(n_purchases > 0)

n_purchases_new <- purchases$n_purchases
first_name <- rep(purchases$first_name,n_purchases_new)
last_name <- rep(purchases$last_name,n_purchases_new)
gender <- rep(purchases$gender,n_purchases_new)
region <- rep(purchases$region,n_purchases_new)

purchases <- inner_join(data_frame(first_name,last_name,gender,region),
                            sign_ups %>% select(-age_at_sign_up,-date_born))

n_scoops <- rep_len(sample(1:3, 60, replace = TRUE),length.out = nrow(purchases))
flavor <- rep_len(c("Vanilla","Chocolate","Strawberry","Coffee"),length.out = nrow(purchases))
days_between <- rep_len(sample(0:100, 70),length.out = nrow(purchases))

purchases <- cbind(purchases,data_frame(n_scoops,flavor),days_between) %>%
  mutate(date_purchased = date_signed_up + days_between) %>%
  filter(date_purchased < as.Date("2014-12-31")) %>%
  select(id, first_name,last_name,gender,region,date_purchased,n_scoops,flavor) %>% arrange(date_purchased)

## Writing tables:
#write_csv(sign_ups,paste0(WORKDIR,"sign_ups.csv"))
#write_csv(purchases,paste0(WORKDIR,"purchases.csv"))

