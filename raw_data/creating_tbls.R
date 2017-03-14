## Add packages
library(lubridate)
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(readr)
require(stats)

## Set Working Directory
WORKDIR <- ("~/Documents/Repositories/sandbox/raw_data/")

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
z_lastnames <- read_csv(paste0(WORKDIR,"lastnames.csv")) %>% 
  filter(Rank <= 5) %>% 
  select(Name)

# regions
z_regions <- c("Northville","Southville","Eastville","Westville","Center City")
five_numbers <- sample(1:5)
five_numbers <- c(4,2,5,3,1)

# dates
z_date_born <- as.character(sample(seq(as.Date('1950/01/01'), as.Date('2000/01/01'), by="day"), 15000))
z_date_signed_up <- as.character(sample(seq(as.Date('2011/01/01'), as.Date('2015/01/01'), by="day")))

# making sign ups table
last_names <- rep_len(z_lastnames$Name, length.out = nrow(baby_names))
regions <- rep_len(rep(z_regions,five_numbers), length.out = nrow(baby_names))
date_born <- rep_len(z_date_born, length.out = nrow(baby_names))
date_signed_up <- rep_len(z_date_signed_up, length.out = nrow(baby_names))


sign_ups <- cbind(baby_names,data_frame(last_names, regions, date_born, date_signed_up)) %>% 
  as.data.frame() %>% ungroup() %>%
  mutate(last_name = as.character(last_names),
         date_born = as.Date(date_born),
         date_signed_up = as.Date(date_signed_up),
         age_at_sign_up = round(as.numeric(date_signed_up - date_born)/365,0)) %>%
  select(first_name = name, last_name,gender, region = regions, date_born, date_signed_up, age_at_sign_up) %>% 
  ungroup()


## Creating purchases data set
n_purchases <- rep_len(sample(0:9, 50, replace = TRUE),length.out = nrow(sign_ups))

purchase_summary <- cbind(sign_ups,n_purchases = n_purchases) %>% as.data.frame()

purchases <- purchase_summary %>% 
  select(first_name,last_name,gender,region,date_signed_up,n_purchases) %>%
  filter(n_purchases > 0)

n_purchases_new <- purchases$n_purchases
first_name <- rep(purchases$first_name,n_purchases_new)
last_name <- rep(purchases$last_name,n_purchases_new)
gender <- rep(purchases$gender,n_purchases_new)
region <- rep(purchases$region,n_purchases_new)

purchases_tbl <- inner_join(data_frame(first_name,last_name,gender,region),purchase_summary %>% 
                              select(-n_purchases,-age_at_sign_up,-date_born))

n_scoops <- rep_len(sample(1:3, 60, replace = TRUE),length.out = nrow(purchases_tbl))
flavor <- rep_len(c("Vanilla","Chocolate","Strawberry","Coffee"),length.out = nrow(purchases_tbl))
order <- data_frame(n_scoops,flavor)
days_between <- rep_len(sample(0:100, 70),length.out = nrow(purchases_tbl))

purchases <- cbind(purchases_tbl,order,days_between) %>%
  mutate(date_purchased = date_signed_up + days_between) %>%
  select(first_name,last_name,gender,region,date_purchased,n_scoops,flavor)

## Writing tables:
write_csv(sign_ups,paste0(WORKDIR,"sign_ups.csv"))
write_csv(purchases,paste0(WORKDIR,"purchases.csv"))

