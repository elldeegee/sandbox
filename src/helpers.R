## load functions
year.month <- function(x){
  mo <- lubridate::month(x)
  yr <- lubridate::year(x)
  year_month <- ifelse(is.na(mo),NA,paste(yr,ifelse(nchar(mo) == 1,
                                                    paste0("0",mo),mo),sep = "_"))
  year_month
}

date.segments <- function(df){
  df <- df %>%
    mutate(
      month = lubridate::month(date),
      year = lubridate::year(date),
      year_month =  year.month(date),
      quarter = lubridate::quarter(date),
      yearquarter = paste0(year,"-Q",quarter)
    )
  df
}

cohort.purchases <- function(df, t_period = "year_month") {
  gb_dots <- lapply(c("region",t_period), as.symbol)
  ar_dots <- lapply(c("region","id",t_period), as.symbol)
  cohort_purhchases <- inner_join(sign_ups %>% ungroup() %>% select_(.dots = ar_dots),
                                  df %>% ungroup() %>% select_(.dots = ar_dots)) %>%
    ungroup() %>% group_by_(.dots = gb_dots) %>%
    summarise(purchasers_from_cohort = n_distinct(id), purchases_from_cohort = n())
  
  cohort_purhchases
}

