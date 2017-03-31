## date functions
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

## analyses functions
cohort.purchases <- function(df, t_period = "year_month") {
  gb_dots <- lapply(c("region",t_period), as.symbol)
  ar_dots <- lapply(c("region","id",t_period), as.symbol)
  cohort_purhchases <- inner_join(sign_ups %>% ungroup() %>% select_(.dots = ar_dots),
                                  df %>% ungroup() %>% select_(.dots = ar_dots)) %>%
    ungroup() %>% group_by_(.dots = gb_dots) %>%
    summarise(purchasers_from_cohort = n_distinct(id), purchases_from_cohort = n())
  
  cohort_purhchases
}

popular.flavor <- function(df, c_var = NULL) {
  gb_dots <- lapply(c("flavor",c_var), as.symbol)
  ar_dots <- lapply(c_var, as.symbol)
  x <- df %>% group_by_(.dots = gb_dots) %>%
    summarise(n_cones = n(),
              scoops = sum(n_scoops), 
              med_scoops = median(n_scoops),
              avg_scoops = round(mean(n_scoops),1)) %>% 
    ungroup() %>% group_by_(.dots = ar_dots) %>%
    mutate(perc_scoop = round(100*scoops/sum(scoops),1),
           perc_cone = round(100*n_cones/sum(n_cones),1))

}