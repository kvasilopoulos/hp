source("01-oecd.R")
library(exuber)

# create possibly functions
poss_diagnostics <- possibly(diagnostics, otherwise = NA)
poss_datestamp <- possibly(datestamp, otherwise = NA)
poss_autoplot <- possibly(autoplot, otherwise = NA)
poss_fortify <- possibly(fortify, otherwise = NA)

gauto <- function(x) autoplot(x) + ggtitle("")

# Prices ------------------------------------------------------------------

radf_price <- plot_price  <- list()
for (cname in colnames(price)[-1]) {

  radf_price[[cname]] <- radf(price %>%
                           select(Date, cname) %>%
                           filter(complete.cases(.)))
  
  plot_price[[cname]] <- ggplot(data = (price %>% select(Date, cname) %>%
                                          na.omit()),
                                aes_string("Date", as.name(cname))) +
    geom_line() + xlab("") + ylab("") + theme_bw() + ggtitle("")
  
}

summary_price <- map(radf_price, summary)

accepted_price <- map(radf_price, poss_diagnostics) %>% 
  map("accepted") %>% 
  reduce(c)

datestamp_price <- map(radf_price, poss_datestamp)

autoplot_price <- radf_price[accepted_price] %>% 
  map(~ autoplot(.) + ggtitle(""))

# autoplot_price[price_accepted] %>% 
#   ggarrange()

# map(datestamp_price[price_accepted], poss_fortify) %>% 
#   reduce(rbind) %>% 
#   autoplot()

# Income ------------------------------------------------------------------


radf_income <- plot_income <- list()
for (cname in colnames(income)[-1]) {
  
  radf_income[[cname]] <- radf(income %>%
                             select(Date, cname) %>%
                             filter(complete.cases(.)))
  plot_income[[cname]] <- ggplot(data = (income %>% select(Date, cname) %>% 
                                          na.omit()), 
                                aes_string("Date", as.name(cname))) + 
    geom_line() + xlab("") + ylab("") + theme_bw()
  
}


summary_income <- map(radf_income, summary)

accepted_income <- map(radf_income, poss_diagnostics) %>% 
  map("accepted") %>% 
  reduce(c)

datestamp_income <- map(radf_income, poss_datestamp)

autoplot_income <- radf_income[accepted_income] %>% 
  map(~ autoplot(.) + ggtitle(""))


# Rent --------------------------------------------------------------------


radf_rent <- plot_rent <- list()
for (cname in colnames(rent)[-1]) {
  
  radf_rent[[cname]] <- radf(rent %>%
                             select(Date, cname) %>%
                             filter(complete.cases(.)))
  plot_rent[[cname]] <-  ggplot(data = (rent %>% select(Date, cname) %>% 
                                          na.omit()), 
                                aes_string("Date", as.name(cname))) + 
    geom_line() + xlab("") + ylab("") + theme_bw()
  
}

summary_rent <- map(radf_rent, summary)

accepted_rent <- map(radf_rent, poss_diagnostics) %>% 
  map("accepted") %>% 
  reduce(c)

datestamp_rent <- map(radf_rent, poss_datestamp)

autoplot_rent <- radf_rent[accepted_rent] %>% 
  map(~ autoplot(.) + ggtitle(""))


# gtitle <- function(...) {
#     map(..., ~.x + ggtitle(""))
  # }

# accepted ----------------------------------------------------------------

countries_accepted <- intersect(accepted_price, accepted_income) %>% 
  intersect(accepted_rent)


# data export -------------------------------------------------------------

estimation_price <- radf_price %>%
  map(poss_fortify) %>% 
  map(`[`, c(1,2)) %>% 
  map(as.tibble) %>% 
  `[`(accepted_price) %>% 
  reduce(dplyr::full_join) %>% 
  setNames(c("Date", accepted_price))

estimation_income <- radf_income %>%
  map(poss_fortify) %>% 
  map(`[`, c(1,2)) %>% 
  map(as.tibble) %>% 
  `[`(accepted_income) %>% 
  reduce(dplyr::full_join) %>% 
  setNames(c("Date", accepted_income))

estimation_rent <- radf_rent %>%
  map(poss_fortify) %>% 
  map(`[`, c(1,2)) %>% 
  map(as.tibble) %>% 
  `[`(accepted_rent) %>% 
  reduce(dplyr::full_join) %>% 
  setNames(c("Date", accepted_rent))


# CV export ---------------------------------------------------------------

ncv <- price %>% 
  is.na() %>% 
  colSums() %>% 
  unique() %>% 
  sort() %>% 
  `[`(-c(1,2))

cv_seq <- crit[ncv] %>% 
  map("bsadf_cv") %>% 
  map(as.tibble) %>% 
  map(`[`, 2)

list_df <- lapply(cv_seq, function(x) {
  na.count <- ncv[length(ncv)] - nrow(x)
  if (na.count > 0L) {
    na.dm <- matrix(NA, na.count, ncol(x))
    colnames(na.dm) <- colnames(x)
    rbind(x, na.dm)
  } else {
    x
  }
})

cv_seq <- list_df %>% 
  map( cbind) %>% 
  reduce(cbind) %>% 
  set_names(ncv) %>% 
  as.tibble()


adf <- crit[ncv] %>% 
  map(`[`, 1) %>% 
  unlist(recursive = FALSE) %>% 
  map(2) %>% 
  reduce(c)
sadf <- crit[ncv] %>% 
  map(`[`, 2) %>% 
  unlist(recursive = FALSE) %>% 
  map(2) %>% 
  reduce(c)
gsadf <- crit[ncv] %>% 
  map(`[`, 3) %>% 
  unlist(recursive = FALSE) %>% 
  map(2) %>% 
  reduce(c)

cv <- rbind(adf, sadf, gsadf) %>% 
  as.data.frame() %>% 
  set_names( ncv)
rownames(cv) <- c("ADF", "SADF", "GSADF")



# Save everything in Rds --------------------------------------------------



store <- c((items <- c("price", "income", "rent")),
  c("countries_accepted", "cv", "cv_seq"),
  paste0("summary_", items),
  paste0("datestamp_", items),
  paste0("estimation_", items),
  paste0("plot_", items),
  paste0("autoplot_", items))

path_store <- paste0("data/", store, ".rds")

for (i in seq_along(store)) saveRDS(get(store[i]), file = path_store[i])




