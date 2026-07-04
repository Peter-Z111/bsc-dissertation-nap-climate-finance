
# Datasets and Libraries --------------------------------------------------
library(tidyverse)
library(tidyr)
library(texreg)
library(dplyr)
library(fixest)
library(bacondecomp)
library(panelView)
library(did)
library(countrycode)
library(mice)
library(fect)
library(kableExtra)
library(sjPlot)
library(sjmisc)
library(modelsummary)
library(stargazer)
library(broom)
library(stringr)
library(forcats)

cfu <- read.csv("Data/cfu_final.csv")

# Run Result --------------------------------------------------------------

# Inspect panelview: 
panelview(funding_ad ~ treat, data = cfu, index = c("Country","Year"), xlab = "Year", 
          ylab = "Country", display.all = F, axis.lab = "time", 
          gridOff = T, by.timing = TRUE, main = "")

# Logged ad as outcome: 
log_ad_treat <- feols(log_funding_ad ~ treat | Country + Year, data = cfu, 
                      cluster = "Country")
summary(log_ad_treat)

log_ad_control <- feols(
  log_funding_ad ~ treat + vul + ins + pol | Country + Year, 
  data = cfu, cluster = "Country"
)
summary(log_ad_control)

log_ad_specific <- feols(
  log_funding_ad ~ treat + vul + ins + pol + Year:factor(Country) | 
    Country + Year, data = cfu, cluster = ~ Country
)
summary(log_ad_specific)

# Logged multi as outcome: 
log_multi_treat <- feols(log_funding_multi ~ treat | Country + Year, data = cfu, 
                         cluster = "Country")
summary(log_multi_treat)

log_multi_control <- feols(
  log_funding_multi ~ treat + vul + ins + pol | 
    Country + Year, data = cfu, cluster = "Country"
)
summary(log_multi_control)

log_multi_specific <- feols(
  log_funding_multi ~ treat + vul + ins + pol + 
    Year:factor(Country) | Country + Year, data = cfu, cluster = ~ Country
)
summary(log_multi_specific)

# Logged miti as outcome: 
log_miti_treat <- feols(log_funding_miti ~ treat | Country + Year, data = cfu, 
                        cluster = "Country")
summary(log_miti_treat)

log_miti_control <- feols(
  log_funding_miti ~ treat + vul + ins + pol | 
    Country + Year, data = cfu, cluster = "Country"
)
summary(log_miti_control)

log_miti_specific <- feols(
  log_funding_miti ~ treat + vul + ins + pol + 
    Year:factor(Country) | Country + Year, data = cfu, cluster = ~ Country
)
summary(log_miti_specific)

# ad as outcome: 
ad_treat <- feols(funding_ad ~ treat | Country + Year, data = cfu, 
                  cluster = "Country")
summary(ad_treat)

ad_control <- feols(
  funding_ad ~ treat + vul + ins + pol | 
    Country + Year, data = cfu, cluster = "Country"
)
summary(ad_control)

ad_specific <- feols(
  funding_ad ~ treat + vul + ins + pol + 
    Year:factor(Country) | Country + Year, data = cfu, cluster = ~ Country
)
summary(ad_specific)

# multi as outcome: 
multi_treat <- feols(funding_multi ~ treat | Country + Year, data = cfu, 
                     cluster = "Country")
summary(multi_treat)

multi_control <- feols(
  funding_multi ~ treat + vul + ins + pol | 
    Country + Year, data = cfu, cluster = "Country"
)
summary(multi_control)

multi_specific <- feols(
  funding_multi ~ treat + vul + ins + pol + 
    Year:factor(Country) | Country + Year, data = cfu, cluster = ~ Country
)
summary(multi_specific)


# Visualisation -----------------------------------------------------------

# ad: 
all_models_ad <- list(
  "(1) Treat only"  = ad_treat, 
  "(2) + controls"   = ad_control, 
  "(3) + trends"  = ad_specific, 
  "(4) Treatment only"  = log_ad_treat, 
  "(5) + controls"   = log_ad_control, 
  "(6) + trends"  = log_ad_specific
)

cm_ad <- c(
  "treat" = "NAP submitted",
  "vul"   = "Climate vulnerability",
  "ins"   = "Institutional capacity",
  "pol"   = "Political stability", 
  "Year:factor(Country)Afghanistan" = "Afghanistan", 
  "Year:factor(Country)Albania" = "Albania", 
  "Year:factor(Country)Algeria" = "Algeria"
)

tab_ad <- modelsummary(
  all_models_ad,
  coef_map  = cm_ad,
  gof_map   = c("nobs", "r.squared"),
  stars     = TRUE,
  title     = "Effect of NAP submission on adaptation funding from MCFs",
  output    = "kableExtra", 
  escape    = FALSE
)

tab_ad %>% 
  add_header_above(c(" " = 1, "Adaptation funding in USD million" = 3, 
                     "Logged adaptation funding" = 3))

# multi: 
all_models_multi <- list(
  "(1) Treatment only"  = multi_treat, 
  "(2) + controls"   = multi_control, 
  "(3) + trends"  = multi_specific, 
  "(4) Treatment only"  = log_multi_treat, 
  "(5) + controls"   = log_multi_control, 
  "(6) + trends"  = log_multi_specific
)

cm_multi <- c(
  "treat" = "NAP submitted",
  "vul"   = "Climate vulnerability",
  "ins"   = "Institutional capacity",
  "pol"   = "Political stability", 
  "Year:factor(Country)Afghanistan" = "Afghanistan", 
  "Year:factor(Country)Albania" = "Albania", 
  "Year:factor(Country)Algeria" = "Algeria"
)

tab_multi <- modelsummary(
  all_models_multi,
  coef_map  = cm_multi,
  gof_map   = c("nobs", "r.squared"),
  stars     = TRUE,
  title     = "Effect of NAP submission on overlap funding from MCFs",
  output    = "kableExtra", 
  escape = FALSE
)

tab_multi %>% 
  add_header_above(c(" " = 1, "Overlap funding in USD million" = 3, 
                     "Logged overlap funding" = 3))

# Robustness Check --------------------------------------------------------
# Goodman-Bacon Decomposition: 
bacon_out_ad <- bacon(funding_ad ~ treat, data = cfu, 
                      id_var = "Country", time_var = "Year")

bacon_out_log <- bacon(log_funding_ad ~ treat, data = cfu, 
                       id_var = "Country", time_var = "Year")

bacon_df <- tibble::tibble(
  Comparison      = c(
    "Earlier vs Later Treated",
    "Later vs Earlier Treated",
    "Treated vs Untreated"
  ),
  Weight = c(0.14945, 0.04494, 0.80561),
  `Avg. Estimate` = c(2.50959, 5.32677, 2.07750)
)

bacon_df %>%
  mutate(
    Weight = round(Weight, 3),
    `Avg. Estimate` = round(`Avg. Estimate`, 3)
  ) %>%
  kbl(
    caption = "Goodman–Bacon weighting decomposition",
    booktabs = TRUE,
    align = c("l", "r", "r"),
    digits = 3
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  column_spec(1, bold = TRUE)

# Event Study Plot: 
cfu$time_to_treat <- get.cohort(data = cfu, D = "treat", 
                                       index = c("Country", "Year"))$Time_to_Treatment
cfu$Year.Submitted <- ifelse(is.na(cfu$Year.Submitted), Inf, cfu$Year.Submitted)
cfu$time_to_treat <- ifelse(is.na(cfu$time_to_treat), -Inf, cfu$time_to_treat)

evt_ad <- feols(
  funding_ad ~ sunab(Year.Submitted, time_to_treat) | Country + Year,
  data = cfu, vcov = ~Country
)

iplot(
  evt_ad,
  ci        = TRUE, 
  ci.col    = "steelblue4",
  ci.lty    = 1, 
  ci.lwd    = 1.2, 
  col       = "steelblue4", 
  pch       = 16, 
  lwd       = 2, 
  cex       = 1.0, 
  ref.line  = TRUE, 
  ref.col   = "gray50", 
  ref.lty   = 2, 
  ref.lwd   = 1.5, 
  xlab      = "Years to NAP submission",
  main      = "Effect on adaptation funding",
  grid      = TRUE
)

evt_log <- feols(
  log_funding_ad ~ sunab(Year.Submitted, time_to_treat) | Country + Year,
  data = cfu, vcov = ~Country
)

iplot(
  evt_log,
  ci        = TRUE, 
  ci.col    = "steelblue4",
  ci.lty    = 1, 
  ci.lwd    = 1.2, 
  col       = "steelblue4", 
  pch       = 16, 
  lwd       = 2, 
  cex       = 1.0, 
  ref.line  = TRUE, 
  ref.col   = "gray50", 
  ref.lty   = 2, 
  ref.lwd   = 1.5, 
  xlab      = "Years to NAP submission",
  main      = "Effect on logged adaptation funding",
  grid      = TRUE
)

evt_multi <- feols(
  funding_multi ~ sunab(Year.Submitted, time_to_treat) | Country + Year,
  data = cfu, vcov = ~Country
)

iplot(
  evt_multi,
  ci        = TRUE, 
  ci.col    = "steelblue4",
  ci.lty    = 1, 
  ci.lwd    = 1.2, 
  col       = "steelblue4", 
  pch       = 16, 
  lwd       = 2, 
  cex       = 1.0, 
  ref.line  = TRUE, 
  ref.col   = "gray50", 
  ref.lty   = 2, 
  ref.lwd   = 1.5, 
  xlab      = "Years to NAP submission",
  main      = "Effect on overlap funding",
  grid      = TRUE
)

evt_log_multi <- feols(
  log_funding_multi ~ sunab(Year.Submitted, time_to_treat) | Country + Year,
  data = cfu, vcov = ~Country
)

iplot(
  evt_log_multi,
  ci        = TRUE, 
  ci.col    = "steelblue4",
  ci.lty    = 1, 
  ci.lwd    = 1.2, 
  col       = "steelblue4", 
  pch       = 16, 
  lwd       = 2, 
  cex       = 1.0, 
  ref.line  = TRUE, 
  ref.col   = "gray50", 
  ref.lty   = 2, 
  ref.lwd   = 1.5, 
  xlab      = "Years to NAP submission",
  main      = "Effect on logged overlap funding",
  grid      = TRUE
)
