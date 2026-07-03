
# Datasets and Libraries --------------------------------------------------
cfu <- read.csv("Data/cfu_baseline.csv")
nap_treat <- read.csv("Data/NAP.csv")
ndgain_vul <- read.csv("Data/ND-GAIN_Vul.csv")
ndgain_read <- read.csv("Data/ND-GAIN_Read.csv")
pol_st <- read.csv("Data/political_stability.csv")

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

## Add a countrycode column into cfu: 
cfu <- cfu %>%
  mutate(
    ISO3 = countrycode(
      sourcevar   = Country,          # the column with country names
      origin      = "country.name",   # how countrycode should interpret them
      destination = "iso3c"           # we want ISO3 codes (3-letter codes)
    )
  ) %>% 
  mutate(
    ISO3 = if_else(Country == "Micronesia", "FSM", ISO3)
  )

# Treatment and Control Variables ---------------------------------------
## Treatment: 
# Create cfu_10to24: 
cfu_10to24 <- cfu %>% 
  filter(Year >= 2010)

# Add a countrycode column into nap_treat: 
nap_treat <- nap_treat %>%
  mutate(
    ISO3 = countrycode(
      sourcevar   = Country,          # the column with country names
      origin      = "country.name",   # how countrycode should interpret them
      destination = "iso3c"           # we want ISO3 codes (3-letter codes)
    )
  )

# Merge based on ISO3: 
cfu_10to24 <- cfu_10to24 %>%
  left_join(
    select(nap_treat, Year.Submitted, ISO3), 
    by = c("ISO3")
  )

# Create treatment variable: 
cfu_10to24$treat <- ifelse(
  is.na(cfu_10to24$Year.Submitted), 
  0,  # Never treated countries: always 0
  ifelse(cfu_10to24$Year >= cfu_10to24$Year.Submitted, 1, 0)  # Treated from Year.Submitted onwards
)

cfu_10to24 <- arrange(cfu_10to24, Country, Year)

## Vulnerability: 
# Pivot from wide to long: 
ndgain_vul_long <- ndgain_vul %>% 
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year", 
    values_to = "vul"
  ) %>% 
  mutate(Year = as.numeric(sub("X", "", Year)))

# Merge based on ISO3: 
cfu_10to24 <- cfu_10to24 %>%
  left_join(
    ndgain_vul_long %>% 
      select(ISO3, Year, vul), 
    by = c("ISO3", "Year" = "Year")
  )

# NAs in vul: 
sum(is.na(cfu_10to24))
sum(is.na(cfu_10to24$Year.Submitted))
sum(is.na(cfu_10to24$vul))

# Multiple imputation: 
# 1. Define countries to EXCLUDE from imputation: 
exclude_countries <- c("Cook Islands", "Niue", "South Sudan", 
                       "St. Kitts and Nevis", "West Bank and Gaza")

# 2. Create a flag to identify rows to impute
cfu_10to24 <- cfu_10to24 %>%
  mutate(impute_flag = ifelse(Year %in% c(2023, 2024) & !(Country %in% exclude_countries), 1, 0))

# 3. Temporarily blank out `vul` in 2023–24 rows that should be imputed
cfu_10to24 <- cfu_10to24 %>%
  mutate(vul_for_impute = ifelse(impute_flag == 1, NA, vul))

# 4. Select variables for imputation model
# Lagged vulnerability: 
cfu_10to24 <- cfu_10to24 %>% 
  arrange(Country, Year) %>% 
  group_by(Country) %>% 
  mutate(vul_lag_1 = lag(vul, n = 1)) %>% 
  mutate(vul_lag_2 = lag(vul, n = 2)) %>% 
  ungroup()

cfu_10to24$Country_ID <- as.integer(as.factor(cfu_10to24$Country))

impute_data <- cfu_10to24 %>%
  select(Country_ID, Year, vul_for_impute, vul_lag_1, vul_lag_2)


# 5. Run mice 
meth <- make.method(impute_data)
pred <- make.predictorMatrix(impute_data)

pred[, "Country_ID"] <- -2  
meth["vul_for_impute"] <- "2l.norm"

imp <- mice(impute_data, m = 5, method = meth, predictorMatrix = pred, 
            seed = 2024)

# 6. Inspect the imputed values
summary(imp)
View(complete(imp, 1))

# 7. Plug in the completed data (e.g., from first imputed set)
cfu_10to24$vul <- complete(imp, 1)$vul_for_impute
cfu_10to24 <- cfu_10to24 %>%
  mutate(vul = ifelse(Country %in% exclude_countries, 
                                NA, vul))

## Readiness 
# Pivot from wide to long: 
ndgain_read_long <- ndgain_read %>% 
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year", 
    values_to = "read"
  ) %>% 
  mutate(Year = as.numeric(sub("X", "", Year)))

# Merge based on ISO3: 
cfu_10to24 <- cfu_10to24 %>%
  left_join(
    ndgain_read_long %>% 
      select(ISO3, Year, read), 
    by = c("ISO3", "Year" = "Year")
  )

# NAs in read: 
sum(is.na(cfu_10to24$read))

# Multiple imputation: 
# 1. Define countries to EXCLUDE from imputation: 
exclude_countries_read <- c("Cook Islands", "Niue", 
                            "South Sudan", "West Bank and Gaza")

# 2. Create a flag to identify rows to impute
cfu_10to24 <- cfu_10to24 %>%
  mutate(impute_flag_read = ifelse(Year %in% c(2023, 2024) & 
                                     !(Country %in% exclude_countries_read), 1, 0))

# 3. Temporarily blank out `Read` in 2023–24 rows that should be imputed
cfu_10to24 <- cfu_10to24 %>%
  mutate(read_for_impute = ifelse(impute_flag_read == 1, NA, read))

# 4. Select variables for imputation model
# Lagged readiness: 
cfu_10to24 <- cfu_10to24 %>% 
  arrange(Country, Year) %>% 
  group_by(Country) %>% 
  mutate(read_lag_1 = lag(read, n = 1)) %>% 
  mutate(read_lag_2 = lag(read, n = 2)) %>% 
  ungroup()

impute_data_read <- cfu_10to24 %>%
  select(Country_ID, Year, read_for_impute, read_lag_1, read_lag_2)

# 5. Run mice 
meth_read <- make.method(impute_data_read)
pred_read <- make.predictorMatrix(impute_data_read)

pred_read[, "Country_ID"] <- -2
meth_read["read_for_impute"] <- "pmm"

imp_read <- mice(impute_data_read, m = 5, method = meth_read, 
                 predictorMatrix = pred_read, seed = 2024)

# 6. Inspect the imputed values
summary(imp_read)
View(complete(imp_read, 3))

# 7. Plug in the completed data (e.g., from first imputed set)
cfu_10to24$ins <- complete(imp_read, 3)$read_for_impute
cfu_10to24 <- cfu_10to24 %>%
  mutate(ins = ifelse(Country %in% exclude_countries, 
                                NA, ins))

## Political Stability 
# Pivot from wide to long: 
pol_long <- pol_st %>% 
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year", 
    values_to = "pol"
  ) %>% 
  mutate(Year = as.numeric(sub("X", "", Year)))

# Merge based on ISO3: 
cfu_10to24 <- cfu_10to24 %>%
  left_join(
    pol_long %>% 
      select(Country.Code, Year, pol), 
    by = c("ISO3" = "Country.Code", "Year" = "Year")
  )

# NAs in pol: 
sum(is.na(cfu_10to24$pol))
cfu_10to24$pol <- as.numeric(cfu_10to24$pol)

# Multiple imputation: 
# 1. Create a flag to identify rows to impute
cfu_10to24 <- cfu_10to24 %>%
  mutate(impute_flag_pol = ifelse(Year %in% 2024, 1, 0))

# 2. Temporarily blank out `Read` in 2023–24 rows that should be imputed
cfu_10to24 <- cfu_10to24 %>%
  mutate(pol_for_impute = ifelse(impute_flag_pol == 1, NA, pol))

# 3. Select variables for imputation model
# Lagged readiness: 
cfu_10to24 <- cfu_10to24 %>% 
  arrange(Country, Year) %>% 
  group_by(Country) %>% 
  mutate(pol_lag_1 = lag(pol, n = 1)) %>% 
  mutate(pol_lag_2 = lag(pol, n = 2)) %>% 
  ungroup()

impute_data_pol <- cfu_10to24 %>%
  select(Country_ID, Year, pol_for_impute, pol_lag_1, pol_lag_2)

# 4. Run mice 
meth_pol <- make.method(impute_data_pol)
pred_pol <- make.predictorMatrix(impute_data_pol)

pred_pol[, "Country_ID"] <- -2
meth_pol["pol_for_impute"] <- "norm"

imp_pol <- mice(impute_data_pol, m = 5, method = meth_pol, 
                 predictorMatrix = pred_pol, seed = 2024)

# 5. Inspect the imputed values
summary(imp_pol)
View(complete(imp_pol, 1))

# 6. Plug in the completed data (e.g., from first imputed set)
cfu_10to24$pol <- complete(imp_pol, 1)$pol_for_impute


cfu_10to24 <- cfu_10to24 %>%
  mutate(across(
    .cols = c(
      funding_total,
      funding_ad,
      funding_multi,
      funding_miti,
      funding_ad_multi,
      funding_ad_miti,
      funding_miti_multi
    ),
    .fns = ~ log1p(.x), 
    .names = "log_{.col}"
  ))

## Write final csv: 
cfu_10to24 <- cfu_10to24 %>% 
  select(Country, ISO3, Year, funding_total, funding_ad, funding_multi, funding_miti, 
         funding_ad_multi, funding_ad_miti, funding_miti_multi, log_funding_total, 
         log_funding_ad, log_funding_multi, log_funding_miti, log_funding_ad_multi, 
         log_funding_ad_miti, log_funding_miti_multi, treat, vul, ins, pol)


write.csv(cfu_10to24, "Data/cfu_final.csv", row.names = FALSE)

# Overview ----------------------------------------------------------------

meta   <- read_csv("Overview.csv")
df     <- read_csv("Data/cfu_final.csv")

meta <- meta %>%
  rename(code = `Variable Codes`) %>%
  fill(Types)

# Compute descriptive statistics: 
descr <- df %>%
  select(all_of(meta$code)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.x, na.rm = TRUE),
      SD   = ~sd(.x,   na.rm = TRUE),
      Min  = ~min(.x,  na.rm = TRUE),
      Max  = ~max(.x,  na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to    = c("code", "stat"),
    names_pattern = "(.+)_(Mean|SD|Min|Max)$",
    values_to   = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(across(c(Mean, SD, Min, Max), ~round(., 3)))

fmt <- function(x, places = 3) {
  s <- formatC(round(x, places),
               format = "f", 
               digits = places)
  sub("\\.?0+$", "", s)
}

final_tbl <- meta %>%
  left_join(descr, by = "code") %>% 
  rename(`Variable Codes` = code) %>%
  mutate(Types = ifelse(duplicated(Types), "", Types)) %>%
  mutate(
    Mean = fmt(Mean),
    SD   = fmt(SD),
    Min  = fmt(Min),
    Max  = fmt(Max)
  ) %>%
  select(Types, Variables, `Variable Codes`, Measures, `Data Sources`, Min, Max, Mean, SD)

# Print with kableExtra
final_tbl %>%
  select(Types, Variables, `Variable Codes`, Measures, `Data Sources`, Mean, SD, Min, Max) %>%
  kable(
    caption  = "Summary of metadata and descriptive statistics",
    digits   = 3,
    booktabs = TRUE
  ) %>%
  add_header_above(c("Metadata" = 5, "Descriptive Stats" = 4)) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width        = FALSE,
    position          = "center"
  )