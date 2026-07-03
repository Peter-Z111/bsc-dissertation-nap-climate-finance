
# Datasets and Libraries --------------------------------------------------
cfu <- read.csv("Data/cfu.csv")
nap_treat <- read.csv("Data/NAP.csv")

library(tidyverse)
library(tidyr)
library(dplyr)
library(fixest)
library(bacondecomp)
library(panelView)
library(did)
library(ggplot2)

# Processing Starts -------------------------------------------------------
## Removing NAs, 
sum(is.na(cfu))
sum(is.na(cfu$Approved.year))
sum(is.na(cfu$Amount.of.Funding.Approved..USD.millions.))
cfu <- na.omit(cfu)

## Transforming LDC, SIDS, and FCAS. 
cfu$Least.Developed.Country <- ifelse(cfu$Least.Developed.Country == "LDC", 
                                      1, 0)
cfu$Small.Island.Developing.Nation <- ifelse(
  grepl("SIDS", cfu$Small.Island.Developing.Nation), 1, 0)
cfu$Fragile.or.Conflict.Affected.State <- 
  ifelse(cfu$Fragile.or.Conflict.Affected.State == "FCAS", 1, 0)

unique(cfu$Theme.Objective)

## Baseline model with all three: 
# 1. Summarise project-level data: 
cfu_ad_multi_miti <- cfu %>% 
  select(!(Theme.Objective)) %>% 
  group_by(Country, Approved.year) %>% 
  summarise(
    funding_total = sum(Amount.of.Funding.Approved..USD.millions., na.rm = TRUE), 
    Region = first(World.Bank.Region), 
    Income_Level = first(Income.Classification), 
    LDC = first(Least.Developed.Country), 
    SIDS = first(Small.Island.Developing.Nation), 
    FCAS = first(Fragile.or.Conflict.Affected.State), 
    .groups = "drop"
  ) %>%
  rename(Year = Approved.year) %>%
  arrange(Country, Year)

# 2. Get unique countries and years: 
countries <- unique(cfu$Country)
years <- unique(cfu$Approved.year)

# 3. Create a full grid of all Country-Year combinations: 
full_panel <- expand.grid(
  Country = countries,
  Year = years
)

# 4. Merge with the summarised data: 
cfu_ad_multi_miti <- full_panel %>% 
  left_join(cfu_ad_multi_miti, by = c("Country", "Year")) %>% 
  arrange(Country, Year)

# 5. Fill in missing values in funding： 
cfu_ad_multi_miti <- cfu_ad_multi_miti %>% 
  mutate(
    funding_total = ifelse(is.na(funding_total), 0, funding_total)
  )

# 6. Fill in NAs in time-invariant variables: 
time_invariant_vars <- c("Region", "Income_Level", "LDC", "SIDS", "FCAS")

cfu_ad_multi_miti <- cfu_ad_multi_miti %>% 
  group_by(Country) %>% 
  fill(all_of(time_invariant_vars), .direction = "downup") %>% 
  ungroup()

## Model with only adaptation: 
cfu_ad <- cfu[grepl("Adapt", cfu$Theme.Objective), ] %>% 
  select(!(Theme.Objective)) %>% 
  group_by(Country, Approved.year) %>% 
  summarise(
    funding_ad = sum(Amount.of.Funding.Approved..USD.millions., na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  rename(Year = Approved.year) %>%
  arrange(Country, Year)

cfu_ad <- full_panel %>% 
  left_join(cfu_ad, by = c("Country", "Year")) %>% 
  arrange(Country, Year) %>% 
  mutate(
    funding_ad = ifelse(is.na(funding_ad), 0, funding_ad)
  )

## Model with only mitigation: 
cfu_miti <- cfu[grepl("Miti", cfu$Theme.Objective), ] %>% 
  select(!(Theme.Objective)) %>% 
  group_by(Country, Approved.year) %>% 
  summarise(
    funding_miti = sum(Amount.of.Funding.Approved..USD.millions., na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  rename(Year = Approved.year) %>%
  arrange(Country, Year)

cfu_miti <- full_panel %>% 
  left_join(cfu_miti, by = c("Country", "Year")) %>% 
  arrange(Country, Year) %>% 
  mutate(
    funding_miti = ifelse(is.na(funding_miti), 0, funding_miti)
  )

## Model with only multiple
cfu_multi <- cfu[grepl("Multi", cfu$Theme.Objective), ] %>% 
  select(!(Theme.Objective)) %>% 
  group_by(Country, Approved.year) %>% 
  summarise(
    funding_multi = sum(Amount.of.Funding.Approved..USD.millions., na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  rename(Year = Approved.year) %>%
  arrange(Country, Year)

cfu_multi <- full_panel %>% 
  left_join(cfu_multi, by = c("Country", "Year")) %>% 
  arrange(Country, Year) %>% 
  mutate(
    funding_multi = ifelse(is.na(funding_multi), 0, funding_multi)
  )

## Model with adaptation and multiple: 
cfu_ad_multi <- cfu[grepl("Adapt", cfu$Theme.Objective) | 
                      grepl("Multi", cfu$Theme.Objective), ] %>% 
  select(!(Theme.Objective)) %>% 
  group_by(Country, Approved.year) %>% 
  summarise(
    funding_ad_multi = sum(Amount.of.Funding.Approved..USD.millions., na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  rename(Year = Approved.year) %>%
  arrange(Country, Year)

cfu_ad_multi <- full_panel %>% 
  left_join(cfu_ad_multi, by = c("Country", "Year")) %>% 
  arrange(Country, Year) %>% 
  mutate(
    funding_ad_multi = ifelse(is.na(funding_ad_multi), 0, funding_ad_multi)
  )

## Model with mitigation and multiple: 
cfu_miti_multi <- cfu[grepl("Miti", cfu$Theme.Objective) | 
                      grepl("Multi", cfu$Theme.Objective), ] %>% 
  select(!(Theme.Objective)) %>% 
  group_by(Country, Approved.year) %>% 
  summarise(
    funding_miti_multi = sum(Amount.of.Funding.Approved..USD.millions., na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  rename(Year = Approved.year) %>%
  arrange(Country, Year)

cfu_miti_multi <- full_panel %>% 
  left_join(cfu_miti_multi, by = c("Country", "Year")) %>% 
  arrange(Country, Year) %>% 
  mutate(
    funding_miti_multi = ifelse(is.na(funding_miti_multi), 0, funding_miti_multi)
  )

## Model with adaptation and mitigation: 
cfu_ad_miti <- cfu[grepl("Adapt", cfu$Theme.Objective) | 
                        grepl("Miti", cfu$Theme.Objective), ] %>% 
  select(!(Theme.Objective)) %>% 
  group_by(Country, Approved.year) %>% 
  summarise(
    funding_ad_miti = sum(Amount.of.Funding.Approved..USD.millions., na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  rename(Year = Approved.year) %>%
  arrange(Country, Year)

cfu_ad_miti <- full_panel %>% 
  left_join(cfu_ad_miti, by = c("Country", "Year")) %>% 
  arrange(Country, Year) %>% 
  mutate(
    funding_ad_miti = ifelse(is.na(funding_ad_miti), 0, funding_ad_miti)
  )

## Baseline Model: 
cfu_baseline <- cfu_ad_multi_miti %>% 
  left_join(cfu_ad, by = c("Country", "Year")) %>% 
  left_join(cfu_multi, by = c("Country", "Year")) %>% 
  left_join(cfu_miti, by = c("Country", "Year")) %>% 
  left_join(cfu_ad_multi, by = c("Country", "Year")) %>% 
  left_join(cfu_ad_miti, by = c("Country", "Year")) %>% 
  left_join(cfu_miti_multi, by = c("Country", "Year")) %>% 
  select(Country, Year, Region, Income_Level, LDC, SIDS, FCAS, funding_total, 
         funding_ad, funding_multi, funding_miti, funding_ad_multi, 
         funding_ad_miti, funding_miti_multi)

## Combine Eswatini and Swaziland: 
# 1. Rename all "Swaziland" entries to "Eswatini"
cfu_baseline <- cfu_baseline %>% 
  mutate(Country = ifelse(Country == "Swaziland", "Eswatini", Country))

# 2. Group by Country and Year, and re-aggregate the funding and keep other vars
cfu_baseline <- cfu_baseline %>% 
  group_by(Country, Year) %>% 
  summarise(
    Region = first(Region), 
    Income_Level = first(Income_Level), 
    LDC = first(LDC), 
    SIDS = first(SIDS), 
    FCAS = first(FCAS), 
    funding_total = sum(funding_total, na.rm = TRUE), 
    funding_ad = sum(funding_ad, na.rm = TRUE), 
    funding_multi = sum(funding_multi, na.rm = TRUE), 
    funding_miti = sum(funding_miti, na.rm = TRUE), 
    funding_ad_multi = sum(funding_ad_multi, na.rm = TRUE), 
    funding_ad_miti = sum(funding_ad_miti, na.rm = TRUE), 
    funding_miti_multi = sum(funding_miti_multi, na.rm = TRUE), 
    .groups = "drop"
  )

## Exporting to csv: 
write.csv(cfu_baseline, "Data/cfu_baseline.csv", row.names = FALSE)
cfu_baseline <- read.csv("Data/cfu_baseline.csv")

cfu_10to24 <- cfu_baseline %>% 
  filter(Year >= 2010)

# Exploring Dataset -------------------------------------------------------
total_sum <- sum(cfu_10to24$funding_total)
ad_sum <- sum(cfu_10to24$funding_ad)
multi_sum <- sum(cfu_10to24$funding_multi)
miti_sum <- sum(cfu_10to24$funding_miti)
ad_multi_sum <- sum(cfu_10to24$funding_ad_multi)
ad_miti_sum <- sum(cfu_10to24$funding_ad_miti)
miti_multi_sum <- sum(cfu_10to24$funding_miti_multi)

sum(is.na(cfu_baseline))

length(unique(cfu_baseline$Country))

hist(cfu_10to24$funding_total[cfu_10to24$funding_total != 0])
summary(cfu_10to24$funding_total[cfu_10to24$funding_total != 0])
hist(cfu_10to24$funding_ad[cfu_10to24$funding_ad != 0])
summary(cfu_10to24$funding_ad[cfu_10to24$funding_ad != 0])
hist(cfu_10to24$funding_multi[cfu_10to24$funding_total != 0])
summary(cfu_10to24$funding_multi)
hist(cfu_10to24$funding_miti)
summary(cfu_10to24$funding_miti)

names(cfu_baseline)

trend_all <- cfu_baseline %>%
  group_by(Year) %>%
  summarise(
    across(
      .cols = c(
        funding_total,
        funding_ad,
        funding_multi,
        funding_miti
      ),
      .fns = ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

trend_long <- trend_all %>%
  pivot_longer(-Year, names_to = "series", values_to = "amount") %>%
  mutate(
    series = recode(series,
                    funding_ad    = "Adaptation",
                    funding_miti  = "Mitigation",
                    funding_multi = "Overlap",
                    funding_total = "Total"
    )
  )

ggplot(trend_long, aes(x = Year, y = amount, color = series)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 2003:2024) +
  labs(x = NULL,
       y = "USD million",
       color = "Funding Types",
       title = "Trends in Funding Provided by MCFs from 2003 to 2024") +
  theme_minimal()

ggplot(trend_long, aes(x = Year, y = amount, color = series, group = series)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Adaptation" = "#E41A1C",   # red
      "Mitigation" = "#4DAF4A",   # green
      "Overlap"    = "#377EB8",   # blue
      "Total"      = "#984EA3"    # purple
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = 2003:2024) +
  labs(
    x = NULL,
    y = "USD million",
    title = "Annual commitments by MCFs from 2003 to 2024"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 12),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    plot.title         = element_text(face = "bold", size = 16, hjust = 0.5)
  )