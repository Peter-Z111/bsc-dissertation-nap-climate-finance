
library(dplyr)
library(ggplot2)

# Figure 2.1 --------------------------------------------------------------
# Data  (typed in from the figure) 
df <- data.frame(
  Year    = factor(2018:2022), 
  Finance = c(14.0, 18.7, 24.0, 22.1, 27.5)
)

# Basic bar chart 
p <- ggplot(df, aes(x = Year, y = Finance)) +
  geom_col(fill = "#6EC4FF",width = 0.6) + 
  geom_text(aes(label = Finance),
            vjust = -0.5, size = 4, fontface = "bold")

# Cosmetics to match the style 
p +
  scale_y_continuous(limits = c(0, 35), 
                     breaks = seq(0, 35, 5),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL,
       y = "USD billion", 
       title = "International public adaptation finance commitments\nfrom Annex II to non-Annex I countries") + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.line.x  = element_line(colour = "black", linewidth = 1),
    axis.ticks.x = element_blank()
  )


# Figure 2.1 --------------------------------------------------------------
fin_raw <- tribble(
  ~Year, ~Provider_type,       ~Finance,
  2018,  "Bilaterals",          4.4,
  2018,  "MDBs",                8.2,
  2018,  "MCFs",                1.4,
  2018,  "Other multilaterals", 0.1,
  2019,  "Bilaterals",          6.8,
  2019,  "MDBs",               10.5,
  2019,  "MCFs",                1.2,
  2019,  "Other multilaterals", 0.2,
  2020,  "Bilaterals",         10.3,
  2020,  "MDBs",               12.5,
  2020,  "MCFs",                1.2,
  2020,  "Other multilaterals", 0.2,
  2021,  "Bilaterals",          9.5,
  2021,  "MDBs",               11.1,
  2021,  "MCFs",                1.5,
  2021,  "Other multilaterals", 0.0,
  2022,  "Bilaterals",          9.3,
  2022,  "MDBs",               16.6,
  2022,  "MCFs",                1.1,
  2022,  "Other multilaterals", 0.6
)

fin_clean <- fin_raw %>%
  mutate(Provider_type = ifelse(Provider_type == "Other multilaterals",
                                "MCFs", Provider_type)) %>%
  group_by(Year, Provider_type) %>%
  summarise(Finance = sum(Finance), .groups = "drop") %>%
  mutate(Provider_type = factor(Provider_type,
                                levels = c("Bilaterals", "MDBs", "MCFs")))  # keep order

cols <- c("Bilaterals" = "#6EC4FF",   # blue
          "MDBs"       = "#45B36B",   # green
          "MCFs"       = "#F4B266")   # orange

# Plot: 
ggplot(fin_clean, aes(x = factor(Year),
                      y = Finance,
                      fill = Provider_type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = Finance),
            position = position_dodge(width = 0.7),
            vjust = -0.25, size = 3.6, fontface = "bold") +
  scale_fill_manual(values = cols, name = NULL) +
  scale_y_continuous(limits  = c(0, 22),
                     breaks  = seq(0, 20, 5),
                     expand  = expansion(mult = c(0, 0.05))) +
  labs(x = NULL,
       y = "USD billion",
       title = "International public adaptation finance commitments from Annex II 
       to non-Annex I countries by finance provider type over time") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 12),
    plot.title         = element_text(hjust = .5, face = "bold", size = 15),
    axis.line.x        = element_line(colour = "black"),
    legend.position    = "bottom"
  )
