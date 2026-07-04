# BSc dissertation: Politics and International Relations, University College London (2025). Supervisor: Dr. Adam Harris.
This repository contains the data-processing code, analysis, and figures behind the dissertation.

## Research question

Amid a widening adaptation-finance gap, does submitting a **National Adaptation Plan (NAP)** causally increase the adaptation funding that developing countries secure from **Multilateral Climate Funds (MCFs)**, such as the Green Climate Fund, the Global Environment Facility, and the Adaptation Fund?

## Data

A country–year panel of **152 developing countries, 2010–2024** (~2,280 observations).

- **Outcome** — adaptation, mitigation, overlap (multi-focus), and total MCF funding (USD millions, also used in logged form), aggregated to the country–year level from the project-level **Climate Funds Update (CFU)** database, which covers MCF-funded climate projects from 2003 to 2024.
- **Treatment** — a binary NAP-submission indicator, coded 1 from a country's submission year onward (staggered adoption), sourced from **NAP Central (2025)**. By 2024, close to 40% of the 152 countries are treated.
- **Covariates** — time-varying controls for climate vulnerability and institutional readiness (**ND-GAIN Vulnerability and Readiness Indices, 2024**) and **political stability** (World Bank Political Stability Index; Kaufmann et al., 2010).

Raw inputs are read from `Data/` (`cfu.csv`, `NAP.csv`). See "Data availability" below.

## Method

**Two-way fixed effects (TWFE)** estimation for staggered treatment timing, run in **R** (`fixest`). Not-yet-treated and never-treated countries serve as counterfactuals for those that have already submitted:

```
Y_it = μ + γ_i + δ_t + τ·NAP_it + β·X_it + ε_it
```

where `Y_it` is (logged) MCF funding for country *i* in year *t*, `γ_i` and `δ_t` are country and year fixed effects, `τ` is the treatment effect of interest, and `X_it` are the time-varying covariates. Standard errors are clustered at the country level. Main results span six specifications (raw and logged).

**Robustness checks**
- **Event-study** specification (leads and lags) to test the parallel-trends assumption.
- Staggered-adoption diagnostics: **Goodman-Bacon decomposition** (`bacondecomp`) and the **Callaway–Sant'Anna** group-time estimator (`did`), to guard against bias from heterogeneous treatment effects.

## Key results

- NAP submission is associated with a **statistically significant increase in dedicated adaptation funding**: around **19–21% in the logged models** (specifications 4–5), rising to **~46% once country-specific time trends are added** (specification 6, *p* < 0.05).
- It is also associated with a significant **~24% reduction in overlap (multi-focus) funding**.
- Interpretation: NAPs appear to unlock dedicated adaptation finance by signalling a country's needs and institutional readiness, but with a strategic trade-off away from integrated, multi-focus projects.

## Repository structure

```
├── R/         # data-processing and analysis scripts
├── Data/      # input data (see Data availability)
├── Output/    # figures and tables (event-study plot, regression tables)
└── README.md
```

## How to run

1. Open the project in R / RStudio (ideally via an `.Rproj` file so file paths stay relative).
2. Install the packages used:
```r
   install.packages(c(
  "tidyverse", "fixest", "did", "bacondecomp", "panelView",
  "fect", "modelsummary", "texreg", "stargazer", "kableExtra",
  "sjPlot", "sjmisc", "countrycode", "mice"
))
```
3. Run the scripts in `R/` in order to rebuild the panel from `Data/`, estimate the models, and reproduce the figures and tables. For output figures and tables see 'Output/'

## Data availability

The four source datasets (Climate Funds Update, NAP Central, ND-GAIN, and World Bank) are all publicly available. This repository includes the compiled country–year panel that the analysis is run on, so the results can be reproduced directly. The scripts in `R/` document how the panel was built from the raw sources.
