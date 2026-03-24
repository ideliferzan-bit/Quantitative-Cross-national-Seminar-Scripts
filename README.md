# Longitudinal Multilevel Analysis of Immigration Attitudes in Europe

This repository contains the Stata code and dataset for our final project in the course **Quantitative Cross-National Comparative Research**.

The project examines whether changes in **MIPEX scores** and **foreign-born population shares** are associated with attitudes toward immigration in Europe between **2015 and 2023**, using **ESS** data.

## Files

- `Final_multilevel_models.do` — main Stata do-file
- `full_final_dataset.dta` — final dataset used in the analysis

## Data and method

The analysis uses repeated cross-sectional **European Social Survey (ESS)** data combined with country-level indicators.

The main models examine:
- attitudes toward immigration
- the perceived economic impact of immigration (robustness check)

The analysis is conducted using **multilevel models in Stata**.

## How to run

1. Open Stata.
2. Put `Final_multilevel_models.do` and `full_final_dataset.dta` in the same folder.
3. Set that folder as your working directory.
4. Run:

```stata
do Final_multilevel_models.do