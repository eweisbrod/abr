# regression_analysis.R
library(modelsummary)
library(kableExtra)
library(fixest)
library(tidyverse)

generate_regression_table <- function() {
  # Load and prepare data
  data_path <- Sys.getenv('data_path')
  regdata <- read_dta(glue::glue("{data_path}/regdata-R.dta")) |> 
    select(gvkey, datadate, calyear, roa, roa_lead_1, loss, at, mve, rd)
  
  # Define models
  models <- list(
    "Base" = feols(roa_lead_1 ~ roa, regdata, fixef.rm = "both"),
    "No FE" = feols(roa_lead_1 ~ roa*loss, regdata, fixef.rm = "both"),
    "Year FE" = feols(roa_lead_1 ~ roa*loss | calyear, regdata, fixef.rm = "both"),
    "Two-Way FE" = feols(roa_lead_1 ~ roa*loss | calyear + gvkey, regdata, fixef.rm = "both"),
    "With Controls" = feols(roa_lead_1 ~ roa*loss + at + rd + mve | calyear + gvkey, regdata, fixef.rm = "both")
  )
  
  # Coefficient map
  cm <- c(
    "roa" = "ROA[t]",
    "loss" = "LOSS",
    "roa:loss" = "ROA[t] × LOSS"
  )
  
  # Generate the table with modelsummary
  tab <- modelsummary(
    models,
    output = "kableExtra",
    coef_map = cm,
    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
    escape = FALSE,

    notes = c("Standard errors in parentheses", 
              "* p<0.1, ** p<0.05, *** p<0.01"),
    gof_map = c("nobs", "r.squared", "r2.within")
  )
  
  # Apply styling directly in the modelsummary call
  return(tab)
}

# Generate the table (don't apply kable_styling here)
regression_table <- generate_regression_table()
print(regression_table)