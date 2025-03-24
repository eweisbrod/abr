library(fixest)
library(haven)
library(kableExtra)
library(dplyr)

# Your R code to generate the regression table
data_path <- "C:/Users/k021s566/Desktop/Eric/ACCT 995/Git/abr_project/abr"
regdata <- read_dta(file.path(data_path, "regdata-R.dta"))

models <- list(
  "Base" = feols(roa_lead_1 ~ roa, regdata, fixef.rm = "both"),
  "No FE" = feols(roa_lead_1 ~ roa * loss, regdata, fixef.rm = "both"),
  "Year FE" = feols(roa_lead_1 ~ roa * loss | calyear, regdata, fixef.rm = "both"),
  "Two-Way FE" = feols(roa_lead_1 ~ roa * loss | calyear + gvkey, regdata, fixef.rm = "both"),
  "With Controls" = feols(roa_lead_1 ~ roa * loss + at + rd + mve | calyear + gvkey, regdata, fixef.rm = "both")
)

regression_summary <- lapply(models, function(model) {
  summary(model)$coefficients
})

summary_df <- bind_rows(regression_summary, .id = "Model")
colnames(summary_df) <- c("Coefficient", "Estimate", "Std. Error", "t-Statistic", "P-value")

summary_df %>%
  kable("latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  add_header_above(c(" " = 1, "Regression Models" = 5)) %>%
  column_spec(2, width = "4em") %>%
  row_spec(0, bold = TRUE) 

# Generate LaTeX table with kableExtra
latex_table <- summary_df %>%
  kable("latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  add_header_above(c(" " = 1, "Regression Models" = 5)) %>%
  column_spec(2, width = "4em") %>%
  row_spec(0, bold = TRUE)  # Bold the header row

# Print the LaTeX table to the console
print(latex_table)