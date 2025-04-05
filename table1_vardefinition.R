# Load necessary libraries
library(knitr)
library(kableExtra)

# Create the data frame with LaTeX math mode
variables <- data.frame(
  Name = c(
    "Main Dependent and Independent Variables",
    "$\\text{ROA}_{i,t}$",
    "$\\text{LOSS}_{i,t}$",
    "Control Variables",
    "$\\text{SIZE}$",
    "$\\text{R\\&D}$",
    "$\\text{TA}$"
  ),
  Definition = c(
    "",
    "Return on assets for firm $i$ in year $t$, calculated as earnings before special items divided by ending total assets. In terms of Compustat data items, it is defined as $(\\text{ib} - \\text{spi})/\\text{at}$.",
    "An indicator variable that equals 1 for observations with negative earnings before special items and 0 otherwise.",
    "",
    "Market value of equity as of the end of fiscal year $t$ (Source: Compustat).",
    "Research and development expense scaled by ending total assets $(\\text{xrd}/\\text{at})$.",
    "Ending total assets $(\\text{at})$."
  ),
  stringsAsFactors = FALSE
)

# Function to generate table
generate_var_table <- function() {
  kable(
    variables,
    col.names = c("Variable", "Definition"),
    format = "latex",
    booktabs = TRUE,
    escape = FALSE  # Critical for LaTeX rendering
  ) %>%
    kable_styling(
      latex_options = c("Hold_position", "scale_down"), #use Hold_positio to remove [!h]
      font_size = 10,
      position = "center"
    ) %>%
    row_spec(c(1, 4), bold = TRUE) %>%
    column_spec(1, width = "5cm") %>%
    column_spec(2, width = "12cm")
}