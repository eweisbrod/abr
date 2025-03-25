# generate_table.R

# Load necessary libraries
library(knitr)
library(kableExtra)

# Create the data frame for the table
table_data <- data.frame(
  Variable = c(
    "Main Dependent and Independent Variables",
    "ROA_{i,t}",
    "LOSS_{i,t}",
    "Control Variables",
    "SIZE",
    "R&D",
    "TA"
  ),
  Definition = c(
    "",
    "Return on assets for firm $i$ in year $t$, calculated as earnings before special items divided by ending total assets. In terms of Compustat data items, it is defined as (ib - spi)/at.",
    "An indicator variable that equals 1 for observations with negative earnings before special items and 0 otherwise.",
    "",
    "Market value of equity as of the end of fiscal year $t$ (Source: Compustat).",
    "Research and development expense scaled by ending total assets (xrd/at).",
    "Ending total assets (at)."
  )
)

# Generate the table using kable with LaTeX options
table_output <- kableExtra::kable(
  table_data,
  col.names = c("Variable", "Definition"),
  align = c("l", "l"),  # Use simple left alignment for both columns
  format = "latex",  # Use "latex" for PDF output
  booktabs = TRUE   # Use booktabs for better-looking tables
  #linesep = "",      # Remove extra spacing between rows
  #longtable = TRUE   # Use longtable for multi-page tables
) %>%
  kable_styling(
    latex_options = c("line","scale_down"),  # Add LaTeX styling
    font_size = 10   # Adjust font size
  ) |> 
  row_spec(1, bold = TRUE) %>%
  row_spec(4, bold = TRUE) %>%
  column_spec(1, width = "5cm") %>%  # Set width for the first column
  column_spec(2, width = "12cm", latex_valign = "p")  # Wrap text in the second column

# Print the table output
print(table_output)