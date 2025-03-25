# Load necessary libraries
library(knitr)
library(kableExtra)

# Create the data frame for the table
sample_selection <- data.frame(
  Description = c(
    "First I downloaded some data from WRDS",
    "After removing financial and utility firms",
    "After requiring lead ROA data",
    "After some other requirements",
    "With data from another database available",
    "Less: firm-years missing data to compute control variables defined in Appendix",
    "Full Sample"
  ),
  Observations = c(
    194728,
    136393,
    127867,
    86702,
    71408,
    -3500,
    163269
  )
)

# Generate the table using kable with LaTeX options
table_output <- kable(
  sample_selection,
  col.names = c("Description", "Observations"),
  align = c("l", "r"),  # Left-align the first column, right-align the second
  format = "latex",  # Use LaTeX format for PDF output
  booktabs = TRUE   # Use booktabs for better LaTeX tables
) %>%
  kable_styling(
    latex_options = c("striped", "scale_down")  # Add LaTeX styling
  ) %>%
  add_header_above(c(" " = 1, "THESE ARE NOT REAL NUMBERS" = 1)) %>%  # Add a header
  row_spec(6, bold = TRUE) %>%  # Bold the 6th row
  row_spec(7, bold = TRUE, italic = TRUE) %>%  # Bold and italicize the 7th row
  #column_spec(2, width = "4cm") |>   # Set width for the second column
  column_spec(1, width = "10cm", latex_valign = "p") |>  # Set width and wrap text in the first column
  add_footnote("This table describes the initial sample selection procedure used to collect the data analyzed in our study. Data requirements specific to individual tables or analyses are provided in the descriptions of those analyses.", notation = "none")

# Print the table output
print(table_output)