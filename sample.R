# sample_selection_table.R
# Simple script to generate and print a formatted table

# Load required packages
library(knitr)
library(kableExtra)

# Create the data frame
sample_data <- data.frame(
  Description = c(
    "Initial data download from WRDS",
    "After removing financial and utility firms",
    "After requiring lead ROA data",
    "After applying additional screening criteria",
    "With matching data from secondary database",
    "Less: firm-years missing control variables",
    "Final Analysis Sample"
  ),
  Observations = c(
    194728,
    136393,
    127867,
    86702,
    71408,
    -3500,
    163269
  ),
  stringsAsFactors = FALSE
)

# Format numbers with commas
sample_data$Observations_formatted <- format(sample_data$Observations, big.mark = ",", scientific = FALSE)

# Generate and print the table
kable(sample_data[, c("Description", "Observations_formatted")], 
      col.names = c("Sample Selection Step", "Firm-Years"),
      align = c("l", "r"),
      format = "simple") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE,
                position = "center") %>%
  row_spec(6, bold = TRUE) %>%
  row_spec(7, bold = TRUE, italic = TRUE) %>%
  add_footnote("Note: This table describes the sample selection procedure.", 
               notation = "none") %>%
  print()