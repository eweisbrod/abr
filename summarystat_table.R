# Load Libraries [i.e., packages]
library(modelsummary)
library(sjlabelled)
library(kableExtra)
library(tinytable)
library(formattable)
library(lubridate)
library(glue)
library(haven)
library(fixest)
library(tictoc) # very optional, mostly as a teaching example
library(tidyverse) # I like to load tidyverse last to avoid package conflicts

# Load Data
data_path<- Sys.getenv('DATA_PATH')
regdata <- read_dta(glue("{data_path}/regdata-R.dta"))

## Create formatting functions --------------------------------------------------

# Set number formats for descriptive table
my_fmt <- function(x) formatC(x, format = "f", digits = 3, big.mark = "")

# N function to handle special format for N with no decimals
NN <- function(x) {
  out <- if (is.logical(x) && all(is.na(x))) {
    length(x)
  } else {
    sum(!is.na(x))
  }
  out <- formattable::comma(out, digits = 0)
  return(out)
}

# If you make a subset of the data 
# you can handle the variable labels with sjlabelled 
descripdata <- regdata |>
  select(
    roa_lead_1,
    roa,
    loss,
    rd,
    at,
    mve
  ) |>
  label_to_colnames()


table_output <- datasummary(
  All(descripdata) ~ (N = NN) + Mean * Arguments(fmt = my_fmt) +
    SD * Arguments(fmt = my_fmt) +
    Min * Arguments(fmt = my_fmt) +
    P25 * Arguments(fmt = my_fmt) +
    Median * Arguments(fmt = my_fmt) +
    P75 * Arguments(fmt = my_fmt) +
    Max * Arguments(fmt = my_fmt),
  data = descripdata,
  output = "data.frame"  # Convert to data.frame instead of tinytable
)

table_output <- kableExtra::kable(table_output,
                                  align = c("l","r","r","r","r","r","r","r","r") ) |>  # Center align all columns (use 'l' for left, 'r' for right) 
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )

# Print the table output
print(table_output)