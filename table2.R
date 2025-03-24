# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)
library(formattable)

# Assuming `regdata` is your dataset
table1 <- regdata |> 
  # Group calyears into decades with labels
  mutate(Year = case_when(
    calyear %in% 1970:1979 ~ "1970 - 1979",
    calyear %in% 1980:1989 ~ "1980 - 1989",
    calyear %in% 1990:1999 ~ "1990 - 1999",
    calyear %in% 2000:2009 ~ "2000 - 2009",
    calyear %in% 2010:2019 ~ "2010 - 2019",
    calyear >= 2020 ~ "2020 - 2022"
  )) |> 
  group_by(Year) |> 
  # Within each year, count the obs and calculate loss percentage
  summarize(
    `Total Firms` = formattable::comma(n(), digits = 0),
    `Loss Firms` = formattable::comma(sum(loss), digits = 0),
    `Pct. Losses` = formattable::percent(sum(loss) / n(), digits = 2)
  )

# Generate the table using kable with LaTeX options
table_output <- kable(
  table1,
  caption = "Summary of Firms by Decade",
  align = c("l", "r", "r", "r"),  # Align columns: left, right, right, right
  format = "latex",  # Use LaTeX format for PDF output
  booktabs = TRUE   # Use booktabs for better LaTeX tables
) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),  # Add LaTeX styling
    font_size = 10   # Adjust font size
  ) %>%
  column_spec(1, width = "4cm") %>%  # Set width for the first column
  column_spec(2:4, width = "3cm")  # Set width for the remaining columns

# Print the table output
print(table_output)