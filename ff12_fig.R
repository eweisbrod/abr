# generate_figure.R

# Load Libraries
library(modelsummary)
library(kableExtra)
library(formattable)
library(lubridate)
library(glue)
library(haven)
library(fixest)
library(forcats)
library(tidyverse)

# Load helper scripts (if needed)
source("src/-Global-Parameters.R")
source("src/utils.R")

# Read in the data
data_path<- Sys.getenv('DATA_PATH')
regdata <- read_dta(glue("{data_path}/regdata-R.dta"))

# Generate the figure
fig <- regdata |> 
  group_by(FF12) |> 
  summarize(pct_loss = sum(loss, na.rm = TRUE) / n()) |> 
  mutate(FF12 = forcats::fct_reorder(factor(FF12), pct_loss)) |> 
  ggplot(aes(x = FF12, y = pct_loss)) + 
  geom_col(fill = "#0051ba") +
  scale_y_continuous(name = "Freq. of Losses", labels = scales::percent) +
  scale_x_discrete(name = "Fama-French Industry") +
  coord_flip() +
  theme_bw(base_family = "serif")
# Display the figure in R
print(fig)
