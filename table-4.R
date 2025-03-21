# Setup ------------------------------------------------------------------------

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
library(tictoc) #very optional, mostly as a teaching example
library(tidyverse) # I like to load tidyverse last to avoid package conflicts


#before you can call the data path, you need to set it in your environment using
#usethis::edit_r_environ("project")
data_path <- Sys.getenv('data_path')

#Set this option for the modelsummary output
options(modelsummary_format_numeric_latex = "plain")

regdata <- read_dta(glue("{data_path}/regdata-R.dta")) |> 
  select(gvkey,datadate,calyear,roa,roa_lead_1,loss,at,mve,rd,FF12,ff12num) |> 
  #add variable labels 
  sjlabelled::var_labels(
    roa_lead_1 = "$ROA_{t+1}$",
    roa = "$ROA_t$",
    loss = "$LOSS$",
    rd = "$R\\&D$",
    at = "$TA$",
    mve = "$SIZE$"
  )



#make a list of regressions to put in the table
#there are lots of options for this in the fixest package as well
#fixef.rm removes singletons for comparability with reghdfe in Stata
#The labels you give each model will be in the column headings
models <- list(
  "Base" = feols(roa_lead_1 ~ roa, regdata, fixef.rm = "both"),
  "No FE" = feols(roa_lead_1 ~ roa*loss, regdata, fixef.rm = "both"),
  "Year FE" = feols(roa_lead_1 ~ roa*loss | calyear, regdata, fixef.rm = "both"),
  "Two-Way FE" = feols(roa_lead_1 ~ roa*loss | calyear + gvkey, regdata, fixef.rm = "both"),
  "With Controls" = feols(roa_lead_1 ~ roa*loss + at + rd + mve | calyear + gvkey, regdata, fixef.rm = "both")
)


#Coefficient map
#The order of the coefficients will follow this map, also if you wish to 
#leave out coefficients, simply don't list them in the map
#there may be ways to experiment with doing this with less work/code, but this
#method gives a lot of control over the output.
#Note how this allows for labelling interaction terms as well. 
cm <- c(
  "roa_lead_1" = "$$
  ROA_{t+1}
  $$",
  "roa" = "$ROA_{t}$",
  "loss" = "$LOSS$",
  "roa:loss" = "$ROA_{t} \\times LOSS$"
)


#Optional custom formula to format the regression N observations in the output
nobs_fmt <- function(x) {
  out <- formattable::comma(x, digits=0)
  #out <- paste0("\\multicolumn{1}{c}{",out,"}")
}

#Optional formula to check if controls are in the model
#note that you need to define at least one variable in the list of controls
#this is adapted from the modelsummary help pages
#https://modelsummary.com/vignettes/modelsummary.html#collapse-control-variables-into-an-indicator
glance_custom.fixest <- function(x, ...) {
  #modify this line with your control variables
  controls <- c("at", "rd","mve")
  if (all(controls %in% names(coef(x)))) {
    #you could modify this to write whatever you want, "included/excluded etc"
    out <- data.frame(Controls = "X")
    #original formating from modelsummary help pages
    #out <- data.frame(Controls = "✓")
  } else {
    out <- data.frame(Controls = "")
    #original formating from modelsummary help pages
    #out <- data.frame(Controls = "✗")
  }
  return(out)
}

#example of helper function to show the names of the possible gof statistics
get_gof(feols(roa_lead_1 ~ roa*loss | calyear + gvkey, regdata, fixef.rm = "both"))

#Optional custom format for the mapping of what to display in the goodness of
#fit statistics below the regression output. See the documentation for 
#modelsummary and the estimation commands you are using, there will be many 
#different possible choices of what to output.
gm <- list(
  list("raw" = "FE: calyear", "clean" = "Year FE", "fmt" = NULL),
  list("raw" = "FE: gvkey", "clean" = "Firm FE","fmt" = NULL),
  list("raw" = "Controls", "clean" = "Controls","fmt" = NULL),
  list("raw" = "nobs", "clean" = "N", "fmt" = nobs_fmt),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
  list("raw" = "r2.within", "clean" = "$R^2$ Within", "fmt" = 3)
)


#Output to latex 




