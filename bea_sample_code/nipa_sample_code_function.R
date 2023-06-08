# Sample code for using BEA API using user-defined functions
#note: this code does the exact same thing as "bea_sample_code.R," but much more efficiently

### INSTALLING PACKAGES ###
# base R has very low utility, install user-defined packages to do cool stuff
# packages installed are saved within "home/R" files
#note: packages only need to be installed once,
#      if packages are installed, comment out next four lines
install.packages("tidyverse")
install.packages("bea.R")
install.packages("haven")
install.packages("here")

### CALL IN PACKAGES ###
# after installation, packages must be called in using library()
library(tidyverse) # R package that is a collection for incredibly useful R data science packages (see CRAN package here: https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf)
library(bea.R) # R package for BEA API (see CRAN package here: https://cran.r-project.org/web/packages/bea.R/bea.R.pdf)
library(haven) # R package to handle "foreign statistical formats" (see CRAN package here: https://cran.r-project.org/web/packages/haven/haven.pdf)
library(here) # R package to set project files based on current working directory (see CRAN package here: https://cran.r-project.org/web/packages/here/here.pdf)

# set BEA API key
# register for key here: https://apps.bea.gov/api/signup/, key will be emailed
# save key in home folder (home/.Renviron) — add the following to file: "BEA_REG_KEY = BEA character string"
bea_key <- Sys.getenv("BEA_REG_KEY")

### DEFINE FUNCTION ###
## loop through bea tables to calculate variables 
# this is standard code format to pull bea data
get_nipa_table <- function(tablename) {
  bea_specs <- list(
    'UserID' = bea_key,
    'Method' = 'GetData',
    'datasetname' = 'NIPA',
    'TableName' = tablename, # set tablename as function argument — allows us to loop through several tablenames at once 
    'Frequency' = 'A',
    'Year' = 'X',
    'ResultFormat' ='json'
  )
  
  # retrieve BEA data for specified NIPA table
  #note: NIPA table is specified by function argument
  beaGet(beaSpec = bea_specs, asWide = FALSE) %>%
    select(TableName, LineNumber, LineDescription, TimePeriod, DataValue) %>%
    rename(description = LineDescription,
           year = TimePeriod,
           value = DataValue,
           line = LineNumber,
           tablename_bea = TableName) %>%
    mutate(year = as.numeric(year),
           line = as.numeric(line),
           value = as.numeric(value))
}

### BEA DATA ###
# define list of bea table codes
#note: used as function arguments
tablelist <- c("T10706",
               "T60900B", "T60900C", "T60900D")

# define restrictions of bea tables
#note: this is a simple csv with the LineNumber defined for each table,
#      this is incredibly useful if pulling multiple NIPA lines from one table
#      (ex. LineNumber 1 & 14 from Table 1.7.6)
codes <- read_csv("bea_codes.csv")

### ALL DATA ###
# get all data
all_data <- map_dfr(tablelist, get_nipa_table) %>%
  # filter selected variables defined by line number
  right_join(codes, by = c("tablename_bea", "line")) %>%
  select(-line, -tablename_bea) %>%
  distinct() %>% # prevents duplicated years from tables
  # reshape data such that the variable label is the column name
  pivot_wider(id_cols = year, 
              names_from = description, 
              values_from = value) %>%
  # using distinct() groups rows — rowwise() undoes this grouping
  rowwise() %>% #note: this is an incredibly important step
  filter(year <= 2019 & year >= 1948) %>% #note: sets the range of years
  arrange(year) #note: sorts by ascending year

### WRITING DATA TO .DTA ###
# use write_dta() in haven package to write "all_data" to .dta file
#note: use here() function to ensure that .dta file is written into file relative to current working directory
write_dta(all_data, here("bea_sample_function.dta"))
