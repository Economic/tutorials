# Sample code for using BEA API
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

# full table lists here: https://apps.bea.gov/iTable/iTable.cfm?ReqID=19&step=4&isuri=1&1921=flatfiles

### GET DATA FOR NET-TO-GROSS DOMESTIC PRODUCT RATIO ###
# define BEA table specifications
bea_specs_GDP <- list(
  'UserID' = bea_key,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T10706', # Table 1.7.6 -> T10706
  'Frequency' = 'A', # annual data (change to 'Q' for quarterly data)
  'Year' = 'X', # change for specific years
  'ResultFormat' ='json'
)

# pull NIPA table 1.7.6 to get net to gross domestic product ratio
#note: use beaGet() from bea.R package to easily pull in BEA data
table_1.7.6 <- beaGet(beaSpec = bea_specs_GDP, asWide = FALSE) %>%
  # select specific line numbers
  # LineNumber == 1 ~ GDP, LineNumber == 14 ~ NDP
  filter(LineNumber == "1" | LineNumber == "14") %>%
  # select important data columns
  #note: rename TimePeriod to year
  select(LineDescription, year = TimePeriod, DataValue) %>% # LineDescription ~ variable label for data value
  # reshape data such that the variable label is the column name
  pivot_wider(id_cols = year, names_from = LineDescription, values_from = DataValue) %>%
  # perform necessary modifications/calculations
  mutate(year = as.numeric(year), # transform year data type (character string -> numeric string)
         net_to_gross_ratio = `Net domestic product`/`Gross domestic product (GDP)`) %>% # calculaute net-to-gross domestic product ratio
  # arrange data in ascending order by year
  arrange(year)

### GET DATA FOR HOURS WORKED ###
# pull data from NIPA table 6.9B-D for hourly compensation calculations

# define BEA table specifications for Table 6.9B 
# Table 6.9B ~ "Hours worked by full-time and part-time employees by industry (1948-1987)"
bea_specs_incB <- list(
  'UserID' = bea_key,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T60900B', # Table 6.9B -> 'T60900B'
  'Frequency' = 'A', # annual data
  'Year' = 'X', # all years available
  'ResultFormat' ='json'
)

# pull NIPA table 6.9B to get hours worked by full-time & part-time employees
table_6.9_b <- beaGet(beaSpec = bea_specs_incB, asWide = FALSE) %>%
  # select specific line numbers
  # LineNumber == 1 ~ "Hours worked by full-time and part-time employees"
  filter(LineNumber == 1) %>%
  select(TimePeriod, DataValue)

# define BEA table specifications for Table 6.9C
# Table 6.9C ~ "Hours worked by full-time and part-time employees by industry (1987-2000)"
bea_specs_incC <- list(
  'UserID' = bea_key,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T60900C', # Table 6.9C -> 'T60900C'
  'Frequency' = 'A', # annual data
  'Year' = 'X', # all available years
  'ResultFormat' ='json'
)

# pull NIPA table 6.9C to get hours worked by full-time & part-time employees for 1993-2000
table_6.9_c <- beaGet(beaSpec = bea_specs_incC, asWide = FALSE) %>%
  # select specific line numbers
  # LineNumber == 1 ~ "Hours worked by full-time and part-time employees"
  filter(LineNumber == 1) %>%
  select(TimePeriod, DataValue)

# define BEA table specifications for Table 6.9D
# Table 6.9D ~ "Hours worked by full-time and part-time employees by industry (2000-present)"
bea_specs_incD <- list(
  'UserID' = bea_key,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T60900D', # Table 6.9D -> 'T60900D'
  'Frequency' = 'A', # annual data
  'Year' = 'X', # all available years
  'ResultFormat' ='json'
)

# pull NIPA table 6.9D to get hours worked by full-time & part-time employees for 2000-present
table_6.9_d <- beaGet(beaSpec = bea_specs_incD, asWide = FALSE) %>%
  # select specific line numbers
  # LineNumber == 1 ~ "Hours worked by full-time and part-time employees"
  filter(LineNumber == 1) %>%
  select(TimePeriod, DataValue)

# append all the 6.9 tables together and clean it up a bit
table_6.9_tot <-rbind(table_6.9_b, table_6.9_c, table_6.9_d) %>%
  distinct() %>% # prevents overlapping years from NIPA tables
  rename(hr_worked = DataValue, year = TimePeriod) %>%
  # transform year data type (character string -> numeric string)
  mutate(year = as.numeric(year)) %>%
  # using distinct() groups rows — rowwise() undoes this grouping
  rowwise() %>% #note: this is an incredibly important step
  # arrange data in ascending order by year
  arrange(year)

### WRITING DATA TO .DTA ###
# use write_dta() in haven package to write "all_data" to .dta file
#note: use here() function to ensure that .dta file is written into file relative to current working directory
write_dta(all_data, here("bea_sample.dta"))
