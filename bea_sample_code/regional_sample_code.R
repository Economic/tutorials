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

# full table lists here: https://apps.bea.gov/itable/iTable.cfm?ReqID=70&step=1&acrdn=1
# full list of API parameters can be found here: https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf

# import state codes .csv (important for convenient filtering)
state_codes_df <- read_csv(here("state_geocodes.csv"))

# isolate state names into vector
state_codes <- state_codes_df$state_name

### GET DATA FOR REAL GDP (MILLIONS OF CHAINED DOLLARS), BY STATE ###
# define BEA table parameters
bea_specs_regional_gdp <- list(
  'UserID' = bea_key,
  'Method' = 'GetData',
  'datasetname' = 'Regional',
  'TableName' = 'SAGDP2N', 
  'LineCode' = 1, # integer (for more information see the interactive tables)
  'GeoFips' = 'STATE', # "All" option, for more information see the API user guide
  'Year' = 'ALL', # change for specific years
  'ResultFormat' ='json'
)

# pull gross domestic summary table
#note: use beaGet() from bea.R package to easily pull in BEA data
regional_gdp <- beaGet(beaSpec = bea_specs_regional_gdp, asWide = FALSE) %>% 
  # filter out national/regional data using imported state codes
  filter(GeoName %in% state_codes) %>% 
  # select & rename important variables 
  select(year = TimePeriod, state = GeoName, gdp_millions_current_dollars = DataValue) %>% 
  # transform class for easier handling
  mutate(year = as.numeric(year)) %>% 
  # arrange data in ascending year and state name
  arrange(year, state) %>% 
  # reshape data such that the state and year are the variables
  pivot_wider(id_cols = year, names_from = "state", values_from = "gdp_millions_current_dollars")

### WRITING DATA TO .DTA ###
# use write_dta() in haven package to write "regional_gdp" to .dta file
#note: use here() function to ensure that .dta file is written into file relative to current working directory
write_dta(regional_gdp, here("regional_gdp.dta"))
