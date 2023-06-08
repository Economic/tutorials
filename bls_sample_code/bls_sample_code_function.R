# Sample code for using BEA API using user-defined functions
#note: this code does the exact same thing as "bea_sample_code.R," but much more efficiently

### INSTALLING PACKAGES ###
# base R has very low utility, install user-defined packages to do cool stuff
# packages installed are saved within "home/R" files
#note: packages only need to be installed once,
#      if packages are installed, comment out next four lines
install.packages("tidyverse")
install.packages("blsAPI")
install.packages("haven")
install.packages("here")

### CALL IN PACKAGES ###
# after installation, packages must be called in using library()
library(tidyverse) # R package that is a collection for incredibly useful R data science packages (see CRAN package here: https://cran.r-project.org/web/packages/tidyverse/tidyverse.pdf)
library(blsAPI) # R package for BEA API (see CRAN package here: https://cran.r-project.org/web/packages/bea.R/bea.R.pdf)
library(haven) # R package to handle "foreign statistical formats" (see CRAN package here: https://cran.r-project.org/web/packages/haven/haven.pdf)
library(here) # R package to set project files based on current working directory (see CRAN package here: https://cran.r-project.org/web/packages/here/here.pdf)

# set BLS API key
# register for key here: https://data.bls.gov/registrationEngine/, key will be emailed
# save key in home folder (home/.Renviron) — add the following to file: "BEA_REG_KEY = BEA character string"
bls_key <- Sys.getenv("BLS_REG_KEY")

### DEFINE FUNCTIONS ####
## function is useful for pulling in a list of codes defined in a separate .csv/object defined
# max number of years is 20 per query, max seriesids is 50 per query, max querys per day is 500 for registered users.
#note: update 'endyear' February of new year
get_bls_data <- function(codes) {
    # list BLS API request
    payload5 <- list('seriesid' = codes, 'startyear'='2019', 'endyear'='2021', 'registrationkey'= bls_key)
    payload4 <- list('seriesid' = codes, 'startyear'='1999', 'endyear'='2018', 'registrationkey'= bls_key)
    payload3 <- list('seriesid' = codes, 'startyear'='1979', 'endyear'='1998', 'registrationkey'= bls_key)  
    payload2 <- list('seriesid' = codes, 'startyear'='1959', 'endyear'='1978', 'registrationkey'= bls_key)     
    payload1 <- list('seriesid' = codes, 'startyear'='1939', 'endyear'='1958', 'registrationkey'= bls_key) 

    # use wrapper function to send request to BLS and call in data
    df1 <- blsAPI(payload1, api_version = 2, return_data_frame = TRUE)
    df2 <- blsAPI(payload2, api_version = 2, return_data_frame = TRUE)
    df3 <- blsAPI(payload3, api_version = 2, return_data_frame = TRUE)
    df4 <- blsAPI(payload4, api_version = 2, return_data_frame = TRUE)
    df5 <- blsAPI(payload5, api_version = 2, return_data_frame = TRUE)
    
    # bind data frame together
    rbind(df1, df2, df3, df4, df5)
};

