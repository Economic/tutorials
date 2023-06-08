#? + command -> help
library(tidyverse)
library(epiextractr)
library(stats)
library(haven)
library(epidatatools)
library(stringr)
library(blsR)
library(bea.R)

# preloaded data in R (like sysuse in Stata)
diamonds

#### DATA TYPES & STRUCTURES ####
# to save as objects: use assignment arrow (dataframe/object <- command), can use "=" as well
# shortcode to import: alt + minus sign
#notes: objects ~ "stuff" stored in R, changes work space
mydata <- diamonds
# use ls() to view objects saved in workspace
ls()

### VECTORS
## vector: simplest type of data structure, sequence of data elements
##note: vector saved in workspace as "value"
##note: vector must all be the same data type
# manually assign vector
vector <- vector("character", length = 5)

# use diamond data to create vector
diamond_vector <- diamonds$carat

# check object class
class(vector)
class(diamond_vector)  

### LISTS
## list: acts like a container, sometimes called generic vectors
## note: lists are integral to using the BLS API
# manually assign list 
list <- list(1, "a", TRUE)

class(list)

### MATRIX
## matrix: vectors with dimensions
# manually assign matrix
matrix <- matrix(1:6, nrow = 2, ncol = 3)
matrix

# check object class
class(matrix)

# can also set up matrix using cbind
x <- 1:3
y <- 10:12
matrix <- cbind(x, y)

# check object class
matrix

class(matrix)

# matrix column must have the same data type
x <- 1:3
y <- c("a", "b", "c")
matrix <- cbind(x, y)

# check object class
class(matrix)

# check object class across columns
#note: sapply is a tidyverse function used to apply another function across lists (which individual columns technicially are)
sapply(matrix, class) #note: numbers are implicitly converted to class "character"

## DATA FRAME
# data frame: the most important data structure for data science, 
#             rectangular list that looks the same as a matrix but is heterogenous
# manually assigning data frame
df <- data.frame(c(2020, 10.5), c(2021, 11), c(2022, 11.5))

# use diamonds data to assign data frame
mydata <- diamonds

#### BASIC DATA MANIPULATION COMMANDS ####
## FILTER
# looking at part of the data set ~ keep or drop in Stata
# filter(number of object, logical condition)
filter(diamonds, carat < 0.25 & price < 400)

mydata <- filter(diamonds, carat < 0.25 & price < 400)

### SUMMARY OPERATIONS
# summary of dataset with replacement (object, mynewcolumn = statistical concept)
#note: summarize ~ collapse/gcollapse (mean) price
mydata <- summarize(mydata, meanprice = mean(price))
# replace matrix 8x79 to matrix 1x1, similar to how collapse/gcollapse collapses down data
mydata

## mean and minimum statistics with replacement
mydata <- diamonds
mydata <- summarize(mydata, meanprice = mean(price), mincarat = min(carat))
mydata

## GROUPING DATA
# recall main dataset to replace "collapsed" data with the full dataset
mydata <- diamonds
# grouping data: group(object, thing you are grouping over), collapsing by group
# need to group data before summarizing/collapsing
mydata <- group_by(mydata, color)
mydata <- summarize(mydata, meanprice = mean(price), mincarat = min(carat))

## FILTERING + SUMMARIZING
# filter lower carat count & lower prices, summarize mean price, minimum carat count,umber of observations by color
mydata <- filter(diamonds, carat < 0.25 & price < 400)
mydata <- group_by(mydata, color)
# n() counts the number of rows
mydata <- summarize(mydata, meanprice = mean(price), mincarat = min(carat), obs = n())

mydata

# good way to stop/break the code
break

#### PIPES ####
### pipes are integral to the tidyverse 
## pipes: syntax for chained method calls, OR simply a way to pass intermediate results onto the next function
# repeat operations from above, but use pipes
#Note: shortcode ~ ctl + shift + m = %>%
mydata <- diamonds %>%
    filter(carat < 0.25 & price < 400) %>%
    group_by(color) %>%
    summarize(meanprice = mean(price), mincarat = min(carat), obs = n())
mydata

#### USING MICROEXTRACTS & EXTERNAL DATA SOURCES ####
# use system command to download CPI-U-RS data to folder
system('wget -N --progress=bar:force https://microdata.epi.org/misc/extremewages.csv')

# read_csv() to read output of the csv
read_csv("extremewages.csv")

# assign CPI data to object, rename data, and drop irrelevant variables
cpidata <- read_csv("extremewages.csv") %>%
    rename(year = `Year`, cpiurs = `CPI-U-RS - extended`) %>%
    select(year, cpiurs)

## pulling out specific values
# using base R syntax
#note: $ is used to select a column, [] is used to index in a vector
cpi_base <- cpidata$cpiurs[cpidata$year == 2021]

# using tidyverse and pull()
cpi_base <- cpidata %>% 
    # filter out to base year, in this case 2021
    filter(year == 2021) %>% 
    # use pull()
    pull()


## ORG DATA
# use epiextractr package to access microdata extracts
org_data <- load_org(2020:2021, year, selfemp, selfinc, age, wage, female, wbhao, orgwgt, emp) %>%
    # filter out observations that are self employed and/or have missing wages
    #note: in R, missing data is NA (which can be any data storage type),
    #      you can use is.*() to filter out observations with a certain data type, and you can use ! as "not" 
    filter(selfemp == 0, selfinc == 0, !is.na(wage), age >= 16) %>% 
    # merge in cpi data 
    left_join(cpidata, by = "year") %>% 
    # calculate real wages
    mutate(realwage = wage * (cpi_base/cpiurs))

org_data

# calculate mean wages for 2019
meanwages <- org_data %>%
    # choose grouping variable
    group_by(year) %>%
    # use weighted.mean() from the `stats` package
    summarize(meanwage = weighted.mean(realwage, w = orgwgt)) %>% 
    # use arrange to sort smallest to largest on year
    arrange(year)

# calculate pooled mean wages by demographic
meanwage_demo <- org_data %>%
    # microdata extracts are stata labelled numeric categorical variables,
    # when they are imported into R, the label is kept but the variable takes on the numberic category
    # in order to unnest the label and assign it as the category, use the following operation
    # this will take female == 1 | female == 0 to female == "female" | female == "male",
    # this can be much easier to work with and it much much better for output
    mutate(across(female | wbhao, ~ as.character(as_factor(.x))),
           pop = 1, wgt = orgwgt/24) %>%
    # choose grouping variables
    group_by(wbhao) %>% 
    # calculate weighted mean, population size, and sample size (using the pop variable)
    #note: notice the different weight being used, in the prior example only orgwgt was used
    #      but here we are using an adjusted weight
    #      orgwgt should add up to total population per month, so if you are taking COUNTS you must divide by the number of months in the sample
    #      for a year of data that will be orgwgt/12, but for pooled data that will be orgwgt/(12 * years in the pool)
    #      it doesn't matter so much when you're running means because they are scalar, but makes a HUGE difference when running counts
    summarize(meanwage = weighted.mean(realwage, w = wgt), sample = n(), population = sum(wgt)) %>% 
    # you can also custom order rows,
    # usually at EPI we reference WBHAO in that order (see this table: https://www.epi.org/chart/state-of-working-america-table-3-hourly-wages-by-race-ethnicity-and-wage-percentile-2000-2019-2019-dollars/),
    # but arrange() only knows how to sort numeric or alphabetical unless you tell it otherwise
    arrange(match(wbhao, c("W", "B", "H", "A", "O")))

# calculate population shares
wbhao_shares <- org_data %>% 
    # adjust weight
    mutate(wgt = orgwgt/12) %>% 
    # filter employed
    filter(emp == 1) %>% 
    # use epidatatools crosstab() function, similiar to tab is Stata
    crosstab(year, wbhao, w = wgt, row = TRUE)

# calculate cross summary
genderXwbhao_shares <- org_data %>% 
    # adjust weight & create crossed categories
    mutate(across(female | wbhao, ~ as.character(as_factor(.x))),
           genderXwbhao = paste(wbhao, str_sub(female)), # note: use str_sub() from stringr package
           wgt = orgwgt/12) %>%
    # filter employed
    filter(emp == 1) %>% 
    # calculate cross share by year
    crosstab(year, genderXwbhao, w = wgt, row = TRUE)

#### BLS API ####
### BLS registration key needed for api version 2. register here: https://data.bls.gov/registrationEngine/
bls_key <- Sys.getenv("BLS_REG_KEY")

### max number of years is 20 per query, max seriesids is 50 per query, max querys per day is 500 for registered users.
bls_data <- get_n_series_table(series_ids = 'CES0000000001', start_year = 1999, end_year = 2022, api_key = bls_key) 

## using vectors to pull multiple series IDs
# define vector of series IDs 
bls_codes <- c("CES0000000001", "CES0500000001", "CES0000000010", "CES0500000003","CES0500000008")

#note: the list item for seriesid NEEDS to be a character vector, this is important when you are using a .csv to import codes (which is often)
# if BLS codes were imported as .csv, it would be a dataframe, here is an example:
#note: this WILL return a blsAPI call failure error
bls_codes <- data.frame(code = c("CES0000000001", "CES0500000001", "CES0000000010", "CES0500000003","CES0500000008")) # notice that this is stored in "Data" not "Values"

bls_data <- get_n_series_table(series_ids = bls_codes, start_year = 2002, end_year = 2022, api_key = bls_key) 

# instead you can define a vector list using $ operator to define a vector using the data frame
bls_codes <- bls_codes$code

bls_data <- get_n_series_table(series_ids = bls_codes, start_year = 1999, end_year = 2022, api_key = bls_key) %>% 
    # filter out annual data
    #note: this is usually not important, because most of the time you must specify in the list call that you want annual data
    #      BUT previous versions of blsAPI() did not have the requirement, so this is just good practice
    filter(period != "M13") %>% 
    # create month variable using period (which is in `MXX` format)
    #this is: create variable month using the 2 and 3 character of string period and convert it to numeric data storage type
    mutate(month = as.numeric(substr(period, 2, 3))) %>% 
    # create data and establish date format
    mutate(date = strptime(paste(year, month, 1, sep = "-"), format = "%Y-%m-%d"), # paste together year, month, and the number 1 and separate them by "-", then convert the data storage type
           date = as.Date(date)) %>% 
    # you can also use vectors to select columns
    select(date, all_of(bls_codes)) %>% 
    arrange(date)


#### BEA DATA ####
# BEA needs an API key. Future users can register for one here: http://www.bea.gov/API/signup/index.cfm
bea_key <- Sys.getenv("BEA_REG_KEY")

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


#### FUNCTIONS ####
### User-Defined Functions (UDFs) are a really big strength of R as a coding language, 
### because it allows the user the ability to automate almost any process without having to use complicated programming syntax
## BEA API function
# one useful way EPI employs the use of functions is by creating a UDF for the importation of BEA data:
get_bea_table <- function(tablename) {
    bea_specs <- list(
        'UserID' = bea_key,
        'Method' = 'GetData',
        'datasetname' = 'NIPA',
        'TableName' = tablename,
        'Frequency' = 'Q',
        'Year' = 'X',
        'ResultFormat' ='json'
    )
    
    beaGet(beaSpec = bea_specs, asWide = FALSE) %>%
        select(SeriesCode, TimePeriod, DataValue) %>% 
        mutate(DataValue = as.numeric(DataValue))
};

# define BEA codes for federal reserve banks from table 6.16 (broken into 3 parts)
fed_reserve_tablelist <- c("T61600B", "T61600C", "T61600D")

# call BEA data for Federal Reserve banks series codes
# there are several ways to apply functions:
#   1. apply(), mapply(), sapply(), lapply(), etc. - R base functions that are not as powerful and can be difficult to use
#   2. map(), map_dfr(), map_dfc(), etc. - tidyverse functions that are much more powerful and more intuitive
#   in this case, map_dfr is employed: each BEA table requested gets pulled and then is row-bound together,
#   this is powerful because it's a two-in-one, which makes it more efficient
fed_reserve <- map_dfr(fed_reserve_tablelist, get_bea_table) %>%
    # use the %in% operator ~ a way to identify if elements belong to a vector,
    # when combined with filter it is a powerful way of filter out codes that you don't want
    filter(SeriesCode %in% c("J397RBC", "J397RC", "B397RC")) %>% 
    # you can alse use select to only deselect variables you don't want
    select(-SeriesCode) %>%
    # sometimes, particularly with BEA data, there will be an overlap of years with identical data,
    # this usually happens at the cutoff years for BEA data tables, so use distinct() or also unique() 
    # to filter out any observations that are identical
    distinct() %>% 
    # rename relevant variables
    rename(fed_reserve_banks = DataValue)
