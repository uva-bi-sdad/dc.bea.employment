# This calculates employment rates in 2015-2019 and earnings per job for VDH request
# packages
library(tidycensus)
library(tigris)
library(RPostgreSQL)
library(plyr)
library(tidyverse)

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

acs_vars <- c(# tot civilian labour force
                "B23025_003",
              # employed
                "B23025_004")

acs_2019 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2019,
                    cache_table=TRUE,
                    output="wide")

acs_2018 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2018,
                    cache_table=TRUE,
                    output="wide")

acs_2017 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2017,
                    cache_table=TRUE,
                    output="wide")

acs_2016 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2016,
                    cache_table=TRUE,
                    output="wide")

acs_2015 <- get_acs(geography="tract",
                    state="VA",
                    variables=acs_vars,
                    year=2015,
                    cache_table=TRUE,
                    output="wide")

acs_est19 <- acs_2019 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2019 = B23025_004E,
  #tot labor force
  denominator_2019 = B23025_003E,
  # employment rate
  emp_rate_2019 = B23025_004E/B23025_003E * 100)

acs_est18 <- acs_2018 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2018 = B23025_004E,
  #tot labor force
  denominator_2018 = B23025_003E,
  # employment rate
  emp_rate_2018 = B23025_004E/B23025_003E * 100)

acs_est17 <- acs_2017 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2017 = B23025_004E,
  #tot labor force
  denominator_2017 = B23025_003E,
  # employment rate
  emp_rate_2017 = B23025_004E/B23025_003E * 100)

acs_est16 <- acs_2016 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2016 = B23025_004E,
  #tot labor force
  denominator_2016 = B23025_003E,
  # employment rate
  emp_rate_2016 = B23025_004E/B23025_003E * 100)

acs_est15 <- acs_2015 %>% transmute(
  GEOID=GEOID,
  NAME = NAME,
  #employed
  numerator_2015 = B23025_004E,
  #tot labor force
  denominator_2015 = B23025_003E,
  # employment rate
  emp_rate_2015 = B23025_004E/B23025_003E *100)

# merge together

acs_est_all <-
  join_all(list(acs_est15, acs_est16, acs_est17, acs_est18, acs_est19),
           by='GEOID', type='left')
acs_est_all <- acs_est_all %>% select(-NAME)

# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")
geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")
dbDisconnect(con)

tracts <- geo_names %>% filter(region_type == "tract")
acs_est_all <- left_join(acs_est_all, tracts, by=c("GEOID" = "geoid"))

emp_rate <- melt(acs_est_all,
                         id.vars=c("GEOID", "region_name", "region_type"),
                         variable.name="measure",
                         value.name="value"
)

emp_rate['year'] =  str_sub(emp_rate$measure,-4,-1)
emp_rate$measure = str_sub(emp_rate$measure,1,-6)
indx1 <- grepl('numerator', emp_rate$measure)
indx2 <- grepl('denominator', emp_rate$measure)
indx3 <- grepl('emp_rate', emp_rate$measure)
emp_rate$measure_type[indx1] <- 'count'
emp_rate$measure_type[indx2] <- 'count'
emp_rate$measure_type[indx3] <- 'percent'

names(emp_rate)[1] <- 'geoid'

# re-arrange columns
emp_rate <- emp_rate[, c(1, 3, 2, 6, 4, 5, 7)]

################################## earnings per job ########################################

# load csv from BEA
tot_emp <- read_csv("bea_earnings/bea_tot_employment.csv")
wage_sup <- read_csv("bea_wage_supplement.csv")
wage_sal <- read_csv("bea_wages_salaries.csv")
prop_inc <- read_csv("prop_income.csv")

# init empty df
earn_job <- tot_emp %>% select(GeoFips, GeoName)

# compute emp rate
earn_job['tot_compensation_2015'] <- (wage_sup$`2015`+wage_sal$`2015`+prop_inc$`2015`) * 1000
earn_job['tot_compensation_2016'] <- (wage_sup$`2016`+wage_sal$`2016`+prop_inc$`2016`) * 1000
earn_job['tot_compensation_2017'] <- (wage_sup$`2017`+wage_sal$`2017`+prop_inc$`2017`) * 1000
earn_job['tot_compensation_2018'] <- (wage_sup$`2018`+wage_sal$`2018`+prop_inc$`2018`) * 1000
earn_job['tot_compensation_2019'] <- (wage_sup$`2019`+wage_sal$`2019`+prop_inc$`2019`) * 1000
earn_job['tot_compensation_2020'] <- (wage_sup$`2020`+wage_sal$`2020`+prop_inc$`2020`) * 1000

earn_job['tot_employment_2015'] <- tot_emp$`2015`
earn_job['tot_employment_2016'] <- tot_emp$`2016`
earn_job['tot_employment_2017'] <- tot_emp$`2017`
earn_job['tot_employment_2018'] <- tot_emp$`2018`
earn_job['tot_employment_2019'] <- tot_emp$`2019`
earn_job['tot_employment_2020'] <- tot_emp$`2020`

earn_job['earnings_per_job_2015'] <- (wage_sup$`2015`+wage_sal$`2015`+prop_inc$`2015`)/tot_emp$`2015` * 1000
earn_job['earnings_per_job_2016'] <- (wage_sup$`2016`+wage_sal$`2016`+prop_inc$`2016`)/tot_emp$`2016` * 1000
earn_job['earnings_per_job_2017'] <- (wage_sup$`2017`+wage_sal$`2017`+prop_inc$`2017`)/tot_emp$`2017` * 1000
earn_job['earnings_per_job_2018'] <- (wage_sup$`2018`+wage_sal$`2018`+prop_inc$`2018`)/tot_emp$`2018` * 1000
earn_job['earnings_per_job_2019'] <- (wage_sup$`2019`+wage_sal$`2019`+prop_inc$`2019`)/tot_emp$`2019` * 1000
earn_job['earnings_per_job_2020'] <- (wage_sup$`2020`+wage_sal$`2020`+prop_inc$`2020`)/tot_emp$`2020` * 1000

# format
# connect to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWROD")

geo_names <- dbGetQuery(con, "SELECT * FROM dc_geographies.ncr_cttrbg_tiger_2010_2020_geo_names")
dbDisconnect(con)

counties <- geo_names %>% filter(region_type=="county")
earn_job$GeoFips <- as.character(earn_job$GeoFips)
earn_job_ct <- left_join(earn_job, counties, by=c("GeoFips" = "geoid"))
earn_job_ct <- earn_job_ct %>% select(-GeoName)

earn_job_ct_long <- melt(earn_job_ct,
                           id.vars=c("GeoFips", "region_type", "region_name"),
                           variable.name="measure",
                           value.name="value"
)

earn_job_ct_long['year'] =  str_sub(earn_job_ct_long$measure,-4,-1)
earn_job_ct_long$measure = str_sub(earn_job_ct_long$measure,1,-6)

earn_job_ct_long['measure_type'] = 'count'
earn_job_ct_long['measure_type'][earn_job_ct_long["measure"] == "earnings_per_job"] <- 'dollars'
earn_job_ct_long['measure_type'][earn_job_ct_long["measure"] == "tot_compensation"] <- 'dollars'

names(earn_job_ct_long)[1] <- 'geoid'

# re-arrange columns
earn_job_ct_long <- earn_job_ct_long[, c(1, 2, 3, 6, 4, 5, 7)]

# upload to database
con <- dbConnect(PostgreSQL(),
                 dbname = "sdad",
                 host = "postgis1",
                 port = 5432,
                 user = "YOUR_USERNAME",
                 password = "YOUR_PASSWORD")

dbWriteTable(con, c("dc_education_training", "va_ct_bea_2015_2020_earnings_per_job"),
             earn_job_ct_long,  row.names = F)
dbWriteTable(con, c("dc_education_training", "va_tr_acs_2015_2019_emp_rate"),
             emp_rate,  row.names = F)

dbRemoveTable(con, c("dc_education_training", "va_ct_bea_2015_2020_earnings_per_job"))
dbRemoveTable(con, c("dc_education_training", "va_tr_acs_2015_2019_emp_rate"))

dbSendStatement(con, "ALTER TABLE dc_education_training.va_ct_bea_2015_2020_earnings_per_job
                    OWNER TO data_commons")
dbSendStatement(con, "ALTER TABLE dc_education_training.va_tr_acs_2015_2019_emp_rate
                    OWNER TO data_commons")

dbDisconnect(con)
