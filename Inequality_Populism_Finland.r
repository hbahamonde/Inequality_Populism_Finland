cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Inequality_Populism_Finland/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import inequality data
p_load("readxl")
inequality.d <- read_excel("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/inequality_data/Inequality_Data.xlsx")

# Repeat year
p_load(dplyr,tidyr)
inequality.d = inequality.d %>%  fill(Year) 

# import voting data
p_load(readxl)
voting.d <- read_excel("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/voting_data/Voting_Data.xlsx")

# Repeat year
p_load(dplyr,tidyr)
voting.d = voting.d %>%  fill(City) 

# Delete numbers from City names (districts?)
voting.d$City = gsub('[0-9]+', '', voting.d$City)

# Deleting numbers leaves a whitespace in the city name. Delet that.
voting.d$City = trimws(voting.d$City)

# Merge both datasets
dat = merge(voting.d, inequality.d, by.x = c("City", "Year"), all = TRUE)

# Calculate differenced inequality
## First: Sort Data
dat <- dat[order(dat$City, dat$Year),] 

## 1 year prior
p_load(dplyr)
dat = dat %>%
  group_by(City) %>%
  mutate(Gini.d1 = Gini - lag(Gini))

#
dat$City = as.factor(dat$City)
p_load(lubridate); 
dat$Year = year(as.Date(as.character(dat$Year), format = "%Y"))



# Drop
## Drop observations for which there were no elections.
dat <- dat[!(dat$Year==1996 | dat$Year==1997 | dat$Year==1998 | dat$Year==2000 | dat$Year==2001 | dat$Year==2002 | dat$Year==2004 | dat$Year==2005 | dat$Year==2006 | dat$Year==2008 | dat$Year==2009 | dat$Year==2010 | dat$Year==2012 | dat$Year==2013 | dat$Year==2014 | dat$Year==2016 | dat$Year==2017 | dat$Year==2018 | dat$Year==2020 | dat$Year==2021),]
## Drop observations for which there were no Gini coefs
dat <- dat[!(dat$Year==1983 | dat$Year==1987 | dat$Year==1991),]
## Drop obs for which GINIs are missing
dat <- dat[!is.na(dat$Gini),]
## Drop obs for which the share of populist party is zero
dat <- dat[!(dat$share.ps==0),]


# todo
## 1. map inequality differences and share of populist party in two separate maps
## 2. run correlation between share and share of populist party in two separate maps

# Models
summary(lm(log(share.ps) ~ log(Gini), dat))











