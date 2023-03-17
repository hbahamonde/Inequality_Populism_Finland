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

## 1st Order Difference 
dat = transform(dat, Gini.diff.1 = ave(Gini, City, FUN = function(x) c(NA, diff(x))))

# lagged gini

## 1 # NOTICE THAT LAGGED VALUE IS WITH RESPECT TO LAST YEAR'S GINI, SO IT WON'T NECESSARILY REFLECT PRIOR *ELECTION* (WHICH IS THE INTERVAL I'M KEEPING)
p_load(dplyr)
dat <- dat %>%
  group_by(City) %>%
  mutate(Gini.lag.1 = dplyr::lag(Gini, n = 1, default = NA))

## 2 # NOTICE THAT LAGGED VALUE IS WITH RESPECT TO LAST YEAR'S GINI, SO IT WON'T NECESSARILY REFLECT PRIOR *ELECTION* (WHICH IS THE INTERVAL I'M KEEPING)
p_load(dplyr)
dat <- dat %>%
  group_by(City) %>%
  mutate(Gini.lag.2 = dplyr::lag(Gini, n = 2, default = NA))

# Format year
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

# sort again
dat <- dat[order(dat$City, dat$Year),] 

############
# Analyses
############

# todo
## 1. map inequality differences and share of populist party in two separate maps
## 2. run correlation between share and share of populist party in two separate maps

# Models
m1 = lm(log(share.ps) ~ log(Gini)  + City, dat)
m2 = lm(log(share.ps) ~ Gini.diff.1 + City, dat)
m3 = lm(log(share.ps) ~ Gini.lag.1  + City, dat)
m4 = lm(log(share.ps) ~ Gini.lag.2  + City, dat)

# Fixed Effects using PLM
# p_load(plm)
# m5 = plm(log(share.ps) ~ Gini.diff.1, data=dat, index=c("City", "Year"), model="within") # Fixed Effects
# m6 = plm(log(share.ps) ~ Gini.lag.1, data=dat, index=c("City","Year"), model="within") # Fixed Effects
# m7 = plm(log(share.ps) ~ Gini.lag.2, data=dat, index=c("City", "Year"), model="within") # Fixed Effects


# Fixed Effects using lm
m5.lm = lm(log(share.ps) ~ Gini.diff.1 + factor(City)-1, data=dat) # Fixed Effects
m6.lm = lm(log(share.ps) ~ Gini.lag.1 + factor(City)-1, data=dat) # Fixed Effects
m6.lm.2 = lm(share.ps ~ Gini.lag.1 + factor(City)-1, data=dat) # Fixed Effects
m7.lm = lm(log(share.ps) ~ Gini.lag.2 + factor(City)-1, data=dat) # Fixed Effects



# https://www.princeton.edu/~otorres/Panel101R.pdf
# pFtest(plm(log(share.ps) ~ Gini.lag.1, data=dat, index=c("City", "Year"), model="within"), lm(log(share.ps) ~ Gini.lag.1, dat)) # If the p-value is < 0.05 then the fixed effects model is a better choice


# Pooling
m8 = lm(log(share.ps) ~ Gini.diff.1 + factor(City)-1, data=dat) # Fixed Effects
m9 = lm(log(share.ps) ~ Gini.lag.1 + factor(City)-1, data=dat) # Fixed Effects
m10 = lm(log(share.ps) ~ Gini.lag.2 + factor(City)-1, data=dat) # Fixed Effects


# plot
predictions.d = data.frame(predict(m6.lm, 
                                   se.fit = T, 
                                   interval = "confidence",
                                   level = 0.95, 
                                   type = "response",
                                   terms = "Gini.lag.1", 
                                   na.action = na.pass))

predictions.d$Gini = as.vector(m6$model$Gini.lag.1)
predictions.d$Share.observed = as.vector(m6$model$`log(share.ps)`)
predictions.d$residual = as.vector(m6$residuals)


# plot
## https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html
p_load(margins)
cplot(m6.lm, "Gini.lag.1", what = "prediction", main = "Title")

#
library(jtools)
effect_plot(m6.lm.2, pred = Gini.lag.1, interval = TRUE)


# Table
p_load(texreg)

screenreg( # screenreg texreg
  list(m5,m6,m7,m5.lm,m6.lm,m7.lm
    #m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13
    ),
  #custom.header = list(
    #"1" = 1,
    #"2" = 2,
    #"3" = 3, 
    #"4" = 4,  
    #"5" = 5, 
    #"6" = 6, 
    #"7" = 7,
    #"8" = 8,
    #"9" = 9, 
    #"10" = 10, 
    #"11" = 11, 
    #"12" = 12),
  #custom.model.names = c(
    # m0, m1.m, m1.w
    #"Full", "Male", "Female", 
    # m2, m3, m4 
    #"Full","Full","Full",  
    # m2.m, m2.w,
    #"Male", "Female", 
    # m3.m, m3.w
    #"Male", "Female", 
    # m4.m, m4.w
    #"Male", "Female"),
  #custom.coef.names = NULL,
  omit.coef = "(City) |(Year) ",
  #custom.coef.names = c("Intercept",
                        #"Appearance-Occupation Congruence",
                        #"Middle Class",
                        #"Working Class",
                        #"Age",
                        #"Appearance-Occupation Congruence X Middle Class",
                        #"Appearance-Occupation Congruence X Working Class",
                        #"Attractiveness",
                        #"Masculinity",
                        #"Femininity"),
  # custom.header = list( "Poisson" = 1),
  stars = c(0.001, 0.01, 0.05),
  include.adjrs = FALSE,
  #symbol = "\\\\cdot",
  label = "reg:t",
  caption = "Statistical Models",
  caption.above = T,
  center = T,
  float.pos="H",
  use.packages = FALSE,
  threeparttable = TRUE,
  scalebox = 0.4,
  custom.note = "\\item %stars. \\item Note Here")
