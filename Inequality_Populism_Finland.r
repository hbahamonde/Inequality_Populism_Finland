cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Inequality_Populism_Finland/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

## ---- loadings:d ----
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

# Save a dataset with city, year, and Gini for structural break tests in Stata
p_load(tidyverse,foreign)
Gini.structural.break.stata.d = dat %>%  select(Year, City, Gini) %>% drop_na()
write.dta(Gini.structural.break.stata.d, "gini.dta")

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

# export subsetted data to stata
p_load(tidyverse,foreign)
dat.stata <- dat %>%  select(Year, City, share.ps, Gini, Gini.diff.1, Gini.lag.1, Gini.lag.2)
write.dta(dat.stata, "dat.dta")
## ----


############
# Analyses
############

## ---- plots:d ----
# Descriptives
p_load(ggplot2,tidyverse)

# Share
share.plot = dat %>% 
  ggplot(aes(x = Year, y = share.ps)) +
  #geom_jitter(width = 0.25, alpha = 1/5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Overtime Electoral Perfomance of the Finns Party") +
  theme_bw() +
  labs(y = "Share of Finns Party", x = "Year") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10),
        legend.position = "none",
        aspect.ratio=4/4)


# Gini
gini.plot = dat %>% 
  ggplot(aes(x = Year, y = Gini)) +
  #geom_jitter(width = 0.25, alpha = 1/5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Overtime Evolution of Gini Index in Finland") +
  theme_bw() +
  labs(y = "Gini Index", x = "Year") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10),
        legend.position = "none",
        aspect.ratio=4/4)

# Combine both plots
p_load(ggpubr)
theme_set(theme_pubr())

dependent.var.plot = ggarrange(share.plot, gini.plot, 
                               #labels = c("A", "B"),
                               ncol = 2, nrow = 1)

ggsave(
  "gini_finns_historical.jpeg",
  device = "jepg",
  plot = dependent.var.plot,
  scale = 1,
  width = NA,
  height = NA,
  units = c("cm"),
  dpi = 300,
  limitsize = TRUE)


# # # # # # # # # # # #
# Maps
# # # # # # # # # # # #

p_load(geofi,ggplot2,sf,paletteer) # do not install packages that need compilation when propmpted

# Get Municipality Names
municipalities = get_municipalities(year = 2020, scale = 4500) 

# Change Name of City
municipalities <- municipalities %>% rename("City" = "name")

# Merge Gini and Share PS
municipalities = merge(x = municipalities, y = dat[ , c("City", "Gini", "share.ps", "Year")], by = "City", all.x=TRUE)

# Keep 1995 and 2019 years
p_load("dplyr")
municipalities = municipalities %>% filter(Year == 1995 | Year == 2019)


# Gini Plot
p_load("ggplot2")

gini.map.plot = ggplot(municipalities) + 
  geom_sf(aes(fill = Gini)) +
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Overtime Evolution of the Gini Coefficient in Finland") +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10))

# Populist Plot
p_load("ggplot2")
populist.map.plot = ggplot(municipalities) + 
  geom_sf(aes(fill = share.ps)) +
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Overtime Share of the Populist Party") +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10))

# Combine both plots
p_load(ggpubr)
theme_set(theme_pubr())

maps.plot = ggarrange(gini.map.plot, populist.map.plot,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)


# Plotting DV and IV
p_load("ggplot2")

# drop if Gini, Year, or share.ps are missing
dat.plot = dat %>% drop_na(c("Gini", "Year", "share.ps"))

gini.dep.var.plot.histogram = ggplot(dat.plot, aes(x=Gini)) + geom_histogram() + facet_wrap(~Year, ncol = length(unique(dat$Year))) +
  theme_bw() +
  labs(y = "Count", x = "Year", title = "Overtime Evolution of the Gini Coefficient in Finland") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10))

share.dep.var.plot.histogram = ggplot(dat.plot, aes(x=share.ps)) + geom_histogram() + facet_wrap(~Year, ncol = length(unique(dat$Year))) +
  theme_bw() +
  labs(y = "Count", x = "Year", title = "Overtime Share of the Populist Party") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10))

# Combine both plots
p_load(ggpubr)
theme_set(theme_pubr())

histogram.dep.var.plot = ggarrange(gini.dep.var.plot.histogram, share.dep.var.plot.histogram,
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2)
## ----


## ---- dep:var:plot ----
histogram.dep.var.plot
histogram.dep.var.plot.legend <- paste(
  "{\\bf Evolution of TEST TEST TEST}.",
  "\\\\\\hspace{\\textwidth}", 
  "{\\bf Note}: Note here.",
  "\n")
## ---- 





## ---- maps:plot ----
maps.plot
maps.plot.legend <- paste(
  "{\\bf Evolution of TEST TEST TEST}.",
  "\\\\\\hspace{\\textwidth}", 
  "{\\bf Note}: Note here.",
  "\n")

## ---- 



############
# Models
############

# justification for having both entity and time fixed effects:
# eliminate bias from unobservables that *change over time* but *are constant over entities* and 
# it controls for factors that *differ across entities* but *are constant over time*.
# https://www.econometrics-with-r.org/10-4-regression-with-time-fixed-effects.html

# https://www.princeton.edu/~otorres/Panel101R.pdf
# pFtest(plm(log(share.ps) ~ Gini.lag.1, data=dat, index=c("City", "Year"), model="within"), lm(log(share.ps) ~ Gini.lag.1, dat)) # If the p-value is < 0.05 then the fixed effects model is a better choice


# GMM
# for Mac users: it NEEDS https://github.com/fxcoudert/gfortran-for-macOS/releases/download/10.2/gfortran-10.2-Catalina.dmg (CATALINA, no newer version)
# install.packages("gmm")
library(gmm)

# Gmm in STATA
# https://www.stata.com/manuals13/rgmm.pdf
# https://www.eco.uc3m.es/~ricmora/mei/materials/Session_14_GMM_estimation.pdf (see "Linear Regression").
# read Wawro again.
# https://blog.stata.com/2016/10/04/estimating-covariate-effects-after-gmm/




## ---- models:d ----
# Fixed Effects using lm
m1 = lm(share.ps ~ Gini.diff.1 + factor(City)-1, data=dat) # Fixed Effects (city intercepts)
m2 = lm(share.ps ~ Gini.lag.1 + factor(City)-1, data=dat) # Fixed Effects (city intercepts)
m3 = lm(share.ps ~ Gini.lag.2 + factor(City)-1, data=dat) # Fixed Effects (city intercepts)

# Pooling (no FE's and pooled std errors)
m4 = lm(log(share.ps) ~ Gini.diff.1, data=dat) 
m5 = lm(log(share.ps) ~ Gini.lag.1, data=dat) 
m6 = lm(log(share.ps) ~ Gini.lag.2, data=dat)


# Fixed Effects using lm LOG OF DV
m7 = lm(log(share.ps) ~ Gini.diff.1 + factor(City)-1, data=dat) # Fixed Effects
m8 = lm(log(share.ps) ~ Gini.lag.1 + factor(City)-1, data=dat) # Fixed Effects
m9 = lm(log(share.ps) ~ Gini.lag.2 + factor(City)-1, data=dat) # Fixed Effects

# Getting Clustered Std Errors by City: ALL MODELS (except for pooled models)
p_load(multiwayvcov)


# Var Covariance Matrix for Clustered Std Error Models
options(scipen=999)
# FE and clustered (no logged DV)
vcov.year.city.m1 <- cluster.vcov(m1, cbind(dat$City))
vcov.year.city.m2 <- cluster.vcov(m2, cbind(dat$City))
vcov.year.city.m3 <- cluster.vcov(m3, cbind(dat$City))
# FE and clustered (logged DV)
vcov.year.city.m7 <- cluster.vcov(m7, cbind(dat$City))
vcov.year.city.m8 <- cluster.vcov(m8, cbind(dat$City))
vcov.year.city.m9 <- cluster.vcov(m9, cbind(dat$City))

# Getting Values:  Std. Error
# options(scipen=999)
# FE and clustered (no logged DV)
# coeftest(m1, vcov.year.city.m1)[1,2] # Std. Error m1
# coeftest(m2, vcov.year.city.m2)[1,2] # Std. Error m2 
# coeftest(m3, vcov.year.city.m3)[1,2] # Std. Error m3 
## pooled
# coeftest(m4)[1,2] # Std. Error m4 
# coeftest(m5)[1,2] # Std. Error m5 
# coeftest(m6)[1,2] # Std. Error m6 
# FE and clustered (logged DV)
# coeftest(m7, vcov.year.city.m7)[1,2] # Std. Error m7 
# coeftest(m8, vcov.year.city.m8)[1,2] # Std. Error m8 
# coeftest(m9, vcov.year.city.m9)[1,2] # Std. Error m9 

# Getting Values:  P-values
# options(scipen=999)
# coeftest(m1, vcov.year.city.m1)[1,4] # pvalue m1
# coeftest(m2, vcov.year.city.m2)[1,4] # pvalue m2 
# coeftest(m3, vcov.year.city.m3)[1,4] # pvalue m3 
## pooled
# coeftest(m4, vcov.year.city.m4)[1,4] # pvalue m4 
# coeftest(m5, vcov.year.city.m5)[1,4] # pvalue m5 
# coeftest(m6, vcov.year.city.m6)[1,4] # pvalue m6 
# FE and clustered (logged DV)
# coeftest(m7, vcov.year.city.m7)[1,4] # pvalue m7 
# coeftest(m8, vcov.year.city.m8)[1,4] # pvalue m8 
# coeftest(m9, vcov.year.city.m9)[1,4] # pvalue m9 

## All coefficients: Std Errors
p_load(lmtest)
m1.clust.std.error = coeftest(m1, vcov.year.city.m1)[,2] # Std. Error m1
m2.clust.std.error = coeftest(m2, vcov.year.city.m2)[,2] # Std. Error m2 
m3.clust.std.error = coeftest(m3, vcov.year.city.m3)[,2] # Std. Error m3 
m4.clust.std.error = coeftest(m4)[,2] # Std. Error m4 
m5.clust.std.error = coeftest(m5)[,2] # Std. Error m5 
m6.clust.std.error = coeftest(m6)[,2] # Std. Error m6 
m7.clust.std.error = coeftest(m7, vcov.year.city.m7)[,2] # Std. Error m7 
m8.clust.std.error = coeftest(m8, vcov.year.city.m8)[,2] # Std. Error m8 
m9.clust.std.error = coeftest(m9, vcov.year.city.m9)[,2] # Std. Error m9 

## All coefficients: P Values
m1.clust.pvalue = coeftest(m1, vcov.year.city.m1)[,4] # pvalue m1
m2.clust.pvalue = coeftest(m2, vcov.year.city.m2)[,4] # pvalue m2 
m3.clust.pvalue = coeftest(m3, vcov.year.city.m3)[,4] # pvalue m3 
m4.clust.pvalue = coeftest(m4)[,4] # pvalue m4 
m5.clust.pvalue = coeftest(m5)[,4] # pvalue m5 
m6.clust.pvalue = coeftest(m6)[,4] # pvalue m6 
m7.clust.pvalue = coeftest(m7, vcov.year.city.m7)[,4] # pvalue m7 
m8.clust.pvalue = coeftest(m8, vcov.year.city.m8)[,4] # pvalue m8 
m9.clust.pvalue = coeftest(m9, vcov.year.city.m9)[,4] # pvalue m9 
## ---- 

## ---- effects:plot:d ----
# plot
## https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html
p_load(margins)
conditional.effects.plot.d = as.data.frame(cplot(m2, 
                                 "Gini.lag.1", 
                                 what = "prediction", 
                                 draw = FALSE # omits plot
                                 ))

conditional.effects.plot = ggplot(conditional.effects.plot.d, aes(xvals)) + 
  geom_line(aes(y=yvals), colour="blue") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2) +
  theme_bw() +
  labs(y = "Predicted Value", x = "Gini (t-1)", title = "Conditional Effect of Gini on the Electoral Share\nof the Populist Party in Finland") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10),
        aspect.ratio=4/4)
## ---- 


## ---- effects:plot ----
conditional.effects.plot
conditional.effects.plot.legend <- paste(
  "{\\bf Evolution of TEST TEST TEST}.",
  "\\\\\\hspace{\\textwidth}", 
  "{\\bf Note}: Note here.",
  "\n")
## ---- 




## ---- table ----
# Table
p_load(texreg)

# c(noquote(paste('m', 1:9, collapse = ", ", sep = "")))

texreg( # screenreg texreg
  list(m1, m2, m3, m4, m5, m6, m7, m8, m9
    #m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13
    ),
  custom.header = list(
    "1" = 1,
    "2" = 2,
    "3" = 3, 
    "4" = 4,  
    "5" = 5, 
    "6" = 6, 
    "7" = 7,
    "8" = 8,
    "9" = 9),
  custom.model.names = c(
    #m1, m2, m3
    "FE", "FE", "FE", 
    # m4, m5, m6
    "Pooled","Pooled","Pooled",  
    # m7, m8, m9
    "FE+Log", "FE+Log","FE+Log"
    ),
  #custom.coef.names = NULL,
  omit.coef = "(City)",
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
  override.se = list(c(m1.clust.std.error), c(m2.clust.std.error), c(m3.clust.std.error), c(m4.clust.std.error), c(m5.clust.std.error), c(m6.clust.std.error), c(m7.clust.std.error), c(m8.clust.std.error), c(m9.clust.std.error)),
  override.pvalues = list(c(m1.clust.pvalue), c(m2.clust.pvalue), c(m3.clust.pvalue), c(m4.clust.pvalue), c(m5.clust.pvalue), c(m6.clust.pvalue), c(m7.clust.pvalue), c(m8.clust.pvalue), c(m9.clust.pvalue)),
  #symbol = "\\\\cdot",
  label = "reg:t",
  caption = "Dynamic Panel-Data Models: The Effect of Inequality on the Share of The Populist Party.",
  caption.above = T,
  center = T,
  float.pos="H",
  use.packages = FALSE,
  threeparttable = TRUE,
  scalebox = 0.8,
  custom.note = "\\item %stars. \\item All models have panel-corrected standard errors (at the city level). Dependent variable is the share of the populist party. First three models have city fixed effects. Second set of models do not have city fixed effects. Third set of models have city fixed effects, and the dependent variable is logged.")
## ----



################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
abstract.c = as.character(c("Abstract here"))
writeLines(abstract.c, fileConn)
close(fileConn)
## ----

## ---- abstract.length ----
abstract.c.l = sapply(strsplit(abstract.c, " "), length)
## ----
