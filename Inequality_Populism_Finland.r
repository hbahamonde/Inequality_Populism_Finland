## ---- loadings:d ----
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
## Statistics Finland Data: https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__evaa/statfin_evaa_pxt_13sw.px/
## We're using parties that have won at least a seat in Parliament: https://en.wikipedia.org/wiki/List_of_political_parties_in_Finland
## Statistics Finland Data: https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__evaa/statfin_evaa_pxt_13sw.px/


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

# Drop "Whole Country"
dat = dat[!grepl("Whole country", dat$City),]

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
dat <- dat[!(dat$Year==1996 | dat$Year==1997 | dat$Year==1998 | dat$Year==2000 | dat$Year==2001 | dat$Year==2002 | dat$Year==2004 | dat$Year==2005 | dat$Year==2006 | dat$Year==2008 | dat$Year==2009 | dat$Year==2010 | dat$Year==2012 | dat$Year==2013 | dat$Year==2014 | dat$Year==2016 | dat$Year==2017 | dat$Year==2018 | dat$Year==2020),]
## Drop observations for which there were no Gini coefs
dat <- dat[!(dat$Year==1983 | dat$Year==1987 | dat$Year==1991),]
## Drop obs for which GINIs are missing
dat <- dat[!is.na(dat$Gini),]
## Drop obs for which the share of populist party is zero
# dat <- dat[!(dat$share.ps==0),]


############
# Religion Data
############

# import religion data
religion.d <- read.csv("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/World_Pop_Review/religion.csv")

# gen muslim variable
religion.d$pop.religion = apply(religion.d[, 3:ncol(religion.d)], 1, max)
religion.d$muslim = ifelse(religion.d$pop.religion == religion.d$religionByCountry_muslims, 1, 0)
# be adviced: original data contains estimations. Thus, small islands or the Vatican, for ex., 
# have 5k for each, showing that there's a majority of Muslims. I will exclude these 
# entries later during merging process.
religion.d = data.frame(Country=religion.d$Country, muslim=religion.d$muslim) # filter out columns


############
# Population Data: Immigration
############

# import immigration data
p_load("readxl")
immigration.d1 <- read_excel("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/Statistics_Finland/Population/Statistics_Finland_Immigration_TS.xlsx")
immigration.d1 = as.data.frame(immigration.d1, header = FALSE)
immigration.d2 = immigration.d1 # copy df
immigration.d1.max.year = max(as.numeric(colnames(immigration.d1)), na.rm = T)
immigration.d1 <- immigration.d1[ ,-c(1) ]
immigration.d1 = c(as.numeric(c(t(immigration.d1[1,]))))


# immigration tot df
#####################

immigration.tot.d = data.frame(
  Year = c(1990:immigration.d1.max.year),
  immigration.yearly = immigration.d1
  )

# Melt immigration df
#######################
immigration.d2 <- immigration.d2[ -c(1:2), ] # excludes Tot and Finland
p_load(data.table)
immigration.d2 <- melt(setDT(immigration.d2), id.vars = c("Country"), variable.name = "year")
immigration.d2 = immigration.d2[order(immigration.d2$Country, immigration.d2$year),]
rownames(immigration.d2) <- NULL
p_load("dplyr")
immigration.d2 <- immigration.d2 %>% rename("immigration" = "value")


############
# Population Data: Population
############

# import population data
p_load("readxl")
population.d1 <- read_excel("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/Statistics_Finland/Population/Statistics_Finland_Population_TS.xlsx")
population.d1 = as.data.frame(population.d1, header = FALSE)
population.d2 = population.d1 # copy df
population.d1.max.year = max(as.numeric(colnames(population.d1)), na.rm = T)
population.d1 <- population.d1[ ,-c(1) ]
population.d1 = c(as.numeric(c(t(population.d1[1,]))))

# tot immigrant population df
#############################

population.tot.d = data.frame(
  Year = c(1990:population.d1.max.year),
  imm.pop.cum = population.d1
  )

# merge two tot immigration/population df's
immigration.tot.d = merge(population.tot.d, immigration.tot.d, by = "Year")


# Melt imm population df
#######################
population.d2 <- population.d2[ -c(1:2), ] # excludes Tot and Finland
p_load(data.table)
population.d2 <- melt(setDT(population.d2), id.vars = c("Country"), variable.name = "year")
population.d2 = population.d2[order(population.d2$Country, population.d2$year),]
rownames(population.d2) <- NULL
p_load("dplyr")
population.d2 <- population.d2 %>% rename("imm.pop" = "value")

# Merge imm population df and immigration df
############################################
imm.pop.d = merge(immigration.d2, population.d2, by = c("Country","year"))
imm.pop.d = merge(imm.pop.d, religion.d, by = c("Country"), all.y = T,all.x = T, incomparables = NA)


# cleaning
imm.pop.d = imm.pop.d[complete.cases(imm.pop.d$year), ] # exclude cases without a year
imm.pop.d = imm.pop.d[imm.pop.d$Country!="Former Soviet Union", ] # Excludes USSR.

p_load("dplyr")
imm.minor.country <- imm.pop.d %>% # excludes countries that did not have immigration nor population of foreign background
  group_by(Country) %>%
  summarize(imm.minor.country.2 = sum(immigration, na.rm = TRUE))
imm.minor.country = as.data.frame(imm.minor.country)
imm.minor.country$imm.minor.country = ifelse(imm.minor.country$imm.minor.country.2 == 0, 1, 0)

imm.pop.minor.country <- imm.pop.d %>% # excludes countries that did not have immigration nor population of foreign background
  group_by(Country) %>%
  summarize(imm.pop.minor.country.2 = sum(imm.pop, na.rm = TRUE))
imm.pop.minor.country = as.data.frame(imm.pop.minor.country)
imm.pop.minor.country$imm.pop.minor.country = ifelse(imm.pop.minor.country$imm.pop.minor.country.2 == 0, 1, 0)

# merge
imm.pop.minor.country.d = merge(imm.pop.minor.country,imm.minor.country, by = "Country")
imm.pop.minor.country.d$unimportant.immigration = ifelse(imm.pop.minor.country.d$imm.pop.minor.country & imm.pop.minor.country.d$imm.minor.country == 0, 1,0)
p_load(tidyverse)
imm.pop.minor.country.d = imm.pop.minor.country.d %>%  select(Country, unimportant.immigration)

# exclude countries where immigration is not important (Like Vatican, and other small islands)
imm.pop.d = merge(imm.pop.d,imm.pop.minor.country.d, by = "Country")

# Delete countries that did not have nor immigration nor population of foreign origin during this time period
p_load(dplyr)
imm.pop.d <- imm.pop.d %>% filter(unimportant.immigration != 1)
imm.pop.d = subset(imm.pop.d, select = -c(unimportant.immigration) )

# Compute yearly "Muslim Immigration"
muslim.immigration.d <- imm.pop.d %>% # 
  filter(muslim == 1) %>%
  group_by(year) %>%
  summarize(muslim.imm.yearly = sum(immigration, na.rm = TRUE))

muslim.immigration.d = data.frame(muslim.immigration.d)
# plot(muslim.immigration.d)

# Compute yearly "Muslim Population"
muslim.population.d <- imm.pop.d %>% # 
  filter(muslim == 1) %>%
  group_by(year) %>%
  summarize(muslim.pop.cum = sum(imm.pop, na.rm = TRUE))

muslim.population.d = data.frame(muslim.population.d)
# plot(muslim.population.d)

# merge both df's
muslim.pop.imm.d = merge(muslim.population.d, muslim.immigration.d, by = "year")
p_load(dplyr)
muslim.pop.imm.d <- muslim.pop.imm.d %>% rename("Year" = "year")
muslim.pop.imm.d$Year = as.numeric(as.character(muslim.pop.imm.d$Year))

# Save rdata
save(muslim.pop.imm.d, file = "muslim_pop_imm_d.RData")

# Now include whether countries are developed or not.
# merge it with "imm.pop.d"

# import econ development data
econ.development.d <- read.csv("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/World_Bank/World_Bank_Income_Classification.csv", 
                                             sep = ";" , 
                                             check.names = FALSE)
econ.development.d = econ.development.d[,2:38] # dropping useless columns
econ.development.d = melt(setDT(econ.development.d), id.vars = c("Country.name"), variable.name = "Year")
econ.development.d$Year = as.numeric(as.character(econ.development.d$Year))
econ.development.d$value = as.factor(econ.development.d$value)
p_load("dplyr")
econ.development.d = econ.development.d %>% filter(Year >= min(dat$Year))
econ.development.d <- econ.development.d %>% rename("Econ.Dev" = "value")


# merge immigration/(foreign)population df with econ.development.d
p_load(dplyr)
imm.pop.d <- imm.pop.d %>% rename("Year" = "year")
econ.development.d <- econ.development.d %>% rename("Country" = "Country.name")

imm.pop.d$Year = as.numeric(as.character(imm.pop.d$Year))
imm.pop.d = merge(econ.development.d, imm.pop.d, by = c("Year", "Country"), all=T) # merges with yearly immigration tot

# compute L% and LM% (combined) immigration per year (use econ.development.d df)
p_load(dplyr)
cat.econ.development.d = na.omit(econ.development.d)
cat.econ.development.d = cat.econ.development.d[!cat.econ.development.d$Econ.Dev == "" | cat.econ.development.d$Econ.Dev == "LM*", ]
cat.econ.development.d = droplevels(cat.econ.development.d)

cat.econ.development.d = cat.econ.development.d %>%
  group_by(Year, Econ.Dev) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100)

# Save rdata
save(cat.econ.development.d, file = "cat_econ_development_d.RData")


# plot multiple categories
pdf(file = "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Imm_Econ_Dev.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches
p_load(car)
par(pty="s")

scatterplot(freq~Year|Econ.Dev, smooth=F, regLine=T, data=cat.econ.development.d, 
            main="Immigration in Finland by Level\nof Economic Development of Country of Origin", 
            xlab="Year", 
            ylab="%",
            legend=list(coords="bottomleft"))

dev.off()

# plot binary (recoded development)
cat.econ.development.d$Low.Econ.Dev = recode_factor(cat.econ.development.d$Econ.Dev, 
                                                    "H"="High", 
                                                    "L" = "Low",
                                                    "LM" = "Low",
                                                    "UM" = "High")

cat.econ.development.d = cat.econ.development.d %>% 
  group_by(Year, Low.Econ.Dev) %>%
  summarise(Perc.Imm.High.Low = sum(freq)) 


pdf(file = "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Imm_Econ_Dev_Binary.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches
p_load(car)
par(pty="s")

scatterplot(Perc.Imm.High.Low~Year|Low.Econ.Dev, smooth=F, regLine=T, data=cat.econ.development.d, 
            main="Immigration in Finland by Level\nof Economic Development of Country of Origin", 
            xlab="Year", 
            ylab="%",
            legend=list(coords="bottomleft"))

dev.off()

# plot difference HIGH - LOW
diff.econ.development.d = cat.econ.development.d %>%
  spread(key = Low.Econ.Dev, value = Perc.Imm.High.Low) %>%
  mutate(Difference = High - Low) %>%
  select(Year, Difference)


pdf(file = "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Imm_Econ_Dev_Diff.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches

p_load(car)
par(pty="s")
scatterplot(Difference~Year, smooth=F, regLine=T, data=diff.econ.development.d, 
            main="Proportion of High Dev. to Low Dev. Immigration in Finland", 
            xlab="Year", 
            ylab="%", 
            boxplots = F,
            legend=list(coords="bottomleft"))

dev.off()


# 
pdf(file = "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Imm_Pop_Cum.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches

p_load(car)
options(scipen=999)
par(pty="s")

scatterplot(imm.pop.cum ~ Year, 
            smooth=F, 
            regLine=F, 
            data=immigration.tot.d, 
            main="Immigrant Population (cumulative) in Finland", 
            xlab="Year", 
            ylab="N", 
            boxplots = F,
            legend=list(coords="bottomleft"),
            col="blue")

dev.off()


#
pdf(file = "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Imm_Pop_Yearly.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches

p_load(car)
options(scipen=999)
par(pty="s", mfrow=c(1,2))


scatterplot(immigration.yearly ~ Year, 
            smooth=F, 
            regLine=F, 
            data=immigration.tot.d, 
            main="Yearly Immigrant Population in Finland", 
            xlab="Year", 
            ylab="N", 
            boxplots = F,
            legend=list(coords="bottomleft"),
            col="red")

dev.off()

#
pdf(file = "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Muslim_Imm_Pop_Cum.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches

p_load(car)
options(scipen=999)
par(pty="s")


scatterplot(muslim.pop.cum ~ Year, 
            smooth=F, 
            regLine=F, 
            data=muslim.pop.imm.d, 
            main="Muslim Population (cumulative) in Finland", 
            xlab="Year", 
            ylab="N", 
            boxplots = F,
            legend=list(coords="bottomleft"),
            col="red")

dev.off()

#
pdf(file = "/Users/hectorbahamonde/research/Inequality_Populism_Finland/Muslim_Imm_Pop_Yearly.pdf",   # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches

p_load(car)
options(scipen=999)
par(pty="s")

scatterplot(muslim.imm.yearly ~ Year, 
            smooth=F, 
            regLine=F, 
            data=muslim.pop.imm.d, 
            main="Yearly Muslim Population in Finland", 
            xlab="Year", 
            ylab="N", 
            boxplots = F,
            legend=list(coords="bottomleft"),
            col="red")

dev.off()


# merge immigration/(foreign)population df with big dataset
dat = merge(dat, immigration.tot.d, by = "Year", all=T) # merges with yearly immigration tot
dat = merge(dat, muslim.pop.imm.d, by = "Year", all=T) # merges with yearly muslim immigration tot 
dat = merge(dat, diff.econ.development.d, by = "Year", all=T) # merges with difference between High-Low dev country immigration

dat <- dat[complete.cases(dat$Year), ] # cleaning
dat <- dat[complete.cases(dat$City), ] # cleaning

# reordering
# dat <- dat %>% select(everything(), imm.pop.cum, ) 

# sort again
dat <- dat[order(dat$City, dat$Year),] 

# Save rdata
save(dat, file = "dat.RData")

# export subsetted data to stata
p_load(tidyverse,foreign)
dat.stata <- dat %>%  select(Year, City, share.ps, Gini, Gini.diff.1, Gini.lag.1, Gini.lag.2, imm.pop.cum, immigration.yearly, muslim.pop.cum, muslim.imm.yearly, Difference)
write.dta(dat.stata, "dat.dta")
## ----

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

load("/Users/hectorbahamonde/research/Inequality_Populism_Finland/dat.RData")


# plot municipal trajectories of Finns vote share
# notice these plots are fitting blue regression lines. Thus, they're estimations.
# They DO NOT reflect actual values (but, again, estimations). 
# It looks like there are two (same) towns that have divergent trajectories in 1995 and 2020 regarding
# their Gini and Finns levels. However, they're NOT the same towns. 

p_load(ggplot2,ggpubr)

panel.finns.p = dat %>% 
  ggplot(aes(x = Year, y = share.ps)) +
  stat_smooth(aes(group = City),
              method = "lm", se = F, size = 0.09) +  
  stat_smooth(method = "lm", se = F, size = 1, color = "red") +  
  theme_light() + 
  labs(y="Share of Finns Party (%)", x="Year") + 
  theme(legend.position = "none", aspect.ratio=1)

panel.gini.p = dat %>% 
  ggplot(aes(x = Year, y = Gini)) +
  stat_smooth(aes(group = City),
              method = "lm", se = F, size = 0.09) +  
  stat_smooth(method = "lm", se = F, size = 1, color = "red") +  
  theme(panel.grid = element_blank()) +
  theme_light() + labs(y="Gini Index", x="Year") + theme(legend.position = "none", aspect.ratio=1)

ggarrange(panel.finns.p, panel.gini.p, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("paul-buerkner/brms")


library("brms")

# with our 0 + Intercept solution, we told brm() to suppress the default intercept and replace it with our smartly-named Intercept parameter. This is our fixed effect for the population intercept and, importantly, brms() will assign default priors to it based on the data themselves without assumptions about centering.
# https://bookdown.org/content/4253/introducing-the-multilevel-model-for-change.html#examining-estimated-fixed-effects

fit3.2 <-
  brm(data = dat,
      family = gaussian,
#       formula = share.ps ~ 0 + Intercept + Gini + (1 + immigration.yearly + muslim.imm.yearly + Year | City),
#       formula = share.ps ~  Gini + muslim.imm.yearly + (1 + muslim.imm.yearly + Year | City), # (working hyp)
       formula = share.ps ~  Gini + immigration.yearly + (1 + immigration.yearly + Year | City),
iter = 2000, warmup = 400, chains = 1, 
#cores = 4, # allow you to sample from all four chains simultaneously
      control = list(adapt_delta = 0.95),
      seed = 3
      )

#print(fit3.2)

plot(conditional_effects(fit3.2), ask = T)

draws <- as_draws_df(fit3.2)

draws[, 1:10] %>%
  glimpse()


draws <- as_draws_df(fit3.2)

draws %>% 
  pivot_longer(starts_with("b_")) %>% 
  
  ggplot(aes(x = value)) +
  geom_density(color = "transparent", fill = "grey25") +
  scale_y_continuous(NULL, breaks = NULL) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ name, scales = "free")




# plot High development immigration in finland
ggplot(econ.development.d[econ.development.d$Econ.Dev=="H"], aes(x = Year, y = Country, color = Econ.Dev)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Economic Development Status of Countries Over Time",
       x = "Year",
       y = "Country",
       color = "Economic Development Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 5, hjust = 1)) 

# plot Low development immigration in finland
ggplot(econ.development.d[econ.development.d$Econ.Dev=="L"], aes(x = Year, y = Country, color = Econ.Dev)) +
  geom_tile() +
  theme_minimal() +
  labs(title = "Economic Development Status of Countries Over Time",
       x = "Year",
       y = "Country",
       color = "Economic Development Status") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(size = 5, hjust = 1)) 


# https://www.alexcernat.com/estimating-multilevel-models-for-change-in-r/
# estimate multilevel for time models

# https://bookdown.org/content/4253/introducing-the-multilevel-model-for-change.html
# bayesian version

# https://www.andrewheiss.com/blog/2021/12/01/multilevel-models-panel-data-guide/


############
# Analyses
############

## ---- plots:d ----
# Descriptives
p_load(ggplot2,tidyverse)

# Share
share.plot = voting.d %>% 
  group_by(Year) %>% 
  ggplot(aes(x = Year, y = PS)) +
  #geom_jitter(width = 0.25, alpha = 1/5) +
  geom_smooth(method = "loess", se = TRUE, fullrange = F, span=1) +
  labs(title = "Overtime Electoral Perfomance of the Finns Party") +
  theme_bw() +
  scale_x_discrete(breaks = seq(1983, 2023, by = 4))  +
  labs(y = "Share of Finns Party (%)", x = "Year") + 
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

dependent.var.plot = ggarrange(gini.plot, share.plot, 
                               #labels = c("A", "B"),
                               ncol = 2, nrow = 1)

ggsave(
  "gini_finns_historical.jpeg",
  device = "jpeg",
  plot = dependent.var.plot,
  scale = 1,
  width = 10, 
  height = 5, 
  units = "in",
  dpi = 600,
  limitsize = TRUE)


# # # # # # # # # # # #
# Maps
# # # # # # # # # # # #

p_load(geofi,ggplot2,sf,paletteer) # do not install packages that need compilation when prompted

# Get Municipality Names
municipalities = get_municipalities(year = 2020, scale = 4500) 

# Change Name of City
municipalities <- municipalities %>% rename("City" = "name")

# Merge Gini and Share PS
municipalities = merge(x = municipalities, y = dat[ , c("City", "Gini", "share.ps", "Year")], by = "City", all.x=TRUE)

# Keep 1995 and 2019 years
p_load("dplyr")
municipalities = municipalities %>% filter(Year == 1995 | Year == 2019 | Year == 2021)

# Adds 2023 elections
municipalities.2023 = get_municipalities(year = 2020, scale = 4500)
municipalities.2023 <- municipalities.2023 %>% rename("City" = "name")
municipalities.2023 = merge(x = municipalities.2023, y = voting.d[ , c("City", "PS", "Year")], by = "City", all.x=TRUE)
p_load("dplyr")
municipalities.2023 = municipalities.2023 %>% filter(Year == 1995 | Year == 2019 | Year == 2021)

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
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        strip.text.x = element_text(size = 7),
        legend.position="bottom")

annotate_figure(gini.map.plot,
                bottom = text_grob("Note: Figure shows average over all cities", color = "black", size = 10)
)


# Populist Plot
p_load("ggplot2")
populist.map.plot = ggplot(municipalities) + 
  geom_sf(aes(fill = share.ps)) +
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Overtime Electoral Share of the Finns Party") +
  guides(fill=guide_legend(title="Electoral Share of\nthe Finns Party")) +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        strip.text.x = element_text(size = 7),
        legend.position="bottom")


# Populist Plot 2023
p_load("ggplot2")
populist.map.plot.2023 = ggplot(municipalities.2023) + 
  geom_sf(aes(fill = PS)) +
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Overtime Electoral Share of the Finns Party (%)") +
  guides(fill=guide_legend(title="Electoral Share of the Finns Party (%)", nrow = 1)) +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=7), 
        axis.title.x = element_text(size=7), 
        legend.text=element_text(size=7), 
        legend.title=element_text(size=7),
        plot.title = element_text(size=7),
        strip.text.x = element_text(size = 7),
        legend.position="bottom")

# Combine both plots
p_load(ggpubr)
theme_set(theme_pubr())

maps.plot = ggarrange(gini.map.plot, populist.map.plot.2023,
                      align = "h",
                      #labels = c("A", "B"),
                      ncol = 2, nrow = 1)


ggsave(
  "maps_plot.jpeg",
  device = "jpeg",
  plot = maps.plot,
  scale = 1,
  width = 10.4, 
  height = 4, 
  units = "in",
  dpi = 600,
  limitsize = TRUE)



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
