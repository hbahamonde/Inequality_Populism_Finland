## ---- loadings:d ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Inequality_Populism_Finland/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import inequality data
# Query from Statistics Finland: https://pxdata.stat.fi:443/PxWeb/sq/8b8c8173-848a-4682-99d9-674cc2173e54
p_load("readxl")
inequality.d <- read_excel("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/inequality_data/Inequality_Data.xlsx")

colnames(inequality.d) <- inequality.d[2,] # changes col names
inequality.d <- inequality.d[-c(1,2), ] # deletes rows
colnames(inequality.d)[2] <- "City"
colnames(inequality.d)[1] <- "Year"

# Repeat year
p_load(dplyr,tidyr)
inequality.d = inequality.d %>%  fill(Year) 

inequality.d = inequality.d[c(1:8680), ] # delete last rows that contain garbage

# colnames(inequality.d)

# Gini Coefficient, Factor Income: This measures the distribution of income before any government taxes or transfers. It primarily includes incomes from labor and capital. This index can help you understand the basic inequalities that exist in the market-generated incomes of a society.
# Gini Coefficient, Gross Income: This includes all pre-tax incomes but before social security and other deductions. It gives a picture of income inequality that includes certain forms of pensions and social incomes but excludes the effects of taxes and most transfers.
# Gini Coefficient, Disposable Cash Income: This reflects income after taxes and transfers. It provides a view of the income distribution after government policies have been applied, such as social security benefits and income taxes. This measure is most relevant when assessing the final impact of governmental policies on reducing income inequality.

# If you are interested in evaluating the raw economic inequality without the influence of government policies, use the Gini Coefficient for Factor Income. 
# If you're more interested in the effects of taxation and governmental redistribution on inequality, the Gini Coefficient for Disposable Cash Income would be more appropriate. 
# For a middle ground that considers some forms of income but not the impact of taxation or most transfers, the Gini Coefficient for Gross Income would be suitable.

# for this project we will use "gross income"
colnames(inequality.d)[colnames(inequality.d)=="Gini coefficient, gross income"] <- "Gini"

# change PS to numeric
p_load(readr)
inequality.d$Gini = parse_number(inequality.d$Gini) # numbers had commas. delete that shit.
inequality.d$Gini = as.numeric(inequality.d$Gini)


# import voting data
## Statistics Finland Data: https://pxdata.stat.fi:443/PxWeb/sq/5817233d-8494-4155-8e27-fb893cd408a0
## We're using parties that have won at least a seat in Parliament: https://en.wikipedia.org/wiki/List_of_political_parties_in_Finland
## Statistics Finland Data: https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__evaa/statfin_evaa_pxt_13sw.px/


p_load(readxl)
voting.d <- read_excel("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/voting_data/Voting_Data.xlsx") # imports

voting.d <- voting.d[-c(1,2), ] # deletes rows
voting.d <- voting.d[ , -c(2,3)] # drops columns
colnames(voting.d)[1] <- "Year"
colnames(voting.d)[2] <- "City"
colnames(voting.d)[3] <- "PS"
colnames(voting.d)[4] <- "share.ps"

# Repeat Year
p_load(dplyr,tidyr)
voting.d <- voting.d %>%  fill(Year) 

# change Year to numeric
p_load(readr)
voting.d$Year = parse_number(voting.d$Year) # numbers had commas. delete that shit.
voting.d$Year = as.numeric(voting.d$Year)

# change PS to numeric
p_load(readr)
voting.d$PS = parse_number(voting.d$PS) # numbers had commas. delete that shit.
voting.d$PS = as.numeric(voting.d$PS)

# change share.ps to numeric
p_load(readr)
voting.d$share.ps = parse_number(voting.d$share.ps) # numbers had commas. delete that shit.
voting.d$share.ps = as.numeric(voting.d$share.ps)

# Delete numbers from City names (districts?)
voting.d$City = gsub('[0-9]+', '', voting.d$City)
voting.d$City = gsub('-', '', voting.d$City)
voting.d$City = gsub('KU', '', voting.d$City)


# Drop "Whole Country"
voting.d = voting.d[!grepl("Whole country", voting.d$City),]
voting.d = voting.d[!grepl("constituency", voting.d$City),]

# Deleting numbers leaves a white space in the city name. Delete that.
voting.d$City = trimws(voting.d$City)

voting.d = voting.d[c(1:5204), ] # delete last rows that contain garbage

p_load(tidyverse,foreign)
voting.d = voting.d %>%  select(Year, City, PS, share.ps)


# Merge both datasets
dat = merge(voting.d, inequality.d, by = c("City", "Year"), all = TRUE)

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

# Format City and Year
dat$City = as.factor(dat$City)
p_load(lubridate); 
dat$Year = year(as.Date(as.character(dat$Year), format = "%Y"))

# Save a dataset with city, year, and Gini for structural break tests in Stata
#p_load(tidyverse,foreign)
#Gini.structural.break.stata.d = dat %>%  select(Year, City, Gini) %>% drop_na()
#write.dta(Gini.structural.break.stata.d, "gini.dta")

# Drop
## Drop observations for which there were no elections.
dat <- dat[!(dat$Year==1996 | dat$Year==1997 | dat$Year==1998 | dat$Year==2000 | dat$Year==2001 | dat$Year==2002 | dat$Year==2004 | dat$Year==2005 | dat$Year==2006 | dat$Year==2008 | dat$Year==2009 | dat$Year==2010 | dat$Year==2012 | dat$Year==2013 | dat$Year==2014 | dat$Year==2016 | dat$Year==2017 | dat$Year==2018 | dat$Year==2020 | dat$Year==2021 | dat$Year==2022),]
## Drop observations for which there were no Gini coefs
dat <- dat[!(dat$Year==1983 | dat$Year==1987 | dat$Year==1991),]
## Drop obs for which GINIs are missing
# dat <- dat[!is.na(dat$Gini),]
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

# raw query: https://pxdata.stat.fi:443/PxWeb/sq/ff657d82-f3a8-498c-afe6-94129545ba2f

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

cat.econ.development.d = cat.econ.development.d %>% # 4 categories
  group_by(Year, Econ.Dev) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100)

# now combine L and ML into one (sum both %'s), so to have one L%+LM% value per year (to then merge these with the rest of the data)
p_load(dplyr)

cat.econ.development.LLM.d <- cat.econ.development.d %>% 
  filter(Econ.Dev %in% c("H", "UM")) %>%
  group_by(Year) %>%
  summarize(HUM.imm = sum(freq))

# Save rdata
save(cat.econ.development.d, file = "cat_econ_development_d.RData")

# merge immigration/(foreign)population df with big dataset
dat = merge(dat, immigration.tot.d, by = "Year", all=T) # merges with yearly immigration tot
dat = merge(dat, muslim.pop.imm.d, by = "Year", all=T) # merges with yearly muslim immigration tot 
dat = merge(dat, cat.econ.development.LLM.d, by = "Year", all=T) # merges with difference between High-Low dev country immigration
#dat = merge(dat, diff.econ.development.d, by = "Year", all=T) # merges with difference between High-Low dev country immigration

# lagged HUM.imm
dat <- dat %>%
  group_by(City) %>%
  mutate(HUM.imm.lag.1 = dplyr::lag(HUM.imm, n = 1, default = NA))

dat <- dat[complete.cases(dat$Year), ] # cleaning
dat <- dat[complete.cases(dat$City), ] # cleaning

# reordering
# dat <- dat %>% select(everything(), imm.pop.cum, ) 

# sort again
dat <- dat[order(dat$City, dat$Year),] 

# drop if share.ps is NA
# dat <- dat[!is.na(dat$share.ps),]


# check for duplicates
# p_load(dplyr)
# dat = dat %>% group_by(City,Year) %>%
#  mutate(duplicated = n() > 1)
# table(dat$duplicated)

# deleting duplicates
# dat = dat[dat$share.ps != 0.0000000000, ] 


# lagged immigration vars

p_load(dplyr)
dat <- dat %>%
  group_by(City) %>%
  mutate(immigration.yearly.lag.1 = dplyr::lag(immigration.yearly, n = 1, default = NA))

dat <- dat %>%
  group_by(City) %>%
  mutate(muslim.imm.yearly.lag.1 = dplyr::lag(muslim.imm.yearly, n = 1, default = NA))


# Save rdata
save(dat, file = "dat.RData")

# export subsetted data to stata
p_load(tidyverse,foreign)
dat.stata <- dat %>%  select(Year, City, Gini, Gini.diff.1, Gini.lag.1, Gini.lag.2, imm.pop.cum, immigration.yearly, muslim.pop.cum, muslim.imm.yearly, HUM.imm, HUM.imm.lag.1)
write.dta(dat.stata, "dat.dta")
## ----


p_load(ggplot2)
muslim.yearly.p = ggplot(muslim.pop.imm.d, aes(x=Year, y=muslim.imm.yearly)) + 
  geom_line(colour="blue", linewidth =1) + 
  theme_bw() +
  labs(y = "Muslim Immigration (N)", x = "Year") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=12), 
        axis.title.x = element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        plot.title = element_text(size=12),
        strip.text.x = element_text(size = 12),
        legend.position = "none",
        aspect.ratio=4/4)

immigration.yearly.p = ggplot(immigration.tot.d, aes(x=Year, y=immigration.yearly)) + 
  geom_line(colour="blue", linewidth =1) + 
  theme_bw() +
  labs(y = "Immigration Yearly (N)", x = "Year") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=12), 
        axis.title.x = element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        plot.title = element_text(size=12),
        strip.text.x = element_text(size = 12),
        legend.position = "none",
        aspect.ratio=4/4)

cat.econ.development.p = ggplot(cat.econ.development.d, aes(x=Year, y=freq, group=Econ.Dev, color=Econ.Dev)) + 
  geom_line(linewidth =1) + 
  theme_bw() +
  labs(y = "Level of development (N)", x = "Year") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=12), 
        axis.title.x = element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        plot.title = element_text(size=12),
        strip.text.x = element_text(size = 12),
        legend.position = c(0.14, 0.227),
        aspect.ratio=4/4) + labs(color="") 

p_load(ggpubr) 
Imm.Mus.EconDev.p = ggarrange(immigration.yearly.p,
                              muslim.yearly.p,
                              cat.econ.development.p,
                              labels = c("A", "B", "C"),
                              ncol = 3, align = "v")


ggsave(
  "Imm_Mus_EconDev_plot.jpeg",
  device = "jpeg",
  plot = Imm.Mus.EconDev.p,
  #scale = 1,
  #width = 4, 
  #height = 2, 
  #units = "in",
  dpi = 1200,
  limitsize = F)


## ---- plots:presentation ----

# Assuming cat.econ.development.d is your data frame and Econ.Dev is the categorical variable
cat.econ.development.d$Econ.Dev <- as.factor(cat.econ.development.d$Econ.Dev)

# Define your custom color palette
custom_colors <- c("H" = "#1f77b4", "L" = "#ff7f0e", "LM" = "#2ca02c", "UM" = "#d62728")

# Check levels of Econ.Dev to ensure they match the custom colors
# levels(cat.econ.development.d$Econ.Dev)

cat.econ.development.p.present = ggplot(cat.econ.development.d, aes(x=Year, y=freq, group=Econ.Dev, color=Econ.Dev)) + 
  geom_line(linewidth = 1) + 
  theme_bw() +
  labs(y = "Level of development (N)", x = "Year") + 
  theme(axis.text.y = element_text(size = 20), 
        axis.text.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.title.x = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.position = c(0.14, 0.227),
        aspect.ratio = 1) + 
  scale_color_manual(values = custom_colors) + 
  labs(color = "")

muslim.yearly.p.present = ggplot(muslim.pop.imm.d, aes(x=Year, y=muslim.imm.yearly)) + 
  geom_line(colour="blue", linewidth =1) + 
  theme_bw() +
  labs(y = "Muslim Immigration (N)", x = "Year") + 
  theme(axis.text.y = element_text(size=20), 
        axis.text.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size = 20),
        legend.position = "none",
        aspect.ratio=4/4)

immigration.yearly.p.present = ggplot(immigration.tot.d, aes(x=Year, y=immigration.yearly)) + 
  geom_line(colour="blue", linewidth =1) + 
  theme_bw() +
  labs(y = "Immigration Yearly (N)", x = "Year") + 
  theme(axis.text.y = element_text(size=20), 
        axis.text.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size = 20),
        legend.position = "none",
        aspect.ratio=4/4)


# for presentation

share.plot.beamer = 
  
  voting.d %>% 
  group_by(Year) %>% 
  ggplot(aes(x = Year, y = PS)) +
  #geom_jitter(width = 0.25, alpha = 1/5) +
  geom_smooth(method = "loess", se = TRUE, fullrange = F, span=1) +
  labs(title = "Overtime Electoral Perfomance\nof the Finns Party") +
  theme_bw() +
  #scale_x_discrete(breaks = seq(1983, 2023, by = 4))  +
  labs(y = "Average number of votes\nat the district level", x = "Year") + 
  theme(axis.text.y = element_text(size=20), 
        axis.text.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size = 20),
        legend.position = "none",
        aspect.ratio=4/4)

gini.plot.beamer = dat %>% 
  ggplot(aes(x = Year, y = Gini)) +
  #geom_jitter(width = 0.25, alpha = 1/5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Overtime Evolution of Gini Coefficient in Finland") +
  theme_bw() +
  labs(y = "Gini Coefficient (gross income)", x = "Year") + 
  theme(axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=14), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size = 20),
        legend.position = "none",
        aspect.ratio=4/4)

ggsave(
  "gini_plot_beamer.pdf",
  device = "pdf",
  plot = gini.plot.beamer,
  scale = 1,
  #width = 10, 
  #height = 5, 
  #units = "in",
  dpi = 1200,
  limitsize = TRUE)

ggsave(
  "finns_historical_presentation.pdf",
  device = "pdf",
  plot = share.plot.beamer,
  scale = 1,
  #width = 10, 
  #height = 5, 
  #units = "in",
  dpi = 1200,
  limitsize = TRUE)

ggsave(
  "cat_econ_development_p_present.pdf",
  device = "pdf",
  plot = cat.econ.development.p.present,
  scale = 1,
  #width = 4, 
  #height = 2, 
  #units = "in",
  dpi = 1200,
  limitsize = F)

ggsave(
  "muslim_yearly_p_present.pdf",
  device = "pdf",
  plot = muslim.yearly.p.present,
  scale = 1,
  #width = 4, 
  #height = 2, 
  #units = "in",
  dpi = 1200,
  limitsize = F)

ggsave(
  "immigration_yearly_p_present.pdf",
  device = "pdf",
  plot = immigration.yearly.p.present,
  scale = 1,
  #width = 4, 
  #height = 2, 
  #units = "in",
  dpi = 1200,
  limitsize = F)
## ----


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


####
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Inequality_Populism_Finland/")

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


# The packages below needed to be installed this way first.
# Once installed, they can be recalled using p_load.
# oo <- options(repos = "https://cran.r-project.org/")
# install.packages("Matrix")
# install.packages("lme4")
# options(oo)
# library("Matrix")
# library("lme4")



cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Inequality_Populism_Finland/")


## ---- models:d ----
# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

load("/Users/hectorbahamonde/research/Inequality_Populism_Finland/dat.RData")

# lme4
# https://rpubs.com/rslbliss/r_mlm_ws
p_load(lme4,Matrix, ggeffects, tidyverse,report)

#  To reverse your transformation y=log(x+0.001) you need x=exp(y)-0.001
# https://stats.stackexchange.com/questions/282188/lmer-predict-with-random-effects-log-transformation
# https://stackoverflow.com/questions/50740727/plot-predicted-values-from-lmer-longitudinal-analysis

options(scipen=999)

# testing working hyp
# p_load(ggeffects, tidyverse,report)
# m.1 <- lmer(PS ~ Gini +  HUM.imm + (1 | City)+ (1 | Year), data = dat);summary(m.1);ggpredict(m.1) %>% plot();report(m.1) # working hyp
# m.2 <- lmer(PS ~ Gini +  muslim.imm.yearly + (1 | City)+ (1 | Year), data = dat);summary(m.2);ggpredict(m.2) %>% plot();report(m.2) # working hyp
# m.3 <- lmer(PS ~ Gini +  muslim.imm.yearly + immigration.yearly + (1 | City)+ (1 | Year), data = dat);summary(m.3);ggpredict(m.3) %>% plot();report(m.3) # working hyp
# m.4 <- lmer(PS ~ Gini +  immigration.yearly.lag.1 + HUM.imm + (1 | City)+ (1 | Year), data = dat);summary(m.4);ggpredict(m.4) %>% plot();report(m.4) # working hyp
# m.5 <- lmer(PS ~ Gini.lag.1 +  immigration.yearly.lag.1 + HUM.imm + (1 | City)+ (1 | Year), data = dat);summary(m.5);ggpredict(m.5) %>% plot();report(m.5) # working hyp
# m.6 <- lmer(PS ~ Gini.lag.1 +  immigration.yearly.lag.1 + HUM.imm.lag.1 + (1 | City)+ (1 | Year), data = dat);summary(m.6);ggpredict(m.6) %>% plot();report(m.6) # working hyp


# In this framework, "fixed effects" refer to coefficients that "DO NOT vary by
# group" (city) or "for group-level coefficients." In this case, immigration 
# (measured at the country level) does not vary by city. In turn, Gini 
# coefficients is at the 

# Notes (1): I don't think I should include "Year" random effects because the 
# immigration variables are also measured in a yearly basis. In any case,
# including year random effects does not alter the results. We should include 
# with/without in the reg table.

# Notes (2): grouping in the model does "not" really matter. 
# I mean, it does, but the point is that in the same formula
# one can declare variables with different levels of aggregation. See post below. 
# https://stats.stackexchange.com/questions/450225/linear-mixed-model-in-r-modelling-fixed-effects-with-multiple-levels-and-intera


# 
m.1 <- lmer(PS ~ Gini +  HUM.imm + (1 | City), data = dat);# summary(m.1);ggpredict(m.1) %>% plot();report(m.1) # working hyp
m.2 <- lmer(PS ~ Gini +  muslim.imm.yearly + (1 | City), data = dat);# summary(m.2);ggpredict(m.2) %>% plot();report(m.2) # working hyp
m.3 <- lmer(PS ~ Gini +  immigration.yearly + (1 | City), data = dat);# summary(m.3);ggpredict(m.3) %>% plot();report(m.3) # working hyp
m.4 <- lmer(PS ~ Gini +  muslim.imm.yearly + immigration.yearly + (1 | City), data = dat);# summary(m.4);ggpredict(m.4) %>% plot();report(m.4) # working hyp
m.5 <- lmer(PS ~ Gini.lag.1 +  HUM.imm.lag.1 + (1 | City), data = dat);# summary(m.5);ggpredict(m.5) %>% plot();report(m.5) # working hyp
m.6 <- lmer(PS ~ Gini.lag.1 +  muslim.imm.yearly.lag.1 + (1 | City), data = dat);# summary(m.6);ggpredict(m.6) %>% plot();report(m.6) # working hyp
m.7 <- lmer(PS ~ Gini.lag.1 +  immigration.yearly.lag.1 + (1 | City), data = dat);# summary(m.7);ggpredict(m.7) %>% plot();report(m.7) # working hyp
m.8 <- lmer(PS ~ Gini.lag.1 +  muslim.imm.yearly.lag.1 + immigration.yearly.lag.1 + (1 | City), data = dat);# summary(m.8);ggpredict(m.8) %>% plot();report(m.8) # working hyp
m.9 <- lmer(PS ~ Gini *  HUM.imm + muslim.imm.yearly + (1 | City), data = dat);# summary(m.9);ggpredict(m.9) %>% plot();report(m.9) # working hyp
m.10 <- lmer(PS ~ Gini *  HUM.imm + immigration.yearly + (1 | City), data = dat);# summary(m.10);ggpredict(m.10) %>% plot();report(m.10) # working hyp

p_load(texreg)
reg.table = texreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
  list(m.1, m.2, m.3, m.4, m.5, m.6, m.7,m.8,m.9,m.10), # list all the saved models here
  #omit.coef = "id"
  custom.coef.names = c("Intercept",
                        "Gini",
                        "High and Upper-medium Country Immigration",
                        "Muslim Immigration",
                        "Immigration Total",
                        "Gini (1 lag)",
                        "High and Upper-medium Country Immigration (1 lag)",
                        "Muslim Immigration (1 lag)",
                        "Immigration Total (1 lag)",
                        "Gini x High and Upper-medium Country Immigration"),
  label = "reg:t",
  caption = "Linear Panel Models: Inequality and the Finns Party",
  caption.above = T,
  center = T,
  float.pos="H",
  use.packages = FALSE,
  threeparttable = TRUE,
  scalebox = 0.4
)
## ----

# table
p_load(texreg)
screenreg( # use "screenreg" or "texreg" // BUT DO KEEP IT IN texreg FOR THE PAPER
  list(m.1, m.2, m.3, m.4, m.5, m.6, m.7,m.8,m.9,m.10)#, # list all the saved models here
  #omit.coef = "id"
)


## ---- interactions:d ----
p_load(sjPlot,sjmisc,ggplot2)
m.9.p = plot_model(m.9, type = "int") + 
  theme_bw() +
  labs(title = "Marginal Effects of Interaction Term (model 9)", y = "Finns Party votes", x = "Gini") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10),
        legend.position = "none",
        #legend.position = c(0.3, 0.1),
        aspect.ratio=1) +
  #guides(colour=guide_legend(title="", nrow = 1)) 
  #scale_fill_manual(labels = c("T999", "T888"), values = c("blue", "red"))
  annotate("text", x = 35, y = 10000, label = "Developed (high)") +
  annotate("text", x = 40, y = -10000, label = "Developed (low)")
  

p_load(sjPlot,sjmisc,ggplot2)
m.5.a.p = plot_model(m.5, terms = "Gini.lag.1", type = "pred") + 
  theme_bw() +
  labs(title = "Predicted values of Finns Party votes (model 5)") +
  labs(y = "Finns Party votes", x = "Gini (lag 1)") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10),
        legend.position = "none",
        aspect.ratio=1)

m.5.b.p = plot_model(m.5, terms = "HUM.imm.lag.1", type = "pred") + 
  theme_bw() +
  labs(title = "Predicted values of Finns Party votes (model 5)") +
  labs(y = "Finns Party votes", x = "High and Upper-middle Development\nCountry Immigration") + 
  theme(axis.text.y = element_text(size=7), 
        axis.text.x = element_text(size=7), 
        axis.title.y = element_text(size=10), 
        axis.title.x = element_text(size=10), 
        legend.text=element_text(size=10), 
        legend.title=element_text(size=10),
        plot.title = element_text(size=10),
        strip.text.x = element_text(size = 10),
        legend.position = "none",
        aspect.ratio=1)


# Combine both plots
p_load(ggpubr)
theme_set(theme_pubr())

models.plot = ggarrange(m.9.p, m.5.a.p, m.5.b.p, 
                        labels = c("A", "B", "C"),
                        align = "v",
                        ncol = 3, nrow = 1)

ggsave(
  "models_plot.pdf",
  device = "pdf",
  plot = models.plot,
  scale = 1.5,
  #width = 10, 
  #height = 5, 
  #units = "in",
  dpi = 1300,
  limitsize = TRUE)

# for presentation
m.9.p.present = plot_model(m.9, type = "int") + 
  theme_bw() +
  labs(title = "Marginal Effects of Interaction Term", y = "Finns Party votes", x = "Gini") + 
  theme(axis.text.y = element_text(size=20), 
        axis.text.x = element_text(size=20), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size=20),
        legend.position = "none",
        #legend.position = c(0.3, 0.1),
        aspect.ratio=1) +
  #guides(colour=guide_legend(title="", nrow = 1)) 
  #scale_fill_manual(labels = c("T999", "T888"), values = c("blue", "red"))
  annotate("text", x = 35, y = 10000, label = "Developed (high)", size = unit(8, "pt")) +
  annotate("text", x = 40, y = -10000, label = "Developed (low)", size = unit(8, "pt"))


ggsave(
  "interact_plot.pdf",
  device = "pdf",
  plot = m.9.p.present,
  scale = 1,
  #width = 10, 
  #height = 5, 
  #units = "in",
  dpi = 1300,
  limitsize = TRUE)
## ----

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
p_load(ggplot2)
# Share
share.plot = voting.d %>% 
  group_by(Year) %>% 
  ggplot(aes(x = Year, y = PS)) +
  #geom_jitter(width = 0.25, alpha = 1/5) +
  geom_smooth(method = "loess", se = TRUE, fullrange = F, span=1) +
  labs(title = "Overtime Electoral Perfomance of the Finns Party") +
  theme_bw() +
  #scale_x_discrete(breaks = seq(1983, 2023, by = 4))  +
  labs(y = "Average number of votes at the district level", x = "Year") + 
  theme(axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=14), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size = 20),
        legend.position = "none",
        aspect.ratio=4/4)

# Gini
gini.plot = dat %>% 
  ggplot(aes(x = Year, y = Gini)) +
  #geom_jitter(width = 0.25, alpha = 1/5) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Overtime Evolution of Gini Coefficient in Finland") +
  theme_bw() +
  labs(y = "Gini Coefficient (gross income)", x = "Year") + 
  theme(axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=14), 
        axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size = 20),
        legend.position = "none",
        aspect.ratio=4/4)

# Combine both plots
p_load(ggpubr)
theme_set(theme_pubr())

dependent.var.plot = ggarrange(gini.plot, share.plot, 
                               labels = c("A", "B"),
                               ncol = 2, nrow = 1)

ggsave(
  "gini_finns_historical.jpeg",
  device = "jpeg",
  plot = dependent.var.plot,
  scale = 1,
  #width = 10, 
  #height = 5, 
  #units = "in",
  dpi = 1200,
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
municipalities = merge(x = municipalities, y = dat[ , c("City", "Gini", "Year", "PS", "share.ps")], by = "City", all.x=TRUE)


# Keep years
p_load("dplyr")
# municipalities = municipalities %>% filter(Year == 1995 | Year == 2022 | Year == 2023)

# Adds 2023 elections
#municipalities.2023 = get_municipalities(year = 2020, scale = 4500)
#municipalities.2023 <- municipalities.2023 %>% rename("City" = "name")
#municipalities.2023 = merge(x = municipalities.2023, y = voting.d[ , c("City", "PS", "Year")], by = "City", all.x=TRUE)
#p_load("dplyr")
#municipalities.2023 = municipalities.2023 %>% filter(Year == 1995 | Year == 2019 | Year == 2021)

# Gini Plot
p_load("ggplot2")

gini.map.plot = municipalities  %>% filter(Year == 1995 | Year == 2007 | Year == 2019) %>% 
  ggplot(.) + 
  geom_sf(aes(fill = Gini)) +
  scale_fill_gradient(low = "blue", high = "red") + # scale_fill_gradient2(midpoint = mean(inequality.d$Gini), low = "blue", high = "red")
  labs(title = "Overtime Evolution of the Gini Coefficient in Finland") +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.y = element_text(size=18), 
        axis.text.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        axis.title.x = element_text(size=18), 
        legend.text=element_text(size=18), 
        legend.title=element_text(size=18),
        plot.title = element_text(size=18),
        strip.text.x = element_text(size=18),
        legend.position="bottom")

annotate_figure(gini.map.plot,
                bottom = text_grob("Note: Figure shows average over all cities", color = "black", size = 10)
)


# Populist Plot
p_load("ggplot2")
populist.map.plot = 
  municipalities %>% filter(Year == 1995 | Year == 2019 | Year == 2023 ) %>% 
  ggplot(.) + 
  geom_sf(aes(fill = share.ps)) +
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Overtime Electoral Share of the Finns Party") +
  guides(fill=guide_legend(title="Electoral Share of the Finns Party", nrow = 1)) +
  facet_wrap(~Year) +
  theme_bw() +
  theme(axis.text.y = element_text(size=18), 
        axis.text.x = element_text(size=18), 
        axis.title.y = element_text(size=18), 
        axis.title.x = element_text(size=18), 
        legend.text=element_text(size=18), 
        legend.title=element_text(size=18),
        plot.title = element_text(size=18),
        strip.text.x = element_text(size=18),
        legend.position="bottom")

# Combine both plots
p_load(ggpubr)
theme_set(theme_pubr())

maps.plot = ggarrange(gini.map.plot, populist.map.plot,
                      align = "h",
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1)


ggsave(
  "maps_plot.jpeg",
  device = "jpeg",
  plot = maps.plot,
  scale = 1,
  #width = 10.4, 
  #height = 4, 
  #units = "in",
  dpi = 1200,
  limitsize = TRUE)



# Plotting DV and IV
p_load("ggplot2")

# drop if Gini, Year, or share.ps are missing
dat.plot = dat %>% drop_na(c("Gini", "Year"))

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

share.dep.var.plot.histogram = ggplot(dat.plot, aes(x=PS)) + geom_histogram() + facet_wrap(~Year, ncol = length(unique(dat$Year))) +
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
# library(gmm)

# Gmm in STATA
# https://www.stata.com/manuals13/rgmm.pdf
# https://www.eco.uc3m.es/~ricmora/mei/materials/Session_14_GMM_estimation.pdf (see "Linear Regression").
# read Wawro again.
# https://blog.stata.com/2016/10/04/estimating-covariate-effects-after-gmm/


## ---- table ----
# Table
p_load(texreg)

# c(noquote(paste('m', 1:9, collapse = ", ", sep = "")))

screenreg( # screenreg texreg
  list(m.1, m.2, m.3, m.4, m.5, m.6, m.7,m.8,m.9,m.10) ,#, # list all the saved models here
  custom.header = list(
    "1" = 1,
    "2" = 2,
    "3" = 3, 
    "4" = 4,  
    "5" = 5, 
    "6" = 6, 
    "7" = 7,
    "8" = 8,
    "9" = 9,
    "10" = 10),
  custom.model.names = c(rep("Votes", 10)),
  # omit.coef = "(City)",
  custom.coef.names = c("Intercept",
  "Gini",
  "High and Upper-medium Country Immigration",
  "Muslim Immigration",
  "Immigration Total",
  "Gini (1 lag)",
  "High and Upper-medium Country Immigration (1 lag)",
  "Muslim Immigration (1 lag)",
  "Immigration Total (1 lag)",
  "Gini x High and Upper-medium Country Immigration"),
  #"Femininity"),
  # custom.header = list( "Poisson" = 1),
  stars = c(0.001, 0.01, 0.05),
  include.adjrs = FALSE,
  #symbol = "\\\\cdot",
  label = "reg:t",
  caption = "Dynamic Panel-Data Models: The Effect of Inequality on the Share of The Populist Party.",
  caption.above = T,
  center = T,
  float.pos="H",
  use.packages = FALSE,
  threeparttable = TRUE,
  scalebox = 0.1,
  #file = "models.doc",
  custom.note = "\\item %stars. \\item All models have city fixed-effects. Dependent variable is the total votes of the Finns Party.")
## ----

################
#### NOTES
################

# 1. Map immigration per year.
# 2. Inglehart and Pippa Norris: it's cultural.
# 3. Party Politics, Political Behavior, West European Politics, European Journal of Political Research.

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
