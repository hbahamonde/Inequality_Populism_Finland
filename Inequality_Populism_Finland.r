cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Inequality_Populism_Finland/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import inequality data
p_load("readxl")
inequality.d <- read_excel("/Users/hectorbahamonde/research/Inequality_Populism_Finland/data/inequality_data/Inequality_Data.xlsx")

# Repeat year
