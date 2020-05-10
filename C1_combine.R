rm(list = ls())

library(AER)
library(tidyverse)
library(caret)
library(ggplot2)
# load data
data.comp = readRDS("span_compustat.rds") # monthly data from the Compustat database
# contains the capital structure information
data.crsp = readRDS("forward_beta.rds") # monthly data from the crsp database
# contains the return, price and market cap. information
link = readRDS("linktable.rds")
#####################################################################################
#combine the two datasets---------------------------------------------------

head(data.comp)
head(data.crsp)

#remove the cusip column in crsp dataset since it is 8 digit
data.crsp = data.crsp[!is.na(data.crsp$b.beta),]
data.crsp = subset(data.crsp, select = c(permno, month, date, ret, shrout, price, b.beta, f.beta, exret))
head(link)
colnames(link) = c("permno","cusip")

# link the two database together
combine.data = merge(data.comp, link, by = "cusip")
head(combine.data)

combine.data = merge(combine.data, data.crsp, by =c("permno","month"))

head(combine.data)
# pick out the useful variables

data = subset(combine.data, select = -c(sic, interest, liability, income, tax, dividend, deposit, long.debt, yearmonth))
head(data)

# define the important variables
data$equity.ratio = data$common.equity / data$asset
data = data[!is.na(data$equity.ratio),]
# remove extreme values
data = data[data$equity.ratio>0,]
head(data)
# define capitalization
data$cap = data$shrout*data$price

data$inverse.tier1 = 1/(data$tier1.ratio/100)
data$inverse.equity.ratio = 1/data$equity.ratio

head(data)
saveRDS(data, "C1_combine_crsp_comp.rds")

#####################################################################
