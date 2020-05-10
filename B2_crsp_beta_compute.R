##################################################################
# import data-----------------------------------------------------
rm(list = ls())
library(readxl)
library(readr);library(xts);library(stats);
library(lubridate)
library(RPostgres)
library(dplyr);library(nlme)
library(plyr); library(ggplot2); library(knitr);library(writexl)
library(kableExtra)
library(numbers)

data = readRDS("crsp_full_data.rds")
market = readRDS("market.rds")
head(data)
head(market)

##################################################################
# merge the two datasets------------------------------------------

data = merge(data, market, by = "month")
head(data)
# sort data by permno (identifier) and time(month)
data = data[order(data$permno, data$month),]
head(data)



##################################################################
# delete some banks---------------------------------------------
# count how many observations in each group, if less than 24 months, we delete
count.table = count(data, vars = "permno")
# data.remove deletes those firms with less than 24 months
data = merge(data, count.table, by = "permno")
data = data[data$freq>=24,]

length(unique(data$permno))

# left with 2916 banks

###################################################################
# calculate backward betas


# remove useless columns
data$price = abs(data$prc)
head(data)
data = subset(data, select = -c(hsiccd, prc, cusip, hml, smb,umd, freq))

# initiate two blank columns to store alpha and beta
# using NAs because not all months can get valid alphas and betas

data$exret = data$ret - data$rf
data$b.alpha = rep(NA, nrow(data))
data$b.beta = rep(NA, nrow(data))
data$b.idio = rep(NA, nrow(data))

head(data)

##################################################################
# define functions to directly obtain the ols coefficient
my_reg_a = function(x,y){ # define a function to obtain alpha directly
  res_a = coef(lm(y ~ x))[1]
  return(res_a)
}

my_reg_b = function(x,y){ # define a function to obtain beta directly
  res_b = coef(lm(y ~ x))[2]
  return(res_b)
}

my_reg_s = function(x,y){ # define a function to obtain the s.d. of the residuals
  res_s = sigma(lm(y ~ x))
  return(res_s)
}

# # fit a model, find the correct function
# model = lm(data = data, ret ~ price)
# summary(model)
# my_reg_s(data$ret,data$price)


# sort data again to ensure the calculation
data = data[order(data$permno, data$month),]

##################################################################
# this is to obtain how many months in each firm group
unique.bank = rle(data$permno)$lengths # same with the funciton count()
# this is to obtain the group start location in the whole table
unique.loc = rep(1, length(unique.bank)+1) #firstly initiate it 

# find the location of each group start
for (i in 1:length(unique.bank)){
  unique.loc[i+1] = sum(unique.bank[1:i])+1
}
tail(unique.loc) # the last number is row number + 1
head(data)


###################################################################
# calculate backward beta, used for sorting

# takes a while !!!!!!!!!!!!!!!!!!!!!

# table.loc = 0
# for (i in 1:length(unique.bank)){#i is group number
#   for (k in 1:unique.bank[i]){#k is location within group
#     table.loc = table.loc + 1 # # every time enter the sub loop, count +1
#     if (k<60 & k>23){ #if the month is less than 24, no valid beta, if between, use the data from group start to itself
#       data$b.alpha[table.loc] = my_reg_a(data$mktrf[unique.loc[i]:table.loc],data$exret[unique.loc[i]:table.loc])
#       data$b.beta[table.loc] = my_reg_b(data$mktrf[unique.loc[i]:table.loc],data$exret[unique.loc[i]:table.loc])
#       data$b.idio[table.loc] = my_reg_s(data$mktrf[unique.loc[i]:table.loc],data$exret[unique.loc[i]:table.loc])
#     } else if (k >= 60){ # if more than 60 months, only use 60 months to calculate
#       data$b.alpha[table.loc] = my_reg_a(data$mktrf[(table.loc-59):table.loc],data$exret[(table.loc-59):table.loc])
#       data$b.beta[table.loc] = my_reg_b(data$mktrf[(table.loc-59):table.loc],data$exret[(table.loc-59):table.loc])
#       data$b.beta[table.loc] = my_reg_s(data$mktrf[(table.loc-59):table.loc],data$exret[(table.loc-59):table.loc])
#     }
#   }
# }

# better loop
table.loc = 0
for (i in 1:length(unique.bank)){#i is group number
  for (k in 1:unique.bank[i]){#k is location within group
    table.loc = table.loc + 1 # # every time enter the sub loop, count +1
    if (k<60 & k>23){ #if the month is less than 24, no valid beta, if between, use the data from group start to itself
      model = lm(data = data[unique.loc[i]:table.loc,], exret ~ mktrf)
      data$b.alpha[table.loc] = coef(model)[1]
      data$b.beta[table.loc] = coef(model)[2]
      data$b.idio[table.loc] = sigma(model)
    } else if (k >= 60){ # if more than 60 months, only use 60 months to calculate
      model = lm(data = data[(table.loc-59):table.loc,], exret ~ mktrf)
      data$b.alpha[table.loc] = coef(model)[1]
      data$b.beta[table.loc] = coef(model)[2]
      data$b.idio[table.loc] = sigma(model)
    }
  }
  print(i)
}


# saveRDS(data, "backward_betas.rds")

######################################################################################
# use the cov / var calculation as a double check
# this method is much faster than the regression method
# backward.beta = rep(NA, nrow(data))
# 
# loc = 0
# for (i in 1:length(unique.bank)){#i is group number
#   data_rm = data$mktrf[unique.loc[i]:(unique.loc[i+1]-1)]
#   data_ri = data$exret[unique.loc[i]:(unique.loc[i+1]-1)]
#   backward.beta[unique.loc[i]:(unique.loc[i]+22)]
#   for (k in 1:(unique.bank[i])){
#     loc = loc + 1
#     if (k<60 & k>23){
#       backward.beta[loc] = cov(data_rm[1:k], data_ri[1:k]) / var(data_rm[1:k])
#     } else if (k >= 60){
#       backward.beta[loc] = cov(data_rm[(k-59):k], data_ri[(k-59):k]) / var(data_rm[(k-59):k])
#     }
#   }
# }
# 
# backward.beta = as.data.frame(backward.beta)
# 
# 
# test = round(backward.beta$backward.beta - data$b.beta , 2)
# print(test)
# unique(test)

##########################################################################
# calculate the forward beta----------------------------------------------
rm(list = ls())
library(readxl)
library(readr);library(xts);library(stats);
library(lubridate)
library(RPostgres)
library(dplyr);library(nlme)
library(plyr); library(ggplot2); library(knitr);library(writexl)
library(kableExtra)

# import dataset
data = readRDS("backward_betas.rds")

data$f.alpha = rep(NA, nrow(data))
data$f.beta = rep(NA, nrow(data))
data$f.idio = rep(NA, nrow(data))

# sort again to ensure correct
data = data[order(data$permno, data$month),]


# this is to obtain how many months in each firm group
unique.bank = rle(data$permno)$lengths
# this is to obtain the group start location in the whole table
unique.loc = rep(1, length(unique.bank)+1) #firstly initiate it 

# find the location of each group start
for (i in 1:length(unique.bank)){
  unique.loc[i+1] = sum(unique.bank[1:i])+1
}
tail(unique.loc) # the last number is row number + 1

######################################################################
# calculate--------------------------------------------------------


for (i in 1:length(unique.bank)){
  table.loc = unique.loc[i]
  len = unique.bank[i]
  if (len <60){ # if the group length is < 60, use the location to end obs.
    for (j in 1:(len-23)){ # the last 23 rows do not have valid forward beta
      model = lm(data = data[table.loc:(unique.loc[i+1]-1),], exret ~ mktrf)
      # pick out certain data, and model it
      data$f.alpha[table.loc] = coef(model)[1]
      data$f.beta[table.loc] = coef(model)[2]
      data$f.idio[table.loc] = sigma(model)
      table.loc = table.loc +1
    }}
  else {# if the group length is > 60, use 60 obs.
      for(j in 1:(len-23)){# the last 23 rows do not have valid forward beta
        model = lm(data = data[table.loc:(table.loc+59),], exret ~ mktrf)
        data$f.alpha[table.loc] = coef(model)[1]
        data$f.beta[table.loc] = coef(model)[2]
        data$f.idio[table.loc] = sigma(model)
        table.loc = table.loc +1
      }
  }
  print(i)
}
saveRDS(data, "forward_beta.rds")













