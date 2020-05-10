# import compustat data
rm(list = ls())
library(lubridate)
data.comp = readRDS("compustat_monthly.rds")
head(data.comp)
# rename all these data to make it more clear
colnames(data.comp) = c("report.date","year","name","cusip","sic","interest",
                        "common.equity","liability","income","tax","dividend",
                        "deposit","long.debt","asset","tier1.ratio","tier2.ratio")
head(data.comp)
data.comp = subset(data.comp, select = -c(year,sic))

############################################################################
############################### processing gamma ############################

library(readxl)
library(lubridate)
library(numbers)
library(readr)
treasury <- read_csv("treasury_10.csv", 
                        col_types = cols(caldt = col_date(format = "%Y%m%d")))

head(treasury)
colnames(treasury) = c("date", "treasury_10")
# form a number to illurstrate the date month
treasury$year = year(treasury$date)
treasury$month = month(treasury$date)
# create a new column to store the month start numbers
head(treasury)
# those with month 12, year should +1
treasury$year[treasury$month==12] = treasury$year[treasury$month==12] +1
treasury$new.month = treasury$month +1
treasury$new.month[treasury$new.month ==13] = 1

treasury$new.date = ymd( 100* (treasury$year*100 + treasury$new.month) +1)
treasury$gamma_1 = treasury$treasury_10 - 0.03

data.gamma = subset(treasury, select = c(new.date, gamma_1))
colnames(data.gamma) = c("date","gamma_1")

#saveRDS(data.gamma, "gamma.rds")
#############################################################################
############################### processing gamma ############################
rm(treasury)
library(ggplot2)

data.comp = data.comp[!is.na(data.comp$income),]
data.comp = data.comp[!is.na(data.comp$dividend),]
# dividend must larger than zero
data.comp = data.comp[data.comp$dividend>=0,]
# dps = dvcq/ibq
data.comp$dps = NA
# if income is smaller or equal to zero, dps = dividend /0.06
data.comp$dps[data.comp$income <=0] = data.comp$dividend[data.comp$income <= 0]/(0.06*data.comp$asset[data.comp$income <= 0])
data.comp$dps[data.comp$income > 0] = data.comp$dividend[data.comp$income > 0] / data.comp$income[data.comp$income > 0]

outlier = data.comp[data.comp$dps >10,]
plot(data.comp$dps)

# span the quarterly data into monthly data, forward span
# i.e., if data date is 1993-12-31, span it into 1994-01,02,03
data.comp$year = year(data.comp$report.date)
data.comp$month = month(data.comp$report.date)


# omit those strange data, if report date is not 3,6,9,12
data.comp = data.comp[data.comp$month == 3 |data.comp$month == 6|data.comp$month == 9|data.comp$month == 12,]

# check
head(data.comp)
# copy the dataset twice, thus we obtain a spanned dataset
data = data.comp
data = rbind(data, data.comp)
data = rbind(data, data.comp)

# firstly, sort the data to make it easier to modify
data = data[order(data$cusip, data$report.date),]
head(data)

# s.year and s.month store the spanned year and month
data$s.year = rep(NA, nrow(data))
data$s.month = rep(NA, nrow(data))

# for those records that month are not 12, span.year is just year
data$s.year[data$month!=12] = data$year[data$month!=12]
data$s.year[data$month==12] = data$year[data$month==12]+1
head(data)

# create a matrix to store 1:12
span = matrix(1:12, nrow = 3)

print(span)



# [,1] [,2] [,3] [,4]
# [1,]    1    4    7   10
# [2,]    2    5    8   11
# [3,]    3    6    9   12


# write a loop to span quarterly data into monthly
for (i in 0:(nrow(data)/3-1)){
  if (data$month[3*i+1] == 12){
    flag = 1 #if the initial month =12, span it into 12,1,2, first row
  } else if (data$month[3*i+1] == 3) {
    flag = 2 #if the initial month =3, span it into 3,4,5, second row
  } else if (data$month[3*i+1] == 6) {
    flag = 3 #if the initial month =6, span it into 6,7,8, third row
  } else if (data$month[3*i+1] == 9) {
    flag = 4 #if the initial month =9, span it into 9,10,11,fourth row
  }
  data$s.month[(3*i+1):(3*i+3)] = span[,flag]
}


data$date = ymd( 100* (data$s.year*100 + data$s.month) +1)
saveRDS(data, "ICC_dps_compustat.rds")





























