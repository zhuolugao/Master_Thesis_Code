

# Link to WRDS -------------------------------------------------------------

# rm(list = ls())
# library(DBI)
# library(RPostgres)
# 
# wrds <- dbConnect(Postgres(),
#                   host='wrds-pgdata.wharton.upenn.edu',
#                   port=9737,
#                   user = '$USER$', # please enter the user name and password
#                   password = '$PASSWORD$',
#                   dbname='wrds',
#                   sslmode='require')
# 
# res1 <- dbSendQuery(wrds, "select datadate, fyearq, conm, cusip, sic,xintq, ceqq,
#                            ltq, ibq, itacoeq, dvcq, dptcq, dlttq, atq, capr1q, capr2q
#                    from bank_fundq
#                    where ceqq IS NOT NULL
#                    and atq IS NOT NULL
#                    and (sic = '6000'
#                    or sic BETWEEN '6020' AND '6036'
#                    or sic BETWEEN '6040' AND '6062'
#                    or sic BETWEEN '6120' AND '6179'
#                    or sic BETWEEN '6190' AND '6199'
#                    or sic BETWEEN '6710' AND '6719')")
# 
# data <- dbFetch(res1, n=-1)
# dbClearResult(res1)
# 
# dbDisconnect(wrds)
# 
# saveRDS(data, "compustat_monthly.rds")
# # save the dataset, load it next time, no need to use database again.
# -----------------------------------------------------------------------------

################################################################################
# rename the variables --------------------------------------------------------

rm(list = ls()) # remove all, close all and load the "compustat_monthly.rds"

data.comp = readRDS("compustat_monthly.rds")
head(data.comp)
# rename all these data to make it more clear
colnames(data.comp) = c("date","year","name","cusip","sic","interest",
                        "common.equity","liability","income","tax","dividend",
                        "deposit","long.debt","asset","tier1.ratio","tier2.ratio")
head(data.comp)
data.comp = subset(data.comp, select = -year)
# now, the data is quarterly data
#-------------------------------------------------------------------------------

# look at data
summary(data.comp) # from 1961.12-31 to 2020-01-31

################################################################################
# calculate the cost of debt, Jens's method -------------------------------------

data.comp$rd = NA # initiate a column to save the cost of debt

# sort data.comp by 1) cusip
#                by 2) date 
data.comp = data.comp[order(data.comp$cusip, data.comp$date),]

# this is to obtain how many quarters in each bank
unique.bank = rle(data.comp$cusip)$lengths # same with the funciton count()

# this is to obtain the group start location in the whole table
unique.loc = rep(1, length(unique.bank)+1) #firstly initiate it 

# find the location of each group start
for (i in 1:length(unique.bank)){
  unique.loc[i+1] = sum(unique.bank[1:i])+1
}
tail(unique.loc) # the last number is row number + 1

# calculate the cost of debt
table.loc = 0
for (i in 1:length(unique.bank)){#i is group number
  for (k in 1:unique.bank[i]){#k is location within group
    table.loc = table.loc + 1 # # every time enter the sub loop, count +1
    if (k > 4){ # one r_d at time t needs to use data of time t, t-1, t-3, t-4 
      data.comp$rd[table.loc] = sum(data.comp$interest[(table.loc-3):table.loc]) / (0.25 * sum(data.comp$liability[(table.loc-4):(table.loc-1)]))
    }
  }
  print(i)
}

# problem: the number of observations of the total liability is too small
# we needs liability to calculate the cost of debt
psych::describe(data.comp$deposit)
psych::describe(data.comp$liability)
psych::describe(data.comp$rd *100)

head(data.comp)
# saveRDS(data.comp, "cost_debt.rds")
## span data of cost of debt capital
# library(lubridate)
# cost_debt$year = year(cost_debt$date)
# cost_debt$month = month(cost_debt$date)
# 
# 
# # omit those strange data, if report date is not 3,6,9,12
# cost_debt = cost_debt[cost_debt$month == 3 |cost_debt$month == 6|cost_debt$month == 9|cost_debt$month == 12,]
# 
# # check
# head(cost_debt)
# # copy the dataset twice, thus we obtain a spanned dataset
# data.cost_debt = cost_debt
# data.cost_debt = rbind(data.cost_debt, cost_debt)
# data.cost_debt = rbind(data.cost_debt, cost_debt)
# # the hardest thing is modify the year and month, especially for month = 12
# 
# # firstly, sort the data to make it easier to modify
# data.cost_debt = data.cost_debt[order(data.cost_debt$cusip, data.cost_debt$date),]
# head(data.cost_debt)
# 
# # create a column to save spanned year and spanned month
# 
# data.cost_debt$s.month = rep(NA, nrow(data.cost_debt))
# 
# 
# 
# # create a matrix to store 1:12
# span = matrix(1:12, nrow = 3)
# 
# print(span)
# # 
# # [,1] [,2] [,3] [,4]
# # [1,]    1    4    7   10
# # [2,]    2    5    8   11
# # [3,]    3    6    9   12
# 
# # write a loop to span quarterly data into monthly
# for (i in 0:(nrow(data.cost_debt)/3-1)){
#   if (data.cost_debt$month[3*i+1] == 3){
#     flag = 1 #if the initial month =12, span it into 12,1,2, first row
#   } else if (data.cost_debt$month[3*i+1] == 6) {
#     flag = 2 #if the initial month =3, span it into 3,4,5, second row
#   } else if (data.cost_debt$month[3*i+1] == 9) {
#     flag = 3 #if the initial month =6, span it into 6,7,8, third row
#   } else if (data.cost_debt$month[3*i+1] == 12) {
#     flag = 4 #if the initial month =9, span it into 9,10,11,fourth row
#   }
#   data.cost_debt$s.month[(3*i+1):(3*i+3)] = span[,flag]
#   print(i)
# }
# 
# 
# head(data.cost_debt)
# data.cost_debt$month = data.cost_debt$year * 100 + data.cost_debt$s.month

###########################################################################
# cusip = unique(data.comp$cusip)
# write.table(cusip, "cusip.txt", append = FALSE, sep = " ", dec = ".",
#             row.names = F, col.names = F)
# unique(nchar(cusip))
###########################################################################


############################################################################
# quarterly ==> monthly---------------------------------------------------


# span the quarterly data into monthly data, forward span
# i.e., if data date is 1993-12-31, span it into 1994-01,02,03
data.comp$year = year(data.comp$date)
data.comp$month = month(data.comp$date)

# omit those strange data, if report date is not 3,6,9,12
data.comp = data.comp[data.comp$month == 3 |data.comp$month == 6|data.comp$month == 9|data.comp$month == 12,]

# check
head(data.comp)
# copy the dataset twice, thus we obtain a spanned dataset
data = data.comp
data = rbind(data, data.comp)
data = rbind(data, data.comp)
# the hardest thing is modify the year and month, especially for month = 12

# firstly, sort the data to make it easier to modify
data = data[order(data$cusip, data$date),]
head(data)

# create a column to save spanned year and spanned month

data$s.year = rep(NA, nrow(data))
data$s.month = rep(NA, nrow(data))

# for those records that month are not 12, span.year is just year
data$s.year[data$month!=12] = data$year[data$month!=12]
# for those records that month are 12, span.year = year + 1
# then there is one wrong year
data$s.year[data$month==12] = data$year[data$month==12]+1
head(data)

# create a matrix to store 1:12
span = matrix(0:11, nrow = 3)
span[1,1] = 12
print(span)
# 
#      [,1]  [,2] [,3] [,4]
# [1,]   12    3    6    9
# [2,]    1    4    7   10
# [3,]    2    5    8   11

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
  print(i)
}

# now, correct the wrong year, when the month = 12, the span.year should be year, instead of year + 1
data$s.year[data$s.month==12] = data$year[data$s.month==12]
# create the final month column
data$month = data$s.year*100 + data$s.month
# clear the useless columns
library(zoo)
data$date = as.yearmon(paste(data$s.year,data$s.month), "%Y %m")
data = subset(data, select = -c(s.year, s.month))
data = subset(data, select = -year)
head(data)
colnames(data)[colnames(data) == 'date'] <- 'yearmonth'
# data = merge(data, data.cost_debt, by = c("cusip", "month"))

#saveRDS(data, "span_compustat.rds")



#########################################################################
#Visulization of cost of debt----------------------------------------
rm(list = ls())
library(zoo)
data = readRDS("span_compustat.rds")

# pick out the useful columns, the month, id, cost of debt, timeseries
data.rd = subset(data, select = c(month, cusip, rd, yearmonth))
data.rd = na.omit(data.rd)
# rank the data by the month
data.rd = data.rd[order(data.rd$yearmonth),]

data.rd = data.rd[data.rd$rd<0.1,]

# use the aggregate function to directly solve the average value within each month
mean.table = aggregate(data.rd[,"rd"], list(data.rd$yearmonth), mean)
colnames(mean.table) = c("month", "mean")
head(mean.table)
mean(na.omit(data$rd))
median(na.omit(data$rd))
# define two functions to solve the 10%, 90% of the data
per_90 = function(x){
  res = quantile(x, 0.9)
  return(res)
}

per_10 = function(x){
  res = quantile(x, 0.1)
  return(res)
}

# use the two functions to find the 10%, 90% within each group
percent90.table = aggregate(data.rd[,"rd"], list(data.rd$yearmonth), per_90)
percent10.table = aggregate(data.rd[,"rd"], list(data.rd$yearmonth), per_10)

# put the 10%, 90% value into the final table
mean.table$percent90 = percent90.table$x

mean.table$percent10 = percent10.table$x

# figure
library(ggplot2)
head(mean.table)
class(mean.table$month)
fig1 = ggplot(data = mean.table[mean.table$month >= "1993-01-01",], aes(x = month))+
  geom_line(aes(y = mean *100, linetype = "Mean", col = "Mean"), size = 1.1)+
  geom_line(aes(y = percent10*100, linetype = "10th percentile",col = "10th percentile"), size = 1.1)+
  geom_line(aes(y = percent90*100, linetype = "90th percentile",col = "90th percentile"), size = 1.1)+
  # geom_point(aes(y = mean *100, shape = "mean"))+
  # geom_point(aes(y = percent90*100, shape = "percent90"))+
  # geom_point(aes(y = percent10*100, shape = "percent10"))+
  ylim(0,7)+
  theme_bw()+
  theme(legend.position = c(0.84, 0.85),
        legend.key = element_blank(),
        legend.key.width = unit(1.5, "cm"),legend.title =  element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())+
  scale_linetype_manual(name = c("Mean","10th percentile","90th percentile"),
                        values = c("Mean"="solid", "10th percentile" = "dotted", "90th percentile"="dashed"))+
  scale_color_manual(name = c("Mean","10th percentile","90th percentile"),
                     values = c("Mean"="black", "10th percentile" = "grey20", "90th percentile"="grey50"))+
  scale_x_yearmon(format = "%Y")+
  labs(y = "Cost of debt capital (percentage point)", x = element_blank())
 
print(fig1)

##################################################################
# REQUIRE DATA FROM WRDS, the interest rate data
# 
# wrds <- dbConnect(Postgres(),
#                   host='wrds-pgdata.wharton.upenn.edu',
#                   port=9737,
#                   user = '$USER$', # please enter the user name and password
#                   password = 'xx',
#                   dbname='wrds',
#                   sslmode='require')
# 
# res1 <- dbSendQuery(wrds, "select caldt, b1ret, t30ret
#                    from crsp.mcti
#                    ")
# 
# interest <- dbFetch(res1, n=-1)
# dbClearResult(res1)
# 
# dbDisconnect(wrds)
# 
# saveRDS(interest, "treasury.rds")

interest = readRDS("treasury.rds")

interest = interest[interest$caldt > "1969-12-31",]
interest = interest[interest$caldt < "2020-01-01",]

colnames(interest) = c("date", "yearly_treasury", "monthly_tresuary")

mean(interest$yearly_treasury)

# library(tidyquant)
# library(dplyr)
# fig2 = ggplot(data = interest, aes(x = date, y = yearly_treasury *100)) +
#   geom_point() +
#   coord_x_date(xlim = c("1993-01-01", "2019-12-31"), ylim= c(-1,3))
# print(fig2)

# delete the column of monthly rate
interest = subset(interest, select = -monthly_tresuary)

# the column of month and year
interest$month_num = month(interest$date)
interest$year = year(interest$date)
# paste them together
interest$month = interest$month_num + interest$year *100

interest = subset(interest, select = c(month, yearly_treasury))

data = merge(data, interest, by = "month")

# saveRDS(data, "span_compustat.rds")



