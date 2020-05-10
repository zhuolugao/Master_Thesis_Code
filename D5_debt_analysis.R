rm(list = ls())
library(AER)

data = readRDS("span_compustat.rds")
data = data[!is.na(data$tier1.ratio),]
data = data[data$month >= 199303,]

data$tier1_100 = data$tier1.ratio / 100
data$tier2_100 = data$tier2.ratio / 100
data$dep.ratio = data$deposit / data$asset

data = data[data$month >= 201101,]
data = data[!is.na(data$rd),]


model.d1 = lm(data = data, rd ~ tier1_100 + factor(month) + factor(cusip) - 1)
model.d2 = lm(data = data, rd ~ tier1_100 + tier2_100 + factor(month) + factor(cusip) - 1)
model.d3 = lm(data = data, rd ~ tier1_100 + tier2_100 + dep.ratio  + factor(month) + factor(cusip) - 1)

summary(model.d1)
summary(model.d2)
summary(model.d3)

# -4.789e-03
# -4.899e-03
# -8.841e-03
# 
# model.dd1 = lm(data = data, rd ~ tier1_100 + factor(month) - 1)
# model.dd2 = lm(data = data, rd~ tier1_100 + tier2_100 + factor(month) - 1)
# model.dd3 = lm(data = data, rd~ tier1_100 + tier2_100 + dep.ratio + factor(month) - 1)
# summary(model.dd1)
# summary(model.dd2)
# summary(model.dd3)
# read data of ICC
head(data)
data.icc = readRDS("icc_final.rds")
head(data.icc)
data.icc = subset(data.icc, select = c(cusip, report.date, s.year, s.month, icc))
data.icc$month = data.icc$s.year * 100 + data.icc$s.month

data.merge = merge(data.icc, data, by = c("month", "cusip"))
head(data.merge)

data.merge$equity.ratio = data.merge$common.equity / data.merge$asset


data.merge$ra = data.merge$equity.ratio * data.merge$icc + (1-data.merge$equity.ratio) * data.merge$rd

summary(data.merge$ra)
model.a1 = lm(data = data.merge, ra ~ tier1_100 + factor(month) + factor(cusip) - 1)
model.a2 = lm(data = data.merge, ra ~ tier1_100 + tier2_100 + factor(month) + factor(cusip) - 1)
model.a3 = lm(data = data.merge, ra ~ tier1_100 + tier2_100 + dep.ratio + factor(month) + factor(cusip) - 1)

summary(model.a1)
summary(model.a2)
summary(model.a3)

library(stargazer)
stargazer(model.d1, model.d2, model.d3,model.a1, model.a2, model.a3,
          omit = c("month","cusip"),
          omit.labels = c("Time effect","Firm effect"))





library(plyr)



hist(data.merge$year)
library(ggplot2)
summary(data.merge$year)
p_hist = ggplot(data= data.merge, aes(x = year)) +
  geom_histogram(breaks=seq(1993, 2019, by=1)) +
  theme_classic()+
  scale_x_continuous(breaks = seq(1993, 2019, by=2))+
  labs(y = "Count of cost of debt", x = element_blank())
print(p_hist)

# 
# data = readRDS("span_compustat.rds")
# data = data[data$date >= "1993-01-01",]
# 
# 
# data = data[order(data$month, data$cusip),]
# 
# data = data[!is.na(data$rd),]
# mean.table = aggregate(data[,"rd"], list(data$month), mean)
# colnames(mean.table) = c("month", "mean")
# 
# per_90 = function(x){
#   res = quantile(x, 0.9)
#   return(res)
# }
# 
# per_10 = function(x){
#   res = quantile(x, 0.1)
#   return(res)
# }
# 
# 
# 
# percent90.table = aggregate(data[,"rd"], list(data$date), per_90)
# percent10.table = aggregate(data[,"rd"], list(data$date), per_10)
# 
# mean.table$percent90 = percent90.table$x
# 
# mean.table$percent10 = percent10.table$x
# 
# library(ggplot2)
# 
# fig1 = ggplot(data = mean.table, aes(x = month))+
#   geom_point(aes(y = mean *100, shape = "mean"))+
#   geom_point(aes(y = percent90*100, shape = "percent90"))+
#   geom_point(aes(y = percent10*100, shape = "percent10"))+
#   theme_bw()+
#   ylim(0,7)
# 
# print(fig1)
# 
# 
# 
# #------------------------------------
# data = data[!is.na(data$tier1.ratio),]
# data$tier1_100 = data$tier1.ratio / 100
# data$tier2_100 = data$tier2.ratio / 100
# data$dep.ratio = data$deposit / data$asset
# 
# model.debt.1 = lm(data = data, rd*100 ~ tier1_100 + factor (cusip) + factor(month) -1)
# model.debt.2 = lm(data = data, rd*100 ~ tier1_100 + tier2_100 + factor (cusip) + factor(month) -1)
# model.debt.3 = lm(data = data, rd*100 ~ tier1_100 + tier2_100 + dep.ratio + factor (cusip) + factor(month) -1)
# 
# 
# summary(model.debt.1)
# summary(model.debt.2)
# summary(model.debt.3)
# 
# 
# ###########data of icc
# data.icc = readRDS("icc_final.rds")
# 
# summary(data.icc)
# data.icc = data.icc[order(data.icc$cusip, data.icc$date),]
# data.icc = subset(data.icc, select = c(cusip, s.year, s.month, icc))
# 
# 
# # summary
# summary(data.icc)
# data.icc$month = data.icc$s.year * 100 + data.icc$s.month
# 
# data = merge(data.icc, data, by = c("month", "cusip"))
# 
# summary(data$rd)
# 
# 
# head(data)
# data$equity.ratio = data$common.equity / data$asset
# data$deb.ratio = data$liability / data$asset
# data$ra = data$equity.ratio * data$icc  + data$deb.ratio * data$rd
# 
# model.ra.1 = lm(data = data, ra*100 ~ tier1_100 + factor (cusip) + factor(month) -1)
# model.ra.2 = lm(data = data, ra*100 ~ tier1_100 + tier2_100 + factor (cusip) + factor(month) -1)
# model.ra.3 = lm(data = data, ra*100 ~ tier1_100 + tier2_100 + dep.ratio + factor (cusip) + factor(month) -1)
# 
# summary(model.ra.1)
# 
# summary(model.ra.2)
# summary(model.ra.3)


