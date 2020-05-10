rm(list = ls())
library(lubridate)
library(ggplot2)
#import data
data = readRDS("forward_beta.rds")
head(data)
market = readRDS("market.rds")
head(market)
mean(market$mktrf)


#remove the data without valid backward beta
data = data[!is.na(data$b.beta),]
head(data)
# devide data into ten portfolios based on the backward beta
# firstly, sort the data by b.beta
data = data[order(data$date, data$b.beta),]
# create a vector to store the group number
data$group.no = 0
data$cap = data$price * data$shrout

# since we want to sort the data in each month
unique.month = rle(data$month)$lengths
# find the location of each group start
unique.loc = rep(1, length(unique.month)+1)

for (i in 1:length(unique.month)){
  unique.loc[i+1] = sum(unique.month[1:i])+1
}


# suppose we want k quantiles
three = 3
ten = 10
twenty = 20
# 
# ##################################################################################
# # reproduce bw -----------------------------------------
# data = data[data$month < 201201]
# data = data[order(data$month, data$b.beta),]
# # since we want to sort the data in each month
# unique.month = rle(data$month)$lengths
# # find the location of each group start
# unique.loc = rep(1, length(unique.month)+1)
# 
# for (i in 1:length(unique.month)){
#   unique.loc[i+1] = sum(unique.month[1:i])+1
# }
# length(unique.month)
# for (i in 1:length(unique.month)){
#   month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
#   th30 = round(quantile(1:unique.month[i], 0.3))
#   th70 = round(quantile(1:unique.month[i], 0.7))
#   res = c(1,th30, th70, unique.month[i])
#   for (j in 1:three){#give value to the group.no
#     month_chosen$group.no[res[j]:(res[j+1])] = j
#   }
#   data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
# }
# 
# 
# 
# # create a new data frame to save the portfolio data
# data.portfolio_three = as.data.frame(rep(unique(data$date),three)) # first time, try 10 portfolios
# colnames(data.portfolio_three) = "date"
# 
# 
# data.portfolio_three$month = year(data.portfolio_three$date) * 100 + month(data.portfolio_three$date)
# data.portfolio_three = data.portfolio_three[order(data.portfolio_three$month),]
# # create a column to store the portfolio
# data.portfolio_three$portfolio = rep(1:three, nrow(data.portfolio_three)/three)
# 
# 
# # fill in the market data
# 
# data.portfolio_three = merge(data.portfolio_three, market, by = "month")
# 
# # in order to estimate the portfolio's forward beta, we need the maket return, risk-free return and portfolio excess ret
# 
# data.portfolio_three$exret = 0 # the equal weighted excess return
# 
# data.portfolio_three$vw.exret = 0 # the equal weighted excess return
# 
# data.portfolio_three$cap = 0  # the average market cap.
# 
# head(data.portfolio_three)
# 
# for (i in 1:nrow(data.portfolio_three)){
#   data.chosen = data[data$group.no == data.portfolio_three$portfolio[i]
#                      & data$month == data.portfolio_three$month[i],]# choose all the banks belonging to the portfolio
#   data.portfolio_three$exret[i] = mean(data.chosen$exret)# take the average of the stocks in that month and in that portfolio
#   data.portfolio_three$cap[i] = mean(data.chosen$cap)
#   data.portfolio_three$vw.exret[i] = weighted.mean(data.chosen$exret,data.chosen$cap)
# }
# 
# head(data.portfolio_three)
# 
# 
# # now we get an entire time series of portfolios
# data.portfolio_three = data.portfolio_three[order(data.portfolio_three$portfolio,data.portfolio_three$month ),]
# summary(data.portfolio_three)
# # if now we estimate one beta, one alpha for the 10 portfolios
# 
# three_portfolio = as.data.frame(rep(0, three))
# colnames(three_portfolio) = "alpha"
# three_portfolio$beta = 0
# three_portfolio$exret = 0
# three_portfolio$vw.alpha = 0
# three_portfolio$vw.beta = 0
# three_portfolio$vw.exret = 0
# 
# three_portfolio$mktrf = mean(market$mktrf)
# three_portfolio$rf = mean(market$rf)
# 
# # estimate the alpha and betas for the portfolios, we do not care about the backward or forward now
# # since we already used all available information
# for (i in 1:three){
#   model = lm(data = data.portfolio_three[data.portfolio_three$portfolio==i,], exret ~ mktrf)
#   three_portfolio$beta[i] = coef(model)[2]# this is the equal weighted portfolio
#   three_portfolio$alpha[i] = coef(model)[1]
#   three_portfolio$exret[i] = mean(data.portfolio_three$exret[data.portfolio_three$portfolio==i])
#   model.vw = lm(data = data.portfolio_three[data.portfolio_three$portfolio==i,], vw.exret ~ mktrf)
#   three_portfolio$vw.beta[i] = coef(model.vw)[2]# this is the value weighted portfolio
#   three_portfolio$vw.alpha[i] = coef(model.vw)[1]
#   three_portfolio$vw.exret[i] = mean(data.portfolio_three$vw.exret[data.portfolio_three$portfolio==i])
# }
# 
# 
# 
# head(three_portfolio)
# 
# # look at the relationship between alpha and beta
# figure_three_a_b = ggplot(three_portfolio)+
#   geom_point(aes(x = beta, y = alpha *10000, col = "EW"))+
#   geom_smooth(aes(x = beta, y = alpha *10000, col = "EW"), method = lm)+
#   geom_point(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"))+
#   geom_smooth(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"), method = lm)+
#   theme_bw()+
#   theme(legend.title = element_blank(), legend.position = c(0.9,0.9))+
#   scale_color_manual(name = c("EW", "VW"),values = c("EW" = "blue", "VW" = "black"))+
#   labs(x= "Beta", y = "Alpha (bps)", title = element_blank())+
#   theme(panel.background = element_rect(fill = NA),
#         panel.grid.major.x = element_line(colour = "grey90"),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_line(colour = "grey90"),
#         panel.grid.minor.y = element_blank())
# print(figure_three_a_b)
# 
# summary(lm(data = three_portfolio, alpha ~ beta))
# summary(lm(data = three_portfolio, vw.alpha ~ vw.beta))
# 
# model.three.ew = lm(data = three_portfolio, alpha ~ beta)
# model.three.vw = lm(data = three_portfolio, vw.alpha ~ vw.beta)
# # mix them
# data.mix_three = subset(three_portfolio, select= c(alpha, beta))
# head(three_portfolio)
# data.mix_three[(three + 1):(three + three),] = three_portfolio[,4:5]
# model.mix_three = lm(data = data.mix_three, alpha * 10000 ~ beta)
# # 
# saveRDS(model.mix_three, "BW_low_risk_model.rds")


##################################################################################
##################################################################################
# ten portfolios##########
length(unique.month)
for (i in 1:length(unique.month)){
  month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
  res = round(seq(1, unique.month[i], by = unique.month[i]/ten)) # find out the 1:10th, 11st : 20th ...
  res = c(res, unique.month[i])
  for (j in 1:ten){#give value to the group.no
    month_chosen$group.no[res[j]:(res[j+1])] = j
  }
  data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
}

# create a new data frame to save the portfolio data
data.portfolio_ten = as.data.frame(rep(unique(data$date),ten)) # first time, try 10 portfolios
colnames(data.portfolio_ten) = "date"


data.portfolio_ten$month = year(data.portfolio_ten$date) * 100 + month(data.portfolio_ten$date)
data.portfolio_ten = data.portfolio_ten[order(data.portfolio_ten$month),]
# create a column to store the portfolio
data.portfolio_ten$portfolio = rep(1:ten, nrow(data.portfolio_ten)/ten)


# fill in the market data

data.portfolio_ten = merge(data.portfolio_ten, market, by = "month")

# in order to estimate the portfolio's forward beta, we need the maket return, risk-free return and portfolio excess ret

data.portfolio_ten$exret = 0 # the equal weighted excess return

data.portfolio_ten$vw.exret = 0 # the equal weighted excess return

data.portfolio_ten$cap = 0  # the average market cap.

head(data.portfolio_ten)

for (i in 1:nrow(data.portfolio_ten)){
  data.chosen = data[data$group.no == data.portfolio_ten$portfolio[i] 
                     & data$month == data.portfolio_ten$month[i],]# choose all the banks belonging to the portfolio
  data.portfolio_ten$exret[i] = mean(data.chosen$exret)# take the average of the stocks in that month and in that portfolio
  data.portfolio_ten$cap[i] = mean(data.chosen$cap)
  data.portfolio_ten$vw.exret[i] = weighted.mean(data.chosen$exret,data.chosen$cap)
}

head(data.portfolio_ten)

# now we get an entire time series of portfolios 
data.portfolio_ten = data.portfolio_ten[order(data.portfolio_ten$portfolio,data.portfolio_ten$month ),]
head(data.portfolio_ten)
# if now we estimate one beta, one alpha for the 10 portfolios

ten_portfolio = as.data.frame(rep(0, ten))
colnames(ten_portfolio) = "alpha"
ten_portfolio$beta = 0
ten_portfolio$exret = 0
ten_portfolio$vw.alpha = 0
ten_portfolio$vw.beta = 0
ten_portfolio$vw.exret = 0

ten_portfolio$mktrf = mean(market$mktrf)
ten_portfolio$rf = mean(market$rf)

# estimate the alpha and betas for the portfolios, we do not care about the backward or forward now
# since we already used all available information
for (i in 1:ten){
  model = lm(data = data.portfolio_ten[data.portfolio_ten$portfolio==i,], exret ~ mktrf)
  ten_portfolio$beta[i] = coef(model)[2]# this is the equal weighted portfolio
  ten_portfolio$alpha[i] = coef(model)[1]
  ten_portfolio$exret[i] = mean(data.portfolio_ten$exret[data.portfolio_ten$portfolio==i])
  model.vw = lm(data = data.portfolio_ten[data.portfolio_ten$portfolio==i,], vw.exret ~ mktrf)
  ten_portfolio$vw.beta[i] = coef(model.vw)[2]# this is the value weighted portfolio
  ten_portfolio$vw.alpha[i] = coef(model.vw)[1]
  ten_portfolio$vw.exret[i] = mean(data.portfolio_ten$vw.exret[data.portfolio_ten$portfolio==i])
}



head(ten_portfolio)

# look at the relationship between alpha and beta

summary(lm(data = ten_portfolio, alpha ~ beta))
summary(lm(data = ten_portfolio, vw.alpha ~ vw.beta))

model.ten.ew = lm(data = ten_portfolio, alpha ~ beta)
model.ten.vw = lm(data = ten_portfolio, vw.alpha ~ vw.beta)

summary(model.ten.ew)
summary(model.ten.vw)


data.mix_ten = subset(ten_portfolio, select= c(alpha, beta))
head(ten_portfolio)
data.mix_ten[(ten + 1):(ten + ten),] = ten_portfolio[,4:5]
model.mix_ten = lm(data = data.mix_ten, alpha * 10000 ~ beta)


# combine.ten = ten_portfolio[,c("alpha", "beta")]
# combine.ten[(ten+1):(ten+ten),] = ten_portfolio[,c("vw.alpha", "vw.beta")]
# 
# summary(lm(data = combine.ten, alpha ~ beta))


##################################################################################
##################################################################################
# three portfolios##########
# length(unique.month)
# for (i in 1:length(unique.month)){
#   month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
#   res = round(seq(1, unique.month[i], by = unique.month[i]/three)) # find out the 1:10th, 11st : 20th ...
#   res = c(res, unique.month[i])
#   for (j in 1:three){#give value to the group.no
#     month_chosen$group.no[res[j]:(res[j+1])] = j
#   }
#   data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
# }
# 
# # saveRDS(data, "with_group.rds")
# 
# # create a new data frame to save the portfolio data
# data.portfolio = as.data.frame(rep(unique(data$date),three)) # first time, try 10 portfolios
# colnames(data.portfolio) = "date"
# 
# 
# data.portfolio$month = year(data.portfolio$date) * 100 + month(data.portfolio$date)
# data.portfolio = data.portfolio[order(data.portfolio$month),]
# # create a column to store the portfolio
# data.portfolio$portfolio = rep(1:three, nrow(data.portfolio)/three)
# 
# 
# # fill in the market data
# 
# data.portfolio = merge(data.portfolio, market, by = "month")
# 
# # in order to estimate the portfolio's forward beta, we need the maket return, risk-free return and portfolio excess ret
# 
# data.portfolio$exret = 0 # the equal weighted excess return
# 
# data.portfolio$vw.exret = 0 # the equal weighted excess return
# 
# data.portfolio$cap = 0  # the average market cap.
# 
# head(data.portfolio)
# 
# for (i in 1:nrow(data.portfolio)){
#   data.chosen = data[data$group.no == data.portfolio$portfolio[i] 
#                      & data$month == data.portfolio$month[i],]# choose all the banks belonging to the portfolio
#   data.portfolio$exret[i] = mean(data.chosen$exret)# take the average of the stocks in that month and in that portfolio
#   data.portfolio$cap[i] = mean(data.chosen$cap)
#   data.portfolio$vw.exret[i] = weighted.mean(data.chosen$exret,data.chosen$cap)
# }
# 
# head(data.portfolio)
# 
# # now we get an entire time series of portfolios 
# data.portfolio = data.portfolio[order(data.portfolio$portfolio,data.portfolio$month ),]
# 
# # if now we estimate one beta, one alpha for the 10 portfolios
# 
# three_portfolio = as.data.frame(rep(0, three))
# colnames(three_portfolio) = "alpha"
# three_portfolio$beta = 0
# three_portfolio$exret = 0
# three_portfolio$vw.alpha = 0
# three_portfolio$vw.beta = 0
# three_portfolio$vw.exret = 0
# 
# three_portfolio$mktrf = mean(market$mktrf)
# three_portfolio$rf = mean(market$rf)
# 
# # estimate the alpha and betas for the portfolios, we do not care about the backward or forward now
# # since we already used all available information
# for (i in 1:three){
#   model = lm(data = data.portfolio[data.portfolio$portfolio==i,], exret ~ mktrf)
#   three_portfolio$beta[i] = coef(model)[2]# this is the equal weighted portfolio
#   three_portfolio$alpha[i] = coef(model)[1]
#   three_portfolio$exret[i] = mean(data.portfolio$exret[data.portfolio$portfolio==i])
#   model.vw = lm(data = data.portfolio[data.portfolio$portfolio==i,], vw.exret ~ mktrf)
#   three_portfolio$vw.beta[i] = coef(model.vw)[2]# this is the value weighted portfolio
#   three_portfolio$vw.alpha[i] = coef(model.vw)[1]
#   three_portfolio$vw.exret[i] = mean(data.portfolio$vw.exret[data.portfolio$portfolio==i])
# }
# 
# 
# 
# head(three_portfolio)
# 
# # look at the relationship between alpha and beta
# figure_three_a_b = ggplot(three_portfolio)+ 
#   geom_point(aes(x = beta, y = alpha, col = "EW"))+
#   geom_smooth(aes(x = beta, y = alpha, col = "EW"), method = lm)+
#   geom_point(aes(x = vw.beta, y = vw.alpha, col = "VW"))+
#   geom_smooth(aes(x = vw.beta, y = vw.alpha, col = "VW"), method = lm)+
#   theme_bw()+
#   theme(legend.title = element_blank(), legend.position = c(0.9,0.9))+
#   scale_color_manual(name = c("EW", "VW"),values = c("EW" = "black", "VW" = "blue"))
# print(figure_three_a_b)
# 
# summary(lm(data = three_portfolio, alpha ~ beta))
# summary(lm(data = three_portfolio, vw.alpha ~ vw.beta))
# 
# combine.three = three_portfolio[,c("alpha", "beta")]
# combine.three[(three+1):(three+three),] = three_portfolio[,c("vw.alpha", "vw.beta")]
# print(combine.three)
# summary(lm(data = combine.three, alpha ~ beta))

##################################################################################
##################################################################################


# twenty portfolios##########
length(unique.month)
for (i in 1:length(unique.month)){
  month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
  res = round(seq(1, unique.month[i], by = unique.month[i]/twenty)) # find out the 1:10th, 11st : 20th ...
  res = c(res, unique.month[i])
  for (j in 1:twenty){#give value to the group.no
    month_chosen$group.no[res[j]:(res[j+1])] = j
  }
  data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
}

# create a new data frame to save the portfolio data
data.portfolio_twenty = as.data.frame(rep(unique(data$date),twenty)) # first time, try 10 portfolios
colnames(data.portfolio_twenty) = "date"


data.portfolio_twenty$month = year(data.portfolio_twenty$date) * 100 + month(data.portfolio_twenty$date)
data.portfolio_twenty = data.portfolio_twenty[order(data.portfolio_twenty$month),]
# create a column to store the portfolio
data.portfolio_twenty$portfolio = rep(1:twenty, nrow(data.portfolio_twenty)/twenty)


# fill in the market data

data.portfolio_twenty = merge(data.portfolio_twenty, market, by = "month")

# in order to estimate the portfolio's forward beta, we need the maket return, risk-free return and portfolio excess ret

data.portfolio_twenty$exret = 0 # the equal weighted excess return

data.portfolio_twenty$vw.exret = 0 # the equal weighted excess return

data.portfolio_twenty$cap = 0  # the average market cap.

head(data.portfolio_twenty)

for (i in 1:nrow(data.portfolio_twenty)){
  data.chosen = data[data$group.no == data.portfolio_twenty$portfolio[i] 
                     & data$month == data.portfolio_twenty$month[i],]# choose all the banks belonging to the portfolio
  data.portfolio_twenty$exret[i] = mean(data.chosen$exret)# take the average of the stocks in that month and in that portfolio
  data.portfolio_twenty$cap[i] = mean(data.chosen$cap)
  data.portfolio_twenty$vw.exret[i] = weighted.mean(data.chosen$exret,data.chosen$cap)
}

head(data.portfolio_twenty)

# now we get an entire time series of portfolios 
data.portfolio_twenty = data.portfolio_twenty[order(data.portfolio_twenty$portfolio,data.portfolio_twenty$month ),]
head(data.portfolio_twenty)
# if now we estimate one beta, one alpha for the 10 portfolios

twenty_portfolio = as.data.frame(rep(0, twenty))
colnames(twenty_portfolio) = "alpha"
twenty_portfolio$beta = 0
twenty_portfolio$exret = 0
twenty_portfolio$vw.alpha = 0
twenty_portfolio$vw.beta = 0
twenty_portfolio$vw.exret = 0

twenty_portfolio$mktrf = mean(market$mktrf)
twenty_portfolio$rf = mean(market$rf)

# estimate the alpha and betas for the portfolios, we do not care about the backward or forward now
# since we already used all available information
for (i in 1:twenty){
  model = lm(data = data.portfolio_twenty[data.portfolio_twenty$portfolio==i,], exret ~ mktrf)
  twenty_portfolio$beta[i] = coef(model)[2]# this is the equal weighted portfolio
  twenty_portfolio$alpha[i] = coef(model)[1]
  twenty_portfolio$exret[i] = mean(data.portfolio_twenty$exret[data.portfolio_twenty$portfolio==i])
  model.vw = lm(data = data.portfolio_twenty[data.portfolio_twenty$portfolio==i,], vw.exret ~ mktrf)
  twenty_portfolio$vw.beta[i] = coef(model.vw)[2]# this is the value weighted portfolio
  twenty_portfolio$vw.alpha[i] = coef(model.vw)[1]
  twenty_portfolio$vw.exret[i] = mean(data.portfolio_twenty$vw.exret[data.portfolio_twenty$portfolio==i])
}



head(twenty_portfolio)

# look at the relationship between alpha and beta


summary(lm(data = twenty_portfolio, alpha ~ beta))
summary(lm(data = twenty_portfolio, vw.alpha ~ vw.beta))

model.twenty.ew = lm(data = twenty_portfolio, alpha ~ beta)
model.twenty.vw = lm(data = twenty_portfolio, vw.alpha ~ vw.beta)

data.mix_twenty = subset(twenty_portfolio, select= c(alpha, beta))
head(twenty_portfolio)
data.mix_twenty[(twenty + 1):(twenty + twenty),] = twenty_portfolio[,4:5]

model.mix_twenty = lm(data = data.mix_twenty, alpha*10000 ~ beta)

##################################################################################
##################################################################################
# replicating BW study three portfolios, 30% 40%, 30%##########

data = data[order(data$month, data$b.beta),]
# since we want to sort the data in each month
unique.month = rle(data$month)$lengths
# find the location of each group start
unique.loc = rep(1, length(unique.month)+1)

for (i in 1:length(unique.month)){
  unique.loc[i+1] = sum(unique.month[1:i])+1
}
length(unique.month)
for (i in 1:length(unique.month)){
  month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
  th30 = round(quantile(1:unique.month[i], 0.3))
  th70 = round(quantile(1:unique.month[i], 0.7))
  res = c(1,th30, th70, unique.month[i])
  for (j in 1:three){#give value to the group.no
    month_chosen$group.no[res[j]:(res[j+1])] = j
  }
  data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
}



# create a new data frame to save the portfolio data
data.portfolio_three = as.data.frame(rep(unique(data$date),three)) # first time, try 10 portfolios
colnames(data.portfolio_three) = "date"


data.portfolio_three$month = year(data.portfolio_three$date) * 100 + month(data.portfolio_three$date)
data.portfolio_three = data.portfolio_three[order(data.portfolio_three$month),]
# create a column to store the portfolio
data.portfolio_three$portfolio = rep(1:three, nrow(data.portfolio_three)/three)


# fill in the market data

data.portfolio_three = merge(data.portfolio_three, market, by = "month")

# in order to estimate the portfolio's forward beta, we need the maket return, risk-free return and portfolio excess ret

data.portfolio_three$exret = 0 # the equal weighted excess return

data.portfolio_three$vw.exret = 0 # the equal weighted excess return

data.portfolio_three$cap = 0  # the average market cap.

head(data.portfolio_three)

for (i in 1:nrow(data.portfolio_three)){
  data.chosen = data[data$group.no == data.portfolio_three$portfolio[i] 
                     & data$month == data.portfolio_three$month[i],]# choose all the banks belonging to the portfolio
  data.portfolio_three$exret[i] = mean(data.chosen$exret)# take the average of the stocks in that month and in that portfolio
  data.portfolio_three$cap[i] = mean(data.chosen$cap)
  data.portfolio_three$vw.exret[i] = weighted.mean(data.chosen$exret,data.chosen$cap)
}

head(data.portfolio_three)


# now we get an entire time series of portfolios 
data.portfolio_three = data.portfolio_three[order(data.portfolio_three$portfolio,data.portfolio_three$month ),]
summary(data.portfolio_three)
# if now we estimate one beta, one alpha for the 10 portfolios

three_portfolio = as.data.frame(rep(0, three))
colnames(three_portfolio) = "alpha"
three_portfolio$beta = 0
three_portfolio$exret = 0
three_portfolio$vw.alpha = 0
three_portfolio$vw.beta = 0
three_portfolio$vw.exret = 0

three_portfolio$mktrf = mean(market$mktrf)
three_portfolio$rf = mean(market$rf)

# estimate the alpha and betas for the portfolios, we do not care about the backward or forward now
# since we already used all available information
for (i in 1:three){
  model = lm(data = data.portfolio_three[data.portfolio_three$portfolio==i,], exret ~ mktrf)
  three_portfolio$beta[i] = coef(model)[2]# this is the equal weighted portfolio
  three_portfolio$alpha[i] = coef(model)[1]
  three_portfolio$exret[i] = mean(data.portfolio_three$exret[data.portfolio_three$portfolio==i])
  model.vw = lm(data = data.portfolio_three[data.portfolio_three$portfolio==i,], vw.exret ~ mktrf)
  three_portfolio$vw.beta[i] = coef(model.vw)[2]# this is the value weighted portfolio
  three_portfolio$vw.alpha[i] = coef(model.vw)[1]
  three_portfolio$vw.exret[i] = mean(data.portfolio_three$vw.exret[data.portfolio_three$portfolio==i])
}



head(three_portfolio)

# look at the relationship between alpha and beta
figure_three_a_b = ggplot(three_portfolio)+ 
  geom_point(aes(x = beta, y = alpha *10000, col = "EW"))+
  geom_smooth(aes(x = beta, y = alpha *10000, col = "EW"), method = lm)+
  geom_point(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"))+
  geom_smooth(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"), method = lm)+
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.9,0.9))+
  scale_color_manual(name = c("EW", "VW"),values = c("EW" = "blue", "VW" = "black"))+
  labs(x= "Beta", y = "Alpha (bps)", title = element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())
print(figure_three_a_b)

summary(lm(data = three_portfolio, alpha ~ beta))
summary(lm(data = three_portfolio, vw.alpha ~ vw.beta))

model.three.ew = lm(data = three_portfolio, alpha ~ beta)
model.three.vw = lm(data = three_portfolio, vw.alpha ~ vw.beta)
# mix them 
data.mix_three = subset(three_portfolio, select= c(alpha, beta))
head(three_portfolio)
data.mix_three[(three + 1):(three + three),] = three_portfolio[,4:5]
model.mix_three = lm(data = data.mix_three, alpha*10000 ~ beta)

############################################################################
## ------plotting-------------------------

model.ols = lm(data = data, b.alpha*10000 ~ b.beta)
model.three_bw_mix = readRDS("BW_low_risk_model.rds")
summary(model.three_bw_mix)
summary(model.mix_three)
library(stargazer)

stargazer(model.three_bw_mix, model.mix_three, model.mix_ten, model.mix_twenty,model.ols)




f_bw = ggplot(data = data.mix)+
  theme_classic()+
  geom_point(data = data.mix[1:3,], aes(x = beta, y = alpha * 10000, shape = "EW portfolio"), size = 3)+
  geom_point(data = data.mix[4:6,], aes(x = beta, y = alpha * 10000,shape = "VW portfolio"), size = 3)+
  geom_smooth(method = lm, aes(x = beta, y = alpha * 10000, col = "Linear"))+
  scale_shape_manual(name = c("EW portfolio", "VW portfolio"),values = c("EW portfolio" = 15, "VW portfolio" = 16))+
  scale_color_manual(name = "linear",values = "blue")+
  theme(legend.title = element_blank(), legend.position = c(0.85,0.85))+
  labs(x= "Beta", y = "Alpha (bps)", title = element_blank())+
  scale_x_continuous(breaks=seq(0,1.5,0.2))
 

print(f_bw)


model.bw = lm(data= data.mix, alpha*10000 ~ beta)

summary(model.bw)


################################################
f_ten = ggplot(data = ten_portfolio, aes(x = beta)) +
  theme_bw()+
  geom_line(aes(y = beta * mktrf * 10000, col = "CAPM"), size = 0.9)+
  geom_point(aes(y = exret * 10000, col = "Anomaly EW"))+
  geom_smooth(aes(y = exret * 10000, col = "Anomaly EW"),method = lm)+
  theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))+
  labs(x= "Beta", y = "Monthly Excess return (bps)", title = element_blank())+
  theme(legend.position = c(0.80, 0.12),legend.title =  element_blank(),
        legend.text = element_text( size = 11),legend.key.width = unit(1.5, "cm"))+
  scale_color_manual(values = c("CAPM" = "black", "Anomaly EW" = "blue"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())

print(f_ten)






f_three = ggplot(data = three_portfolio, aes(x = beta)) +
  theme_bw()+
  geom_line(aes(y = beta * mktrf * 10000, col = "CAPM"), size = 0.9)+
  geom_point(aes(y = exret * 10000, col = "Anomaly EW"))+
  geom_smooth(aes(y = exret * 10000, col = "Anomaly EW"),method = lm)+
  theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))+
  labs(x= "Beta", y = "Monthly Excess return (bps)", title = element_blank())+
  theme(legend.position = c(0.80, 0.12),legend.title =  element_blank(),
        legend.text = element_text( size = 11),legend.key.width = unit(1.5, "cm"))+
  scale_color_manual(values = c("CAPM" = "black", "Anomaly EW" = "blue"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())

print(f_three)



f_twenty = ggplot(data = twenty_portfolio, aes(x = beta)) +
  theme_bw()+
  geom_line(aes(y = beta * mktrf * 10000, col = "CAPM"), size = 0.9)+
  geom_point(aes(y = exret * 10000, col = "Anomaly EW"))+
  geom_smooth(aes(y = exret * 10000, col = "Anomaly EW"),method = lm)+
  theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))+
  labs(x= "Beta", y = "Monthly Excess return (bps)", title = element_blank())+
  theme(legend.position = c(0.80, 0.12),legend.title =  element_blank(),
        legend.text = element_text( size = 11),legend.key.width = unit(1.5, "cm"))+
  scale_color_manual(values = c("CAPM" = "black", "Anomaly EW" = "blue"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())

print(f_twenty)


figure_ten_a_b = ggplot(ten_portfolio)+ 
  geom_point(aes(x = beta, y = alpha *10000, col = "EW"))+
  geom_smooth(aes(x = beta, y = alpha *10000, col = "EW"), method = lm)+
  geom_point(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"))+
  geom_smooth(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"), method = lm)+
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.9,0.9))+
  scale_color_manual(name = c("EW", "VW"),values = c("EW" = "blue", "VW" = "black"))+
  labs(x= "Beta", y = "Alpha (bps)", title = element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())


print(figure_ten_a_b)


figure_twenty_a_b = ggplot(twenty_portfolio)+ 
  geom_point(aes(x = beta, y = alpha *10000, col = "EW"))+
  geom_smooth(aes(x = beta, y = alpha *10000, col = "EW"), method = lm)+
  geom_point(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"))+
  geom_smooth(aes(x = vw.beta, y = vw.alpha *10000, col = "VW"), method = lm)+
  theme_bw()+
  theme(legend.title = element_blank(), legend.position = c(0.9,0.9))+
  scale_color_manual(name = c("EW", "VW"),values = c("EW" = "blue", "VW" = "black"))+
  labs(x= "Beta", y = "Alpha (bps)", title = element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())

print(figure_twenty_a_b)


library(gridExtra)
grid.arrange(f_three, figure_three_a_b, nrow = 1)
grid.arrange(f_ten, figure_ten_a_b, nrow = 1)
grid.arrange(f_twenty, figure_twenty_a_b, nrow = 1)

mean(market$mktrf)





