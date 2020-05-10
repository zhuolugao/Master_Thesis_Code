# rm(list = ls())
# library(RPostgres)
# 
# wrds <- dbConnect(Postgres(),
#                   host='wrds-pgdata.wharton.upenn.edu',
#                   port=9737,
#                   user = '$USER$', # please enter the user name and password
#                   password = 'xxxxxx',
#                   dbname='wrds',
#                   sslmode='require')
# 
# res1 <- dbSendQuery(wrds, "select datadate, fyear, conm, cusip, sic,ceq,lt, dptc, dltt, at, capr1, capr2
#                    from bank_funda
# 
#                    where (sic = '6000'
#                    or sic BETWEEN '6020' AND '6036'
#                    or sic BETWEEN '6040' AND '6062'
#                    or sic BETWEEN '6120' AND '6179'
#                    or sic BETWEEN '6190' AND '6199'
#                    or sic BETWEEN '6710' AND '6719')")
# 
# data <- dbFetch(res1, n=-1)
# dbClearResult(res1)
# 
# saveRDS(data, "compustat_yearly.rds")


# load the data found above-----------------------------------------------------
# look at the yearly data
rm(list = ls())
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(ggpubr)

data = readRDS("compustat_yearly.rds")
# order data by year and by its identifier
data = data[order(data$fyear, data$cusip),]
head(data)

# rename the variables to make it clearer
colnames(data) = c("date","year","name","cusip","sic","equity","liability","deposit",
                   "long.debt","asset","tier1.ratio","tier2.ratio")
head(data)
# if we look at the book leverage ratio
# define book_lev_ratio = book value of total liabilities / total assets
# define book_eq_ratio = book value of common equity / total assets
# define dep_liability = deposits / total liabilities

data$book_lev_ratio = data$liability / data$asset * 100

data$book_eq_ratio = data$equity / data$asset * 100

data$dep_liability = data$deposit/data$liability * 100

min(data$year)
head(data)


# filter data with very extreme values
data = data[data$year >= 1970,] # only look at after year 1970
data = data[!is.na(data$equity),]
data = data[!is.na(data$liability),]
data = data[!is.na(data$asset),]
data = data[!is.na(data$deposit),] #remove data with NA

# filter extreme values

data = data[data$book_lev_ratio>50,]
data = data[data$book_eq_ratio>0,]
data = data[data$dep_liability>0,]
data = data[data$book_lev_ratio<100,]
data = data[data$book_eq_ratio<100,]
data = data[data$dep_liability<100,]

summary(data$year)

# plotting ------------------------------------------------------
f_deposit_liability_whole <- ggplot(data = data, aes(x = factor(year), y = dep_liability))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(breaks=c(0,25,50,75,round(mean(data$dep_liability)),100))+
  labs(x= element_blank(), y = "Deposit / Total Liability Ratio (Percentage)", 
       title = element_blank())+
  ggpubr::rotate_x_text()+
  geom_hline(yintercept = mean(data$dep_liability), col = "red", linetype = "dashed")+
  ggsave("f_deposit_liability_whole.eps", width = 20, height = 14, units = "cm")

print(f_deposit_liability_whole)


f_liability_asset_whole <- ggplot(data = data, aes(x = factor(year), y = book_lev_ratio))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(breaks=c(50,75,round(mean(data$book_lev_ratio)),100))+
  labs(x= element_blank(), y = "Total Liability / Total Asset Ratio (Percentage)", 
       title = element_blank())+
  ggpubr::rotate_x_text()+
  geom_hline(yintercept = mean(data$book_lev_ratio), col = "red", linetype = "dashed")+
  ggsave("f_liability_asset_whole.eps", width = 20, height = 14, units = "cm")

print(f_liability_asset_whole)

f_equity_asset_whole <- ggplot(data = data, aes(x = factor(year), y = book_eq_ratio))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(breaks=c(0,25,round(mean(data$book_eq_ratio))))+
  labs(x= element_blank(), y = "Common Equity / Total Asset Ratio (Percentage)", 
       title = element_blank())+
  ggpubr::rotate_x_text()+
  geom_hline(yintercept = mean(data$book_eq_ratio), col = "red", linetype = "dashed")+
  ggsave("f_equity_asset_whole.eps", width = 20, height = 14, units = "cm")

print(f_equity_asset_whole)

data$test = 100 - data$book_lev_ratio
test.figure <- ggplot(data = data, aes(x = factor(year), y = test))+
  geom_boxplot()+
  theme_bw()+
  labs(x= element_blank(), y = "Total Equity / Total Asset Ratio (Percentage)", 
       title = element_blank())+
  ggpubr::rotate_x_text()+
  geom_hline(yintercept = mean(data$test), col = "red", linetype = "dashed")+
  ggsave("f_book_eq.eps", width = 20, height = 14, units = "cm")

print(test.figure)

cor(data$test, data$book_eq_ratio)




## look at the data after 1993, compare the tier 1 ratio to the book-equity ratio

# save data of after 1993

filter.data = data[!is.na(data$tier1.ratio),]
filter.data = filter.data[filter.data$tier1.ratio<50,]
summary(filter.data$year)


f_tier1 = ggplot(data = filter.data, aes(x = factor(year), y = tier1.ratio)) +
  geom_boxplot()+
  theme_bw()+
  labs(x= element_blank(), y = "Tier 1 Ratio (Percentage)", 
       title = element_blank())+
  ggpubr::rotate_x_text()+
  geom_hline(yintercept = mean(filter.data$tier1.ratio), col = "red", linetype = "dashed")+
  ggsave(file="f_tier1.eps", width = 20, height = 12, units = "cm")

print(f_tier1)

# the above plot shows the annual distribution of the tier 1 ratio

f_equity_asset_1993 = ggplot(data = filter.data, aes(x = factor(year), y = book_eq_ratio)) +
  geom_boxplot()+
  theme_bw()+
  labs(x= element_blank(), y = "Book Equity / Total Asset Ratio (Percentage)", 
       title = element_blank())+
  ggpubr::rotate_x_text()+
  geom_hline(yintercept = mean(filter.data$book_eq_ratio), col = "red", linetype = "dashed")+
  ggsave(file="f_equity_asset_1993.eps", width = 20, height = 12, units = "cm")

print(f_equity_asset_1993)


#-------------------------------------------------------------------------------------------
# look at the correlation between the tier 1 capital ratio and the equity/asset ratio
cor(filter.data$tier1.ratio, filter.data$book_eq_ratio)


sub.data1 = filter.data[filter.data$year<2011,]
sub.data2 = filter.data[filter.data$year>2010,]

cor(sub.data1$tier1.ratio, sub.data1$book_eq_ratio)
cor(sub.data2$tier1.ratio, sub.data2$book_eq_ratio)


# make a correlation table
cor.table = rep(0, length(1993:2019))

k = 1
for (i in 1993:2019){
  cor.table[k] = cor(filter.data$tier1.ratio[filter.data$year==i], filter.data$book_eq_ratio[filter.data$year==i])
  k = k + 1
}

plot(cor.table)

cor.table = as.data.frame(cor.table)

colnames(cor.table) = "Correlation"
cor.table$year = 1993:2019

corr.fig = ggplot(data = cor.table, aes(x = year, y = Correlation)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  scale_x_continuous(labels = cor.table$year, breaks =cor.table$year)+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank())+
  labs(x= element_blank(), y = "Correlation", 
       title = element_blank())+
  ggpubr::rotate_x_text()+
  geom_segment(aes(x=1993,xend=2010,
                   y=cor(sub.data1$tier1.ratio, sub.data1$book_eq_ratio),
                   yend=cor(sub.data1$tier1.ratio, sub.data1$book_eq_ratio)),color="red",linetype = "dashed")+
  geom_segment(aes(x=2011,xend=2019,
                   y=cor(sub.data2$tier1.ratio, sub.data2$book_eq_ratio),
                   yend=cor(sub.data2$tier1.ratio, sub.data2$book_eq_ratio)),color="blue",linetype = "dashed")+
  ggsave("correlation_tier_equity.eps")


print(corr.fig)


# portfolios --------------------------------------------------------------


########### look at the size
data = data[order(data$year, data$asset),]
data$group.no = 0
three = 3
# since we want to sort the data in each month
unique.month = rle(data$year)$lengths
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



# three groups ##########

data.portfolio_three = as.data.frame(rep(unique(data$year),three)) # first time, try 10 portfolios
colnames(data.portfolio_three) = "year"
data.portfolio_three$portfolio = 0
data.portfolio_three = data.portfolio_three[order(data.portfolio_three$year),]


# create a column to store the portfolio
data.portfolio_three$portfolio = rep(1:three, nrow(data.portfolio_three)/three)

head(data)

data.portfolio_three$tier1.ratio=0
data.portfolio_three$book_eq_ratio=0

for (i in 1:nrow(data.portfolio_three)){
  data.chosen = data[data$group.no == data.portfolio_three$portfolio[i] 
                     & data$year == data.portfolio_three$year[i],]# choose all the banks belonging to the portfolio
  data.portfolio_three$tier1.ratio[i] = mean(na.omit(data.chosen$tier1.ratio))# take the average of the stocks in that month and in that portfolio
  data.portfolio_three$book_eq_ratio[i] = mean(na.omit(data.chosen$book_eq_ratio))
}



# plot

# f.size.eq = ggplot() +
#   geom_line(data = data.portfolio_three[data.portfolio_three$portfolio == 1,], aes(x = year, y = book_eq_ratio))+
#   geom_point(data = data.portfolio_three[data.portfolio_three$portfolio == 1,], aes(x = year, y = book_eq_ratio, shape = "Bottom 30%"))+
#   geom_line(data = data.portfolio_three[data.portfolio_three$portfolio == 2,], aes(x = year, y = book_eq_ratio), linetype = "dashed")+
#   geom_point(data = data.portfolio_three[data.portfolio_three$portfolio == 2,], aes(x = year, y = book_eq_ratio, shape = "Middle 40%"))+
#   geom_line(data = data.portfolio_three[data.portfolio_three$portfolio == 3,], aes(x = year, y = book_eq_ratio), linetype = "dotted")+
#   geom_point(data = data.portfolio_three[data.portfolio_three$portfolio == 3,], aes(x = year, y = book_eq_ratio, shape = "Top 30%"))+
#   theme_bw()+
#   scale_shape_manual(name = c("Bottom 30%", "Middle 40%","Top 30%"),values = c("Bottom 30%" = 16, "Middle 40%" = 2,"Top 30%" = 4))+
#   theme(panel.background = element_rect(fill = NA),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_line(colour = "grey80"),
#         panel.grid.minor.y = element_blank(),
#         axis.line = element_line(size = 1, colour = "grey30"))+
#   labs(x= element_blank(), y = "Common Equity / Total Asset ratio (%)", 
#        title = element_blank())+
#   theme(legend.title = element_blank(), legend.position = c(0.9,0.2))+
#   scale_x_continuous(breaks=seq(1970,2019,by = 5))
# print(f.size.eq)
# 
# 
# subset = data.portfolio_three[!is.na(data.portfolio_three$tier1.ratio),]
# 
# f.size.tier = ggplot() +
#   geom_line(data = subset[subset$portfolio == 1,], aes(x = year, y = tier1.ratio))+
#   geom_point(data = subset[subset$portfolio == 1,], aes(x = year, y = tier1.ratio, shape = "Bottom 30%"))+
#   geom_line(data = subset[subset$portfolio == 2,], aes(x = year, y = tier1.ratio))+
#   geom_point(data = subset[subset$portfolio == 2,], aes(x = year, y = tier1.ratio, shape = "Middle 40%"))+
#   geom_line(data = subset[subset$portfolio == 3,], aes(x = year, y = tier1.ratio))+
#   geom_point(data = subset[subset$portfolio == 3,], aes(x = year, y = tier1.ratio, shape = "Top 30%"))+
#   theme_bw()+
#   scale_shape_manual(name = c("Bottom 30%", "Middle 40%","Top 30%"),values = c("Bottom 30%" = 16, "Middle 40%" = 2,"Top 30%" = 4))+
#   theme(panel.background = element_rect(fill = NA),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_line(colour = "grey80"),
#         panel.grid.minor.y = element_blank(),
#         axis.line = element_line(size = 1, colour = "grey30"))+
#   labs(x= element_blank(), y = "Tier 1 ratio (%)", 
#        title = element_blank())+
#   theme(legend.title = element_blank(), legend.position = c(0.9,0.2))+
#   scale_x_continuous(breaks=seq(1993,2019,by = 3))
# print(f.size.tier)
# 





f.size.eq = ggplot() +
  geom_line(data = data.portfolio_three[data.portfolio_three$portfolio == 1,], aes(x = year, y = book_eq_ratio,linetype = "Bottom 30%"), size = 1.2)+
  geom_line(data = data.portfolio_three[data.portfolio_three$portfolio == 2,], aes(x = year, y = book_eq_ratio, linetype = "Middle 40%"), size = 1.2)+
  geom_line(data = data.portfolio_three[data.portfolio_three$portfolio == 3,], aes(x = year, y = book_eq_ratio, linetype = "Top 30%"), size = 1.2)+
  theme_bw()+
  scale_linetype_manual(name = c("Bottom 30%", "Middle 40%","Top 30%"),values = c("Bottom 30%" = "solid", "Middle 40%" = "dashed","Top 30%" = "dotted"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank())+
  labs(x= element_blank(), y = "Common Equity / Total Asset ratio (%)", 
       title = element_blank())+
  theme(legend.title = element_blank(),  legend.position = c(0.82,0.2),legend.key.width = unit(1.5, "cm"))+
  scale_x_continuous(breaks=seq(1970,2019,by = 5))
print(f.size.eq)


subset = data.portfolio_three[!is.na(data.portfolio_three$tier1.ratio),]

f.size.tier = ggplot() +
  geom_line(data = subset[subset$portfolio == 1,], aes(x = year, y = tier1.ratio,linetype = "Bottom 30%"), size = 1.2)+
  geom_line(data = subset[subset$portfolio == 2,], aes(x = year, y = tier1.ratio,linetype = "Middle 40%"), size = 1.2)+
  geom_line(data = subset[subset$portfolio == 3,], aes(x = year, y = tier1.ratio,linetype = "Top 30%"), size = 1.2)+
  theme_bw()+
  scale_linetype_manual(name = c("Bottom 30%", "Middle 40%","Top 30%"),values = c("Bottom 30%" = "solid", "Middle 40%" = "dashed","Top 30%" = "dotted"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank())+
  labs(x= element_blank(), y = "Tier 1 ratio (%)", 
       title = element_blank())+
  theme(legend.title = element_blank(),  legend.position = c(0.82,0.2),legend.key.width = unit(1.5, "cm"))+
  scale_x_continuous(breaks=seq(1993,2019,by = 3))
print(f.size.tier)


# tables ------------------------------------------------------------------
library(psych)
head(data)
table.statistics = matrix(0, nrow = 8, ncol = 5)
rownames(table.statistics) = c("equity","liability", "deposit", "asset", "tier1.ratio",
                               "book_lev_ratio", "book_eq_ratio", "dep_liability")
colnames(table.statistics) = c("Obs","Mean", "Median","Min", "Max")
table.statistics = as.data.frame(table.statistics)

head(data)
# use the describe function to directly get more statistics

describe(data$equity)[,c(2,3,5,8,9) ]


table.statistics["equity",] = describe(data$equity)[,c(2,3,5,8,9)]
table.statistics["liability",] = describe(data$liability)[,c(2,3,5,8,9)]
table.statistics["deposit",] = describe(data$deposit)[,c(2,3,5,8,9)]
table.statistics["asset",] = describe(data$asset)[,c(2,3,5,8,9)]
table.statistics["tier1.ratio",] = describe(data$tier1.ratio)[,c(2,3,5,8,9)]
table.statistics["book_lev_ratio",] = describe(data$book_lev_ratio)[,c(2,3,5,8,9)]
table.statistics["book_eq_ratio",] = describe(data$book_eq_ratio)[,c(2,3,5,8,9)]
table.statistics["dep_liability",] = describe(data$dep_liability)[,c(2,3,5,8,9)]


table.statistics = round(table.statistics, 2)

library(kableExtra)
kable(table.statistics, "latex", caption = "Table name", booktabs = T)%>%
  kable_styling(font_size = 10, latex_options = c("hold_position"))












