#############################################################
# obtain data from CRSP database-----------------------------

# rm(list = ls())
# library(RPostgres)
# library(lubridate)
# 
# wrds <- dbConnect(Postgres(),
#                   host='wrds-pgdata.wharton.upenn.edu',
#                   port=9737,
#                   user = '$USER$', # please enter the user name and password
#                   password = '$PASSWORD$',
#                   dbname='wrds',
#                   sslmode='require')
# 
# res1 <- dbSendQuery(wrds, "select permno,permco,date,ret,hsiccd,prc,shrout,cusip
#                    from crsp.msf
#                    where date between '1970-01-01' and '2019-12-31'
#                    and ret IS NOT NULL
#                    and prc IS NOT NULL
#                    and shrout IS NOT NULL
#                    and (hsiccd = 6000
#                    or hsiccd BETWEEN 6020 AND 6036
#                    or hsiccd BETWEEN 6040 AND 6062
#                    or hsiccd BETWEEN 6120 AND 6179
#                    or hsiccd BETWEEN 6190 AND 6199
#                    or hsiccd BETWEEN 6710 AND 6719)")
# 
# 
# data <- dbFetch(res1, n=-1)
# dbClearResult(res1)
# data$month = year(data$date)*100+month(data$date)
# 
# saveRDS(data, "crsp_full_data.rds")


# find the market data from wrds-------------------------------------------
# res <- dbSendQuery(wrds, "select date,mktrf,rf,hml,smb,umd
#                    from ff.factors_monthly
#                    where date between '1970-01-01' and '2019-12-31'")
# 
# 
# market <- dbFetch(res, n=-1)
# dbClearResult(res)
# 
# market$month = year(market$date)*100+month(market$date)
# market = market[,-1]
# 
# saveRDS(market, "market.rds")


########################################################################
# Summary of dataset ------------------------------------------------------
rm(list = ls())
library(readxl)
library(readr);library(xts);library(stats);
library(lubridate)
library(RPostgres)
library(dplyr);library(nlme)
library(ggplot2); library(knitr);library(writexl)
library(kableExtra)
# import dataset
data = readRDS("crsp_full_data.rds")
market = readRDS("market.rds")
# look at the dataset
head(data)

# write the PERMCO into excel file------------------------------------------
# permco =as.data.frame(unique(data$permco))
# colnames(permco) = "permco"
# writexl::write_xlsx(permco, "permco.xlsx")


# count how many banks in a certain group and in a certain time period
data = na.omit(data)
data.sum = data[, c("permno","date","hsiccd")]

# import a formatted table to save the summary data
table_sic <- read_excel("siccode.xlsx")

data.sum$period <- 0
data.sum$period[data.sum$date < "1980-01-01"] <- 1
data.sum$period[data.sum$date >= "1980-01-01" & data.sum$date < "1990-01-01"] <- 2
data.sum$period[data.sum$date >= "1990-01-01" & data.sum$date < "2000-01-01"] <- 3
data.sum$period[data.sum$date >= "2000-01-01" & data.sum$date < "2010-01-01"] <- 4
data.sum$period[data.sum$date >= "2010-01-01"] <- 5

for (i in 1:5){
  table_sic[1,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd == 6000]))
  table_sic[3,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd == 6020]))
  table_sic[4,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd == 6021]))
  table_sic[5,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd == 6022]))
  table_sic[6,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6023 & data.sum$hsiccd <= 6024]))
  table_sic[7,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd == 6025]))
  table_sic[8,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd == 6026]))
  table_sic[9,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd == 6027]))
  table_sic[10,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6028 & data.sum$hsiccd <= 6029]))
  table_sic[11,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6030 & data.sum$hsiccd <= 6036]))
  table_sic[12,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6040 & data.sum$hsiccd <= 6050]))
  table_sic[13,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6060 & data.sum$hsiccd <= 6062]))
  
  table_sic[19,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6120 & data.sum$hsiccd <= 6129]))
  table_sic[20,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6130 & data.sum$hsiccd <= 6139]))
  table_sic[21,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6140 & data.sum$hsiccd <= 6149]))
  table_sic[22,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6150 & data.sum$hsiccd <= 6159]))
  table_sic[23,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6160 & data.sum$hsiccd <= 6169]))
  table_sic[24,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6170 & data.sum$hsiccd <= 6179]))
  table_sic[25,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6190 & data.sum$hsiccd <= 6199]))
  table_sic[26,i+2] = length(unique(data.sum$permno[data.sum$period == i & data.sum$hsiccd >= 6710 & data.sum$hsiccd <= 6719]))
  table_sic[27,i+2] = length(unique(data.sum$permno[data.sum$period == i]))

}

table_sic[1,8] = length(unique(data.sum$permno[data.sum$hsiccd == 6000]))
table_sic[3,8] = length(unique(data.sum$permno[data.sum$hsiccd == 6020]))
table_sic[4,8] = length(unique(data.sum$permno[data.sum$hsiccd == 6021]))
table_sic[5,8] = length(unique(data.sum$permno[data.sum$hsiccd == 6022]))
table_sic[6,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6023 & data.sum$hsiccd <= 6024]))
table_sic[7,8] = length(unique(data.sum$permno[data.sum$hsiccd == 6025]))
table_sic[8,8] = length(unique(data.sum$permno[data.sum$hsiccd == 6026]))
table_sic[9,8] = length(unique(data.sum$permno[data.sum$hsiccd == 6027]))
table_sic[10,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6028 & data.sum$hsiccd <= 6029]))
table_sic[11,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6030 & data.sum$hsiccd <= 6036]))
table_sic[12,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6040 & data.sum$hsiccd <= 6050]))
table_sic[13,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6060 & data.sum$hsiccd <= 6062]))

table_sic[19,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6120 & data.sum$hsiccd <= 6129]))
table_sic[20,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6130 & data.sum$hsiccd <= 6139]))
table_sic[21,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6140 & data.sum$hsiccd <= 6149]))
table_sic[22,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6150 & data.sum$hsiccd <= 6159]))
table_sic[23,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6160 & data.sum$hsiccd <= 6169]))
table_sic[24,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6170 & data.sum$hsiccd <= 6179]))
table_sic[25,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6190 & data.sum$hsiccd <= 6199]))
table_sic[26,8] = length(unique(data.sum$permno[data.sum$hsiccd >= 6710 & data.sum$hsiccd <= 6719]))
table_sic[27,8] = 3212


write_xlsx(x = table_sic, path = "table_sic.xlsx", col_names = TRUE)


kable(table_sic, "latex", caption = "Table name", booktabs = T)%>%
  kable_styling(font_size = 10, latex_options = c("hold_position"))

# summay the return, price, market excess risk free return
library(psych)
table.statistics = matrix(0, nrow = 5, ncol = 5)
rownames(table.statistics) = c("Return","Price", "Shares", "Mkt_ret", "Rf")
colnames(table.statistics) = c("Obs","Mean", "Median","Min", "Max")
table.statistics = as.data.frame(table.statistics)

head(data)
# use the describe function to directly get more statistics
data$price = abs(data$prc)
describe(data$price)[,c(2,3,5,8,9) ]


table.statistics["Return",] = describe(data$ret)[,c(2,3,5,8,9)]
table.statistics["Price",] = describe(data$price)[,c(2,3,5,8,9)]
table.statistics["Shares",] = describe(data$shrout)[,c(2,3,5,8,9)]/1000
table.statistics["Mkt_ret",] = describe(market$mktrf)[,c(2,3,5,8,9)]
table.statistics["Rf",] = describe(market$rf)[,c(2,3,5,8,9)]


table.statistics["Return",] = round(table.statistics["Return",],4)
table.statistics["Mkt_ret",] = round(table.statistics["Mkt_ret",],4)
table.statistics["Rf",] = round(table.statistics["Rf",],4)

table.statistics$Max = round(table.statistics$Max, 2)


kable(table.statistics, "latex", caption = "Table name", booktabs = T)%>%
  kable_styling(font_size = 10, latex_options = c("hold_position"))



















