rm (list = ls())

# import the dataset of the relationship between ISIN and CUSIP
library(readr)
ISIN <- read_csv("ISIN2271.txt", col_names = FALSE)
colnames(ISIN) = c("cusip", "isin")
head(ISIN)
# from IBES database in Refinitiv using the ISINs
library(readxl)
ibes <- read_excel("ibes.xlsx")

# the dataset contains data from 01-02-1976 to 2020-01-01, 
ibes[1,]

# the first line in the raw data file contains some dates information in double format
# so we want to tranform them into the data format

dates = t(as.vector(ibes[1,2:ncol(ibes)]))
dates = as.Date(dates, origin = "1899-12-30")
head(dates) #check
tail(dates)
# correct!!!!!

# initiate a big table to save the useful firms
data = as.data.frame(rep(dates, nrow(ISIN)))
head(data)
colnames(data) = "date"
data$isin = 0
data$eps.1 = 0
data$eps.2 = 0
data$ltg = 0
data$dps.1 = 0
data$shares = 0
data$price = 0
data$end.date = 0

# fill in the ISIN in the big table
for (i in 0:(nrow(ISIN)-1)){#length(dates) months
  data$isin[(i*length(dates)+1):(i*length(dates)+length(dates))] = ISIN$isin[i+1]
}

# fill in the other variables in the table
ibes = ibes[-1,] # remove the first row which is the dates vector

# the 3rd to 9th columns are matching the ibes
for (i in 0:(nrow(ISIN)-1)){
  data[(i*length(dates)+1):(i*length(dates)+length(dates)),3:9] = t(ibes[(i*7+1):(i*7+7),2:ncol(ibes)])
}
tail(data)

data$end.date = as.Date(data$end.date, origin = "1899-12-30")


######################################################################
##################################################################

data.dps = data[, c("date","isin","dps.1")]
data.remove = subset(data, select = -dps.1 )

data.remove = na.omit(data.remove)
length(unique(data.remove$isin))
# there are 908 banks left

data.final = merge(data.dps, data.remove, by=c("date","isin"))
#saveRDS(data.final, file = "IBES_data.rds")

jpm = data.final[data.final$isin == "US46625H1005",]
jpm = jpm[jpm$date >= "1993-01-01",]


jpm.tail = jpm[(nrow(jpm)-2):nrow(jpm), ]
jpm.tail =subset(jpm.tail, select = -c(shares, end.date))
library(dplyr)
library(kableExtra)
kable(jpm.tail, "latex", caption = "Example of I/B/E/S raw data", booktabs = T)%>%
  kable_styling(font_size = 10, latex_options = c("hold_position"))











