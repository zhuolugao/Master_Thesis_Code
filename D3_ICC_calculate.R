#######################################################################
# combine datasets
rm (list = ls())
library(dplyr)
library(readr)
library(ggplot2)
ISIN <- read_csv("ISIN2271.txt", col_names = FALSE)
colnames(ISIN) = c("cusip", "isin")
ibes.data = readRDS(file = "IBES_data.rds")
payout.data = readRDS(file = "ICC_dps_compustat.rds")
gamma.data = readRDS(file = "gamma.rds")

# merge the datasets
head(ibes.data)
data = merge(ibes.data, ISIN, by = "isin")
head(data)

head(payout.data)


data = merge(data, payout.data, by = c("cusip", "date"))

head(gamma.data)
data = merge(data, gamma.data, by = "date")

# combine the forecasting dps and the estimated dps
data$dps.raw = data$dps.1

# if there is NA in the raw dps, them, set it equal to estimated dps
data$dps.1[is.na(data$dps.raw)] = data$dps[is.na(data$dps.raw)]

#############################################################################
##################### calculation using formulas #############################

data$eps.1 = abs(data$eps.1)
data$eps.2 = abs(data$eps.2)
data$ltg = data$ltg /100
# remove reporting errors
data$A = 0.5*(data$gamma_1 + data$dps.1 / data$price)
# stg!!!!!
data$stg = sqrt((data$eps.2-data$eps.1)/data$eps.1 * data$ltg)

data$stg.altern = (data$eps.2 - data$eps.1) / data$eps.1

data$icc_oj = data$A + sqrt((data$A)^2 + data$eps.1 /data$price *(data$stg - data$gamma_1))
data$icc_peg = sqrt( data$eps.1 /data$price * data$stg)



data$icc = 0.5*(data$icc_oj + data$icc_peg)

# data$icc_oj_alt = data$A + sqrt(data$A^2 + data$eps.1 /data$price *(data$stg.altern - data$gamma_1))
# data$icc_peg_alt = sqrt( data$eps.1 /data$price * data$stg.altern)
# data$icc_alt = 0.5*(data$icc_oj_alt + data$icc_peg_alt)
#################################################################
##################### Plotting!!!! #############################
data = data[!is.na(data$icc),]
data = data[data$date>="1993-01-01",]
summary(data$icc)

data = data[order(data$date, data$cusip),]

data$icc_100 = data$icc *100
summary(data$icc_100)
sd(data$icc_100)
library(plyr)
icc.ave = ddply(data, .(date), summarize, mean=mean(icc_100), 
                q1 = quantile(icc_100, 0.1), q9 = quantile(icc_100, 0.9))



head(icc.ave)
library(lubridate)
p1 = ggplot(data = icc.ave,aes(x = date))+
  theme_bw()+
  geom_line(aes(y = mean, linetype = "Mean",col = "Mean"), size = 1.1)+
  geom_line(aes(y = q1, linetype = "10th percentile", col = "10th percentile"),size = 1.1)+
  geom_line(aes(y = q9, linetype = "90th percentile" ,col = "90th percentile"),size = 1.1)+
  labs(y = "Implied cost of equity (percentage point)", x = element_blank())+
  theme(legend.position = c(0.86, 0.88),legend.title =  element_blank(),
        legend.text = element_text( size = 11),legend.key.width = unit(1.5, "cm"))+
  scale_linetype_manual(name = c("Mean","10th percentile","90th percentile"),
                        values = c("Mean"="solid", "10th percentile" = "dotted", "90th percentile"="dashed"))+
  scale_color_manual(name = c("Mean","10th percentile","90th percentile"),
                     values = c("Mean"="black", "10th percentile" = "grey20", "90th percentile"="grey50"))+
  scale_x_date(date_breaks = "3 year",date_labels = "%Y")+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())
print(p1)

head(data)


test.jpm = data[data$cusip=="46625H100",]
p2.jpm = ggplot(data =test.jpm[test.jpm$icc<0.3,], aes(x = date, y = icc) ) +
  theme_bw()+
  geom_line(size = 1.1)+
  labs(y = "ICC (cost of equity)", x = element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank())+
  scale_x_date(date_breaks = "3 year",date_labels = "%Y")
print(p2.jpm)
  
saveRDS(data, "icc_final.rds")
##########################################################################





