rm(list = ls())

library(readxl)
library(readr);library(xts);library(stats);
library(lubridate)
library(RPostgres)
library(dplyr);library(nlme)
library(plyr); library(ggplot2); library(knitr);library(writexl)
library(kableExtra)
library(data.table)

# import data and remove rows without valid backward values
data =readRDS("forward_beta.rds")
head(data)
data = data[!is.na(data$b.alpha),]
# import data and remove rows without valid forward values
data.f = readRDS("forward_beta.rds")
data.f = data.f[!is.na(data.f$f.alpha),]


summary(data$b.beta)

##############################################################
# make a table to store the important data--------------------

table = matrix(0, nrow = 4, ncol = 7)
colnames(table) = c("Obs","Min.", "1stQu.",  "Median",    "Mean", "3rdQu.", "Max.")
rownames(table) = c("b.beta", "f.beta", "f.alpha", "f.idio")

table["b.beta",2:7] = round(summary(data$b.beta),4)

table["f.beta",2:7] = round(summary(data.f$f.beta),4)
table["f.alpha",2:7] = round(summary(data.f$f.alpha),4)
table["f.idio",2:7] = round(summary(data.f$f.idio),4)

table = as.data.frame(table)
table$Obs = nrow(data)

# export to the latex code
kable(table, "latex", caption = "Table name", booktabs = T)%>%
  kable_styling(font_size = 10, latex_options = c("hold_position"))

###################################################################
# create a table to store the unique month and unique date
table.ts = unique(data$date)
table.ts = as.data.frame(table.ts)
head(table.ts)

colnames(table.ts) = "date"
# make it into month

table.ts$month = year(table.ts$date) *100 + month(table.ts$date)

table.ts$ew.beta = 0
table.ts$vw.beta = 0
# sort table to ensure correct
table.ts = table.ts[order(table.ts$month),]


data$cap = data$price * data$shrout
# write a loop to calculate the EW/VW mean of beta in each month
for (i in 1:nrow(table.ts)){
  # create a dummy to save the chosen month
  month_chosen = data[data$month == table.ts$month[i],]
  table.ts$ew.beta[i] = mean(month_chosen$b.beta)
  table.ts$vw.beta[i] = weighted.mean(month_chosen$b.beta, month_chosen$cap)
}


########################################################################
# plot-----------------------------------------------------------------

library(ggplot2)

figure_emean = ggplot(data = table.ts, aes(x = date)) +
  geom_line(size=1.1, aes(y = ew.beta, linetype = "EW"))+
  geom_line(size=1.1, aes(y = vw.beta, linetype = "VW"))+
  
  labs(x= element_blank(), y = "Backward Beta", 
       title = element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(size = 1, colour = "grey30"))+
  theme(legend.position = c(0.9, 0.2),
        legend.key = element_blank(),
        legend.key.width = unit(1.5, "cm"),legend.title =  element_blank())+
  theme(plot.title = element_text(size=18, hjust = 0.5))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")+
  ggsave("ew_vw_beta.eps", width = 14, height = 8, units = "cm")

print(figure_emean)

####################################################################
# make the EW, VW average of beta in portfolios figures
# sort based on cap -------------------------------------------------------

rm(list = ls())
library(psych)
data = readRDS("backward_betas.rds")

data = na.omit(data)

head(data)
# define the market cap.
data$capital = abs(data$price * data$shrout)/1000
# sort the data by month, and by backward beta
data = data[order(data$month, data$b.beta),]
# since we want to sort the data in each month
unique.month = rle(data$month)$lengths
# find the location of each group start
unique.loc = rep(1, length(unique.month)+1)

for (i in 1:length(unique.month)){
  unique.loc[i+1] = sum(unique.month[1:i])+1
}

# initiate a column to save the group number
data$group.no = 0
for (i in 1:length(unique.month)){
  month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
  th30 = round(quantile(1:unique.month[i], 0.3))
  th70 = round(quantile(1:unique.month[i], 0.7))
  res = c(1,th30, th70, unique.month[i])
  for (j in 1:3){#give value to the group.no
    month_chosen$group.no[res[j]:(res[j+1])] = j
  }
  data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
}



capportfolio_table = as.data.frame(unique(data$date))
colnames(capportfolio_table) = "date"



capportfolio_table$group1_emean = rep(0, nrow(capportfolio_table))
capportfolio_table$group1_vmean = rep(0, nrow(capportfolio_table))
capportfolio_table$group2_emean = rep(0, nrow(capportfolio_table))
capportfolio_table$group2_vmean = rep(0, nrow(capportfolio_table))
capportfolio_table$group3_emean = rep(0, nrow(capportfolio_table))
capportfolio_table$group3_vmean = rep(0, nrow(capportfolio_table))
capportfolio_table$group0_emean = rep(0, nrow(capportfolio_table))
capportfolio_table$group0_vmean = rep(0, nrow(capportfolio_table))



unique.month = rle(data$month)$lengths
# find the location of each group start
unique.loc = rep(1, length(unique.month)+1)



for (i in 1:length(unique.month)){
  unique.loc[i+1] = sum(unique.month[1:i])+1
}


# calculate the equal weighted average of beta in each month
# as well as the capital value weighted average of beta in each month



for (i in 1:nrow(capportfolio_table)){
  month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]
  beta_1 = month_chosen[month_chosen$group.no == 1,]
  beta_2 = month_chosen[month_chosen$group.no == 2,]
  beta_3 = month_chosen[month_chosen$group.no == 3,]
  
  capportfolio_table[i,"group0_emean"] = mean(month_chosen$b.beta)
  capportfolio_table[i,"group1_emean"] = mean(beta_1$b.beta)
  capportfolio_table[i,"group2_emean"] = mean(beta_2$b.beta)
  capportfolio_table[i,"group3_emean"] = mean(beta_3$b.beta)
  
  capportfolio_table[i,"group0_vmean"] = weighted.mean(month_chosen$b.beta, month_chosen$capital)
  capportfolio_table[i,"group1_vmean"] = weighted.mean(beta_1$b.beta, beta_1$capital)
  capportfolio_table[i,"group2_vmean"] = weighted.mean(beta_2$b.beta, beta_2$capital)
  capportfolio_table[i,"group3_vmean"] = weighted.mean(beta_3$b.beta, beta_3$capital)
}


# plotting


figure_emean_cap = ggplot(data = capportfolio_table, aes(x = date)) +
  geom_line(size=1.3, aes(y = group1_emean, linetype = "Bottom 30%"))+
  geom_line(size=1.3, aes(y = group2_emean, linetype = "Middle 40%"))+
  geom_line(size=1.3, aes(y = group3_emean, linetype = "Top 30%"))+
  geom_line(size=1, aes(y = group0_emean, col = "Whole"))+
  labs(x= element_blank(), y = "Beta, Equal value weighted", 
       title = element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(size = 1, colour = "grey30"))+
  theme(legend.direction = "horizontal", legend.position = "bottom",
        legend.key = element_blank(),legend.background = element_rect(fill = "white", colour = "gray30"),
        legend.key.width = unit(2, "cm"),legend.title =  element_blank())+
  theme(plot.title = element_text(size=18, hjust = 0.5))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")

print(figure_emean_cap)



figure_vmean_cap = ggplot(data = capportfolio_table, aes(x = date)) +
  geom_line(size=1.3, aes(y = group1_vmean, linetype = "Bottom 30%"))+
  geom_line(size=1.3, aes(y = group2_vmean, linetype = "Middle 40%"))+
  geom_line(size=1.3, aes(y = group3_vmean, linetype = "Top 30%"))+
  geom_line(size=1, aes(y = group0_vmean, col = "Whole"))+
  labs(x= element_blank(), y = "Beta, Capital value weighted", 
       title = element_blank())+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(size = 1, colour = "grey30"))+
  theme(legend.direction = "horizontal", legend.position = "bottom",
        legend.key = element_blank(),legend.background = element_rect(fill = "white", colour = "gray30"),
        legend.key.width = unit(2, "cm"), legend.title =  element_blank())+
  theme(plot.title = element_text(size=18, hjust = 0.5))+
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")

print(figure_vmean_cap)





