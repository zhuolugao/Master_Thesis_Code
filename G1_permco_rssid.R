rm(list = ls())
library(readxl)
library(numbers)
# the excel file "inverse_tier1.xlsx" is from the database of Bank Regulatory
data <- read_excel("inverse_tier1.xlsx", 
                            col_types = c("skip", "skip", "numeric", 
                                          "numeric", "numeric"))
# test = mod(data$month,100)

# span the quarterly data into monthly data
data$year = as.integer(data$month/100)
data$s.month = mod(data$month,100)
data$s.year = data$year
data$s.year[data$s.month == 12] = data$year[data$s.month == 12] +1

# create a matrix to store 1:12
span = matrix(0:11, nrow = 3)
span[1,1] = 12
print(span)
# 
#      [,1]  [,2] [,3] [,4]
# [1,]   12    3    6    9
# [2,]    1    4    7   10
# [3,]    2    5    8   11


data.copy = data
data = rbind(data, data.copy)
data = rbind(data, data.copy)


head(data)
data = data[order(data$permco, data$month),]

data$s.month = 0

for (i in 0:(nrow(data)/3-1)){
  test = mod(data$month[3*i+1],100)
  if (test == 12){
    flag = 1 #if the initial month =12, span it into 12,1,2, first row
  } else if (test == 3) {
    flag = 2 #if the initial month =3, span it into 3,4,5, second row
  } else if (test == 6) {
    flag = 3 #if the initial month =6, span it into 6,7,8, third row
  } else if (test == 9) {
    flag = 4 #if the initial month =9, span it into 9,10,11,fourth row
  }
  data$s.month[(3*i+1):(3*i+3)] = span[,flag]
}


data$s.year[data$s.month == 12] = data$year[data$s.month == 12]
head(data)

data$month = data$s.year *100 + data$s.month
data = subset(data, select = c(month, permco, inverse.tier1))

# import beta data
data.crsp = readRDS("forward_beta.rds") # monthly data from the crsp database
head(data.crsp)
data.crsp = subset(data.crsp, select= c(permno, permco, month, date, f.beta, f.idio, b.beta))

data.final = merge(data, data.crsp, by = c("permco", "month"))
head(data.final)
###########################
library(KernSmooth)
summary(data.final$month)

data.final = data.final[data.final$inverse.tier1>0,]
data.final = data.final[data.final$inverse.tier1<20,]

data_f_full = data.final[!is.na(data.final$f.beta),]

test_f_full=locpoly(x = data_f_full$inverse.tier1, y = data_f_full$f.beta, bandwidth = 0.3, kernel=EpaK,
               gridsize = 30)
# plot
model.full_f = lm(data = data_f_full, f.beta ~ 0 +inverse.tier1)
summary(model.full_f)

library(ggplot2)
p2 = ggplot()+
  geom_line(aes(x =test_f_full$x, y = test_f_full$y))+
  geom_line(aes(x = test_f_full$x ,y = model.full_f$coefficients * test_f_full$x), linetype = "dashed")+
  
  xlim(c(1,15))+
  ylim(c(0,1.1))+
  theme_classic() +
  labs(x= "1/e", y = "Forward Equity Beta", 
       title = element_blank())


print(p2)


# clean dataset
subdata = data_f_full[data_f_full$month<201103,]

# summary the models
summary(subdata)


# reproduce BW, exactly same
model.f = lm(data = subdata, f.beta ~ 0 +inverse.tier1 )
summary(model.f)


# same time, use backward beta
model.b = lm(data = subdata, b.beta ~ 0 +inverse.tier1 )
summary(model.b)


test_f=locpoly(x = subdata$inverse.tier1, y = subdata$f.beta, bandwidth = 0.3, kernel=EpaK,
               gridsize = 30)



# make latex
library(stargazer)
stargazer(model.f,model.b)

#########################################################
library(ggplot2)
p = ggplot()+
  geom_line(aes(x =test_f$x, y = test_f$y))+
  geom_line(aes(x = test_f$x ,y = model.f$coefficients * test_f$x), linetype = "dashed")+
  
  xlim(c(0,15))+
  ylim(c(0,1.1))+
  theme_classic() +
  labs(x= "1/e", y = "Forward Equity Beta", 
       title = element_blank())
print(p)





