
rm(list = ls())
library(stargazer)
data = readRDS("C1_combine_crsp_comp.rds")

head(data)
summary(data)
#######################################################################
# longer horizion---------------------------------------------------

# clean data
# remove data without tier 1 ratio
data.ex_tier = data[!is.na(data$tier1.ratio),]
# remove data before 1996
data.ex_tier = data.ex_tier[data.ex_tier$month >= 199603,]
# remove extreme values
data.ex_tier = data.ex_tier[data.ex_tier$inverse.tier1 > 0,]
data.ex_tier = data.ex_tier[data.ex_tier$inverse.tier1 < 20,]


summary(data.ex_tier$month)
# min is 199603, max is 201912

#------------perform models-------------------
model.b_inverse_tier = lm(data = data.ex_tier, b.beta ~ inverse.tier1 + 0)
model.f_inverse_tier = lm(data = data.ex_tier, f.beta ~ inverse.tier1 + 0)
summary(model.b_inverse_tier)
summary(model.f_inverse_tier)

stargazer(model.f_inverse_tier,model.b_inverse_tier)




########## test BW ########################################
# use the capital structure data from the Compustat database, not Bank regulatory
# in bw, the data starts from 199309, ends at 201102
data.bw = data.ex_tier[data.ex_tier$month < 201103,]
summary(data.bw$month)

#----------- perform models-----------

model.bw.f = lm(data = data.bw, f.beta ~ inverse.tier1 + 0)
model.bw.b = lm(data = data.bw, b.beta ~ inverse.tier1 + 0)

summary(model.bw.f)
summary(model.bw.b)

# read the reproduced version from bank regulatoty database
model.bw.f_rep = readRDS("reproducing_bw_f.rds")
model.bw.b_rep = readRDS("reproducing_bw_b.rds")

stargazer(model.bw.b_rep, model.bw.b, model.b_inverse_tier)
stargazer(model.bw.f_rep, model.bw.f, model.f_inverse_tier)


############################################################




#############################################################
# piecewise linear regression
# ------------- sort data on the 1/e -------------------
data.ex_tier = data.ex_tier[order(data.ex_tier$month, data.ex_tier$inverse.tier1),] 
##################################################################
# # since we want to sort the data in each month
unique.month = rle(data.ex_tier$month)$lengths
# find the location of each group start
unique.loc = rep(1, length(unique.month)+1)

for (i in 1:length(unique.month)){
  unique.loc[i+1] = sum(unique.month[1:i])+1
}


# divide data into k groups based on the capitalization

k = 10

data.ex_tier$group.no = 0
for (i in 1:length(unique.month)){
  month_chosen = data.ex_tier[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
  res = round(seq(1, unique.month[i], by = unique.month[i]/k)) # find out the 1:10th, 11st : 20th ...
  res = c(res, unique.month[i])
  for (j in 1:k){#give value to the group.no
    month_chosen$group.no[res[j]:(res[j+1])] = j
  }
  data.ex_tier$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
}

#-------- run the linear models-----

table.slope = matrix(nrow = 10, ncol = 2)
colnames(table.slope) = c("group", "slope")
table.slope = as.data.frame(table.slope)
table.slope$group = 1:k

leverage = rep(0,10)
# data.ex_tier$tier1.ratio
for (i in 1:k){
  data.tem = data.ex_tier[data.ex_tier$group.no == i,]
  model.tem = lm (data = data.tem, f.beta ~ 0 + inverse.tier1)
  model.tem.b = lm (data = data.tem, b.beta ~ 0 + inverse.tier1)
  print(mean(data.tem$tier1.ratio))
  leverage[i] = mean(data.tem$tier1.ratio)
  table.slope$slope[i] = round(model.tem$coefficients,3)
}

library(kableExtra)

kable(t(table.slope), "latex", caption = "Sorted by leverage", booktabs = T)%>%
  kable_styling(font_size = 11, latex_options = c("hold_position"))
leverage = round(leverage, 2)
kable(t(leverage), "latex", caption = "Sorted by leverage", booktabs = T)%>%
  kable_styling(font_size = 11, latex_options = c("hold_position"))

# ----------backward beta as y -----------------
table.slope_b = matrix(nrow = 10, ncol = 2)
colnames(table.slope_b) = c("group", "slope")
table.slope_b = as.data.frame(table.slope_b)
table.slope_b$group = 1:k

for (i in 1:k){
  data.tem = data.ex_tier[data.ex_tier$group.no == i,]
  model.tem = lm (data = data.tem, b.beta ~ 0 + inverse.tier1)
  table.slope_b$slope[i] = round(model.tem$coefficients,3)
}

kable(t(table.slope_b), "latex", caption = "Sorted by leverage", booktabs = T)%>%
  kable_styling(font_size = 11, latex_options = c("hold_position"))




## define an IV-------------------------------------------------
data$basel = 0
data$basel[data$year>=1993] = 1
library(AER)
model.re_e = ivreg(data = data, ret ~ inverse.equity.ratio|basel)
summary(model.re_e,vcov = sandwich, diagnostics = TRUE)
library(stargazer)
stargazer(model.re_e)


#########################################################################
# useless!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# data$log.cap = log(data$cap)
# model.ka1 = lm(data = data, b.beta ~ 0+inverse.tier1 + log.cap + factor(year) + factor(permno))
# summary(model.ka1)
# 
# data$tier1_100 = data$tier1.ratio / 100
# model.ka2 = lm(data = data, b.beta ~ tier1_100 + log.cap + factor(year) + factor(permno))
# summary(model.ka2)
######################################################################
############ analysis#############################################

model.b_inverse_tier = lm(data = data, b.beta ~ inverse.tier1 + 0)
model.f_inverse_tier = lm(data = data, f.beta ~ inverse.tier1 + 0)
summary(model.b_inverse_tier)
summary(model.f_inverse_tier)
summary(data$month)

model.b_inverse_tier_1 = lm(data = data[data$group.no==1,], b.beta ~ inverse.tier1 + 0)
model.f_inverse_tier_1 = lm(data = data[data$group.no==1,], f.beta ~ inverse.tier1 + 0)
model.b_inverse_tier_2 = lm(data = data[data$group.no==2,], b.beta ~ inverse.tier1 + 0)
model.f_inverse_tier_2 = lm(data = data[data$group.no==2,], f.beta ~ inverse.tier1 + 0)
model.b_inverse_tier_3 = lm(data = data[data$group.no==3,], b.beta ~ inverse.tier1 + 0)
model.f_inverse_tier_3 = lm(data = data[data$group.no==3,], f.beta ~ inverse.tier1 + 0)


# # control factors
# model.b_inverse_tier_fe = lm(data = data, b.beta ~ inverse.tier1 + factor(month) + factor(permno))
# model.f_inverse_tier_fe = lm(data = data, f.beta ~ inverse.tier1 + factor(month) + factor(permno))

# model.b_inverse_tier_fe_0 = lm(data = data, b.beta ~ inverse.tier1 + factor(month) + factor(permno)-1 +0)
# model.f_inverse_tier_fe_0 = lm(data = data, f.beta ~ inverse.tier1 + factor(month) + factor(permno) +0)
library(stargazer)

stargazer(model.b_inverse_tier, model.f_inverse_tier, model.b_inverse_tier_1,
          model.f_inverse_tier_1, model.b_inverse_tier_2, model.f_inverse_tier_2,
          model.b_inverse_tier_3,model.f_inverse_tier_3)

model.modify.b = lm(data = data[data$group.no!=1,], b.beta ~ inverse.tier1 + 0)
model.modify.f= lm(data = data[data$group.no!=1,], f.beta ~ inverse.tier1 + 0)

stargazer(model.b_inverse_tier, model.b_inverse_tier_1,
          model.b_inverse_tier_2, model.b_inverse_tier_3, model.modify.b)


stargazer(model.f_inverse_tier, model.f_inverse_tier_1,
          model.f_inverse_tier_2, model.f_inverse_tier_3,model.modify.f)

# summary(lm(data = data, b.beta ~ inverse.tier1 + factor(month) +0))

# summary(lm(data = data, b.beta ~ inverse.equity.ratio + 0)) # small
# this regression has problem, not good

# mean(data$b.beta)*mean(na.omit(data$equity.ratio)) #0.058
# mean(data$b.beta[data$group.no==1])*mean(na.omit(data$equity.ratio[data$group.no==1])) #00.04
# 
# mean(data$b.beta[data$group.no==10])*mean(na.omit(data$equity.ratio[data$group.no==10])) # 0.08
# mean(data$b.beta[data$group.no==5])*mean(na.omit(data$equity.ratio[data$group.no==5]))

# 
# summary(lm(data = data[data$group.no == 3,], b.beta ~ inverse.tier1 + 0))  # 0.08
# summary(lm(data = data[data$group.no == 1,], b.beta ~ inverse.tier1 + 0))  # 0.002
## plotting!!!!!!!!!!!!!!!!!!!!!!!
#############################################################################
constant = as.data.frame(rep(0,k))
colnames(constant) = "constant"
constant$inverse.tier1 = rep(0,k)
constant$coef = rep(0, k)
constant$beta = rep(0, k)
for (i in 1:k){
  constant$constant[i] = i
  data.chosen = data[data$group.no==i,]
  data.chosen = data.chosen[!is.na(data.chosen$inverse.tier1),]
  #constant$constant[i] = mean(data.chosen$b.beta)*mean(data.chosen$equity.ratio)
  constant$inverse.tier1[i] = mean(data.chosen$inverse.tier1)
  constant$coef[i] = coef(lm(data = data.chosen, b.beta ~ inverse.tier1 + 0))
  constant$beta[i] = mean(data.chosen$b.beta)
}

constant = t(constant)

model.modify.b = lm(data = data[data$group.no!=1,], b.beta ~ inverse.tier1 + 0)
model.modify.f= lm(data = data[data$group.no!=1,], f.beta ~ inverse.tier1 + 0)
library(kableExtra)
kable(round(constant,3), "latex", caption = "xxxxx", booktabs = T)%>%
  kable_styling(font_size = 10, latex_options = c("hold_position"))

###########################################################




############################################################
library(AER)
model.iv.b = ivreg(data = data, b.beta ~ inverse.equity.ratio+0|0+basel)
model.iv.b1 = ivreg(data = data[data$group.no==1,], b.beta ~ inverse.equity.ratio+0|0+basel)
model.iv.b2 = ivreg(data = data[data$group.no==2,], b.beta ~ inverse.equity.ratio+0|0+basel)
model.iv.b3 = ivreg(data = data[data$group.no==3,], b.beta ~ inverse.equity.ratio+0|0+basel)
model.iv.b.modify = ivreg(data = data[data$group.no!=1,], b.beta ~ inverse.equity.ratio+0|0+basel)

model.iv.f = ivreg(data = data, f.beta ~ inverse.equity.ratio+0|0+basel)
model.iv.f1 = ivreg(data = data[data$group.no==1,], f.beta ~ inverse.equity.ratio+0|0+basel)

model.iv.f2 = ivreg(data = data[data$group.no==2,], f.beta ~ inverse.equity.ratio+0|0+basel)
model.iv.f3 = ivreg(data = data[data$group.no==3,], f.beta ~ inverse.equity.ratio+0|0+basel)
model.iv.f.modify = ivreg(data = data[data$group.no!=1,], f.beta ~ inverse.equity.ratio+0|0+basel)

stargazer(model.iv.b, model.iv.b1, model.iv.b2,model.iv.b3)


stargazer(model.iv.f, model.iv.f1, model.iv.f2,model.iv.f3,model.iv.f.modify)


summary(model.iv.b,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.b1,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.b2,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.b3,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.b.modify,vcov = sandwich, diagnostics = TRUE)
##########
summary(model.iv.f,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.f1,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.f2,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.f3,vcov = sandwich, diagnostics = TRUE)
summary(model.iv.f.modify,vcov = sandwich, diagnostics = TRUE)





# 
# 
# fit.b = coeftest(model.iv.b, function(x) vcovHC(x, type="HC0"))
# fit.b1 =coeftest(model.iv.b1, function(x) vcovHC(x, type="HC0"))
# fit.b2 =coeftest(model.iv.b2, function(x) vcovHC(x, type="HC0"))
# fit.b3 =coeftest(model.iv.b3, function(x) vcovHC(x, type="HC0"))
# 
# 
# summ.fit.b <- summary(model.iv.b, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# summ.fit.b1 <- summary(model.iv.b1, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# summ.fit.b2 <- summary(model.iv.b2, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# summ.fit.b3<- summary(model.iv.b3, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# 
# 
# fit.f = coeftest(model.iv.f, function(x) vcovHC(x, type="HC0"))
# fit.f1 =coeftest(model.iv.f1, function(x) vcovHC(x, type="HC0"))
# fit.f2 =coeftest(model.iv.f2, function(x) vcovHC(x, type="HC0"))
# fit.f3 =coeftest(model.iv.f3, function(x) vcovHC(x, type="HC0"))
# 
# 
# summ.fit.f <- summary(model.iv.f, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# summ.fit.f1 <- summary(model.iv.f1, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# summ.fit.f2 <- summary(model.iv.f2, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# summ.fit.f3<- summary(model.iv.f3, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
# 


# stargazer(model.iv.b, model.iv.b1,model.iv.b2,model.iv.b3, type = "latex", 
#           se = list(fit.b[,"Std. Error"], fit.b1[,"Std. Error"],fit.b2[,"Std. Error"],fit.b3[,"Std. Error"]), 
#           add.lines = list(c(rownames(summ.fit.b$diagnostics)[1], 
#                              round(summ.fit.b$diagnostics[1, "p-value"], 2), 
#                              round(summ.fit.b1$diagnostics[1, "p-value"], 2),
#                              round(summ.fit.b2$diagnostics[1, "p-value"], 2), 
#                              round(summ.fit.b3$diagnostics[1, "p-value"], 2)), 
#                            c(rownames(summ.fit1$diagnostics)[2], 
#                              round(summ.fit.b$diagnostics[2, "p-value"], 2), 
#                              round(summ.fit.b1$diagnostics[2, "p-value"], 2),
#                              round(summ.fit.b2$diagnostics[2, "p-value"], 2), 
#                              round(summ.fit.b3$diagnostics[2, "p-value"], 2))
#           ))



# summary(ivreg(data = data, ret ~ inverse.equity.ratio|basel),
#         vcov = sandwich, diagnostics = TRUE)
# 0.00069
# summary(lm(data = data, b.beta ~ inverse.equity.ratio +0))
# 
# model.beta_inv_equity = lm(data = data, b.beta ~ inverse.equity.ratio + factor(permno) + factor(date.y) -1)
# subdata = na.omit(data)
# model.beta_inv_tier = lm(data = subdata, b.beta ~ inverse.tier1 + factor(permno) + factor(date.y) -1)
# summary(model.beta_inv_equity)
# summary(model.beta_inv_tier)
# summary(ivreg(data = data, b.beta ~ inverse.equity.ratio+0|basel),
#         vcov = sandwich, diagnostics = TRUE)


2.52e-05
# 
# 
# # iv reg vs. lm reg

# 
# 
# 
# summary(lm(data = subdata, b.beta ~ inverse.tier1 + 0))
# 
# 
# summary(ivreg(data = data, f.beta ~ inverse.equity.ratio+0|basel),
#         vcov = sandwich, diagnostics = TRUE)
# summary(lm(data = data, f.beta ~ inverse.equity.ratio+0))
# 
# summary(lm(data = subdata, f.beta ~ inverse.tier1 + 0))
# 
# 
# 
# 
# for (i in 1:k){
#   model.iv=ivreg(data = data[data$group.no==i,], b.beta ~ inverse.equity.ratio+0|basel)
#   print(coef(model.iv))
# }
# 
# #look at the linear regression
# 
# 
# for (i in 1:k){
#   model.tier = lm(data = subdata[subdata$group.no==i,], b.beta ~ inverse.tier1 + 0)
#   print(coef(model.tier))
# }
# 
# 
# # directly regress the return on capital structure
# summary(ivreg(data = data, ret ~ inverse.equity.ratio|basel),
#         vcov = sandwich, diagnostics = TRUE)
# 
# for (i in 1:k){
#   model.iv=ivreg(data = data[data$group.no==i,], ret ~ inverse.equity.ratio|basel)
#   print(coef(model.iv))
# }
# 


