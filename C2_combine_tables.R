rm(list = ls())

data = readRDS("C1_combine_crsp_comp.rds")
####################################################################################
#############################################################
data = data[!is.na(data$tier1.ratio),]
data = data[data$month >= 199303,]
data = data[order(data$month, data$tier1.ratio),] # sort based on !!!!!!!!!!!!!!!!!!!
##################################################################
# # since we want to sort the data in each month
unique.month = rle(data$month)$lengths
# find the location of each group start
unique.loc = rep(1, length(unique.month)+1)

for (i in 1:length(unique.month)){
  unique.loc[i+1] = sum(unique.month[1:i])+1
}

# divide data into three groups based on the capitalization

k = 10

data$group.no = 0
for (i in 1:length(unique.month)){
  month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
  res = round(seq(1, unique.month[i], by = unique.month[i]/k)) # find out the 1:10th, 11st : 20th ...
  res = c(res, unique.month[i])
  for (j in 1:k){#give value to the group.no
    month_chosen$group.no[res[j]:(res[j+1])] = j
  }
  data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
}

table.equity.ratio = matrix(0, nrow = 10, ncol = 10)
colnames(table.equity.ratio) = c("portfolio","Cap","equity.ratio","equity.ratio.vw",
                                 "Ret","Ret.vw","b.beta","b.beta.vw","f.beta","f.beta.vw")
table.equity.ratio = as.data.frame(table.equity.ratio)

for (i in 1:k){
  data.chosen = data[data$group.no==i,]
  table.equity.ratio$portfolio[i] = i
  table.equity.ratio$Cap[i] = mean(data.chosen$cap)
  table.equity.ratio$equity.ratio[i] = mean(data.chosen$tier1.ratio/100)
  table.equity.ratio$equity.ratio.vw[i] = weighted.mean(data.chosen$tier1.ratio/100, data.chosen$cap)
  table.equity.ratio$Ret[i] = mean(data.chosen$ret)
  table.equity.ratio$Ret.vw[i] = weighted.mean(data.chosen$ret, data.chosen$cap)
  table.equity.ratio$b.beta[i] = mean(data.chosen$b.beta)
  table.equity.ratio$b.beta.vw[i] =  weighted.mean(data.chosen$b.beta, data.chosen$cap)
  table.equity.ratio$f.beta[i] = mean(na.omit(data.chosen$f.beta))
  table.equity.ratio$f.beta.vw[i] =  weighted.mean(na.omit(data.chosen$f.beta), data.chosen$cap[!is.na(data.chosen$f.beta)])
}

library(kableExtra)

kable(round(table.equity.ratio,3), "latex", caption = "Sorted by equity ratio", booktabs = T)%>%
  kable_styling(font_size = 11, latex_options = c("hold_position"))

# ############################################################################
# # sort based on cap
# data = data[order(data$month, data$cap),] # sort based on !!!!!!!!!!!!!!!!!!!
# ##################################################################
# # # since we want to sort the data in each month
# unique.month = rle(data$month)$lengths
# # find the location of each group start
# unique.loc = rep(1, length(unique.month)+1)
# 
# for (i in 1:length(unique.month)){
#   unique.loc[i+1] = sum(unique.month[1:i])+1
# }
# 
# # divide data into three groups based on the capitalization
# 
# k = 10
# 
# data$group.no = 0
# for (i in 1:length(unique.month)){
#   month_chosen = data[unique.loc[i]:(unique.loc[i+1]-1),]# pick out the group, easier to deal with data
#   res = round(seq(1, unique.month[i], by = unique.month[i]/k)) # find out the 1:10th, 11st : 20th ...
#   res = c(res, unique.month[i])
#   for (j in 1:k){#give value to the group.no
#     month_chosen$group.no[res[j]:(res[j+1])] = j
#   }
#   data$group.no[unique.loc[i]:(unique.loc[i+1]-1)] = month_chosen$group.no
# }
# 
# table.cap = matrix(0, nrow = 10, ncol = 6)
# colnames(table.cap) = c("portfolio","cap","equity.ratio","Ret","b.beta","f.beta")
# table.cap = as.data.frame(table.cap)
# 
# for (i in 1:k){
#   data.chosen = data[data$group.no==i,]
#   table.cap$portfolio[i] = i
#   table.cap$equity.ratio[i] = mean(data.chosen$equity.ratio)
#   table.cap$cap[i] = mean(data.chosen$cap)
#   table.cap$Ret[i] = mean(data.chosen$ret)
#   table.cap$b.beta[i] = mean(data.chosen$b.beta)
#   table.cap$f.beta[i] = mean(na.omit(data.chosen$f.beta))
# }
# 
# 
# kable(round(table.cap,3), "latex", caption = "Sorted by cap", booktabs = T)%>%
#   kable_styling(font_size = 11, latex_options = c("hold_position"))








