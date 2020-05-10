# rm(list = ls())
# library(RPostgres)
# 
# 
# wrds <- dbConnect(Postgres(),
#                   host='wrds-pgdata.wharton.upenn.edu',
#                   port=9737,
#                   user = '$USER$', # please enter the user name and password
#                   password = '0000',
#                   dbname='wrds',
#                   sslmode='require')
# 
# res1 <- dbSendQuery(wrds, "select permno,date,ret
#                    from crsp.msf
#                    where date between '1992-12-01' and '2019-12-31'
#                    and ret IS NOT NULL
#                    and (hsiccd = 6000
#                    or hsiccd BETWEEN 6020 AND 6036
#                    or hsiccd BETWEEN 6040 AND 6062
#                    or hsiccd BETWEEN 6120 AND 6179
#                    or hsiccd BETWEEN 6190 AND 6199
#                    or hsiccd BETWEEN 6710 AND 6719)")
# crsp <- dbFetch(res1, n=-1)
# dbClearResult(res1)
# # crsp$month = month(crsp$date)
# # crsp$year = year(crsp$date)
# # crsp$s.year = 0
# # crsp$s.month = 0
# # 
# # head(crsp)
# # crsp$s.year[crsp$month==12] = crsp$year[crsp$month==12] + 1
# # crsp$s.year[crsp$month!=12] = crsp$year[crsp$month!=12]
# # 
# # ##
# # crsp$s.month[crsp$month==12] = 1
# # crsp$s.month[crsp$month!=12] = crsp$month[crsp$month!=12] + 1
# # head(crsp)
# crsp$new.date = ymd(100*(100*(year(crsp$date))+month(crsp$date))+1)
# 
# saveRDS(crsp, "CRSP_ICC.rds")
########################################################

rm(list = ls())
library(AER)
data = readRDS("icc_final.rds")

# trim data
head(data)
data = subset(data, select = -c(dps.1,eps.1, eps.2,ltg, end.date, 
                                dps, dps.raw,year, month, s.year, s.month,
                                gamma_1, A, stg, stg.altern, icc_oj, icc_peg))

jpm = data[data$isin == "US46625H1005",]

plot(jpm$date, jpm$icc, type = "l")

# 
# # define an iv variable
data$basel1 = 0
data$basel1[data$date >="1993-01-01"] = 1

# define a capital ratio
data$equity.ratio = data$common.equity / data$asset
data$inverse.equity.ratio  = 1/data$equity.ratio

# make the ratios as ratios, not percentage points
data$tier1.ratio = data$tier1.ratio/100
data$tier2.ratio = data$tier2.ratio/100
data$inverse.tier1 = 1/(data$tier1.ratio)
data$inverse.tier2 = 1/(data$tier2.ratio)
# define the deposit ratio
data$dep.ratio = data$deposit / data$asset
data = data[!is.na(data$equity.ratio ),]
data = data[data$equity.ratio  >0,]

model.re.cap = lm(data = data, icc~ equity.ratio  + factor(date) + factor(cusip) - 1)
model.re.inv_cap = lm(data = data, icc~ inverse.equity.ratio  + factor(date) + factor(cusip) - 1)
model.re.cap.iv = ivreg(data = data, icc ~ inverse.equity.ratio  |basel1)

summary(model.re.cap)
# -6.556e-02
summary(model.re.inv_cap)
# -2.404e-04
# summary(model.re.cap.iv,vcov = sandwich, diagnostics = TRUE)
#########################using tier 1 ratio, not equity.ratio ################
head(data)
sub.data = data[!is.na(data$tier1.ratio),]
head(sub.data)
sub.data = subset(sub.data, select = c(date, cusip, icc, tier2.ratio, tier1.ratio, inverse.tier1, inverse.tier2, dep.ratio, tier2.ratio))


head(sub.data)
sub.data = sub.data[!is.infinite(sub.data$inverse.tier2),]

sub.data$inverse.dep.ratio = 1/sub.data$dep.ratio
# ols regression between the icc and the tier1 ratio
model.re_tier1 = lm(data = sub.data, icc ~ tier1.ratio + factor(date) + factor(cusip) -1)
model.re_tier1_2 = lm(data = sub.data, icc ~ tier1.ratio + tier2.ratio + factor(date) + factor(cusip) -1)
# ols regression between the icc and the tier1 ratio, controlling the deposit ratio
model.re_tier1_dep = lm(data = sub.data, icc ~ tier1.ratio +tier2.ratio + dep.ratio+ factor(date) + factor(cusip) -1)
library(stargazer)
stargazer(model.re_tier1, model.re_tier1_2, model.re_tier1_dep,
          omit = c("date","cusip"),
          omit.labels = c("Time effect","Firm effect"))


model.re_inv_tier1 = lm(data = sub.data, icc/12 ~ inverse.tier1 + factor(date) + factor(cusip) -1)
model.re_inv_tier1_2 = lm(data = sub.data, icc/12 ~ inverse.tier1 + inverse.tier2 + factor(date) + factor(cusip) -1)
model.re_inv_tier1_dep = lm(data = sub.data, icc/12 ~ inverse.tier1 +inverse.tier2+dep.ratio+ factor(date) + factor(cusip) -1)

summary(model.re_inv_tier1)

stargazer(model.re_inv_tier1, model.re_inv_tier1_2, model.re_inv_tier1_dep,
          omit = c("date","cusip"),
          omit.labels = c("Time effect","Firm effect"))

2.663e-05 * 10000


###############################################################################
# useless--------------------------------------------------------------


# check jens estimation
# ols regression between the icc and the tier1 ratio
# sub.data$icc100 = sub.data$icc *100
# model.re_tier = lm(data = sub.data, icc100 ~ tier1.ratio + factor(date) + factor(cusip) -1)
# summary(model.re_tier)



# summary(model.re_tier1_2)
# summary(model.re_tier1_dep)
# 
# 
# summary(model.re_inv_tier1)
# 3.194e-04
# summary(model.re_inv_tier1_2)
# summary(model.re_inv_tier1_dep)
# 
# 3.194e-04


# fit_linear = as.vector(model.re_tier1$fitted.values)
# fit_non_linear = as.vector(model.re_inv_tier1$fitted.values)



# simulation =as.data.frame(cbind(data$tier1.ratio, fit_linear, fit_non_linear,
#                                 fit_linear.1,fit_non_linear.1,data$icc)) 
# 
# f_simulation = ggplot(data = simulation[simulation$V1 > 0.1& simulation$V1 < 0.105,],  aes(x = V1)) +
#   geom_line(aes(y = fit_linear, col = "Linear"))+
#   geom_line(aes(y = fit_non_linear, col = "Non-Linear"))+
#   geom_line(aes(y = fit_linear.1, col = "Linear_easy"))+
#   geom_line(aes(y = fit_non_linear.1, col = "Non-Linears_easy"))+
#   geom_line(aes(y = V6, col = "Real"))
# print(f_simulation)
# library(Metrics)




# crsp = readRDS("CRSP_ICC.rds")
# head(crsp)
# crsp = subset(crsp, select = -date)
# colnames(crsp) = c("permno","ret","date")
# link = readRDS("linktable.rds")
# head(link)
# colnames(link) = c("permno", "cusip")
# 
# crsp = merge(crsp, link, by = "permno")
# head(crsp)
# 
# # merge crsp and data by cusip and date
# data = merge(data, crsp, by = c("cusip","date"))
# model_icc_ret = lm(data = data, ret ~ icc )
# summary(model_icc_ret)
# 
# model_icc_ret_fe = lm(data = data, ret ~ icc + factor(cusip) -1 )
# summary(model_icc_ret_fe)
# 
# plot(data$icc, data$ret)


# #############################################################
# ####### simulation###########################################
# 
# e = seq(from = 0.04, to = 0.4, by = 0.01)
# 
# 
# library(ggplot2)
# 
# 
# nonlinear_partial = -2.404e-04*1/(e^2) * 0.1*10000
# 
# simulaion = as.data.frame(cbind(e,nonlinear_partial)) 
# 
# simulaion$linear_partial = -0.07 * 0.1*10000
# 
# print(ggplot(data = simulaion, aes(x = e))+
#         geom_line(aes(y = linear_partial, col = "linear"), size = 1.1)+
#         geom_line(aes(y = nonlinear_partial, col = "nonlinear"), size = 1.1))
# 
# simulaion$test = (e+0.1) * simulaion$nonlinear_partial+7
# simulaion$test.linear = e * -0.07 * 0.1*10000 +7
# 
# print(ggplot(data = simulaion, aes(x = e))+
#         theme_bw()+
#         geom_line(aes(y = test.linear, col = "linear"), size = 1.1)+
#         geom_line(aes(y = test, col = "nonlinear"), size = 1.1)+
#         scale_color_manual(name = c("linear","nonlinear"), values = c("black", "blue"))+
#         theme(legend.position = c(0.9, 0.12), legend.title = element_blank())+
#         scale_x_continuous(breaks = seq(0,0.4,by = 0.05))+
#         theme(panel.background = element_rect(fill = NA),
#               panel.grid.major.x = element_line(colour = "grey90"),
#               panel.grid.minor.x = element_blank(),
#               panel.grid.major.y = element_line(colour = "grey90"),
#               panel.grid.minor.y = element_blank())+
#         labs(x= "e", y = "Monthly increase in total cost of capital (bps)", 
#              title = element_blank())
#       )
# 
















