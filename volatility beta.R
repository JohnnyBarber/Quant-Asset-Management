setwd("C:/Users/Jiaqi Li/Desktop/class materials/quarter 3/Quantitative Asset Management/presentation")

library(ggplot2)
library(data.table)
library(dplyr)
library(zoo)
library(dplyr)
library(RPostgres)
library(matrixStats)
library(bindrcpp)
library(moments)
library(ggplot2)

# #------------------------------------------VIX-------------------------------------------#
# VIX = read.csv("COBE VIX.csv") %>% as.data.table()
# VIX[,Date := as.Date(as.character(Date),format = "%d%B%Y")]
# qplot(Date,vxo/100,data=VIX,geom = "line",main = "VIX Index")+theme_bw()
# VIX[,delta_VIX := vxo-shift(vxo)]
# mean(VIX$vxo,na.rm = T)
# sd(VIX$vxo,na.rm = T)
# mean(VIX$delta_VIX,na.rm = T)
# sd(VIX$delta_VIX,na.rm = T)

# #------------------------------------------market-------------------------------------------#
# market = read.csv("market daily.csv") %>% as.data.table()
# market[,DATE := as.Date(as.character(DATE),format = "%Y%m%d")]
# market_sample = market[DATE<="2000-12-30" & DATE >= "1986-01-01"]
# market_sample[,`:=` (CumRet = cumsum(vwretd),Year = format(DATE,"%Y"), Month = format(DATE,"%m"))]
# qplot(DATE,vwretd,data = market_sample,geom = "line")+theme_bw()
# qplot(DATE,CumRet,data = market_sample,geom = "line")+theme_bw()

#------------------------------------------FF_3-------------------------------------------#
FF = read.csv("F-F_Research_Data_Factors_daily.csv") %>% as.data.table()
FF[,Date := as.Date(as.character(X),format = "%Y%m%d")]
FF[,`:=`(Mkt.RF=Mkt.RF/100, SMB=SMB/100,HML= HML/100,RF=RF/100)]
FF[,`:=` (Year = year(Date), Month = month(Date))]
FF_sample = FF[Date<= "2000-12-31" & Date >= "1986-01-01"]
# FF_sample[,CumExRet := cumsum(Mkt.RF)/100]
FF_sample$X = NULL
# qplot(Date,RF,data = FF_sample, geom = "line")+theme_bw()
# qplot(Date,Mkt.RF,data = FF_sample, geom = "line")+theme_bw()
# qplot(Date,CumExRet,data = FF_sample, geom = "line")+theme_bw()

# #Following is the same as the Mkt.RF in FF data.
# market_FF = merge(market_sample,FF_sample[,list(Year,Month,RF)],by=c("Year","Month"), all.x = T, allow.cartesian = T)
# market_FF[,RF := RF/100]
# market_FF[,ExRet := sprtrn - RF]
# market_FF[,CumExRet := cumsum(ExRet)]
# qplot(DATE,ExRet, data = market_FF, geom = "line")+theme_bw()
# qplot(DATE,CumExRet, data = market_FF, geom = "line")+theme_bw()

#------------------------------------------FF_3 monthly-------------------------------------------#
FF_monthly = read.csv("F-F_Research_Data_Factors.csv") %>% as.data.table()
FF_monthly[,`:=`(Year = as.integer(X/100),Month = X%%100)]
FF_monthly[,`:=`(Mkt.RF=Mkt.RF/100, SMB=SMB/100,HML= HML/100,RF=RF/100)]
FF_monthly_sample = FF_monthly[X<= 200012 & X >= 198601]
# FF_sample[,CumExRet := cumsum(Mkt.RF)/100]
FF_monthly_sample$X = NULL

#------------------------------------------stock daily-------------------------------------------#
stock = read.csv("stock daily_link.csv") %>% as.data.table()
stock = stock[(EXCHCD==1 | EXCHCD==2 | EXCHCD==3) & (SHRCD==10| SHRCD==11)]
stock = stock[!RET %in% c(-66.0,-77.0,-88.0,-99.0)]
stock = stock[!DLRET %in% c(-66.0,-55.0,-88.0,-99.0)]
stock[,date := as.Date(as.character(date),format = "%Y%m%d")]
stock[,`:=`(RET = as.numeric(as.character(RET)),
            DLRET = as.numeric(as.character(DLRET)),
            PRC = as.numeric(as.character(PRC)),
            SHROUT = as.numeric(as.character(SHROUT)))]
stock[is.na(DLRET) & !is.na(RET), cRET := RET]
stock[is.na(RET) & !is.na(DLRET), cRET := DLRET]
stock[!is.na(DLRET) & !is.na(RET), cRET := (1+DLRET)*(1+RET)-1]
stock[,`:=`(Year = year(date), Month = month(date))]
# stock = stock[,list(date,PERMNO,cRET)]
stock[,keep := ifelse(length(RET)>=17,1,0), .(PERMNO,Year,Month)]
# Stock = dcast(stock,date~PERMNO, value.var = "cRET") %>% as.data.frame()
# stock = stock[,list(date,PERMNO,RET)]
# stock[,keep := ifelse(length(RET)>=17,1,0), by = PERMNO]
stock = stock[keep == 1]
stock[,MktCap := abs(PRC)*SHROUT/1000]
stock[,MktCap_lag := shift(MktCap), .(PERMNO)]
stock[,weight := MktCap_lag/sum(MktCap_lag,na.rm = T),.(PERMCO,date)]
stock[,cRET_CO := sum(weight*cRET,na.rm = T),.(PERMCO,date)]
Stock = stock[,list(PERMCO,date,Year,Month,cRET_CO)]

#------------------------------------------stock monthly-------------------------------------------#
stock_month = read.csv("stock monthly_link.csv") %>% as.data.table()
stock_month = stock_month[(EXCHCD==1 | EXCHCD==2 | EXCHCD==3) & (SHRCD==10| SHRCD==11)]
stock_month = stock_month[!RET %in% c(-66.0,-77.0,-88.0,-99.0)]
stock_month = stock_month[!DLRET %in% c(-66.0,-55.0,-88.0,-99.0)]
stock_month[,date := as.Date(as.character(date),format = "%Y%m%d")]
stock_month = stock_month[date>="1986-01-01" & date<="2000-12-31"]
stock_month[,`:=`(RET = as.numeric(as.character(RET)),
            DLRET = as.numeric(as.character(DLRET)),
            PRC = as.numeric(as.character(PRC)),
            SHROUT = as.numeric(as.character(SHROUT)))]
stock_month[is.na(DLRET) & !is.na(RET), cRET := RET]
stock_month[is.na(RET) & !is.na(DLRET), cRET := DLRET]
stock_month[!is.na(DLRET) & !is.na(RET), cRET := (1+DLRET)*(1+RET)-1]
# stock_month[,keep := ifelse(length(cRET)>=17,1,0), by = PERMNO]
# stock_month = stock_month[keep == 1]
stock_month[,MktCap := abs(PRC)*SHROUT/1000]
stock_month[,MktCap_lag := shift(MktCap), .(PERMNO)]
stock_month[,`:=`(Year = year(date), Month = month(date))]
stock_month[,weight := MktCap_lag/sum(MktCap_lag,na.rm = T),.(PERMCO,date)]
stock_month[,cRET_CO := sum(weight*cRET,na.rm = T),.(PERMCO,date)]

#--------------------------------------link table--------------------------------------#
wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = 'johnnyli',
                  password = 'L310427xiaoxiao',
                  sslmode = 'require',
                  dbname = 'wrds')

sql = "select gvkey, linkprim, liid, linktype, lpermno, lpermco, USEDFLAG, linkdt, linkenddt from CRSPA.CCMXPF_LINKTABLE where linktype = 'LC' or linktype = 'LU'"
res = dbSendQuery(wrds, sql)
LinkTable = data.table(dbFetch(res))
LinkTable[,gvkey := as.integer(gvkey)]
dbClearResult(res)

## Disconnect from WRDS
dbDisconnect(wrds)
rm(res, wrds, sql)

#----------------------------------------replicate--------------------------------------#
#Idiosyncratic---------------------------------------------------------------------------

# #slow mode
# FF_Stock = merge(FF,Stock,by.x = "Date",by.y = "date")
# FF_Stock[,`:=`(Year = format(Date,"%Y"),Month = format(Date,"%m"))]
# 
# get_residual = function(x){
#   out = lm(x~FF_Stock$Mkt.RF+FF_Stock$SMB+FF_Stock$HML)$residuals
#   return(out)
# }
# 
# resid = matrix(0,nrow = length(unique(stock$PERMNO)), ncol = length(unique(stock$PERMNO)))
# for(i in 1:length(unique(stock$PERMNO))){
#   resid[i,] = sd(get_residual(FF_Stock[,8+i]),na.rm = T)
# }
# sort_panel = cbind(PERMNO = unique(stock$PERMNO),residual = resid) %>% as.data.table()
# sort_panel[,decile := as.numeric(cut(residual,
#                                     breaks = quantile(residual,probs=seq(0,1,0.2),type=7),
#                                     include.lowest = T))]

#regression short cut
FF_stock = merge(Stock,FF_sample, by.x = c("date","Year","Month"), by.y = c("Date","Year","Month"))
# !is.na(cRET) & !is.na(Mkt.RF) & !is.na(SMB) & !is.na(HML)
# FF_stock = FF_stock[,list(date,Year,Month,PERMCO,cRET_CO,Mkt.RF,SMB,HML)]
FF_stock = FF_stock[complete.cases(FF_stock),]
sort_panel = FF_stock[,c(sigma = as.list(sd(lm((cRET_CO-RF)~Mkt.RF+SMB+HML)$residuals,na.rm = T))), .(PERMCO,Year,Month)]
# write.csv(sort_panel,"monthly regression residual.csv")
# sort_panel = read.csv("monthly regression residual.csv")

sort_panel = na.omit(sort_panel)
sort_panel[, lag_sigma := shift(sigma), by = PERMCO]
sort_panel = na.omit(sort_panel)
sort_panel[,decile := as.numeric(cut(lag_sigma,
                                     breaks = quantile(lag_sigma,probs=seq(0,1,0.2),type=7),
                                     include.lowest = T)),.(Year,Month)]
stock_sort = merge(stock_month,sort_panel,by = c("PERMCO","Year","Month"))
stock_sort[,firm_ME := sum(MktCap_lag,na.rm = T), by = PERMCO]
stock_sort$DLRET = NULL
stock_sort = merge(stock_sort,FF_monthly_sample[,list(Year,Month,RF)],by = c("Year","Month"))
# stock_sort = stock_sort[complete.cases(firm_ME),]
Mean_temp = stock_sort[,c(mean_temp = as.list(round(weighted.mean(cRET_CO-RF,firm_ME,na.rm = T)*100,2))),.(Year,Month,decile)]
Mean = Mean_temp[,c(Mean = as.list(mean(mean_temp,na.rm=T))),by = decile]
setorder(Mean,decile)

Std = Mean_temp[,c(`Std Dev` = as.list(round(sd(mean_temp,na.rm = T),2))),.(decile)]
setorder(Std,decile)

mkt_share = stock_sort[,c(MktCap_decile = as.list(sum(MktCap_lag,na.rm = T))),.(decile)]
mkt_share[,`%Mkt Share` := paste0(round(MktCap_decile/sum(MktCap_decile)*100,1),"%")]
mkt_share$MktCap_decile = NULL

Size = stock_sort[,c(Size = as.list(round(log(mean(firm_ME/1000,na.rm = T)),2))),.(decile)]
setorder(Size,decile)

FF_monthly_stock = merge(stock_sort,FF_monthly_sample[,list(Year,Month,Mkt.RF,SMB,HML)],by.x = c("Year","Month"), by.y = c("Year","Month"))

CAPM_a = FF_monthly_stock[,c(`CAPM alpha` = as.list(round(coef(lm((cRET_CO-RF)~Mkt.RF))[1]*100,2))),by = decile]
setorder(CAPM_a,decile)

FF_a = FF_monthly_stock[,c(`FF alpha` = as.list(round(coef(lm((cRET_CO-RF)~Mkt.RF+SMB+HML))[1]*100,2))),by = decile]
setorder(FF_a,decile)

output = merge(Mean,Std,by="decile") %>% merge(mkt_share,by="decile") %>% merge(Size,by="decile") %>%
  merge(CAPM_a,by="decile") %>% merge(FF_a,by = "decile")
output

# #volatility betas------------------------------------------------------------------------
# FF_VIX = merge(FF,VIX,by = "Date")
# All = merge(FF_VIX,Stock,by.x = "Date",by.y = "date") %>% as.data.frame()
# 
# get_beta = function(x){
#   out = lm(x~All$Mkt.RF+All$delta_VIX)
#   return(out$coefficients)
# }
# betas = matrix(0,nrow = length(unique(stock$PERMNO)),ncol = 3)
# for(i in 1:length(unique(stock$PERMNO))){
#   betas[i,] = get_beta(All[,10+i])
# }
# betas = as.data.table(betas)
# names(betas) = c("(intercept)","beta1","beta2")
# # betas = sapply(All[,c(11:length(unique(stock$PERMNO)))],get_beta) %>% as.data.table()
# betas[,decile := as.numeric(cut(beta2,
#                                 breaks = quantile(beta2,probs=seq(0,1,0.2),type=7),
#                                 include.lowest = T))]
# betas = cbind(betas,PERMNO = unique(stock$PERMNO))
# for(i in 1:5){
#   assign(paste0("decile",i),betas[decile == i,list(PERMNO)])
#   assign(paste0("stock",i),na.omit(merge(stock,get(paste0("decile",i)),by = "PERMNO")))
# }
# out = matrix(0,nrow = 5,ncol = 2)
# for(i in 1:5){
#   out[i,] = c(mean(get(paste0("stock",i))$RET)*21,sd(get(paste0("stock",i))$RET))
# }
# out*100
