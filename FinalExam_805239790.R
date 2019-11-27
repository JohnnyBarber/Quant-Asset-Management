library(dplyr)
library(RPostgres)
library(matrixStats)
library(bindrcpp)
library(moments)
library(ggplot2)
library(data.table)
library(lubridate)
library(readxl)
library(Hmisc)
library(xts)
library(zoo)
library(moments)

setwd("C:/Users/Jiaqi Li/Desktop/class materials/quarter 3/Quantitative Asset Management/final")

LinkTable = read.csv("linktable.csv") %>% as.data.table()

#---------------------------------table 1--------------------------------------------#
qfactor = read_excel("HXZ q-Factors (monthly 1967 to 2018).xlsx") %>% as.data.table()
FF3 = read.csv("F-F_Research_Data_Factors.csv") %>% as.data.table()
UMD = read.csv("F-F_Momentum_Factor.csv") %>% as.data.table()

qfactor = qfactor[,`:=`(RF = RF/100, `MKT-RF`=`MKT-RF`/100)]
qfactor = qfactor[Year>=1972 & Year<=2018,]
FF3$X=FF3$X*100+1
FF3$date<-as.Date(as.character(FF3$X),format = "%Y%m%d")
FF3$year<-year(FF3$date)
FF3$month<-month(FF3$date)
FF3<-FF3[,c(6,7,8,2,3,4,5)]
FF3 = FF3[year>=1972 & year<=2018,]
UMD$X=UMD$X*100+1
UMD$date<-as.Date(as.character(UMD$X),format = "%Y%m%d")
UMD$year<-year(UMD$date)
UMD$month<-month(UMD$date)
UMD<-UMD[,c(3,4,5,2)]
UMD = UMD[year>=1972 & year<=2018,]

together = merge(qfactor,FF3,by.x = c("Year","Month"), by.y = c("year","month"))
together = merge(together,UMD,by.x=c("date","Year","Month"),by.y = c("date","year","month"))
names(together)[13] = "UMD"

factor = c("ME","IA","ROE")
for(i in factor){
  assign(paste0("out1_",i),summary(lm(get(i)~Mkt.RF,data=together)))
  assign(paste0("E_",i),mean(together[,get(i)]))
  assign(paste0("t_",i),get(paste0("E_",i))/sd(together[,get(i)])*sqrt(length(together[,get(i)])))
  assign(paste0("out2_",i),summary(lm(get(i)~Mkt.RF+SMB+HML,data=together)))
  assign(paste0("out3_",i),summary(lm(get(i)~Mkt.RF+SMB+HML+UMD,data=together)))
}

N = length(together$date)
round(mean(together$ME),2)
round(mean(together$ME)/sd(together$ME)*sqrt(N),2)
round(mean(together$IA),2)
round(mean(together$IA)/sd(together$IA)*sqrt(N),2)
round(mean(together$ROE),2)
round(mean(together$ROE)/sd(together$ROE)*sqrt(N),2)

round(out1_ME$coef,2)
round(out2_ME$coef,2)
round(out3_ME$coef,2)
round(out1_IA$coef,2)
round(out2_IA$coef,2)
round(out3_IA$coef,2)
round(out1_ROE$coef,2)
round(out2_ROE$coef,2)
round(out3_ROE$coef,2)

round(out1_ME$r.squared,2)
round(out2_ME$r.squared,2)
round(out3_ME$r.squared,2)
round(out1_IA$r.squared,2)
round(out2_IA$r.squared,2)
round(out3_IA$r.squared,2)
round(out1_ROE$r.squared,2)
round(out2_ROE$r.squared,2)
round(out3_ROE$r.squared,2)


table2_data = as.data.frame(together)
table2_data = table2_data[,c(6,7,8,5,10,11,13)] %>% as.data.table()
table1B = cor(table2_data) %>% as.data.frame()
table1B$ME =NULL
table1B = round(table1B,2)
table1B[lower.tri(table1B, diag = FALSE)] = NA
correlation = round(rcorr(as.matrix(table2_data))$P,2)
table1B

#---------------------------------replicate--------------------------------------------#
CRSP = read.csv("CRSP.csv") %>% as.data.table()
# convert date to date datatype
CRSP[, date:= ymd(date)]

setorder(CRSP, PERMCO)

# Filter by sharecode 10 and 11  and Filter by EXCHCD 1 2 3
CRSP <- CRSP[SHRCD %in% c(10,11)]
CRSP <- CRSP[EXCHCD %in% c(1,2,3)]

# Filter out missing ret and dlret data
for(i in c('RET','DLRET')){
  CRSP[,paste0(i) := as.character(get(i))]
  CRSP[get(i) %in% c('', ' ','A','C','P','S','T','B','C'), paste0(i) := NA]
  CRSP[, paste0(i) := as.numeric(get(i))]
  CRSP[get(i) %in% c(-66,-77,-88,-99), paste0(i) := NA]
}

# convert the ret and delisting return into numeric data type for easy calculation
CRSP[, PRC := abs(as.numeric(as.character(PRC)))]
CRSP[, SHROUT := as.numeric(as.character(SHROUT))]

# calculates the cum-Dividend returns
CRSP[, `:=`(Ret, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]

# Market Cap and find the MktCap aggregate by PERMCO, which is the same firm
CRSP[, Mkt_cap := abs(PRC) * SHROUT/1000]
setorder(CRSP, PERMNO, date)

# set the year and month as integer
CRSP[, Year:= year(date)]
CRSP[, Month:= month(date)]
# write.csv(CRSP,"cleaned_CRSP.csv")
# CRSP = read.csv("cleaned_CRSP.csv") %>% as.data.table()

CRSP[, ME := sum(Mkt_cap,na.rm = T), .(PERMCO,Year,Month)]
CRSP[, laggedME := shift(Mkt_cap), by = PERMCO]
CRSP[, weight_company := laggedME / sum(laggedME, na.rm = T), by = list(PERMCO,date)]
CRSP[, CRET := sum(weight_company * Ret, na.rm = T), by = list(PERMCO,date)]
CRSP[,SICCD := as.numeric(as.character(SICCD))]
CRSP = CRSP[SICCD > 6199 | SICCD < 6190]
CRSP[Month>7,fyear := Year-1]
CRSP[Month<7,fyear := Year-2]

#---------------------------------clean compustat--------------------------------------------#
annual<- as.data.table(read.csv("CompustatAnnual.csv"))
annual[,datadate := as.Date(as.character(datadate),format="%Y%m%d")]
quarter<- as.data.table(read.csv("CompustatQuarter.csv"))
quarter[,rdq := as.Date(as.character(rdq),format="%Y%m%d")]
quarter[,SHE:= coalesce(seqq, ceqq + pstkq, atq - ltq)]
quarter[,DT:= coalesce(txditcq)]
quarter[,PS:= coalesce(pstkrq, pstkq)]
quarter[,c("MinusPS"):=-PS]
quarter[,BE:=rowSums(.SD,na.rm = T),.SDcols=c("SHE","DT","MinusPS")]
quarter = quarter[BE>=0 & fyearq>=1972 & fyearq<=2018,]

annual[, lag_at := shift(at), by = gvkey]
annual[, IA := (at-lag_at)/lag_at]
quarter[, lag_BE := shift(BE), by = gvkey]
quarter[, ROE := ibq/lag_BE]

quarter[,month := as.numeric(format(rdq,"%m"))]
quarter[,year := as.numeric(format(rdq,"%Y"))]
annual[,month := as.numeric(format(datadate,"%m"))]
annual[,year := as.numeric(format(datadate,"%Y"))]

setkey(annual,gvkey,datadate)
setkey(quarter,gvkey,year)

#---------------------------------CRSP compustat merge--------------------------------------------#
CRSP_ = merge(CRSP,LinkTable,by.x = "PERMCO",by.y = "lpermco",all.x = T,allow.cartesian = T)
CRSP_[,linkdt := ymd(linkdt)]
CRSP_[linkenddt == 'E', linkenddt := NA]
CRSP_[,linkenddt := ymd(linkenddt)]
CRSP_ <- CRSP_[(is.na(linkdt) | date >= linkdt) & (is.na(linkenddt) | date <= linkenddt)]
setorder(CRSP_, gvkey, date)

### First, if LC not LC linktype, only keep LC
# identify Same PERMCO but different PERMNO
CRSP_[, prob := .N > 1, by = .(PERMCO, date)]
CRSP_[, Good_match := sum(linktype == 'LC'), by =.(PERMCO, date)]
CRSP_ <- CRSP_[!(prob == T & Good_match == T & linktype != 'LC')]

### Second, if P and not P linkprim, only keep p
CRSP_[, prob := .N > 1, by= .(PERMCO, date)]
CRSP_[, Good_match := sum(linkprim == 'P'), by =.(PERMCO, date)]
CRSP_ <- CRSP_[!(prob == T & Good_match == T & linkprim != 'P')]

### Third, if 1 and not liid, only keep 1 
CRSP_[, prob := .N > 1, by = .(PERMCO, date)]
CRSP_[, Good_match := sum(liid == 1), by =.(PERMCO,date)]
CRSP_ <- CRSP_[!(prob == T & Good_match == T & liid != 1)]

### Fourth, use the link that's current
CRSP_[, prob := .N > 1, by = .(PERMCO, date)]
CRSP_[, Good_match := sum(is.na(linkenddt)), by = .(PERMCO, date)]
CRSP_ <- CRSP_[!(prob==T & Good_match == T & !is.na(linkenddt))]

### Fifth, use the link that's been around the longest
CRSP_[, prob := .N > 1, by = .(PERMCO, date)]
CRSP_[, Good_match := NULL]
CRSP_[is.na(linkenddt), linkenddt := as.Date('2019-12-31', '%Y-%m-%d')]
CRSP_[, Date_diff := as.integer(linkenddt - linkdt)]
setorder(CRSP_, PERMCO, date, Date_diff)
CRSP_[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
CRSP_ <- CRSP_[!(prob==T & Good_match != T)]

### Sixth, use the gvkey that has been around the longest
CRSP_[, prob := .N > 1, by = .(PERMCO, date)]
CRSP_[, Good_match :=NULL]
setorder(CRSP_, gvkey, linkdt)
CRSP_[prob == T, start_Date := linkdt[1], by = .(gvkey)]
setorder(CRSP_, gvkey, linkenddt)
CRSP_[prob == T, end_Date := linkenddt[.N], by = .(gvkey)]
CRSP_[, Date_diff := as.integer(end_Date - start_Date)]
setorder(CRSP_, PERMCO, date, Date_diff)
CRSP_[prob == T, Good_match := Date_diff == Date_diff[.N], by = .(PERMCO, date)]
CRSP_ <- CRSP_[!(prob == T & Good_match != T)]

### Seventh, use the smaller gvkey
setorder(CRSP_, PERMCO, date, gvkey)
CRSP_ <- unique(CRSP_, by = c('PERMCO', 'date'))

### Clean up extra variables and final check of match
CRSP_ <- CRSP_[, .(gvkey, date, EXCHCD, laggedME, PERMCO, PERMNO, CRET, fyear, Year, Month)]


#function----------------------------
PS1_Q1 = function(RAW){
  data = RAW
  data = data[complete.cases(data$PRC),]
  
  #create cum_dividend total return
  setkey(data)
  data[is.na(DLRET), cum_divid_return := RET]
  data[is.na(RET), cum_divid_return := DLRET]
  data[!is.na(DLRET)&&!is.na(RET), cum_divid_return := (1+DLRET)*(1+RET)-1]
  data = data[complete.cases(data$cum_divid_return),]
  
  #market cap (size)
  data[,MktCap := abs(PRC)*SHROUT]
  data = data[complete.cases(data$MktCap),]
  
  #compute Lagged MV, Ew Return, Vw Return
  setorder(data, PERMNO, date)
  data = data[,lag_MV := shift(MktCap),by = PERMNO]
  data = data[complete.cases(data$lag_MV),]
  
  data = data[,TotalCap := sum(lag_MV), by = date]
  data[, ValueWR := cum_divid_return*lag_MV/TotalCap]
  
  data[, NoPERMNO := length(unique(PERMNO)), by = date]
  data[, EqualWR := cum_divid_return/NoPERMNO]
  
  test = data[,Stock_Vw_Ret := sum(ValueWR),by = date]
  test = data[,Stock_Ew_Ret := sum(EqualWR),by = date]
  test = data[,total_lag_MV := sum(lag_MV), by = date]
  
  Output = test[,list(Year = head(format(date, "%Y"),1),
                      Month = head(format(date, "%m"),1),
                      Stock_Vw_Ret = head(Stock_Vw_Ret,1)), by = date]
  Output[,Year := as.numeric(Year)]
  Output[,Month := as.numeric(Month)]
  Output = Output[,-1]
  setorder(Output,Year,Month)
  return(Output)
}

#market-------------------------
Monthly_CRSP_Stocks = PS1_Q1(CRSP)
CRSP_rf = merge(Monthly_CRSP_Stocks,FF3,by.x = c("Year","Month"),by.y = c("year","month"))
Mkt.Rf_rep = CRSP_rf[,list(Year,Month,Mkt.Rf_rep = Stock_Vw_Ret - RF/100)]


#---------------------------------------seperate--------------------------------------------#
test = quarter
setorder(test,fyearq)

CRSP_quarter <- CRSP_ %>%
  left_join(test,by =c("gvkey"="gvkey","Year"="year","Month"="month"))%>%
  select(gvkey, date, Year, fyear, Month, PERMCO, EXCHCD, CRET, laggedME, SHE,DT,PS,BE,ROE) %>% as.data.table()

CRSP_quarter<-unique(CRSP_quarter,by=c("PERMCO","date"))
CRSP_quarter[,ROE_lagged:=coalesce(shift(ROE),shift(ROE,2),shift(ROE,3),
                                   shift(ROE,4),shift(ROE,5),shift(ROE,6)),.(gvkey)]

# CRSP_annual = CRSP_ %>%
#   left_join(annual,by =c("gvkey"="gvkey","fyear"="year"))%>%
#   select(gvkey, date, Year, fyear, Month, PERMCO, EXCHCD, IA) %>% as.data.table()


CRSP_compustat = merge(CRSP_quarter,annual,by.x = c("gvkey","Year"), by.y = c("gvkey","year"),all.x = T)
CRSP_compustat = CRSP_compustat[Year>=1972 & Year<=2018]

#---------------------------------------sorting--------------------------------------------#
size_IA = CRSP_compustat[Month == 7,]
size_IA[,size_decile := as.numeric(cut(laggedME,
                                    breaks = c(-Inf,unique(quantile(.SD[EXCHCD == 1,laggedME],
                                                      probs = 0.5,type=7,na.rm = T)),Inf),
                                    include.lowest = T)),.(Year)]
size_IA[,IA_decile := as.numeric(cut(IA,
                                     breaks = c(-Inf,unique(quantile(.SD[EXCHCD == 1,IA],
                                                       probs = c(0.3,0.7),type=7,na.rm = T)),Inf),
                                     include.lowest = T)),.(Year)]
CRSP_compustat = merge(CRSP_compustat,size_IA[,.(PERMCO,Year,size_decile,IA_decile)],by = c("PERMCO","Year"),all.x = T,allow.cartesian = T)
CRSP_compustat[, `:=`(size_decile = shift(size_decile,6),
                      IA_decile = shift(IA_decile,6)),by=PERMCO]

CRSP_compustat[, ROE_decile := as.numeric(cut(ROE_lagged,
                                               breaks = c(-Inf,unique(quantile(.SD[EXCHCD == 1,ROE_lagged],
                                               probs = c(0.3,0.7),type=7,na.rm = T)),Inf),
                                               include.lowest = T)),.(Year,Month)]

# Portfolio returns
portfolio_return = CRSP_compustat
portfolio_return[,portfolio_return := sum(laggedME*CRET,na.rm = T)/sum(laggedME,na.rm=T),
                 by = .(Year,Month,size_decile,IA_decile,ROE_decile)]

portfolio_return = unique(portfolio_return[,.(Year, Month, size_decile,IA_decile,ROE_decile, portfolio_return)])
portfolio_return = portfolio_return[!is.na(size_decile) & !is.na(IA_decile) & !is.na(ROE_decile) & Year >= 1972,]
setkey(portfolio_return,Year,Month,size_decile,IA_decile,ROE_decile)

# Calculate Factors
portfolio_return[,ME := mean(portfolio_return[which(size_decile == 1)])-mean(portfolio_return[which(size_decile == 2)]),
                 by = .(Year,Month)]

portfolio_return[,IA := mean(portfolio_return[which(IA_decile == 1)])-mean(portfolio_return[which(IA_decile == 3)]),
                 by = .(Year,Month)]

portfolio_return[,ROE := mean(portfolio_return[which(ROE_decile == 3)])-mean(portfolio_return[which(ROE_decile == 1)]),
                 by = .(Year,Month)]

factors = unique(portfolio_return[,.(Year,Month,ME,IA,ROE)])

TEST = merge(factors,qfactor,by=c("Year","Month")) %>% na.omit()
TEST = merge(TEST,Mkt.Rf_rep,by=c("Year","Month"))

m1=mean(TEST$Mkt.Rf_rep,na.rm=T)*100*12
m2=mean(factors$ME,na.rm=T)*100*12
m3=mean(factors$IA,na.rm=T)*100*12
m4=mean(factors$ROE,na.rm=T)*100*12

std1=sd(TEST$Mkt.Rf_rep,na.rm=T)*100*sqrt(12)
std2=sd(factors$ME,na.rm=T)*100*sqrt(12)
std3=sd(factors$IA,na.rm=T)*100*sqrt(12)
std4=sd(factors$ROE,na.rm=T)*100*sqrt(12)

SR1 = m1/std1
SR2 = m2/std2
SR3 = m3/std3
SR4 = m4/std4

skew1 = skewness(TEST$Mkt.Rf_rep)
skew2 = skewness(factors$ME)
skew3 = skewness(factors$IA)
skew4 = skewness(factors$ROE)

kurt1 = kurtosis(TEST$Mkt.Rf_rep)-3
kurt2 = kurtosis(factors$ME)-3
kurt3 = kurtosis(factors$IA)-3
kurt4 = kurtosis(factors$ROE)-3

#-------------------------------------summary--------------------------------------#
diff1 = summary(abs(TEST$Mkt.Rf_rep-TEST$`MKT-RF`))*10000
diff2 = summary(abs(TEST$ME.x-TEST$ME.y))*10000
diff3 = summary(abs(TEST$IA.x-TEST$IA.y))*10000
diff4 = summary(abs(TEST$ROE.x-TEST$ROE.y))*10000

sigma1 = sd(TEST$Mkt.Rf_rep-TEST$`MKT-RF`)*10000
sigma2 = sd(TEST$ME.x-TEST$ME.y)*10000
sigma3 = sd(TEST$IA.x-TEST$IA.y)*10000
sigma4 = sd(TEST$ROE.x-TEST$ROE.y)*10000

cor(TEST$`MKT-RF`,TEST$Mkt.Rf_rep)
cor(TEST$ME.x,TEST$ME.y)
cor(TEST$IA.x,TEST$IA.y)
cor(TEST$ROE.x,TEST$ROE.y)

#-------------------------------------plot--------------------------------------#
plot(TEST$`MKT-RF`,type="l",main="Mkt.RF (original)")
plot(TEST$Mkt.Rf_rep, type = "l",main="Mkt.RF (replicate)")

plot(TEST$ME.x,type="l",main="ME (original)")
plot(TEST$ME.y, type = "l",main="ME (replicate)")

plot(TEST$IA.x,type="l",main="IA (original)")
plot(TEST$IA.y, type = "l",main="IA (replicate)")

plot(TEST$ROE.x,type="l",main="ROE (original)")
plot(TEST$ROE.y, type = "l",main="ROE (replicate)")

plot(abs(TEST$Mkt.Rf_rep-TEST$`MKT-RF`),type="l",main="Difference MKT-RF")
plot(abs(TEST$ME.x-TEST$ME.y),type="l",main="Difference ME")
plot(abs(TEST$IA.x-TEST$IA.y),type="l",main="Difference IA")
plot(abs(TEST$ROE.x-TEST$ROE.y),type="l",main="Difference ROE")

#--------------------------------------replicate-----------------------------------#
qfactors_rep = merge(Mkt.Rf_rep,factors,by=c("Year","Month"))

together = merge(qfactors_rep,FF3,by.x = c("Year","Month"), by.y = c("year","month"))
together = merge(together,UMD,by.x=c("Year","Month"),by.y = c("year","month"))
names(together)[13] = "UMD"

factor = c("ME","IA","ROE")
for(i in factor){
  assign(paste0("out1_",i),summary(lm(get(i)~Mkt.RF,data=together)))
  assign(paste0("E_",i),mean(together[,get(i)],na.rm = T))
  assign(paste0("t_",i),get(paste0("E_",i))/sd(together[,get(i)])*sqrt(length(together[,get(i)])))
  assign(paste0("out2_",i),summary(lm(get(i)~Mkt.RF+SMB+HML,data=together)))
  assign(paste0("out3_",i),summary(lm(get(i)~Mkt.RF+SMB+HML+UMD,data=together)))
}

N = length(together$date)
round(mean(together$ME),2)
round(mean(together$ME)/sd(together$ME)*sqrt(N),2)
round(mean(together$IA),2)
round(mean(together$IA)/sd(together$IA)*sqrt(N),2)
round(mean(together$ROE),2)
round(mean(together$ROE)/sd(together$ROE)*sqrt(N),2)

round(out1_ME$coef,2)
round(out2_ME$coef,2)
round(out3_ME$coef,2)
round(out1_IA$coef,2)
round(out2_IA$coef,2)
round(out3_IA$coef,2)
round(out1_ROE$coef,2)
round(out2_ROE$coef,2)
round(out3_ROE$coef,2)

out1_ME$r.squared
out2_ME$r.squared
out3_ME$r.squared
out1_IA$r.squared
out2_IA$r.squared
out3_IA$r.squared
out1_ROE$r.squared
out2_ROE$r.squared
out3_ROE$r.squared


table2_data = as.data.frame(together)
table2_data = table2_data[,c(4,5,6,3,9,10,13)] %>% as.data.table()
table1B = cor(table2_data) %>% as.data.frame()
table1B$ME =NULL
table1B = round(table1B,2)
table1B[lower.tri(table1B, diag = FALSE)] = NA
correlation = round(rcorr(as.matrix(table2_data))$P,2)
table1B
