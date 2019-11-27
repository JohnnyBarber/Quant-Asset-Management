library(data.table)
library(dplyr)
library(zoo)
library(stargazer)
library(moments)
library(readxl)
library(ggplot2)

####################################################################
#                           Question 1                             #
####################################################################
CRSP_Stocks = read.csv("datacsv.csv") %>% as.data.table()
CRSP_Stocks[,date := as.Date(as.character(date),format = "%Y%m%d")]

PS3_Q1 = function(Data){
  Data = Data[Data$EXCHCD==1|Data$EXCHCD==2|Data$EXCHCD==3]
  Data = Data[Data$SHRCD==10|Data$SHRCD==11]
  Data[,RET := as.numeric(as.character(RET))]
  Data[,DLRET := as.numeric(as.character(DLRET))]
  #Data(is.na(Data$PRC))
  #Data = Data[complete.cases(Data$PRC),]
  
  Data[is.na(DLRET), cum_divid_return := RET]
  Data[is.na(RET), cum_divid_return := DLRET]
  Data[!is.na(DLRET)&&!is.na(RET), cum_divid_return := (1+DLRET)*(1+RET)-1]
  #Data = Data[complete.cases(Data$cum_divid_return),]
  
  Sample = Data[,MktCap := abs(PRC)*SHROUT]
  #Sample = Data[complete.cases(Data$MktCap),]
  Length = function(...){
    test = c(...)
    out = sum(!is.na(test))
    return(out)
  }
  
  Sample[, lag_MktCap := shift(MktCap)]
  Sample[, lag_ret := as.double(shift(cum_divid_return,2)), by = PERMNO]
  Sample[,`:=` (shift_PRC = shift(PRC,13), shift_RET = shift(RET,2))]
  Sample[,valid := ifelse(!is.na(shift_PRC) & !is.na(shift_RET) & !is.na(lag_MktCap), 1, 0), by = PERMNO]
  
  # Cretiria 2
  # Sample[, lag_ret := as.double(shift(cum_divid_return,1)), by = PERMNO]
  # Sample[, valid := as.integer(rollapplyr(lag_ret,11,Length,align = "right",fill = NA)>8),
  #        by = PERMNO]
  
  Fine = Sample
  Fine[, rankRET := as.numeric(rollapplyr(log(1+lag_ret),11,sum,align = "right",fill = NA)),by = PERMNO]
  Fine = Fine[ifelse(valid == 1,T,F),]
  Fine = Fine[ifelse(rankRET == -Inf,F,T),]
  
  out = Fine[,list(Year = format(date,"%Y"),Month = format(date,"%m"),
                   PERMNO,EXCHCD,lag_Mkt_Cap = lag_MktCap,Ret = RET,
                   Ranking_Ret = rankRET)]
  out[,`:=` (Year = as.integer(Year),Month = as.integer(Month))]
  setkey(out,Year)
  out = out[.(1927:2018)]
  setorder(out,Year,Month,PERMNO)
  return(out)
}

CRSP_Stocks_Momentum = PS3_Q1(CRSP_Stocks)
# for testing purpose
# test = as.data.frame(CRSP_Stocks_Momentum)
# mean(exp(CRSP_Stocks_Momentum$Ranking_Ret)-1,na.rm=T)


####################################################################
#                           Question 2                             #
####################################################################
PS3_Q2 = function(Data){
  Data=na.omit(Data)
  Data[,DM_decile := as.numeric(cut(Ranking_Ret,
                                    breaks = quantile(Ranking_Ret,probs=seq(from=0,to=1,by=0.1),type=7),
                                    include.lowest = T)),.(Year,Month)]
  Data[,KRF_decile := as.numeric(cut(Ranking_Ret,
                                     breaks = quantile(.SD[EXCHCD == 1,Ranking_Ret],probs=seq(from=0,to=1,by=0.1),type=7),
                                     include.lowest = T)),.(Year,Month)]
  out = Data[,list(Year,Month,PERMNO,lag_Mkt_Cap,Ret,DM_decile,KRF_decile)]
  out[,`:=` (Year = as.integer(Year),Month = as.integer(Month))]
  setorder(out,Year,Month)
  return(out)
}

CRSP_Stocks_Momentum_decile = PS3_Q2(CRSP_Stocks_Momentum)


####################################################################
#                           Question 3                             #
####################################################################
FFraw = read.csv("F-F_Research_Data_Factors.csv") %>% as.data.table()

FFraw = FFraw[,list(date = X, ExRet = Mkt.RF, SMB = SMB, HML = HML, RF = RF)]
FFdata = FFraw[,date := as.integer(as.character(date))]
FFdata[,Year := as.integer(date/100)]
FFdata[,Month := date%%100]
FF_mkt = FFdata[,list(Year = Year,Month=Month,Market_minus_Rf=as.numeric(as.character(ExRet))/100,
                      SMB = as.numeric(as.character(SMB))/100,
                      HML = as.numeric(as.character(HML))/100,
                      Rf = as.numeric(as.character(RF))/100)]

PS3_Q3 = function(data1,data2){
  data1[,`:=` (Year = as.integer(Year),Month = as.integer(Month))]
  setkey(data1,Year,Month)
  setkey(data2,Year,Month)
  temp = na.omit(merge(data1,data2,all=T)) %>% as.data.table()
  temp[,lag_Tot_Cap_DM := sum(lag_Mkt_Cap,na.rm=T),by = list(Year,Month,DM_decile)]
  temp[,lag_Tot_Cap_KRF := sum(lag_Mkt_Cap,na.rm=T),by = list(Year,Month,KRF_decile)]
  temp_DM = temp[,.(DM_Ret = weighted.mean(Ret,lag_Mkt_Cap/lag_Tot_Cap_DM)),by = list(Year,Month,DM_decile)]
  temp_KRF = temp[,.(KRF_Ret = weighted.mean(Ret,lag_Mkt_Cap/lag_Tot_Cap_KRF)),by = list(Year,Month,KRF_decile)]
  setkey(temp_DM,Year,Month);setkey(temp_KRF,Year,Month)
  out = cbind(temp_DM,temp_KRF)[,c(-5,-6)]
  out = merge(out,data2,by=c("Year","Month"))[,c(-7,-8,-9)]
  setorder(out,Year,Month)
  return(out)
}

CRSP_Stocks_Momentum_returns = PS3_Q3(CRSP_Stocks_Momentum_decile,FF_mkt)
# for testing purpose
# data1 = CRSP_Stocks_Momentum_decile
# data2 = FF_mkt


####################################################################
#                           Question 4                             #
####################################################################
PS3_Q4 = function(data){
  data = as.data.frame(data) %>% as.data.table()
  data[,`:=` (DM_mean = mean(DM_Ret-Rf,na.rm = T)*12*100,DM_sigma = sd(DM_Ret,na.rm = T)*sqrt(12)*100,
              DM_skew = skewness(log(1+DM_Ret))),by=DM_decile]
  data[,DM_SR := DM_mean/DM_sigma, by = DM_decile]
  DM = data[,list(DM_mean,DM_sigma,DM_SR,DM_skew,decile = DM_decile,DM_Ret)]
  setkey(DM,decile)
  out = matrix(nrow=4,ncol=11)
  for(i in 1:10){
    for(j in 1:4){
      out[j,i] = as.data.frame(DM[.(i)])[1,j]
    }
  }
  
  out[,11] = c(mean(DM[.(10)]$DM_Ret-DM[.(1)]$DM_Ret)*12*100,
               sd(DM[.(10)]$DM_Ret-DM[.(1)]$DM_Ret)*sqrt(12)*100,
               (as.data.frame(DM[.(10)])[1,1]-as.data.frame(DM[.(1)])[1,1])/(sd(DM[.(10)]$DM_Ret-DM[.(1)]$DM_Ret)*sqrt(12)*100),
               skewness(log(1+DM[.(10)]$DM_Ret)-log(1+DM[.(1)]$DM_Ret)))
  out = as.data.frame(out)
    for(i in 1:10){
    colnames(out)[i] = paste("decile",i,sep="")
    }
  colnames(out)[11] = "wml"
  rownames(out) = c("mean r DM(%/yr)","sigma DM (%)","SR DM", "Skewness DM")
  return(out)
}

Compare_Q4 = PS3_Q4(CRSP_Stocks_Momentum_returns)


####################################################################
#                           Question 5                             #
####################################################################
DM_returns = read.csv("10_Portfolios_Prior_12_2.csv") %>% as.data.table()
KRF_returns = fread("m_m_pt_tot.txt")
colnames(KRF_returns) = c("date","decile","Ret","na","na")

DM_returns[,date := as.integer(as.character(X))]
DM_returns[,Year := as.integer(date/100)]
DM_returns[,Month := date%%100]
DM_returns = DM_returns[,list(Year,Month,deceile1 = Lo.PRIOR,deceile2 = PRIOR.2,
                              deceile3 = PRIOR.3,deceile4 = PRIOR.4,deceile5 = PRIOR.5,
                              deceile6 = PRIOR.6,deceile7 = PRIOR.7,deceile8 = PRIOR.8,
                              deceile9 = PRIOR.9,deceile10 = Hi.PRIOR)]

DM_returns = as.data.frame(DM_returns)
N = length(DM_returns$deceile1)
Ret = c(); decile = c(); Year = c(); Month = c()
for(i in 1:10){
  Ret = c(Ret,DM_returns[,(2+i)])
  decile = c(decile,rep(i,N))
  Year = c(Year,DM_returns$Year)
  Month = c(Month,DM_returns$Month)
}
DM_returns = cbind(cbind(cbind(Year,Month),decile),DM_Ret = Ret/100) %>% as.data.table()

KRF_returns[,date := as.Date(as.character(date),format = "%Y%m%d")]
KRF_returns[,Year := as.integer(format(date,"%Y"))]
KRF_returns[,Month := as.integer(format(date,"%m"))]
KRF_returns = KRF_returns[,list(Year,Month,decile,KRF_Ret = Ret)]

PS3_Q5 = function(data,dataDM,dataKRF){
  data_DM = as.data.frame(dcast(data,Year+Month~DM_decile, value.var = "DM_Ret"))
  data_KRF = as.data.frame(dcast(data,Year+Month~KRF_decile, value.var = "KRF_Ret"))[-c(1081:1104),]
  dataDM = as.data.frame(dcast(dataDM,Year+Month~decile,value.var = "DM_Ret"))[c(-1107,-1106,-1105),]
  dataKRF = as.data.frame(dcast(dataKRF,Year+Month~decile,value.var = "KRF_Ret"))
  cor_DM = c(); cor_KRF = c()
  for(i in 1:10){
    cor_DM[i] = cor(dataDM[,(i+2)],data_DM[,(i+2)])
    cor_KRF[i] = cor(dataKRF[,(i+2)],data_KRF[,(i+2)])
  }
  WML_DM = data_DM$`10`-data_DM$`1`
  WML_KRF = data_KRF$`10`-data_KRF$`1`
  WMLDM = dataDM$`10`-dataDM$`1`
  WMLKRF = dataKRF$`10`-dataKRF$`1`
  cor_DM[11] = cor(WMLDM,WML_DM)
  cor_KRF[11] = cor(WMLKRF,WML_KRF)
  out = as.data.frame(rbind(cor_DM,cor_KRF))
  for(i in 1:10){
    colnames(out)[i] = paste("decile",i,sep="")
  }
  colnames(out)[11] = "wml"
  return(round(out,4))
}

Compare_Q5 = PS3_Q5(CRSP_Stocks_Momentum_returns,DM_returns,KRF_returns)
# for testing purpose
# data = CRSP_Stocks_Momentum_returns
# dataDM = DM_returns
# dataKRF = KRF_returns


####################################################################
#                           Question 6                             #
####################################################################
dataDM = DM_returns
setkey(dataDM,Year)
dataDM = dataDM[.(2015:2018)]
dataDM = dcast(dataDM,Year+Month~decile,value.var = "DM_Ret")
dataDM[,date := as.Date(as.character(Year*10000+Month*100+1),format = "%Y%m%d")]

qplot(date,`1`,data = dataDM,color = I("green"),geom="line",linetype=I("dotted"),
      main = "decile 1 (green) decile 10 (blue) WML (red)") + theme_bw()+
  geom_line(aes(y=`10`),color = I("blue"),linetype=I("dotted"))+
  geom_line(aes(y=`10`-`1`),color = I("red"))



