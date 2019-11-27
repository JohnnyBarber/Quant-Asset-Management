library(data.table)
#install.packages("stargazer")
library(stargazer)
library(moments)
library(dplyr)

####################################################################
#                           Question 1                             #
####################################################################
CRSP_Stocks = read.csv("datacsv.csv") %>% as.data.table()
summary(CRSP_Stocks)

PS1_Q1 = function(RAW){
  #clear data
  data = RAW[RAW$EXCHCD==1|RAW$EXCHCD==2|RAW$EXCHCD==3]
  data = data[data$SHRCD==10|data$SHRCD==11]
  data[,RET := as.numeric(as.character(RET))]
  data[,DLRET := as.numeric(as.character(DLRET))]
  sum(is.na(data$PRC))
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
  
  #set Date type
  data[,date := as.Date(as.character(date),format = "%Y%m%d")]
  
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
                      Stock_lag_MV = head(total_lag_MV,1),
                      Stock_Ew_Ret = head(Stock_Ew_Ret,1),
                      Stock_Vw_Ret = head(Stock_Vw_Ret,1)), by = date]
  Output[,Year := as.numeric(Year)]
  Output[,Month := as.numeric(Month)]
  Output = Output[,-1]
  setorder(Output,Year,Month)
  write.csv(Output,"Output.csv")
  return(Output)
}

Monthly_CRSP_Stocks = PS1_Q1(CRSP_Stocks)
print(Monthly_CRSP_Stocks)


####################################################################
#                           Question 2                             #
####################################################################
FF_mkt = read.csv("F-F_Research_Data_Factors.csv") %>% as.data.table()

PS1_Q2 = function(FFraw){
  FFraw = FFraw[,list(date = X, ExRet = Mkt.RF, SMB = SMB, HML = HML, RF = RF)]
  FFdata = FFraw[,date := as.integer(as.character(date))]
  FFdata[,Year := as.integer(date/100)]
  FFdata[,Month := date%%100]
  FFdata = FFdata[,list(Year = Year,Month=Month,Market_minus_Rf=as.numeric(as.character(ExRet)),
                        SMB = as.numeric(as.character(SMB)),
                        HML = as.numeric(as.character(HML)),
                        Rf = as.numeric(as.character(RF)))]
  
  table1 = FFdata[,list(Annualized_Mean = mean(Market_minus_Rf)*12/100,
                        Annualized_Standard_Deviation = sd(Market_minus_Rf)*sqrt(12)/100,
                        Skewness = skewness(Market_minus_Rf),
                        Excess_Kurtosis = kurtosis(Market_minus_Rf)-3)]
  table1 = table1[,Annualized_Sharpe_Ratio := Annualized_Mean/Annualized_Standard_Deviation]
  
  estimated = merge(Monthly_CRSP_Stocks,FFdata,all = T)
  estimated = estimated[!is.na(estimated$Rf),]
  estimated = estimated[!is.na(estimated$Stock_lag_MV),]
  table2 = estimated[,list(Annualized_Mean = mean(Stock_Vw_Ret*100-Rf)*12/100,
                           Annualized_Standard_Deviation = sd(Stock_Vw_Ret*100-Rf)*sqrt(12)/100,
                           Skewness = skewness(Stock_Vw_Ret*100-Rf),
                           Excess_Kurtosis = kurtosis(Stock_Vw_Ret*100-Rf)-3)]
  table2 = table2[,Annualized_Sharpe_Ratio := Annualized_Mean/Annualized_Standard_Deviation]
  
  output = as.data.frame(rbind(table1,table2))
  rownames(output) = c("Actual","Estimated")
  write.csv(output,"Output2.csv")
  return(output)
}

Output2 = PS1_Q2(FF_mkt)
stargazer(t(Output2),type = 'text',summary = F, digits = 4)


####################################################################
#                           Question 3                             #
####################################################################
PS1_Q3 = function(data1,data2){
  data2 = data2[,list(date = X, ExRet = Mkt.RF, SMB = SMB, HML = HML, RF = RF)]
  data2[,date := as.integer(as.character(date))]
  data2[,Year := as.integer(date/100)]
  data2[,Month := date%%100]
  data2 = data2[,list(Year = Year,Month=Month,Market_minus_Rf=as.numeric(as.character(ExRet)),
                        SMB = as.numeric(as.character(SMB)),
                        HML = as.numeric(as.character(HML)),
                        Rf = as.numeric(as.character(RF)))]
  
  table = merge(data1,data2,all=T)
  table = table[!is.na(table$Stock_Vw_Ret),]
  table = table[!is.na(table$Market_minus_Rf)]
  output = table[,list(correlatoin = cor(Stock_Vw_Ret*100-Rf,Market_minus_Rf),
                      Max_abs_diff = max(abs(Stock_Vw_Ret*100-Rf-Market_minus_Rf)/100))]
  output = as.data.frame(output)
  colnames(output) = c("Correlation","Maximum_absolute_difference")
  write.csv(output,"Output3.csv")
  return(output)
}

Output3 = PS1_Q3(Monthly_CRSP_Stocks,FF_mkt)
stargazer(t(Output3),type = 'text',summary = F,digits = 8)
