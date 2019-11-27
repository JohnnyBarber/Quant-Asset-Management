options(warn = -1)
library(ggplot2)
library(data.table)
library(dplyr)
library(readr)
library(moments)

####################################################################
#                           Question 1                             #
####################################################################
CRSP_Bonds = read.csv("bond.csv") %>% as.data.table()
CRSP_Bonds = CRSP_Bonds[,list(KYCRSPID,MCALDT,TMRETNUA,TMTOTOUT)]
CRSP_Bonds[, MCALDT := as.Date(as.character(CRSP_Bonds$MCALDT),format="%m/%d/%Y")]

PS2_Q1 = function(bond){
  setorder(bond, KYCRSPID, MCALDT)
  bond = bond[,lag_MV := shift(TMTOTOUT),by = KYCRSPID]
  bond = bond[TMRETNUA == -99, TMRETNUA := 0]
  
  bond[, NoPERMNO := length(unique(KYCRSPID)), by = MCALDT]
  bond[, EqualWR := TMRETNUA/NoPERMNO]
  
  bond = bond[complete.cases(bond$lag_MV),]
  bond = bond[,TotalCap := sum(lag_MV), by = MCALDT]
  bond[, ValueWR := TMRETNUA*lag_MV/TotalCap]
  
  bond = bond[,Bond_Vw_Ret := sum(ValueWR,na.rm = T),by = MCALDT]
  bond = bond[,Bond_Ew_Ret := sum(EqualWR,na.rm = T),by = MCALDT]
  bond = bond[,total_lag_MV := sum(lag_MV,na.rm = T), by = MCALDT]
  
  Output = bond[,list(Year = head(format(MCALDT, "%Y"),1),
                      Month = head(format(MCALDT, "%m"),1),
                      Bond_lag_MV = head(total_lag_MV,1),
                      Bond_Ew_Ret = head(Bond_Ew_Ret,1),
                      Bond_Vw_Ret = head(Bond_Vw_Ret,1)), by = MCALDT]
  Output[,Year := as.numeric(Year)]
  Output[,Month := as.numeric(Month)]
  Output = Output[,-1]
  setorder(Output,Year,Month)
  write.csv(Output,"Output.csv")
  return(Output)
}

Monthly_CRSP_Bonds = PS2_Q1(CRSP_Bonds)


####################################################################
#                           Question 2                             #
####################################################################
CRSP_Stocks = read.csv("datacsv.csv") %>% as.data.table()

PS1_Q1 = function(RAW){
  #clear data
  data = RAW[RAW$EXCHCD==1|RAW$EXCHCD==2|RAW$EXCHCD==3]
  data = data[data$SHRCD==10|data$SHRCD==11]
  data[,RET := as.numeric(as.character(RET))]
  data[,DLRET := as.numeric(as.character(DLRET))]
  data = data[complete.cases(data$PRC),]
  
  #set Date type
  data[,date := as.Date(as.character(date),format = "%Y%m%d")]
  
  #create cum_dividend total return
  setkey(data,date)
  data[is.na(DLRET), cum_divid_return := RET]
  data[is.na(RET), cum_divid_return := DLRET]
  data[!is.na(DLRET)&&!is.na(RET), cum_divid_return := (1+DLRET)*(1+RET)-1]
  #data = data[complete.cases(data$cum_divid_return),]
  
  #market cap (size)
  data[,MktCap := abs(PRC)*SHROUT]
  #data = data[complete.cases(data$MktCap),]
  
  #compute Lagged MV, Ew Return, Vw Return
  setorder(data, PERMNO, date)
  data = data[,lag_MV := shift(MktCap),by = PERMNO]
  data = data[complete.cases(data$lag_MV),]
  
  data = data[,TotalCap := sum(lag_MV), by = date]
  data[, ValueWR := cum_divid_return*lag_MV/TotalCap]
  
  data[, NoPERMNO := length(unique(PERMNO)), by = date]
  data[, EqualWR := cum_divid_return/NoPERMNO]
  
  test = data[,Stock_Vw_Ret := sum(ValueWR,na.rm = T),by = date]
  test = data[,Stock_Ew_Ret := sum(EqualWR,na.rm = T),by = date]
  test = data[,total_lag_MV := sum(lag_MV,na.rm = T), by = date]
  
  Output = test[,list(Year = head(format(date, "%Y"),1),
                      Month = head(format(date, "%m"),1),
                      Stock_lag_MV = head(total_lag_MV,1)/1000,
                      Stock_Ew_Ret = head(Stock_Ew_Ret,1),
                      Stock_Vw_Ret = head(Stock_Vw_Ret,1)), by = date]
  Output[,Year := as.numeric(Year)]
  Output[,Month := as.numeric(Month)]
  setorder(Output,Year,Month)
  write.csv(Output,"OutputStock.csv")
  return(Output)
}

Monthly_CRSP_Stocks = PS1_Q1(CRSP_Stocks)

Monthly_CRSP_Riskless = read_csv("riskless.csv") %>% as.data.table()
Monthly_CRSP_Riskless[, caldt := as.Date(as.character(caldt),format = "%Y%m%d")]

#for test purpose
# data1 = Monthly_CRSP_Stocks
# data2 = Monthly_CRSP_Bonds
# data3 = Monthly_CRSP_Riskless

PS2_Q2 = function(data1,data2,data3){
  data3[,Year := format(caldt,"%Y")]
  data3[,Month := format(caldt, "%m")]
  data3[,Year := as.numeric(Year)]
  data3[,Month := as.numeric(Month)]
  data3 = data3[,list(Year,Month,t90ret,t30ret)]
  
  setkey(data1,Year,Month)
  setkey(data2,Year,Month)
  setkey(data3,Year,Month)
  temp = merge(data1,data2,all=T)
  temp = merge(temp,data3,all=T)
  temp = na.omit(temp)
  
  temp[, Stock_Excess_Vw_Ret := Stock_Vw_Ret - t30ret]
  temp[, Bond_Excess_Vw_Ret := Bond_Vw_Ret - t30ret]
  
  Output = temp[,list(Year,Month,Stock_lag_MV,
                      Stock_Excess_Vw_Ret,
                      Bond_lag_MV = Bond_lag_MV,
                      Bond_Excess_Vw_Ret)]
  
  write.csv(Output,"Output2.csv")
  return(Output)
}

Monthly_CRSP_Universe = PS2_Q2(Monthly_CRSP_Stocks,
                               Monthly_CRSP_Bonds,
                               Monthly_CRSP_Riskless)


####################################################################
#                           Question 3                             #
####################################################################

PS2_Q3 = function(data){
  data[,total_weight := Stock_lag_MV+Bond_lag_MV]
  data[,Excess_Vw_Ret := Stock_Excess_Vw_Ret*(Stock_lag_MV/total_weight) +
         Bond_Excess_Vw_Ret*(Bond_lag_MV/total_weight)]
  data[,Excess_60_40_Ret := 0.4*Bond_Excess_Vw_Ret + 0.6*Stock_Excess_Vw_Ret]
  Stock_inverse_sigma_hat = c(0)
  Bond_inverse_sigma_hat = c(0)
  for(i in 2:length(data$Year)){
    if(i<=36){
      Stock_inverse_sigma_hat[i] = sd(data$Stock_Excess_Vw_Ret[1:i])
      Bond_inverse_sigma_hat[i] = sd(data$Bond_Excess_Vw_Ret[1:i])
    }else{
      Stock_inverse_sigma_hat[i] = sd(data$Stock_Excess_Vw_Ret[(i-35):i])
      Bond_inverse_sigma_hat[i] = sd(data$Bond_Excess_Vw_Ret[(i-35):i])
    }
    
  }
  Stock_inverse_sigma_hat = Stock_inverse_sigma_hat^(-1)
  Bond_inverse_sigma_hat = Bond_inverse_sigma_hat^(-1)
  data = cbind(data,Stock_inverse_sigma_hat)
  data = cbind(data,Bond_inverse_sigma_hat)
  data[,Unlevered_k := 1/(Stock_inverse_sigma_hat+
         Bond_inverse_sigma_hat)]
  data[,RP_weight := Unlevered_k*shift(Stock_inverse_sigma_hat)]
  data[,RP_weight_last := shift(RP_weight)]
  data[,Excess_Unlevered_RP_Ret := RP_weight_last*Stock_Excess_Vw_Ret +
         (1-RP_weight_last)*Bond_Excess_Vw_Ret]
  Vw_sigma = sd(data$Excess_Vw_Ret)
  temp_p_ret = shift(data$Stock_inverse_sigma_hat)*data$Stock_Excess_Vw_Ret +
    shift(data$Bond_inverse_sigma_hat)*data$Bond_Excess_Vw_Ret
  temp_p_sigma = sd(na.omit(temp_p_ret))
  k = Vw_sigma/temp_p_sigma
  data[,Levered_k := k]
  data[,RP_stock_weight_const := Levered_k*shift(Stock_inverse_sigma_hat)]
  data[,RP_bond_weight_const := Levered_k*shift(Bond_inverse_sigma_hat)]
  data[,RP_stock_weight_const_last := shift(RP_stock_weight_const)]
  data[,RP_bond_weight_const_last := shift(RP_bond_weight_const)]
  data[,Excess_Levered_RP_Ret := RP_stock_weight_const_last*Stock_Excess_Vw_Ret +
         RP_bond_weight_const_last*Bond_Excess_Vw_Ret]
  
  Output = data[,list(Year,Month,Stock_Excess_Vw_Ret,Bond_Excess_Vw_Ret,
                      Excess_Vw_Ret,Excess_60_40_Ret,Stock_inverse_sigma_hat,
                      Bond_inverse_sigma_hat,Unlevered_k,Excess_Unlevered_RP_Ret,
                      Levered_k,Excess_Levered_RP_Ret)]

  write.csv(Output,"Output3.csv")
  return(Output)
}

Port_Rets = PS2_Q3(Monthly_CRSP_Universe)


####################################################################
#                           Question 4                             #
####################################################################
setkey(Port_Rets,Year)
Port_Rets = Port_Rets[.(1930:2010)]

PS2_Q4 = function(data){
  temp = data[,list(Stock_Excess_Vw_Ret,
                    Bond_Excess_Vw_Ret,
                    Excess_Vw_Ret,
                    Excess_60_40_Ret,
                    Excess_Unlevered_RP_Ret,
                    Excess_Levered_RP_Ret)]
  Excess_Return = apply(temp,2,mean)*12*100
  Volatility = apply(temp,2,sd)*sqrt(12)*100
  n = length(unique(data$Year))
  t_Stat_Excess_Return = sqrt(n)*Excess_Return/Volatility
  Sharpe_Ratio = t_Stat_Excess_Return/sqrt(n)
  Skewness = apply(temp,2,skewness)
  Excess_Kurtosis = apply(temp,2,kurtosis)-3
  name = c("Excess_Return","t_Stat_Excess_Return",
           "Volatility","Sharpe_Ratio",
           "Skewness","Excess_Kurtosis")
  out = cbind(Excess_Return,t_Stat_Excess_Return)
  for(i in name[3:6]){
    out = cbind(out,get(i))
  }
  out = as.data.frame(out)
  colnames(out) = name
  rownames(out) = c("CRSP_stocks","CRSP_bonds",
                  "Value_weighted_portfolio",
                  "60/40_portfolio","RP,unlevered",
                  "RP")
  
  write.csv(out,"Output4.csv")
  return(out)
}

PS2_Q4(Port_Rets)
