library(data.table)
library(dplyr)
library(RPostgres)
library(matrixStats)
library(bindrcpp)
library(moments)
library(ggplot2)
setwd("C:/Users/Jiaqi Li/Desktop/class materials/quarter 3/Quantitative Asset Management/hw 4")

#use permco to match compnay
#use permco to match company instead of permno, which is security

#--------------------------------------get link table--------------------------------------#
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

#--------------------------------------clean up CRSP--------------------------------------#
CRSP = read.csv("CRSP.csv") %>% as.data.table()
CRSP[,RET := as.numeric(as.character(RET))]
CRSP[,DLRET := as.numeric(as.character(DLRET))]

CRSP = CRSP[(EXCHCD==1 | EXCHCD==2 | EXCHCD==3) & (SHRCD==10| SHRCD==11)]
CRSP[,date := as.Date(as.character(date),format = "%Y%m%d")]
for(i in c("RET","DLRET")){
  CRSP[get(i) %in% c('','A','B','C','P','S','T'), paste0(i) := NA]
  CRSP[, paste0(i) := as.numeric(get(i))]
  CRSP[get(i) %in% c(-66,-77,-88,-99), paste0(i) := NA]
}
CRSP[,`:=`(Year = as.numeric(format(date,"%Y")),
           Month = as.numeric(format(date,"%m")))]

CRSP[is.na(DLRET) & !is.na(RET), cRET := RET]
CRSP[is.na(RET) & !is.na(DLRET), cRET := DLRET]
CRSP[!is.na(DLRET) & !is.na(RET), cRET := (1+DLRET)*(1+RET)-1]

CRSP[,obs_MktCap := abs(PRC)*SHROUT/1000]
CRSP[,obs_MktCap_lag := shift(obs_MktCap), .(PERMNO)]
CRSP[,MktCap := sum(obs_MktCap_lag,na.rm = T), .(PERMCO,Year,Month)]

#--------------------------------------clean up Compustat-------------------------------------#
CompustatPension = read.csv("CompustatPension.csv") %>% as.data.table()
CompustatPension[, datadate := as.Date(as.character(datadate),format = "%Y%m%d")]
CompustatPension[, year := as.integer(format(datadate, "%Y"))]
CompustatAccounting = read.csv("CompustatAccounting.csv") %>% as.data.table()
CompustatAccounting[, datadate := as.Date(as.character(datadate),format = "%Y%m%d")]
Compustat = merge(CompustatAccounting, CompustatPension, by.x = c("gvkey","fyear"), by.y = c("gvkey","year"),all.x = T)
Compustat = Compustat[Compustat$sic > 6199 |Compustat$sic < 6190 ]
rm(CompustatAccounting,CompustatPension)
Compustat[, `:=`(SHE = coalesce(seq,ceq+pstk,at-lt-mib,at-lt),
                 DT = coalesce(txditc,itcb+txdb,sum(txditc,itcb,txdb)),
                 PS = coalesce(pstkrv,pstkl,pstk))]
Compustat[, `:=`(neg_DT = -DT, neg_prba = -prba)]
Compustat[, BE := ifelse(is.na(SHE),NA,rowSums(.SD,na.rm = T)),.SDcols=c("SHE","neg_DT","PS","neg_prba")]
Compustat = Compustat[, list(gvkey,datadate = datadate.x,fyear,at,SHE,DT,PS,BE,sic)]

#--------------------------------------link up CRSP and Compustat------------------------------#
Link_Compustat = merge(Compustat,LinkTable,by.x = "gvkey", by.y = "gvkey", allow.cartesian = T)
setkey(Link_Compustat)
Link_Compustat = Link_Compustat[(is.na(linkdt) | datadate >= linkdt) & (is.na(linkenddt) | datadate <= linkenddt)]

Link_CRSP_Compustat = merge(CRSP, Link_Compustat, by.x = c("PERMCO","Year"), by.y = c("lpermco","fyear"))
Link_CRSP_Compustat = Link_CRSP_Compustat[,list(date,PERMCO,PERMNO,gvkey,EXCHCD,cRET,obs_MktCap,obs_MktCap_lag,MktCap,
                                                at,SHE,DT,PS,BE,sic,linktype,linkprim,liid,linkdt,linkenddt)]
Link_CRSP_Compustat = Link_CRSP_Compustat[(is.na(linkdt) | date >= linkdt) & (is.na(linkenddt) | date <= linkenddt)]

#first
Link_CRSP_Compustat[, prob := .N > 1, by = .(PERMCO,date)]
Link_CRSP_Compustat[, Good_Match := sum(linktype == 'LC'), by = .(PERMCO,date)]
Link_CRSP_Compustat = Link_CRSP_Compustat[!(prob == T & Good_Match == T & linktype != 'LC')]
#second
Link_CRSP_Compustat[, prob := .N > 1, by = .(PERMCO,date)]
Link_CRSP_Compustat[, Good_Match := sum(linkprim == 'P'), by = .(PERMCO,date)]
Link_CRSP_Compustat = Link_CRSP_Compustat[!(prob == T & Good_Match == T & linkprim != 'P')]
#third
Link_CRSP_Compustat[, prob := .N > 1, by = .(PERMCO,date)]
Link_CRSP_Compustat[, Good_Match := sum(liid == 1), by = .(PERMCO,date)]
Link_CRSP_Compustat = Link_CRSP_Compustat[!(prob == T & Good_Match == T & liid != 1)]
#fourth
Link_CRSP_Compustat[, prob := .N > 1, by = .(PERMCO,date)]
Link_CRSP_Compustat[, Good_Match := sum(is.na(linkenddt)), by = .(PERMCO,date)]
Link_CRSP_Compustat = Link_CRSP_Compustat[!(prob == T & Good_Match == T & !is.na(linkenddt))]
#fifth
Link_CRSP_Compustat[, prob := .N > 1, by = .(PERMCO,date)]
Link_CRSP_Compustat[, Good_Match := NULL]
Link_CRSP_Compustat[is.na(linkenddt), linkenddt := as.Date("2018-11-30","%Y-%m-%d")]
Link_CRSP_Compustat[, Date_diff := as.integer(linkenddt-linkdt)]
setorder(Link_CRSP_Compustat,PERMCO,date,Date_diff)
Link_CRSP_Compustat[prob == T, Good_Match := Date_diff == Date_diff[.N], by = .(PERMCO,date)]
Link_CRSP_Compustat = Link_CRSP_Compustat[!(prob == T & Good_Match != T)]
#sixth
Link_CRSP_Compustat[, prob := .N > 1, by = .(PERMCO,date)]
Link_CRSP_Compustat[, Good_Match := NULL]
setorder(Link_CRSP_Compustat,gvkey,linkdt)
Link_CRSP_Compustat[prob == T, start_date := linkdt[1],by = .(gvkey)]
setorder(Link_CRSP_Compustat,gvkey,linkenddt)
Link_CRSP_Compustat[prob == T, end_date := linkenddt[.N], by = .(gvkey)]
Link_CRSP_Compustat[, Date_diff := as.integer(end_date-start_date)]
setorder(Link_CRSP_Compustat,PERMCO,date,gvkey)
Link_CRSP_Compustat[prob == T, Good_Match := Date_diff == Date_diff[.N], by = .(PERMCO,date)]
Link_CRSP_Compustat = Link_CRSP_Compustat[!(prob == T & Good_Match != T)]
#seventh
setorder(Link_CRSP_Compustat,PERMCO,date,gvkey)
Link_CRSP_Compustat = unique(Link_CRSP_Compustat,by = c('PERMCO','date'))

#clean up
if(nrow(unique(Link_CRSP_Compustat, by = c('gvkey','date'))) != nrow(Link_CRSP_Compustat) |
   nrow(unique(Link_CRSP_Compustat, by = c('PERMCO','date'))) != nrow(Link_CRSP_Compustat)){
  stop('1. Monthly Firm-level returns.R: There is an issue with the merge!')
}
setorder(Link_CRSP_Compustat,PERMCO,date)
Link_CRSP_Compustat = Link_CRSP_Compustat[,list(date,PERMCO,PERMNO,gvkey,EXCHCD,sic,cRET,
                                                MktCap,obs_MktCap,obs_MktCap_lag,at,SHE,DT,PS,BE)]
Link_CRSP_Compustat = Link_CRSP_Compustat[complete.cases(Link_CRSP_Compustat),]

#--------------------------------------build portfolio------------------------------#
#size--------------------------------------------------------------------------------
Link_CRSP_Compustat[,`:=`(Year = as.numeric(format(date,"%Y")),
                          Month = as.numeric(format(date, "%m")))]

Link_CRSP_Compustat_size = Link_CRSP_Compustat[Month == 6, .(PERMCO,EXCHCD,Year,Month,MktCap)]
Link_CRSP_Compustat_size[, size_decile := as.numeric(cut(MktCap,
                                                    breaks = quantile(.SD[EXCHCD == 1,MktCap],
                                                    probs = seq(from=0,to=1,by=0.1),type=7),
                                                    include.lowest = T)),.(Year)]
Link_CRSP_Compustat_size = Link_CRSP_Compustat_size[,list(PERMCO,Year,size_decile)]
Link_CRSP_Compustat = merge(Link_CRSP_Compustat,Link_CRSP_Compustat_size,by = c('PERMCO','Year'), all.x = T)
Link_CRSP_Compustat[, keep_size := ifelse(!is.na(shift(size_decile,6)) &
                                            !is.na(shift(obs_MktCap)),1,0),by = PERMNO]
Link_CRSP_Compustat = Link_CRSP_Compustat[keep_size == 1,]
Size_Port = Link_CRSP_Compustat[, (size_RET = weighted.mean(cRET,obs_MktCap,na.rm = T)),.(Year,Month,size_decile)]

#B/M--------------------------------------------------------------------------------
Link_CRSP_Compustat_BM = Link_CRSP_Compustat[Month == 12,]
Link_CRSP_Compustat_BM[,BM := BE/MktCap]
Link_CRSP_Compustat_BM[,BM_decile := as.numeric(cut(BM,
                                                    breaks = quantile(.SD[EXCHCD==1,BM],
                                                    probs = seq(0,1,0.1), type = 7,na.rm = T),
                                                    include.lowest = T)),.(Year)]
Link_CRSP_Compustat_BM[, min_rank := quantile(.SD[EXCHCD==1,BM],
                                              probs = seq(0,1,0.1), type = 7,na.rm = T)[1],
                       .(Year)]
Link_CRSP_Compustat_BM[is.na(BM_decile), BM_decile := ifelse(is.na(BM)|BM<0,NA,ifelse(BM<min_rank,1,10))]
Link_CRSP_Compustat = merge(Link_CRSP_Compustat,Link_CRSP_Compustat_BM[,list(PERMCO,Year,BM_decile)],
                            by = c("PERMCO","Year"),all.x = T)
Link_CRSP_Compustat[, keep_BM := ifelse(!is.na(shift(BM_decile,6))&
                                          !is.na(shift(obs_MktCap)),1,0), by = PERMNO]
Link_CRSP_Compustat = Link_CRSP_Compustat[keep_BM == 1]
BM_Port = Link_CRSP_Compustat[,(BM_RET = weighted.mean(cRET,obs_MktCap)),.(Year,Month,BM_decile)]

#SMB-----------------------------------------------------------------------------------------
Link_CRSP_Compustat_SMB = Link_CRSP_Compustat[Month == 6, .(PERMCO,EXCHCD,Year,Month,MktCap)]
Link_CRSP_Compustat_SMB[,SMB_decile := as.numeric(cut(MktCap,
                                                      breaks = quantile(.SD[EXCHCD == 1,MktCap],
                                                                        probs = c(0,0.3,0.7,1),type=7),
                                                      include.lowest = T)),.(Year)]
Link_CRSP_Compustat_SMB = Link_CRSP_Compustat_SMB[,list(PERMCO,Year,SMB_decile)]
Link_CRSP_Compustat = merge(Link_CRSP_Compustat,Link_CRSP_Compustat_SMB, by = c('PERMCO','Year'), all.x = T)
Link_CRSP_Compustat[, keep_SMB := ifelse(!is.na(shift(SMB_decile,6)) &
                                            !is.na(shift(obs_MktCap)),1,0),by = PERMNO]
Link_CRSP_Compustat = Link_CRSP_Compustat[keep_SMB == 1,]


SMB_Port = Link_CRSP_Compustat[, (SMB_RET = weighted.mean(cRET,obs_MktCap,na.rm = T)),.(Year,Month,SMB_decile)]
setorder(SMB_Port,Year,Month)
SMB_Port = SMB_Port[Year>=1973&Year<=2018]
SMB_Port = SMB_Port[SMB_decile==1]$V1-SMB_Port[SMB_decile==3]$V1

#HML--------------------------------------------------------------------------------
Link_CRSP_Compustat_HML = Link_CRSP_Compustat[Month == 12,]
Link_CRSP_Compustat_HML[,BM := BE/MktCap]
Link_CRSP_Compustat_HML[,HML_decile := as.numeric(cut(BM,
                                                    breaks = quantile(.SD[EXCHCD==1,BM],
                                                    probs = c(0,0.5,1), type = 7,na.rm = T),
                                                    include.lowest = T)),.(Year)]
Link_CRSP_Compustat_HML[, min_rank_HML := quantile(.SD[EXCHCD==1,BM],
                                              probs = c(0,0.5,1), type = 7)[1],
                       .(Year)]
Link_CRSP_Compustat_HML[is.na(HML_decile), HML_decile := ifelse(is.na(BM)|BM<0,NA,ifelse(BM<min_rank_HML,1,10))]
Link_CRSP_Compustat = merge(Link_CRSP_Compustat,Link_CRSP_Compustat_HML[,list(PERMCO,Year,HML_decile)],
                            by = c("PERMCO","Year"),all.x = T)
Link_CRSP_Compustat[, keep_HML := ifelse(!is.na(shift(HML_decile,6))&
                                          !is.na(shift(obs_MktCap)),1,0), by = PERMNO]
Link_CRSP_Compustat = Link_CRSP_Compustat[keep_HML == 1]
HML_Port = Link_CRSP_Compustat[,(HML_RET = weighted.mean(cRET,obs_MktCap)),.(Year,Month,HML_decile)]
setorder(HML_Port,Year,Month)
HML_Port = HML_Port[Year>=1973&Year<=2018]
HML_Port = HML_Port[HML_decile==2]$V1-HML_Port[HML_decile==1]$V1

#--------------------------------------------------------------------------------------
# Link_CRSP_Compustat_SMB = Link_CRSP_Compustat[Month == 6, .(PERMCO,EXCHCD,Year,Month,MktCap)]
# Link_CRSP_Compustat_SMB[,SMB_decile := as.numeric(cut(MktCap,
#                                                       breaks = quantile(.SD[EXCHCD == 1,MktCap],
#                                                                         probs = c(0,0.3,0.7,1),type=7),
#                                                       include.lowest = T)),.(Year)]
# Link_CRSP_Compustat_SMB = Link_CRSP_Compustat_SMB[,list(PERMCO,Year,SMB_decile)]
# Link_CRSP_Compustat = merge(Link_CRSP_Compustat,Link_CRSP_Compustat_SMB, by = c('PERMCO','Year'), all.x = T)
# Link_CRSP_Compustat[, keep_SMB := ifelse(!is.na(shift(SMB_decile,6)) &
#                                            !is.na(shift(obs_MktCap)),1,0),by = PERMNO]
# Link_CRSP_Compustat = Link_CRSP_Compustat[keep_SMB == 1,]
# Link_CRSP_Compustat_HML = Link_CRSP_Compustat[Month == 12,]
# Link_CRSP_Compustat_HML[,BM := BE/MktCap]
# Link_CRSP_Compustat_HML[,HML_decile := as.numeric(cut(BM,
#                                                       breaks = quantile(.SD[EXCHCD==1,BM],
#                                                                         probs = c(0,0.5,1), type = 7,na.rm = T),
#                                                       include.lowest = T)),.(Year)]
# Link_CRSP_Compustat_HML[, min_rank_HML := quantile(.SD[EXCHCD==1,BM],
#                                                    probs = c(0,0.5,1), type = 7)[1],
#                         .(Year)]
# Link_CRSP_Compustat_HML[is.na(HML_decile), HML_decile := ifelse(is.na(BM)|BM<0,NA,ifelse(BM<min_rank_HML,1,10))]
# Link_CRSP_Compustat = merge(Link_CRSP_Compustat,Link_CRSP_Compustat_HML[,list(PERMCO,Year,HML_decile)],
#                             by = c("PERMCO","Year"),all.x = T)
# Link_CRSP_Compustat[, keep_HML := ifelse(!is.na(shift(HML_decile,6))&
#                                            !is.na(shift(obs_MktCap)),1,0), by = PERMNO]
# Link_CRSP_Compustat = Link_CRSP_Compustat[keep_HML == 1]
# 
# Port1 = Link_CRSP_Compustat[SMB_decile == 1 & HML_decile == 1,]
# Port1 = Port1[,(PortRet = weighted.mean(cRET,MktCap,na.rm = T)), .(Year,Month)]
# setorder(Port1,Year,Month)
# Port1 = Port1[Year>=1973&Year<=2018]
# Port2 = Link_CRSP_Compustat[SMB_decile == 2 & HML_decile == 1,]
# Port2 = Port2[,(PortRet = weighted.mean(cRET,MktCap,na.rm = T)), .(Year,Month)]
# setorder(Port2,Year,Month)
# Port2 = Port2[Year>=1973&Year<=2018]
# Port3 = Link_CRSP_Compustat[SMB_decile == 3 & HML_decile == 1,]
# Port3 = Port3[,(PortRet = weighted.mean(cRET,MktCap,na.rm = T)), .(Year,Month)]
# setorder(Port3,Year,Month)
# Port3 = Port3[Year>=1973&Year<=2018]
# Port4 = Link_CRSP_Compustat[SMB_decile == 1 & HML_decile == 2,]
# Port4 = Port4[,(PortRet = weighted.mean(cRET,MktCap,na.rm = T)), .(Year,Month)]
# setorder(Port4,Year,Month)
# Port4 = Port4[Year>=1973&Year<=2018]
# Port5 = Link_CRSP_Compustat[SMB_decile == 2 & HML_decile == 2,]
# Port5 = Port5[,(PortRet = weighted.mean(cRET,MktCap,na.rm = T)), .(Year,Month)]
# setorder(Port5,Year,Month)
# Port5 = Port5[Year>=1973&Year<=2018]
# Port6 = Link_CRSP_Compustat[SMB_decile == 3 & HML_decile == 2,]
# Port6 = Port6[,(PortRet = weighted.mean(cRET,MktCap,na.rm = T)), .(Year,Month)]
# setorder(Port6,Year,Month)
# Port6 = Port6[Year>=1973&Year<=2018]
# 
# SMB_low = cbind(Port1$V1,Port2$V1,Port3$V1)
# SMB_low = apply(SMB_low,1,mean)
# SMB_high = cbind(Port4$V1,Port5$V1,Port6$V1)
# SMB_high = apply(SMB_high,1,mean)
# 
# SMB_Port = SMB_low-SMB_high
# 
# HML_low = cbind(Port1$V1,Port4$V1)
# HML_low = apply(HML_low,1,mean)
# HML_high = cbind(Port3$V1,Port6$V1)
# HML_high = apply(HML_high,1,mean)
# 
# HML_Port = HML_low-HML_high

###################################

#--------------------------------------Compare data------------------------------------#
#Fama French size portfolio-------------------------------------------------------------
FF_size = read.csv("Portfolios_Formed_on_ME.csv") %>% as.data.table()
FF_size[,`:=`(Year = as.integer(X/100), Month = X%%100)]
setorder(FF_size,Year,Month)
FF_size = FF_size[Year>=1973&Year<=2018] %>% as.data.frame()

#Fama French BM portfolio-------------------------------------------------------------
FF_bm = read.csv("Portfolios_Formed_on_BE-ME.csv") %>% as.data.table()
FF_bm[,`:=`(Year = as.integer(X/100), Month = X%%100)]
setorder(FF_bm,Year,Month)
FF_bm = FF_bm[Year>=1973&Year<=2018] %>% as.data.frame()

#Fama French RF rate--------------------------------------------------------------------------
FF_research = read.csv("F-F_Research_Data_Factors.csv") %>% as.data.table()
FF_research[,`:=`(Year = as.integer(X/100), Month = X%%100)]
setorder(FF_research,Year,Month)
FF_rf = FF_research[Year>=1973&Year<=2018,list(Year,Month,RF)] %>% as.data.frame()
FF_SMB = FF_research[Year>=1973&Year<=2018]$SMB
FF_HML = FF_research[Year>=1973&Year<=2018]$HML

#replicated size, BM, SMB, HML portfolio-------------------------------------------------------------
setorder(Size_Port,Year,Month)
setorder(BM_Port,Year,Month)
test_size = Size_Port[Year>=1973&Year<=2018]
test_bm = BM_Port[Year>=1973&Year<=2018]

Return_size = c()
Return_bm = c()
std_size = c()
std_bm = c()
skew_size = c()
skew_bm = c()
correlation_size = c()
correlation_bm = c()
for(i in 1:10){
  Return_size[i] = mean(test_size[size_decile==i]$V1*100-FF_rf$RF)*12
  Return_bm[i] = mean(test_bm[BM_decile==i]$V1*100-FF_rf$RF)*12
  std_size[i] = sd(test_size[size_decile==i]$V1*100-FF_rf$RF)*sqrt(12)
  std_bm[i] = sd(test_bm[BM_decile==i]$V1*100-FF_rf$RF)*sqrt(12)
  skew_size[i] = skewness(test_size[size_decile==i]$V1)
  skew_bm[i] = skewness(test_bm[BM_decile==i]$V1)
  correlation_size[i] = cor(test_size[size_decile==i]$V1*100-FF_rf$RF,FF_size[,10+i])
  correlation_bm[i] = cor(test_bm[BM_decile==i]$V1*100-FF_rf$RF,FF_bm[,10+i])
}
Return_size[11] = mean(test_size[size_decile==10]$V1*100-test_size[size_decile==1]$V1*100)*12
Return_bm[11] = mean(test_bm[BM_decile==10]$V1*100-test_bm[BM_decile==1]$V1*100)*12
std_size[11] = sd(test_size[size_decile==10]$V1*100-test_size[size_decile==1]$V1*100)*sqrt(12)
std_bm[11] = sd(test_bm[BM_decile==10]$V1*100-test_bm[BM_decile==1]$V1*100)*sqrt(12)
skew_size[11] = skewness(test_size[size_decile==10]$V1*100-test_size[size_decile==1]$V1*100)
skew_bm[11] = skewness(test_bm[BM_decile==10]$V1*100-test_bm[BM_decile==1]$V1*100)
correlation_size[11] = cor(test_size[size_decile==1]$V1*100-test_size[size_decile==10]$V1*100,FF_SMB)
correlation_bm[11] = cor(test_bm[BM_decile==10]$V1*100-test_bm[BM_decile==1]$V1*100,FF_HML)

SR_size = Return_size/std_size
SR_bm = Return_bm/std_bm

Size = round(rbind(Return_size,std_size,SR_size,skew_size,correlation_size),3) %>% as.data.frame()
BM = round(rbind(Return_bm,std_bm,SR_bm,skew_bm,correlation_bm),3) %>% as.data.frame()
for(i in 1:10){
  colnames(Size)[i] = paste0("decile",i)
  colnames(BM)[i] = paste0("decile",i)
}
colnames(Size)[11] = "LongShort"; colnames(BM)[11] = "LongShort"

Return_SMB = mean(SMB_Port*100-FF_rf$RF)*12
Return_HML = mean(HML_Port*100-FF_rf$RF)*12
std_SMB = sd(SMB_Port*100-FF_rf$RF)*sqrt(12)
std_HML = sd(HML_Port*100-FF_rf$RF)*sqrt(12)
SR_SMB = Return_SMB/std_SMB
SR_HML = Return_HML/std_HML
skew_SMB = skewness(SMB_Port)
skew_HML = skewness(HML_Port)
correlation_SMB = cor(SMB_Port*100-FF_rf$RF,FF_SMB)
correlation_HML = cor(HML_Port*100-FF_rf$RF,FF_HML)

SMB = rbind(Return_SMB,std_SMB,SR_SMB,skew_SMB,correlation_SMB)
HML = rbind(Return_HML,std_HML,SR_HML,skew_HML,correlation_HML)
colnames(SMB) = "SMB"
colnames(HML) = "HML"

#question 4 plot---------------------------------------------------------------------------
FF_size = read.csv("Portfolios_Formed_on_ME.csv") 
FF_size<-as.data.frame(FF_size)
FF_size$LS<-as.numeric(as.character(FF_size$Decile10))-as.numeric(as.character(FF_size$Decile1))
LS_size<-FF_size$LS[492:552]/100

FF_bm = read.csv("Portfolios_Formed_on_BE-ME.csv")
FF_BM<-as.data.frame(FF_bm)
FF_BM$LS<-as.numeric(as.character(FF_BM$Decile10))-as.numeric(as.character(FF_BM$Decile1))
LS_BM<-FF_BM$LS[492:552]/100

plot(LS_size,type="l",col="steelblue",main="Return for Size and BM(Long minus Short)",
     xlab="time",ylab="return",lty=1,ylim=(c(-0.1,0.2)))
lines(LS_BM,type="l",col="green",lty=1)
legend("bottomright",legend=c("size", "BM"),
       col=c("steelblue", "green"),lty=1,cex=0.5)