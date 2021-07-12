
################################################################################
#ELL starts here

cat("\014")
rm(list=ls())
#Loan111=Loan
#Race=read.csv('C:/Users/WendiLi/Desktop/Race.csv')
#Race$unique.l.df.region.=str_pad(Race$unique.l.df.region.,5,pad='0')
library(DBI)
library(RSQLite)
library(ff)
library(data.table)
library(matlab)
library(dplyr)
library(plyr)
library(reshape2)
library(stringr)
library(Hmisc)
library(RODBC)
date='2021-04-30'
TheMonth = substr(Sys.Date(),1,7)
loan=odbcDriverConnect()



LoanOri = sqlQuery(loan, "select * from loan,loan_status_data where loan.loan.id = loan_status_data.loan_id and loan.loan.portfolio_id=8  ")
LoanOri$created_at = as.Date(as.POSIXct(LoanOri$created_at/1000,origin='1970-01-01'))

LoanOri$loan_status_text = trimws(LoanOri$loan_status_text)
LoanOri = LoanOri[!LoanOri$loan_status_text%in%c("VOIDED","Positive Withdrawn", "Negative Withdrawn", "Voided", "Agent Review", "Customer Review","Underwriter Review",   "Corrector Review"  ,   "Tribe Review" ,"Withdrawn","Pending Approved" ),]
# LoanOri$Ori = as.Date(as.POSIXct(LoanOri$approved_at/1000,origin='1970-01-01'))


PTOLMapping = LoanOri[,c('id','parent_id')]
PTOLMapping$parent_id[is.na(PTOLMapping$parent_id)] = PTOLMapping$id[is.na(PTOLMapping$parent_id)]
PTOLMapping$parent_id=abs(PTOLMapping$parent_id)
rownames(LoanOri) = 1:length(LoanOri$id)
TT1=(split(LoanOri, (as.numeric(rownames(LoanOri))-1) %/% 200))


for (i in 1:length(TT1))
  
{
  l=c(TT1[[i]]$id)
  print(i)
  
  A=sqlQuery(loan,paste("select * from payment_installment_data where portfolio_id=8 and  payment_installment_data.loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  B=sqlQuery(loan,paste("select * from loan_operation_data where loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  if (i==1)
  {
    av=A
    ap=B
  }else{
    av=rbind(av,A)
    ap=rbind(ap,B)}
}

#Baddebit
av = merge(av,ap,by.x='loan_id', by.y='loan_id',all.x=T)

# av = merge(av,PTOLMapping,by.x='loan_id',by.y='id',all.x=T)
# 
# av = merge(av,ap,by.x='parent_id', by.y='loan_id',all.x=T)


bd = av[!is.na(av$collected_at) & av$collected_at !=0,]
bd$collected_at = as.Date(as.POSIXct(as.numeric(bd$collected_at)/1000,origin='1970-01-01'))
bd$transaction_date = as.Date(as.POSIXct(as.numeric(bd$transaction_date)/1000,origin='1970-01-01'))
bd$baddate = bd$collected_at

Kickloanst = c('Approved','Good Standing','Good Standing Paid Off','Paid Off','New',
               'Pending Collection','Transferred Paid Off','Pending Transferred Paid Off','Processing','Warning','Warning Paid Off')

Goodnumber = LoanOri$parent_id[LoanOri$loan_status_text%in%Kickloanst]

bdcollected = bd [bd$baddate>bd$transaction_date,]
bdcollected = bdcollected[!bdcollected$loan_id%in%Goodnumber,]

#bd$transaction_date<=bd$baddate &
bdcollected = bdcollected[bdcollected$transaction_status==3,]%>%group_by(loan_id)%>%dplyr::summarise(P=sum(as.numeric(principal[transaction_type==14001])),
                                                                                                     out = sum(as.numeric(principal[transaction_type==14000])),
                                                                                                     Baddate = first(baddate)
)
bdcollected$Outstanding = bdcollected$out-bdcollected$P

coll = bd [bd$baddate<=bd$transaction_date,]
coll = coll [coll$parent_id%in%bdcollected$parent_id,]

coll$CM = substr(coll$baddate,1,7)

coll$DM = substr(coll$transaction_date,1,7)
coll=coll[coll$transaction_status==3,]%>%group_by(CM,DM)%>%dplyr::summarise(n=sum(as.numeric(transaction_amount)))%>%tidyr::spread(DM,n)



Pri=bdcollected%>%group_by(substr(Baddate,1,7))%>%dplyr::summarise(Outstanding=sum(Outstanding,na.rm=TRUE), n= length(loan_id))



bdcollected1 = bd[bd$baddate<=bd$transaction_date,]
bdcollected1$amount = as.numeric(bdcollected1$amount)
bdcollected1$amount[bdcollected1$transaction_type==14002] = -bdcollected1$amount[bdcollected1$transaction_type==14002] # 14002 refund, we give money to customer 
bdcollected1=bdcollected1[bdcollected1$transaction_status==3,] # check


bdcollected1$Ori=substr(bdcollected1$baddate,1,7)
bdcollected1$IsOrigination=0
bdcollected1$IsOrigination[bdcollected1$transaction_type==14000]=1
bdcollected1$IsDebit=0
bdcollected1$IsDebit[bdcollected1$IsDebit!=14000]=1
bdcollected1$Pay[bdcollected1$IsDebit!=14000]=substr(bdcollected1$transaction_date,1,7)

# Principle=ALL1%>%group_by(Ori,application_id)%>%dplyr::summarise(Pri=sum(first(Outstanding),na.rm=T))
# Principle=Principle%>%group_by(Ori)%>%dplyr::summarise(Pri=sum(Pri,na.rm=T), n =length(application_id))

Income=as.data.frame.matrix(xtabs(IsDebit*amount~ Pay + Ori, bdcollected1))
Income = t(Income)

library("xlsx")
filename_LR=paste('ALL_',as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),sep = '')
filename_LR=paste('Liquidation Rate/',filename_LR,'.xlsx',sep='')
write.xlsx(as.data.frame(Pri), file = filename_LR, sheetName = "ELL_Pri", 
           col.names = TRUE, row.names = FALSE, append = F)
write.xlsx(as.data.frame(Income), file = filename_LR, sheetName = "ELL_Income", 
           col.names = TRUE, row.names = T, append = T)



################################################################################
LoanOri = sqlQuery(loan, "select * from loan,loan_status_data where loan.loan.id = loan_status_data.loan_id and loan.loan.portfolio_id=1  ")
LoanOri$created_at = as.Date(as.POSIXct(LoanOri$created_at/1000,origin='1970-01-01'))

LoanOri$loan_status_text = trimws(LoanOri$loan_status_text)
LoanOri = LoanOri[!LoanOri$loan_status_text%in%c("VOIDED","Positive Withdrawn", "Negative Withdrawn", "Voided", "Agent Review", "Customer Review","Underwriter Review",   "Corrector Review"  ,   "Tribe Review" ,"Withdrawn","Pending Approved" ),]
# LoanOri$Ori = as.Date(as.POSIXct(LoanOri$approved_at/1000,origin='1970-01-01'))


PTOLMapping = LoanOri[,c('id','parent_id')]
PTOLMapping$parent_id[is.na(PTOLMapping$parent_id)] = PTOLMapping$id[is.na(PTOLMapping$parent_id)]
PTOLMapping$parent_id=abs(PTOLMapping$parent_id)
rownames(LoanOri) = 1:length(LoanOri$id)
TT1=(split(LoanOri, (as.numeric(rownames(LoanOri))-1) %/% 200))


for (i in 1:length(TT1))
  
{
  l=c(TT1[[i]]$id)
  print(i)
  
  A=sqlQuery(loan,paste("select * from payment_installment_data where portfolio_id=1 and  payment_installment_data.loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  B=sqlQuery(loan,paste("select * from loan_operation_data where loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  if (i==1)
  {
    av=A
    ap=B
  }else{
    av=rbind(av,A)
    ap=rbind(ap,B)}
}

#Baddebit
av = merge(av,ap,by.x='loan_id', by.y='loan_id',all.x=T)

# av = merge(av,PTOLMapping,by.x='loan_id',by.y='id',all.x=T)
# 
# av = merge(av,ap,by.x='parent_id', by.y='loan_id',all.x=T)


bd = av[!is.na(av$collected_at) & av$collected_at !=0,]
bd$collected_at = as.Date(as.POSIXct(as.numeric(bd$collected_at)/1000,origin='1970-01-01'))
bd$transaction_date = as.Date(as.POSIXct(as.numeric(bd$transaction_date)/1000,origin='1970-01-01'))
bd$baddate = bd$collected_at

Kickloanst = c('Approved','Good Standing','Good Standing Paid Off','Paid Off','New',
               'Pending Collection','Transferred Paid Off','Pending Transferred Paid Off','Processing','Warning','Warning Paid Off')

Goodnumber = LoanOri$parent_id[LoanOri$loan_status_text%in%Kickloanst]

bdcollected = bd [bd$baddate>bd$transaction_date,]
bdcollected = bdcollected[!bdcollected$loan_id%in%Goodnumber,]

#bd$transaction_date<=bd$baddate &
bdcollected = bdcollected[bdcollected$transaction_status==3,]%>%group_by(loan_id)%>%dplyr::summarise(P=sum(as.numeric(principal[transaction_type==14001])),
                                                                                                     out = sum(as.numeric(principal[transaction_type==14000])),
                                                                                                     Baddate = first(baddate)
)
bdcollected$Outstanding = bdcollected$out-bdcollected$P

coll = bd [bd$baddate<=bd$transaction_date,]
coll = coll [coll$parent_id%in%bdcollected$parent_id,]

coll$CM = substr(coll$baddate,1,7)

coll$DM = substr(coll$transaction_date,1,7)
coll=coll[coll$transaction_status==3,]%>%group_by(CM,DM)%>%dplyr::summarise(n=sum(as.numeric(transaction_amount)))%>%tidyr::spread(DM,n)



Pri=bdcollected%>%group_by(substr(Baddate,1,7))%>%dplyr::summarise(Outstanding=sum(Outstanding,na.rm=TRUE), n= length(loan_id))



bdcollected1 = bd[bd$baddate<=bd$transaction_date,]
bdcollected1$amount = as.numeric(bdcollected1$amount)
bdcollected1$amount[bdcollected1$transaction_type==14002] = -bdcollected1$amount[bdcollected1$transaction_type==14002] # 14002 refund, we give money to customer 
bdcollected1=bdcollected1[bdcollected1$transaction_status==3,] # check


bdcollected1$Ori=substr(bdcollected1$baddate,1,7)
bdcollected1$IsOrigination=0
bdcollected1$IsOrigination[bdcollected1$transaction_type==14000]=1
bdcollected1$IsDebit=0
bdcollected1$IsDebit[bdcollected1$IsDebit!=14000]=1
bdcollected1$Pay[bdcollected1$IsDebit!=14000]=substr(bdcollected1$transaction_date,1,7)

# Principle=ALL1%>%group_by(Ori,application_id)%>%dplyr::summarise(Pri=sum(first(Outstanding),na.rm=T))
# Principle=Principle%>%group_by(Ori)%>%dplyr::summarise(Pri=sum(Pri,na.rm=T), n =length(application_id))

Income=as.data.frame.matrix(xtabs(IsDebit*amount~ Pay + Ori, bdcollected1))
Income = t(Income)

#library("xlsx")
#filename_LR=paste('IBL_',as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),sep = '')
#filename_LR=paste('Liquidation Rate/',filename_LR,'.xlsx',sep='')
write.xlsx(as.data.frame(Pri), file = filename_LR, sheetName = "IBL_Pri", 
           col.names = TRUE, row.names = FALSE, append = T)
write.xlsx(as.data.frame(Income), file = filename_LR, sheetName = "IBL_Income", 
           col.names = TRUE, row.names = T, append = T)

################################################################################
LoanOri = sqlQuery(loan, "select * from loan,loan_status_data where loan.loan.id = loan_status_data.loan_id and loan.loan.portfolio_id=3  ")
LoanOri$created_at = as.Date(as.POSIXct(LoanOri$created_at/1000,origin='1970-01-01'))

LoanOri$loan_status_text = trimws(LoanOri$loan_status_text)
LoanOri = LoanOri[!LoanOri$loan_status_text%in%c("VOIDED","Positive Withdrawn", "Negative Withdrawn", "Voided", "Agent Review", "Customer Review","Underwriter Review",   "Corrector Review"  ,   "Tribe Review" ,"Withdrawn","Pending Approved" ),]
# LoanOri$Ori = as.Date(as.POSIXct(LoanOri$approved_at/1000,origin='1970-01-01'))


PTOLMapping = LoanOri[,c('id','parent_id')]
PTOLMapping$parent_id[is.na(PTOLMapping$parent_id)] = PTOLMapping$id[is.na(PTOLMapping$parent_id)]
PTOLMapping$parent_id=abs(PTOLMapping$parent_id)
rownames(LoanOri) = 1:length(LoanOri$id)
TT1=(split(LoanOri, (as.numeric(rownames(LoanOri))-1) %/% 200))


for (i in 1:length(TT1))
  
{
  l=c(TT1[[i]]$id)
  print(i)
  
  A=sqlQuery(loan,paste("select * from payment_installment_data where portfolio_id=3 and  payment_installment_data.loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  B=sqlQuery(loan,paste("select * from loan_operation_data where loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  if (i==1)
  {
    av=A
    ap=B
  }else{
    av=rbind(av,A)
    ap=rbind(ap,B)}
}

#Baddebit
av = merge(av,ap,by.x='loan_id', by.y='loan_id',all.x=T)

# av = merge(av,PTOLMapping,by.x='loan_id',by.y='id',all.x=T)
# 
# av = merge(av,ap,by.x='parent_id', by.y='loan_id',all.x=T)


bd = av[!is.na(av$collected_at) & av$collected_at !=0,]
bd$collected_at = as.Date(as.POSIXct(as.numeric(bd$collected_at)/1000,origin='1970-01-01'))
bd$transaction_date = as.Date(as.POSIXct(as.numeric(bd$transaction_date)/1000,origin='1970-01-01'))
bd$baddate = bd$collected_at

Kickloanst = c('Approved','Good Standing','Good Standing Paid Off','Paid Off','New',
               'Pending Collection','Transferred Paid Off','Pending Transferred Paid Off','Processing','Warning','Warning Paid Off')

Goodnumber = LoanOri$parent_id[LoanOri$loan_status_text%in%Kickloanst]

bdcollected = bd [bd$baddate>bd$transaction_date,]
bdcollected = bdcollected[!bdcollected$loan_id%in%Goodnumber,]

#bd$transaction_date<=bd$baddate &
bdcollected = bdcollected[bdcollected$transaction_status==3,]%>%group_by(loan_id)%>%dplyr::summarise(P=sum(as.numeric(principal[transaction_type==14001])),
                                                                                                     out = sum(as.numeric(principal[transaction_type==14000])),
                                                                                                     Baddate = first(baddate)
)
bdcollected$Outstanding = bdcollected$out-bdcollected$P

coll = bd [bd$baddate<=bd$transaction_date,]
coll = coll [coll$parent_id%in%bdcollected$parent_id,]

coll$CM = substr(coll$baddate,1,7)

coll$DM = substr(coll$transaction_date,1,7)
coll=coll[coll$transaction_status==3,]%>%group_by(CM,DM)%>%dplyr::summarise(n=sum(as.numeric(transaction_amount)))%>%tidyr::spread(DM,n)



Pri=bdcollected%>%group_by(substr(Baddate,1,7))%>%dplyr::summarise(Outstanding=sum(Outstanding,na.rm=TRUE), n= length(loan_id))



bdcollected1 = bd[bd$baddate<=bd$transaction_date,]
bdcollected1$amount = as.numeric(bdcollected1$amount)
bdcollected1$amount[bdcollected1$transaction_type==14002] = -bdcollected1$amount[bdcollected1$transaction_type==14002] # 14002 refund, we give money to customer 
bdcollected1=bdcollected1[bdcollected1$transaction_status==3,] # check


bdcollected1$Ori=substr(bdcollected1$baddate,1,7)
bdcollected1$IsOrigination=0
bdcollected1$IsOrigination[bdcollected1$transaction_type==14000]=1
bdcollected1$IsDebit=0
bdcollected1$IsDebit[bdcollected1$IsDebit!=14000]=1
bdcollected1$Pay[bdcollected1$IsDebit!=14000]=substr(bdcollected1$transaction_date,1,7)

# Principle=ALL1%>%group_by(Ori,application_id)%>%dplyr::summarise(Pri=sum(first(Outstanding),na.rm=T))
# Principle=Principle%>%group_by(Ori)%>%dplyr::summarise(Pri=sum(Pri,na.rm=T), n =length(application_id))

Income=as.data.frame.matrix(xtabs(IsDebit*amount~ Pay + Ori, bdcollected1))
Income = t(Income)

#library("xlsx")
#filename_LR=paste('FL_',as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),sep = '')
#filename_LR=paste('Liquidation Rate/',filename_LR,'.xlsx',sep='')
write.xlsx(as.data.frame(Pri), file = filename_LR, sheetName = "FL_Pri", 
           col.names = TRUE, row.names = FALSE, append = T)
write.xlsx(as.data.frame(Income), file = filename_LR, sheetName = "FL_Income", 
           col.names = TRUE, row.names = T, append = T)

################################################################################
LoanOri = sqlQuery(loan, "select * from loan,loan_status_data where loan.loan.id = loan_status_data.loan_id and loan.loan.portfolio_id=3 and loan_status in (50201, 50202) ")
LoanOri$created_at = as.Date(as.POSIXct(LoanOri$created_at/1000,origin='1970-01-01'))

LoanOri$loan_status_text = trimws(LoanOri$loan_status_text)
LoanOri = LoanOri[!LoanOri$loan_status_text%in%c("VOIDED","Positive Withdrawn", "Negative Withdrawn", "Voided", "Agent Review", "Customer Review","Underwriter Review",   "Corrector Review"  ,   "Tribe Review" ,"Withdrawn","Pending Approved" ),]
# LoanOri$Ori = as.Date(as.POSIXct(LoanOri$approved_at/1000,origin='1970-01-01'))


PTOLMapping = LoanOri[,c('id','parent_id')]
PTOLMapping$parent_id[is.na(PTOLMapping$parent_id)] = PTOLMapping$id[is.na(PTOLMapping$parent_id)]
PTOLMapping$parent_id=abs(PTOLMapping$parent_id)
rownames(LoanOri) = 1:length(LoanOri$id)
TT1=(split(LoanOri, (as.numeric(rownames(LoanOri))-1) %/% 200))


for (i in 1:length(TT1))
  
{
  l=c(TT1[[i]]$id)
  print(i)
  
  A=sqlQuery(loan,paste("select * from payment_installment_data where portfolio_id=3 and  payment_installment_data.loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  B=sqlQuery(loan,paste("select * from loan_operation_data where loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  if (i==1)
  {
    av=A
    ap=B
  }else{
    av=rbind(av,A)
    ap=rbind(ap,B)}
}

#Baddebit
av = merge(av,ap,by.x='loan_id', by.y='loan_id',all.x=T)

# av = merge(av,PTOLMapping,by.x='loan_id',by.y='id',all.x=T)
# 
# av = merge(av,ap,by.x='parent_id', by.y='loan_id',all.x=T)


bd = av[!is.na(av$collected_at) & av$collected_at !=0,]
bd$collected_at = as.Date(as.POSIXct(as.numeric(bd$collected_at)/1000,origin='1970-01-01'))
bd$transaction_date = as.Date(as.POSIXct(as.numeric(bd$transaction_date)/1000,origin='1970-01-01'))
bd$baddate = bd$collected_at

Kickloanst = c('Approved','Good Standing','Good Standing Paid Off','Paid Off','New',
               'Pending Collection','Transferred Paid Off','Pending Transferred Paid Off','Processing','Warning','Warning Paid Off')

Goodnumber = LoanOri$parent_id[LoanOri$loan_status_text%in%Kickloanst]

bdcollected = bd [bd$baddate>bd$transaction_date,]
bdcollected = bdcollected[!bdcollected$loan_id%in%Goodnumber,]

#bd$transaction_date<=bd$baddate &
bdcollected = bdcollected[bdcollected$transaction_status==3,]%>%group_by(loan_id)%>%dplyr::summarise(P=sum(as.numeric(principal[transaction_type==14001])),
                                                                                                     out = sum(as.numeric(principal[transaction_type==14000])),
                                                                                                     Baddate = first(baddate)
)
bdcollected$Outstanding = bdcollected$out-bdcollected$P

coll = bd [bd$baddate<=bd$transaction_date,]
coll = coll [coll$parent_id%in%bdcollected$parent_id,]

coll$CM = substr(coll$baddate,1,7)

coll$DM = substr(coll$transaction_date,1,7)
coll=coll[coll$transaction_status==3,]%>%group_by(CM,DM)%>%dplyr::summarise(n=sum(as.numeric(transaction_amount)))%>%tidyr::spread(DM,n)



Pri=bdcollected%>%group_by(substr(Baddate,1,7))%>%dplyr::summarise(Outstanding=sum(Outstanding,na.rm=TRUE), n= length(loan_id))



bdcollected1 = bd[bd$baddate<=bd$transaction_date,]
bdcollected1$amount = as.numeric(bdcollected1$amount)
bdcollected1$amount[bdcollected1$transaction_type==14002] = -bdcollected1$amount[bdcollected1$transaction_type==14002] # 14002 refund, we give money to customer 
bdcollected1=bdcollected1[bdcollected1$transaction_status==3,] # check


bdcollected1$Ori=substr(bdcollected1$baddate,1,7)
bdcollected1$IsOrigination=0
bdcollected1$IsOrigination[bdcollected1$transaction_type==14000]=1
bdcollected1$IsDebit=0
bdcollected1$IsDebit[bdcollected1$IsDebit!=14000]=1
bdcollected1$Pay[bdcollected1$IsDebit!=14000]=substr(bdcollected1$transaction_date,1,7)

# Principle=ALL1%>%group_by(Ori,application_id)%>%dplyr::summarise(Pri=sum(first(Outstanding),na.rm=T))
# Principle=Principle%>%group_by(Ori)%>%dplyr::summarise(Pri=sum(Pri,na.rm=T), n =length(application_id))

Income=as.data.frame.matrix(xtabs(IsDebit*amount~ Pay + Ori, bdcollected1))
Income = t(Income)

#library("xlsx")
#filename_LR=paste('FL_',as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),sep = '')
#filename_LR=paste('Liquidation Rate/',filename_LR,'.xlsx',sep='')
write.xlsx(as.data.frame(Pri), file = filename_LR, sheetName = "FL_WA_Pri", 
           col.names = TRUE, row.names = FALSE, append = T)
write.xlsx(as.data.frame(Income), file = filename_LR, sheetName = "FL_WA_Income", 
           col.names = TRUE, row.names = T, append = T)
################################################################################
LoanOri = sqlQuery(loan, "select * from loan,loan_status_data where loan.loan.id = loan_status_data.loan_id and loan.loan.portfolio_id=7  ")
LoanOri$created_at = as.Date(as.POSIXct(LoanOri$created_at/1000,origin='1970-01-01'))

LoanOri$loan_status_text = trimws(LoanOri$loan_status_text)
LoanOri = LoanOri[!LoanOri$loan_status_text%in%c("VOIDED","Positive Withdrawn", "Negative Withdrawn", "Voided", "Agent Review", "Customer Review","Underwriter Review",   "Corrector Review"  ,   "Tribe Review" ,"Withdrawn","Pending Approved" ),]
# LoanOri$Ori = as.Date(as.POSIXct(LoanOri$approved_at/1000,origin='1970-01-01'))


PTOLMapping = LoanOri[,c('id','parent_id')]
PTOLMapping$parent_id[is.na(PTOLMapping$parent_id)] = PTOLMapping$id[is.na(PTOLMapping$parent_id)]
PTOLMapping$parent_id=abs(PTOLMapping$parent_id)
rownames(LoanOri) = 1:length(LoanOri$id)
TT1=(split(LoanOri, (as.numeric(rownames(LoanOri))-1) %/% 200))


for (i in 1:length(TT1))
  
{
  l=c(TT1[[i]]$id)
  print(i)
  
  A=sqlQuery(loan,paste("select * from payment_installment_data where portfolio_id=7 and  payment_installment_data.loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  B=sqlQuery(loan,paste("select * from loan_operation_data where loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  if (i==1)
  {
    av=A
    ap=B
  }else{
    av=rbind(av,A)
    ap=rbind(ap,B)}
}

#Baddebit
av = merge(av,ap,by.x='loan_id', by.y='loan_id',all.x=T)

# av = merge(av,PTOLMapping,by.x='loan_id',by.y='id',all.x=T)
# 
# av = merge(av,ap,by.x='parent_id', by.y='loan_id',all.x=T)


bd = av[!is.na(av$collected_at) & av$collected_at !=0,]
bd$collected_at = as.Date(as.POSIXct(as.numeric(bd$collected_at)/1000,origin='1970-01-01'))
bd$transaction_date = as.Date(as.POSIXct(as.numeric(bd$transaction_date)/1000,origin='1970-01-01'))
bd$baddate = bd$collected_at

Kickloanst = c('Approved','Good Standing','Good Standing Paid Off','Paid Off','New',
               'Pending Collection','Transferred Paid Off','Pending Transferred Paid Off','Processing','Warning','Warning Paid Off')

Goodnumber = LoanOri$parent_id[LoanOri$loan_status_text%in%Kickloanst]

bdcollected = bd [bd$baddate>bd$transaction_date,]
bdcollected = bdcollected[!bdcollected$loan_id%in%Goodnumber,]

#bd$transaction_date<=bd$baddate &
bdcollected = bdcollected[bdcollected$transaction_status==3,]%>%group_by(loan_id)%>%dplyr::summarise(P=sum(as.numeric(principal[transaction_type==14001])),
                                                                                                     out = sum(as.numeric(principal[transaction_type==14000])),
                                                                                                     Baddate = first(baddate)
)
bdcollected$Outstanding = bdcollected$out-bdcollected$P

coll = bd [bd$baddate<=bd$transaction_date,]
coll = coll [coll$parent_id%in%bdcollected$parent_id,]

coll$CM = substr(coll$baddate,1,7)

coll$DM = substr(coll$transaction_date,1,7)
coll=coll[coll$transaction_status==3,]%>%group_by(CM,DM)%>%dplyr::summarise(n=sum(as.numeric(transaction_amount)))%>%tidyr::spread(DM,n)



Pri=bdcollected%>%group_by(substr(Baddate,1,7))%>%dplyr::summarise(Outstanding=sum(Outstanding,na.rm=TRUE), n= length(loan_id))



bdcollected1 = bd[bd$baddate<=bd$transaction_date,]
bdcollected1$amount = as.numeric(bdcollected1$amount)
bdcollected1$amount[bdcollected1$transaction_type==14002] = -bdcollected1$amount[bdcollected1$transaction_type==14002] # 14002 refund, we give money to customer 
bdcollected1=bdcollected1[bdcollected1$transaction_status==3,] # check


bdcollected1$Ori=substr(bdcollected1$baddate,1,7)
bdcollected1$IsOrigination=0
bdcollected1$IsOrigination[bdcollected1$transaction_type==14000]=1
bdcollected1$IsDebit=0
bdcollected1$IsDebit[bdcollected1$IsDebit!=14000]=1
bdcollected1$Pay[bdcollected1$IsDebit!=14000]=substr(bdcollected1$transaction_date,1,7)

# Principle=ALL1%>%group_by(Ori,application_id)%>%dplyr::summarise(Pri=sum(first(Outstanding),na.rm=T))
# Principle=Principle%>%group_by(Ori)%>%dplyr::summarise(Pri=sum(Pri,na.rm=T), n =length(application_id))

Income=as.data.frame.matrix(xtabs(IsDebit*amount~ Pay + Ori, bdcollected1))
Income = t(Income)

#library("xlsx")
#filename_LR=paste('WSL_',as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),sep = '')
#filename_LR=paste('Liquidation Rate/',filename_LR,'.xlsx',sep='')
write.xlsx(as.data.frame(Pri), file = filename_LR, sheetName = "WSL_Pri", 
           col.names = TRUE, row.names = FALSE, append = T)
write.xlsx(as.data.frame(Income), file = filename_LR, sheetName = "WSL_Income", 
           col.names = TRUE, row.names = T, append = T)

################################################################################
################################################################################
LoanOri = sqlQuery(loan, "select * from loan,loan_status_data where loan.loan.id = loan_status_data.loan_id and loan.loan.portfolio_id=7 and loan_status in (50201, 50202) ")
LoanOri$created_at = as.Date(as.POSIXct(LoanOri$created_at/1000,origin='1970-01-01'))

LoanOri$loan_status_text = trimws(LoanOri$loan_status_text)
LoanOri = LoanOri[!LoanOri$loan_status_text%in%c("VOIDED","Positive Withdrawn", "Negative Withdrawn", "Voided", "Agent Review", "Customer Review","Underwriter Review",   "Corrector Review"  ,   "Tribe Review" ,"Withdrawn","Pending Approved" ),]
# LoanOri$Ori = as.Date(as.POSIXct(LoanOri$approved_at/1000,origin='1970-01-01'))


PTOLMapping = LoanOri[,c('id','parent_id')]
PTOLMapping$parent_id[is.na(PTOLMapping$parent_id)] = PTOLMapping$id[is.na(PTOLMapping$parent_id)]
PTOLMapping$parent_id=abs(PTOLMapping$parent_id)
rownames(LoanOri) = 1:length(LoanOri$id)
TT1=(split(LoanOri, (as.numeric(rownames(LoanOri))-1) %/% 200))


for (i in 1:length(TT1))
  
{
  l=c(TT1[[i]]$id)
  print(i)
  
  A=sqlQuery(loan,paste("select * from payment_installment_data where portfolio_id=7 and  payment_installment_data.loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  B=sqlQuery(loan,paste("select * from loan_operation_data where loan_id in (",paste0(paste(l,collapse=","),")"),"",sep = ""),as.is=T)
  if (i==1)
  {
    av=A
    ap=B
  }else{
    av=rbind(av,A)
    ap=rbind(ap,B)}
}

#Baddebit
av = merge(av,ap,by.x='loan_id', by.y='loan_id',all.x=T)

# av = merge(av,PTOLMapping,by.x='loan_id',by.y='id',all.x=T)
# 
# av = merge(av,ap,by.x='parent_id', by.y='loan_id',all.x=T)


bd = av[!is.na(av$collected_at) & av$collected_at !=0,]
bd$collected_at = as.Date(as.POSIXct(as.numeric(bd$collected_at)/1000,origin='1970-01-01'))
bd$transaction_date = as.Date(as.POSIXct(as.numeric(bd$transaction_date)/1000,origin='1970-01-01'))
bd$baddate = bd$collected_at

Kickloanst = c('Approved','Good Standing','Good Standing Paid Off','Paid Off','New',
               'Pending Collection','Transferred Paid Off','Pending Transferred Paid Off','Processing','Warning','Warning Paid Off')

Goodnumber = LoanOri$parent_id[LoanOri$loan_status_text%in%Kickloanst]

bdcollected = bd [bd$baddate>bd$transaction_date,]
bdcollected = bdcollected[!bdcollected$loan_id%in%Goodnumber,]

#bd$transaction_date<=bd$baddate &
bdcollected = bdcollected[bdcollected$transaction_status==3,]%>%group_by(loan_id)%>%dplyr::summarise(P=sum(as.numeric(principal[transaction_type==14001])),
                                                                                                     out = sum(as.numeric(principal[transaction_type==14000])),
                                                                                                     Baddate = first(baddate)
)
bdcollected$Outstanding = bdcollected$out-bdcollected$P

coll = bd [bd$baddate<=bd$transaction_date,]
coll = coll [coll$parent_id%in%bdcollected$parent_id,]

coll$CM = substr(coll$baddate,1,7)

coll$DM = substr(coll$transaction_date,1,7)
coll=coll[coll$transaction_status==3,]%>%group_by(CM,DM)%>%dplyr::summarise(n=sum(as.numeric(transaction_amount)))%>%tidyr::spread(DM,n)



Pri=bdcollected%>%group_by(substr(Baddate,1,7))%>%dplyr::summarise(Outstanding=sum(Outstanding,na.rm=TRUE), n= length(loan_id))



bdcollected1 = bd[bd$baddate<=bd$transaction_date,]
bdcollected1$amount = as.numeric(bdcollected1$amount)
bdcollected1$amount[bdcollected1$transaction_type==14002] = -bdcollected1$amount[bdcollected1$transaction_type==14002] # 14002 refund, we give money to customer 
bdcollected1=bdcollected1[bdcollected1$transaction_status==3,] # check


bdcollected1$Ori=substr(bdcollected1$baddate,1,7)
bdcollected1$IsOrigination=0
bdcollected1$IsOrigination[bdcollected1$transaction_type==14000]=1
bdcollected1$IsDebit=0
bdcollected1$IsDebit[bdcollected1$IsDebit!=14000]=1
bdcollected1$Pay[bdcollected1$IsDebit!=14000]=substr(bdcollected1$transaction_date,1,7)

# Principle=ALL1%>%group_by(Ori,application_id)%>%dplyr::summarise(Pri=sum(first(Outstanding),na.rm=T))
# Principle=Principle%>%group_by(Ori)%>%dplyr::summarise(Pri=sum(Pri,na.rm=T), n =length(application_id))

Income=as.data.frame.matrix(xtabs(IsDebit*amount~ Pay + Ori, bdcollected1))
Income = t(Income)

#library("xlsx")
#filename_LR=paste('WSL_',as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),sep = '')
#filename_LR=paste('Liquidation Rate/',filename_LR,'.xlsx',sep='')
write.xlsx(as.data.frame(Pri), file = filename_LR, sheetName = "WSL_WA_Pri", 
           col.names = TRUE, row.names = FALSE, append = T)
write.xlsx(as.data.frame(Income), file = filename_LR, sheetName = "WSL_WA_Income", 
           col.names = TRUE, row.names = T, append = T)

################################################################################
################################################################################
#Vertical WA



Loan=sqlQuery(conn1,paste(
  
  "SELECT leads.loan_number,leads.lead_status_id,leads.payment_schedule_id,leads.id",
  "FROM leads",
  "WHERE leads.lead_status_id IN (11010,11011,11014,11015,11016,11021,11027,11029)",
  "AND leads.portfolio_id ='19007'",
  "AND leads.is_using_payment_processing=1"
))
Loan=Loan[!duplicated(Loan$loan_number),]



pp_in_collection1 = sqlQuery(conn1,"SELECT loan_number, lead_id, lead_changes.field, convert_tz(lead_changes.created_at,'GMT','America/New_York') as incollectiondate, notes
                            from lead_changes, leads 
                             where leads.id = lead_changes.lead_id 
                             and lead_changes.field = 'collection_status_id' 
                             and new_value IN (11034)
                             and old_value  IN (11033)
                             and leads.portfolio_id IN ('19007')
                             and is_using_payment_processing = 1")



a=pp_in_collection1



a=a%>%group_by(loan_number)%>%dplyr::slice(which.min(incollectiondate))


Loan=Loan[Loan$loan_number%in%a$loan_number,]
Loan$loan_number=as.character(Loan$loan_number)
a$loan_number=as.character(a$loan_number)
Loan=merge(Loan,a,by.x='loan_number',by.y='loan_number',all.x=T)

TT1=(split(Loan, ((1:length(Loan$loan_number))-1) %/% 800))


for (i in 1:length(TT1))
  
{
  l=c(TT1[[i]]$payment_schedule_id)
  print(i)
  A=sqlQuery(conn1,paste("select payment_sid,collection_provider_id,amount,payment_line_type_id,effective_date,payment_schedule_id,principal_change,nsf_fee,late_fee,finance_fee,unpaid_finance_fee,payment_status_id,payment_mode_id ,interest_to_waive,ach_endpoint_id from payment_schedule_lines where payment_schedule_id in ('",paste0(paste(l,collapse="','"),"')"),"",sep = ""))
  print(i)
  if (i==1)
  {
    Payment=A
  }else{
    Payment=rbind(Payment,A)}
}


ALL=merge(Payment,Loan,by='payment_schedule_id',all.x=T)


#ALL$payment_status_id[ALL$payment_status_id==16004]=16000


ALL$incollectiondate=as.Date(substr(ALL$incollectiondate,1,10))

ALL$effective_date=as.Date(substr(ALL$effective_date,1,10))

ALL=ALL[order(ALL$loan_number,ALL$effective_date),]

ALL$effective_date[ALL$payment_line_type_id==14000]=ALL$originated_at[ALL$payment_line_type_id==14000]



ALL$amount=as.numeric(ALL$amount)




ALL$amount=as.numeric(ALL$amount)
ALL$finance_fee=as.numeric(ALL$finance_fee)
ALL$unpaid_finance_fee=as.numeric(ALL$unpaid_finance_fee)
ALL$nsf_fee=as.numeric(ALL$nsf_fee)
ALL$interest_to_waive=as.numeric(ALL$interest_to_waive)
ALL$late_fee=as.numeric(ALL$late_fee)

ALL$principal_change=as.numeric(ALL$principal_change)


ALL$amount[ALL$payment_line_type_id==14002]=-ALL$amount[ALL$payment_line_type_id==14002]
ALL$finance_fee[ALL$payment_line_type_id==14002]=-ALL$finance_fee[ALL$payment_line_type_id==14002]
ALL$unpaid_finance_fee[ALL$payment_line_type_id==14002]=-ALL$unpaid_finance_fee[ALL$payment_line_type_id==14002]
ALL$interest_to_waive[ALL$payment_line_type_id==14002]=-ALL$interest_to_waive[ALL$payment_line_type_id==14002]
ALL$late_fee[ALL$payment_line_type_id==14002]=-ALL$late_fee[ALL$payment_line_type_id==14002]
ALL$nsf_fee[ALL$payment_line_type_id==14002]=-ALL$nsf_fee[ALL$payment_line_type_id==14002]
ALL$principal_change[ALL$payment_line_type_id==14002]=-ALL$principal_change[ALL$payment_line_type_id==14002]


library(tidyverse)
v=c('finance_fee','unpaid_finance_fee','interest_to_waive','amount','principal_change','late_fee','nsf_fee')

for (x in v ){
  
  ALL[,x][is.na(ALL[,x])]=0
  
  
}



ALL$incollectiondate=as.Date(ALL$incollectiondate)
ALL1=ALL[which(ALL$effective_date>=ALL$incollectiondate),]
ALL1$amount=as.numeric(ALL1$amount)
ALL1=ALL1[ALL1$payment_status_id==16000,]

ALL2=ALL[which(ALL$payment_line_type_id==14000 | ALL$effective_date<ALL$incollectiondate),]
ALL2=ALL2[ALL2$payment_status_id==16000,]
ALL2$principal=as.numeric(ALL2$principal)
ALL2=ALL2%>%group_by(loan_number)%>%dplyr::summarise(Outstanding=sum(principal[payment_line_type_id==14000])-sum(principal[payment_line_type_id!=14000]),incollectiondate=first(incollectiondate))



Pri=ALL2%>%group_by(substr(incollectiondate,1,7))%>%dplyr::summarise(Outstanding=sum(Outstanding,na.rm=TRUE), n= length(loan_number))




ALL1$Ori=substr(ALL1$incollectiondate,1,7)
ALL1$IsOrigination=0
ALL1$IsOrigination[ALL1$transaction_type==14000]=1
ALL1$IsDebit=0
ALL1$IsDebit[ALL1$IsDebit!=14000]=1 
ALL1$Pay=substr(ALL1$effective_date,1,7)

# Principle=ALL1%>%group_by(Ori,application_id)%>%dplyr::summarise(Pri=sum(first(Outstanding),na.rm=T))
# Principle=Principle%>%group_by(Ori)%>%dplyr::summarise(Pri=sum(Pri,na.rm=T), n =length(application_id))

Income=as.data.frame.matrix(xtabs(IsDebit*amount~ Pay + Ori, ALL1))
Income = t(Income)
filename_LR=paste('BDL_WA_',as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),sep = '')
filename_LR=paste('Liquidation Rate/',filename_LR,'.xlsx',sep='')
write.xlsx(as.data.frame(Pri), file = filename_LR, sheetName = "Pri", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
write.xlsx(as.data.frame(Income), file = filename_LR, sheetName = "Income", 
           col.names = TRUE, row.names = T, append = TRUE)


################################################################################
