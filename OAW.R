SWB=read.csv('COVID19FieldDeployer_DATA_2022-03-04_1513.csv')
EOC=read.csv("HQ_OperationAlliesWelco_DATA_2022-03-04_1551.csv")#EOC
Field=read.csv('OperationAlliesWelco_DATA_2022-03-04_1552.csv')#Field
source("COVID19FieldDeployer_R_2022-03-04_1513.r")
source("HQ_OperationAlliesWelco_R_2022-03-04_1551.r")
source("OperationAlliesWelco_R_2022-03-04_1552.r")


library(fs)
library(dplyr)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(gmodels)
library(tidyr)




EOC %>% filter(responder_survey_complete.factor=='Complete')%>%
  count()

EOC %>% filter(!is.na(today))%>%
  count()


Field %>% filter(responder_survey_complete.factor=='Complete')%>%
  count()

Field %>% filter(!is.na(today))%>%
  count()

 
Field<- Field %>% 
  mutate(CompDate=substr(responder_survey_timestamp,1,10),CompDate=as.Date(CompDate),
         CompMonth=month(CompDate),sentMonth=month(today)) %>%
  filter(responder_survey_complete.factor=='Complete')

EOC<- EOC %>%  
  filter(responder_survey_timestamp!="[not completed]")%>%
  separate(responder_survey_timestamp, into=c('CompDate', 'Time'), sep=' ') %>%
  mutate(CompDate=as.Date(CompDate,"%m/%d/%y"),
         today=as.Date(today,format = "%m/%d/%y"),
    CompMonth=month(CompDate),sentMonth=month(today)) %>%
  filter(responder_survey_complete.factor=='Complete')

#Survey Completed
SWB %>% 
  mutate(CompDate=substr(responder_survey_timestamp,1,10),CompDate=as.Date(CompDate),
         CompMonth=month(CompDate),
         swb=grepl("SWB|Southwest Border Migrant Health Task Force",dtname)) %>%
   filter(swb=='TRUE',responder_survey_complete.factor=='Complete') %>%
  count(responder_survey_complete.factor) #93
 

#Survey Sent
SWB %>% 
  mutate(CompDate=substr(responder_survey_timestamp,1,10),
         CompDate=as.Date(CompDate),
        CompMonth=month(CompDate),
        swb=grepl("SWB|Southwest Border Migrant Health Task Force",dtname)) %>%
  filter(swb=='TRUE') %>%
  filter(!is.na(today)) %>%
  count()#156

 

 
SWB<- SWB %>% 
  mutate(CompDate=substr(responder_survey_timestamp,1,10),CompDate=as.Date(CompDate),
         CompMonth=month(CompDate),
         swb=grepl("SWB|Southwest Border Migrant Health Task Force",dtname)) %>%
         filter(swb=='TRUE',responder_survey_complete.factor=='Complete')

 

# freq function  
f<- function(data,var){
  data%>%filter(responder_survey_complete.factor=='Complete')%>%select(var)%>%tbl_summary()
}


#freq function by one variable
p<- function(s){
round(prop.table(table(s)),2)*100 
}

p(EOC$support01.factor)
 

 

round(prop.table(table(EOC$stress01)))

#freq function by two variable
prop_month<- function(a,b){
  round(addmargins(prop.table(table(a, b))),2)*100
}
#call the function 
prop_month(Field$team02,Field$CompMonth) 

Field %>% filter(responder_survey_complete.factor=='Complete',!is.na(cio))%>%
  select(cio) %>%
  count(cio)%>%
  mutate(prop=round(n/sum(n),2)) 


f(Field,'firstfieldresp')
f(EOC,'firsteocresp')

f(Field,'cio')
f(EOC,'cio')

#Employee Type 
f(Field,'personnel')
f(EOC,'personnel')

source<- paste0('source___',1:9)
f(Field,source)
f(EOC,source)

f(Field,"job_original.factor")
f(EOC,"job_original.factor")

summary(Field$personksa)
Field %>% filter(responder_survey_complete.factor=='Complete',!is.na(personksa))%>%
  select(personksa) %>%
  count()

summary(EOC$personksa)
EOC %>% filter(responder_survey_complete.factor=='Complete',!is.na(personksa))%>%
  select(personksa) %>%
  count()

f(Field,"comm01")
f(Field,"comm02")
f(Field,"comm03")
f(Field,"comm05")
f(Field,"comm06")
f(Field,"comm08")
f(Field,"comm09")
f(EOC,"comm03")
f(Field,"comm10")




f(Field,"travel01.factor")
f(Field,"travel02.factor")
f(Field,"ohs03")
f(Field,"ohs02")


#Equipment
f(Field,"equip")
f(Field,"equip02.factor")
f(Field,"equip03.factor")


f(Field,"ppe01___1")
f(Field,"ppe01___2")
f(Field,"ppe01___3")
f(Field,"ppe01___4")
f(Field,"ppe01___5")
f(Field,"ppe01___6")
f(Field,"ppe01___7")
f(Field,"ppe01___8")

ppe02<- paste0("ppe02___",1:8)
f(Field,ppe02)

ppe03<- paste0("ppe03___",1:8)
f(Field,ppe03)

ppe04<- paste0("ppe04___",1:8)
f(Field,ppe04)

ppe05<- paste0("ppe05___",1:8)
f(Field,ppe05)

ppe06<- paste0("ppe06___",1:8)
f(Field,ppe06)

ppe07<- paste0("ppe07___",1:8)
f(Field,ppe07)

ppe08<- paste0("ppe08___",1:8)
f(Field,ppe08)


f(Field,'hlthprep___1')
f(Field,'hlthprep___2')
f(Field,'hlthprep___3')
f(Field,'hlthprep___4')
f(Field,'hlthprep___9')

f(Field,"incidentreport___1") 
f(Field,"incidentreport___2") 
f(Field,"incidentreport___3") 
f(Field,"incidentreport___4") 
f(Field,"incidentreport___5") 


f(Field,"incidentaddress.factor")
Field$unsafe___1


f(Field,"unsafe___1")
f(Field,"unsafe___2")
f(Field,"unsafe___3")
f(Field,"unsafe___4")
f(Field,"unsafe___9")


#During
during<- paste0("during0",1:8)
f(Field,during)
f(EOC,during)


f(Field,"jobprep.factor")
f(EOC,"jobprep.factor")


#Preparedness
f(Field,"jobprep2.factor")
#f(EOC,"jobprep2.factor") #item doesn't exist

commtf<- paste0('commtf_feel___',1:6)
f(Field,commtf)
f(EOC,commtf)


commtl<- paste0('commtl_feel___',1:6)
f(Field,commtl)
f(EOC,commtl)


f(Field,"team01.factor")
f(EOC,"team01.factor")
f(Field,"team02.factor")
f(EOC,"team02.factor")

poc<- paste0('poc___',1:6)
f(Field,poc)
f(Field,'poc___9')

stress<- paste0('stress0',1:9)
f(Field,stress)
f(EOC,stress)
 
library(gmodels)

chisq.test(Field$stress01,Field$CompMonth)
chisq.test(Field$stress02,Field$CompMonth)
chisq.test(Field$stress03,Field$CompMonth)
chisq.test(Field$stress04,Field$CompMonth)
chisq.test(Field$stress05,Field$CompMonth)
chisq.test(Field$stress06,Field$CompMonth)
chisq.test(Field$stress07,Field$CompMonth)
chisq.test(Field$stress08,Field$CompMonth)
chisq.test(Field$stress09,Field$CompMonth)


chisq.test(EOC$stress01,EOC$CompMonth)
chisq.test(EOC$stress02,EOC$CompMonth)
chisq.test(EOC$stress03,EOC$CompMonth)
chisq.test(EOC$stress04,EOC$CompMonth)
chisq.test(EOC$stress05,EOC$CompMonth)
chisq.test(EOC$stress06,EOC$CompMonth)
chisq.test(EOC$stress07,EOC$CompMonth)
chisq.test(EOC$stress08,EOC$CompMonth)
chisq.test(EOC$stress09,EOC$CompMonth)


EOC1<-EOC%>%
  select(stress,CompMonth)
join<-Field %>%
  select(stress,CompMonth) %>%
  rbind(EOC1)

chisq.test(join$stress01,join$CompMonth)
chisq.test(join$stress02,join$CompMonth)
chisq.test(join$stress03,join$CompMonth)
chisq.test(join$stress04,join$CompMonth)
chisq.test(join$stress05,join$CompMonth)
chisq.test(join$stress06,join$CompMonth)
chisq.test(join$stress07,join$CompMonth)
chisq.test(join$stress08,join$CompMonth)
chisq.test(join$stress09,join$CompMonth)


support<- paste0("support0",1:6)
support1<-c("support01","support02","support03","support04","support05")

EOC2<-EOC%>%
  select(support1,CompMonth)

colnames(EOC2)[colnames(EOC2) %in% c("support03","support04","support05")] <- c("support04","support05","support06")
EOC2$support03<- NA

EOC2<- EOC2 %>% 
  select(support,CompMonth)

joins<-Field %>%
  select(support,CompMonth) %>%
  rbind(EOC2)

  


chisq.test(join$stress01,join$CompMonth)
chisq.test(join$stress02,join$CompMonth)
chisq.test(join$stress03,join$CompMonth)
chisq.test(join$stress04,join$CompMonth)
chisq.test(join$stress05,join$CompMonth)
chisq.test(join$stress06,join$CompMonth)
chisq.test(join$stress07,join$CompMonth)
chisq.test(join$stress08,join$CompMonth)
chisq.test(join$stress09,join$CompMonth)

 



chisq.test(joins$support01,joins$CompMonth)
chisq.test(joins$support02,joins$CompMonth)
chisq.test(joins$support03,joins$CompMonth)
chisq.test(joins$support04,joins$CompMonth)
chisq.test(joins$support05,joins$CompMonth)
chisq.test(joins$support06,joins$CompMonth)
 
 


library(gmodels)
chisq.test(SWB$stress01,SWB$CompMonth)

round(prop.table(table(Field$support01.factor)),2)*100
round(prop.table(table(Field$support02.factor)),2)*100
round(prop.table(table(Field$support03.factor)),2)*100
round(prop.table(table(Field$support04.factor)),2)*100
round(prop.table(table(Field$support05.factor)),2)*100
round(prop.table(table(Field$support06.factor)),2)*100

 

round(prop.table(table(EOC$support01.factor)),2)*100
round(prop.table(table(EOC$support02.factor)),2)*100
round(prop.table(table(EOC$support03.factor)),2)*100
round(prop.table(table(EOC$support04.factor)),2)*100
round(prop.table(table(EOC$support05.factor)),2)*100


round(prop.table(table(Field$extend.factor)),2)*100
round(prop.table(table(EOC$extend.factor)),2)*100 
 
round(prop.table(table(Field$postdep___1.factor)),2)*100
round(prop.table(table(Field$postdep___2.factor)),2)*100
round(prop.table(table(Field$postdep___3.factor)),2)*100

round(prop.table(table(EOC$postdep___1.factor)),2)*100
round(prop.table(table(EOC$postdep___2.factor)),2)*100
round(prop.table(table(EOC$postdep___3.factor)),2)*100
 

round(prop.table(table(Field$barrier___1.factor)),2)*100
round(prop.table(table(Field$barrier___2.factor)),2)*100
round(prop.table(table(Field$barrier___3.factor)),2)*100
round(prop.table(table(Field$barrier___4.factor)),2)*100
round(prop.table(table(Field$barrier___5.factor)),2)*100
round(prop.table(table(Field$barrier___6.factor)),2)*100
round(prop.table(table(Field$barrier___7.factor)),2)*100
round(prop.table(table(Field$barrier___8.factor)),2)*100
round(prop.table(table(Field$barrier___9.factor)),2)*100
round(prop.table(table(Field1$barrier___10.factor)),2)*100

 
 
round(prop.table(table(EOC$barrier___1.factor)),2)*100
round(prop.table(table(EOC$barrier___2.factor)),2)*100
round(prop.table(table(EOC$barrier___3.factor)),2)*100
round(prop.table(table(EOC$barrier___4.factor)),2)*100
round(prop.table(table(EOC$barrier___5.factor)),2)*100
round(prop.table(table(EOC$barrier___6.factor)),2)*100
round(prop.table(table(EOC$barrier___7.factor)),2)*100
round(prop.table(table(EOC$barrier___8.factor)),2)*100
round(prop.table(table(EOC$barrier___9.factor)),2)*100
round(prop.table(table(EOC$barrier___10.factor)),2)*100
 
 
f(Field,"depagain_covid.factor")
f(EOC,"depagain_covid.factor")
 


round(addmargins(prop.table(table(Field$team02.factor,Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$jobprep.factor,Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$jobprep2.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$comm01.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$comm06.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$comm08.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during01.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during02.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during03.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during04.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during05.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during06.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during07.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$during08.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$job_original.factor, Field$CompMonth))),2)*100

round(addmargins(prop.table(table(Field$job_original, Field$CompMonth))),2)*100

round(addmargins(prop.table(table(Field$support01.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$support02.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$support03.factor, Field$CompMonth))),2)*100
round(addmargins(prop.table(table(Field$support04.factor, Field$CompMonth))),2)*100

var<-c("jobprep","jobprep2","comm01","comm08","during01","during02",
       "during03","during04","during05","during06","during07","during08" ,
       "job_original","support01","support02","support03","support04")

#SWB
round(addmargins(prop.table(table(SWB$jobprep.factor,SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$jobprep2.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$comm01.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$comm08.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during01.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during02.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during03.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during04.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during05.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during06.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during07.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$during08.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$job_original.factor, SWB$CompMonth))),2)*100

round(addmargins(prop.table(table(SWB$job_original, SWB$CompMonth))),2)*100

round(addmargins(prop.table(table(SWB$support01.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$support02.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$support03.factor, SWB$CompMonth))),2)*100
round(addmargins(prop.table(table(SWB$support04.factor, SWB$CompMonth))),2)*100

#stress01-04 Field data

stress<-paste0('stress0',1:4)
stress.factor<- paste0(stress,'.factor')
fs0104<-Field %>% select(stress.factor,CompMonth)%>%
  pivot_longer(stress.factor)
 
m(fs0104,"value",9)
m(fs0104,"value",10)
m(fs0104,"value",11)
m(fs0104,"value",12)
m(fs0104,"value",1)
m(fs0104,"value",2)




#stress01-04  SWB data

ss0104<-SWB %>% select(stress.factor,CompMonth)%>%
  pivot_longer(stress.factor)

m(ss0104,"value",4)
m(ss0104,"value",5)
m(ss0104,"value",6)
m(ss0104,"value",7)
m(ss0104,"value",8)
m(ss0104,"value",9)
m(ss0104,"value",10)
m(ss0104,"value",11)
m(ss0104,"value",12)

##stress01-04  joined data
stress0104<- rbind(fs0104,ss0104)
chisq.test(stress0104$CompMonth,stress0104$value)


#stress05-09  Field data
stress1<-paste0('stress0',5:9)
stress1.factor<- paste0(stress1,'.factor')
s0509<-Field %>% select(stress1.factor,CompMonth)%>%
  pivot_longer(stress1.factor)
m(s0509,"value",9)
m(s0509,"value",10)
m(s0509,"value",11)
m(s0509,"value",12)
m(s0509,"value",1)
m(s0509,"value",2)




#stress05-09  SWB data

s0509_<-SWB%>% select(stress1.factor,CompMonth)%>%
  pivot_longer(stress1.factor)
 


m(s0509_,"value",4)
m(s0509_,"value",5)
m(s0509_,"value",6)
m(s0509_,"value",7)
m(s0509_,"value",8)
m(s0509_,"value",9)
m(s0509_,"value",10)
m(s0509_,"value",11)
m(s0509_,"value",12)

#stress05-09  joined data
stress0509<- rbind(s0509,s0509_)
chisq.test(stress0509$CompMonth,stress0509$value)

chisq.test(Field$jobprep,Field$CompMonth)
chisq.test(SWB$jobprep,SWB$CompMonth)


#jobprep
J<- Field %>% select(CompMonth,jobprep) 
job<- SWB %>%select(CompMonth,jobprep)%>%
   rbind(J)

chisq.test(job$jobprep,job$CompMonth)



Field1<- Field%>%
  select(var,CompMonth)
 
Joindata<- SWB%>%
  select(var,CompMonth)%>%
  rbind(Field1)



var<-c("jobprep","jobprep2","comm01","comm08","during01","during02",
       "during03","during04","during05","during06","during07","during08" ,
       "job_original","support01","support02","support03","support04")
chisq.test(Joindata$jobprep2,Joindata$CompMonth)
chisq.test(Joindata$comm01,Joindata$CompMonth)
chisq.test(Joindata$comm08,Joindata$CompMonth)
chisq.test(Joindata$during01,Joindata$CompMonth)
chisq.test(Joindata$during02,Joindata$CompMonth)
chisq.test(Joindata$during03,Joindata$CompMonth)
chisq.test(Joindata$during04,Joindata$CompMonth)
chisq.test(Joindata$during05,Joindata$CompMonth)
chisq.test(Joindata$during06,Joindata$CompMonth)
chisq.test(Joindata$during07,Joindata$CompMonth)
chisq.test(Joindata$during08,Joindata$CompMonth)



chisq.test(Joindata$job_original,Joindata$CompMonth)
chisq.test(Joindata$support01,Joindata$CompMonth)
chisq.test(Joindata$support02,Joindata$CompMonth)
chisq.test(Joindata$support03,Joindata$CompMonth)
chisq.test(Joindata$support04,Joindata$CompMonth)




m<- function(data,var,month){
  data%>%filter(CompMonth==month)%>%select(var)%>%tbl_summary()
}

m(Field,"team02.factor",9)
m(Field,"team02.factor",10)
m(Field,"team02.factor",11)
m(Field,"team02.factor",12)
m(Field,"team02.factor",1)
m(Field,"team02.factor",2)


m(Field,"jobprep.factor",9)
m(Field,"jobprep.factor",10)
m(Field,"jobprep.factor",11)
m(Field,"jobprep.factor",12)
m(Field,"jobprep.factor",1)
m(Field,"jobprep.factor",2)


m(Field,"jobprep2.factor",9)
m(Field,"jobprep2.factor",10)
m(Field,"jobprep2.factor",11)
m(Field,"jobprep2.factor",12)
m(Field,"jobprep2.factor",1)
m(Field,"jobprep2.factor",2)

m(Field,"comm01.factor",9)
m(Field,"comm01.factor",10)
m(Field,"comm01.factor",11)
m(Field,"comm01.factor",12)
m(Field,"comm01.factor",1)
m(Field,"comm01.factor",2)


var1<-c("team02.factor","jobprep.factor","jobprep2.factor","comm01.factor","comm06.factor","comm08.factor","during01.factor","during02.factor",
       "during03.factor","during04.factor","during05.factor","during06.factor","during07.factor","during08.factor" ,
       "job_original.factor","job_original","support01.factor","support02.factor","support03.factor","support04.factor")
m(Field,var1,9)
m(Field,var1,10)
m(Field,var1,11)
m(Field,var1,12)
m(Field,var1,1)
m(Field,var1,2)




m(SWB,var1,4)
m(SWB,var1,5)
m(SWB,var1,6)
m(SWB,var1,7)
m(SWB,var1,8)
m(SWB,var1,9)
m(SWB,var1,10)
m(SWB,var1,11)
m(SWB,var1,12)

 
