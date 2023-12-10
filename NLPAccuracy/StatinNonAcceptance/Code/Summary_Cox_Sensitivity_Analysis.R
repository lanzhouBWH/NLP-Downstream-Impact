####### Loading Required Libraries #############################################
library(haven)
library(survival)
library(ggplot2)
library(ggpubr)
library(Publish)
library(openxlsx2)
################################################################################


####### Defining Paths #########################################################
Data_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Data/"
Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Output/"
Code_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Code/"
################################################################################

####### Loading the data #######################################################
data <- read_sas(paste(Data_Path,"statindeclinetbl15wrace.sas7bdat",sep=""), NULL)
################################################################################

N.VAR=24
Variable_Name=c("Age", "Study Entry Year", "Baseline LDL","Median Household Income" , "Charlson Comorbidity Index", "Number of Drug Allergies",
                "English as Primary Language", "Married", "Govornment Insurance", "Smoker", "Ezetimibe Use", "CAD", "CVA", "PVD", "DM", "History of LDL>190 mg/dl",
                "Family History of DM",
                "Family History of CVD", "Female", "Statin Non-Acceptance", "RaceAsian", "RaceBlack" ,"RaceOther", "RaceHispanic")
Plot_List=list()
Non_Clinical_index=c(1,4,10,19,2,21:24,7,8,9)
NLP=20
Clinical_index=setdiff(1:24,c(Non_Clinical_index,NLP))

####### Original Fitting Result ###################################################
data$event=(data$daysToLdl100Censored==0)*1 ### Note R need event variable to be in the model
Ori_fit<-coxph(Surv(daysToLdl100,event) ~ studyEntryAge +dateStudyEntryYear+
             baselineLdl10 +demoIncome10k +cci+
             demoDrugAllergy +demoEnglish+ demoMarried +
             demoGovIns +demoSmoker+ ezetimibeRx +diagCAD+
             diagCVA+ diagPVD+ diagDM+ diagLDL+ demoFamilyDM+
             demoFamilyCVD+demoFemale+ statinDeclined+
             RaceAsian +RaceBlack+ RaceOther+ RaceHispanic, data,ties=c("breslow"),id=statinRecommenderProviderId,na.action = na.omit) 
result=cbind(Variable_Name,publish(Ori_fit)$regressionTable[,3:5])
################################################################################

###### Simulation Fitting Result ###############################################
Sim_fit=list()
for(REP in 1:500){
  remove(fit)
  load(file=paste(Output_Path,"Cox_Sensitivity_",REP,".Rdata",sep=""))
  Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Output/"
  Sim_fit[[REP]]<-fit
}
################################################################################




#### Hazard Ratio Summary ######################################################
HR_data=data.frame(HR=NULL,Name=NULL,Original=NULL)
for(v in 1:24 ){
  SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v]),Original=exp(Ori_fit$coefficients[v]),Name=Variable_Name[v])
  HR_data=rbind(HR_data,SimHR)
}

Perc_Change=sapply(1:24, function(i) mean((HR_data$HR[HR_data$Name==Variable_Name[i]]-HR_data$Original[HR_data$Name==Variable_Name[i]])/
                                            HR_data$Original[HR_data$Name==Variable_Name[i]],na.rm=TRUE) )
result$Perc_Change=Perc_Change

i=20
quantile((HR_data$HR[HR_data$Name==Variable_Name[i]]-HR_data$Original[HR_data$Name==Variable_Name[i]])/
           HR_data$Original[HR_data$Name==Variable_Name[i]])

tiff(paste(Output_Path, "statin.tiff",sep=""),width=900,height=900)
ggplot(HR_data, aes(y=HR,fill=Name)) + theme(
  axis.text.x=element_blank() ,
  #axis.title.x = element_text(color="blue", size=10, face="bold"),
  axis.text.y = element_text(size = 20),
  axis.title.y = element_text(color="#993333", size=20, face="bold")
)+theme(legend.position = "none")+geom_boxplot()+ylab("Hazard Ratio")+geom_hline(aes(yintercept=Original), linetype="dashed", color = "red", size=1)+
  facet_wrap(~Name,ncol = 4,labeller = labeller(Name = label_wrap_gen(width = 20)),scales = "free")+
  theme(
    strip.text.x = element_text(
      size =15
    ))
dev.off()
################################################################################


#### P-values #####
Pvalue_data=data.frame(Pvalue=NULL,Name=NULL,Original=NULL)
for(v in 1:24){
  SimPvalue=data.frame(Pvalue=sapply(1:500,function(REP) summary(Sim_fit[[REP]])$coefficients[v,"Pr(>|z|)"]),
                       Name=Variable_Name[v],Original=summary(Ori_fit)$coefficients[v,"Pr(>|z|)"])
  
  Pvalue_data=rbind(Pvalue_data,SimPvalue)
  
}

freq=sapply(1:24, function(i) mean(Pvalue_data$Pvalue[Pvalue_data$Name==Variable_Name[i]]<0.05) )
result$significance_freq=freq
write_xlsx(result,file=paste(Output_Path, "ori_table.xlsx",sep=""))
save(result,file=paste(Output_Path, "table.Rdata",sep=""))
################################################################################






# 
# 
# #### Hazard Ratio #####
# for(v in Non_Clinical_index ){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="blue", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("Hazard Ratio")+geom_hline(yintercept=exp(Ori_fit$coefficients[v]), linetype="dashed", 
#                                                    color = "red", size=1)
# }
# 
# for(v in Clinical_index ){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="darkgreen", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("Hazard Ratio")+geom_hline(yintercept=exp(Ori_fit$coefficients[v]), linetype="dashed", 
#                                                                           color = "red", size=1)
# }
# 
# for(v in NLP){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="purple", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("Hazard Ratio")+geom_hline(yintercept=exp(Ori_fit$coefficients[v]), linetype="dashed", 
#                                                                           color = "red", size=1)
# }
# 
# Plot_List_New=lapply(c(Non_Clinical_index,Clinical_index,NLP), function(i) Plot_List[[i]])
# ggarrange(plotlist = Plot_List_New,nrow=6,ncol=4)
# 
# 
# 
# #### Percentage Change in Hazard Ratio #####
# for(v in Non_Clinical_index ){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v])/exp(Ori_fit$coefficients[v]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="blue", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("Percentage Change")+geom_hline(yintercept=1, linetype="dashed", 
#                                                                                color = "red", size=1)
# }
# 
# for(v in Clinical_index ){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v])/exp(Ori_fit$coefficients[v]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="darkgreen", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("Percentage Change")+geom_hline(yintercept=1, linetype="dashed", 
#                                                                                color = "red", size=1)
# }
# 
# for(v in NLP){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v])/exp(Ori_fit$coefficients[v]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="purple", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("Percentage Change")+geom_hline(yintercept=1, linetype="dashed", 
#                                                                                color = "red", size=1)
# }
# 
# Plot_List_New=lapply(c(Non_Clinical_index,Clinical_index,NLP), function(i) Plot_List[[i]])
# ggarrange(plotlist = Plot_List_New,nrow=4,ncol=6)
# 
# 
# 
# #### P-values #####
# for(v in Non_Clinical_index ){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) summary(Sim_fit[[REP]])$coefficients[v,"Pr(>|z|)"]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="blue", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("P-value")+geom_hline(yintercept=summary(Ori_fit)$coefficients[v,"Pr(>|z|)"], linetype="dashed", 
#                                                                           color = "red", size=1)
# }
# 
# for(v in Clinical_index ){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) summary(Sim_fit[[REP]])$coefficients[v,"Pr(>|z|)"]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="darkgreen", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("P-value")+geom_hline(yintercept=summary(Ori_fit)$coefficients[v,"Pr(>|z|)"], linetype="dashed", 
#                                                                           color = "red", size=1)
# }
# 
# for(v in NLP){
#   SimHR=data.frame(HR=sapply(1:500,function(REP) summary(Sim_fit[[REP]])$coefficients[v,"Pr(>|z|)"]),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="purple", size=10, face="bold"),
#     axis.title.y = element_text(color="#993333", size=10, face="bold")
#   )+scale_x_discrete(labels = function(x) 
#     stringr::str_wrap(x, width = 15))+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("P-value")+geom_hline(yintercept=summary(Ori_fit)$coefficients[v,"Pr(>|z|)"], linetype="dashed", 
#                                                                           color = "red", size=1)
# }
# 
# Plot_List_New=lapply(c(Non_Clinical_index,Clinical_index,NLP), function(i) Plot_List[[i]])
# ggarrange(plotlist = Plot_List_New,nrow=6,ncol=4)
# 
# 
# 
# ################################################################################
# 
# 
