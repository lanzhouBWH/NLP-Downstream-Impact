####### Loading Required Libraries #############################################
library(haven)
library(lme4)
library(ggplot2)
library(ggpubr)
library(Publish)
library(openxlsx2)
################################################################################

####### Defining Paths #########################################################
Data_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Data/"
Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Output/"
Code_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Code/"
################################################################################

####### Loading the data #######################################################
data <- read_sas(paste(Data_Path,"output_project135ad_annualized_1.sas7bdat",sep=""), NULL)
################################################################################

N.VAR=15
Variable_Name=c("Female",  "Age at study entry","Race-White" , "Married", "BMI-Baseline",
                "English as Primary Language", "Nongovernment insurance","Median household income by zip code ($1,000)",
                "Charlson Co-morbidity Index","Diabetes","Hypertension","Coronary artery disease","Stroke","Smoking",
                "Receiving bariatric surgery discussion within 1 year of study entry")


####### Original Fitting Result ###################################################
Ori_fit<-glm(SecondaryOutcome_BS_Project135ad ~ Female +AgeAtStudyEntry+ RaceWhite+ Married+ 
               BMIbaseline+ EnglishPrimaryLanguage+ GovernmentInsurance+ Income1000+ CCI+ DM+ HTN+ CAD+ CVA+ 
               Smoking+ Predictor_DiscBin_Project135ad , data = data,family = binomial(link = "logit"))
result=cbind(Variable_Name,publish(Ori_fit)$regressionTable[,c(3,5)])
################################################################################

###### Simulation Fitting Result ###############################################
Sim_fit=list()
for(REP in 1:500){
  remove(fit)
  load(file=paste(Output_Path,"Surgery_Sensitivity_",REP,".Rdata",sep=""))
  Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Output/"
  Sim_fit[[REP]]<-fit
}
################################################################################



#### Fixed Effect Summary ######################################################
HR_data=data.frame(HR=NULL,Name=NULL,Original=NULL)
for(v in 1:15 ){
  SimHR=data.frame(HR=sapply(1:500,function(REP) exp(coef(Sim_fit[[REP]])[v+1])), Original=exp(coef(Ori_fit)[v+1]),Name=Variable_Name[v])
  HR_data=rbind(HR_data,SimHR)
}

Perc_Change=sapply(1:15, function(i) mean((HR_data$HR[HR_data$Name==Variable_Name[i]]-HR_data$Original[HR_data$Name==Variable_Name[i]])/
                                            HR_data$Original[HR_data$Name==Variable_Name[i]],na.rm=TRUE) )
result$Perc_Change=Perc_Change

i=15
quantile((HR_data$HR[HR_data$Name==Variable_Name[i]]-HR_data$Original[HR_data$Name==Variable_Name[i]])/
           HR_data$Original[HR_data$Name==Variable_Name[i]])

tiff(paste(Output_Path, "Surgery.tiff",sep=""),width=900,height=900)
ggplot(HR_data, aes(y=HR,fill=Name)) + theme(
  axis.text.x=element_blank() ,
  #axis.title.x = element_text(color="blue", size=10, face="bold"),
  axis.text.y = element_text(size = 20),
  axis.title.y = element_text(color="#993333", size=20, face="bold")
)+geom_boxplot()+ylab("Odds Ratio")+geom_hline(aes(yintercept=Original), linetype="dashed", color = "red", size=1)+theme(legend.position = "none")+
  facet_wrap(~Name,nrow = 4,labeller = labeller(Name = label_wrap_gen(width = 20)),scales = "free")+
  theme(
    strip.text.x = element_text(
      size = 15
    ))
dev.off()
################################################################################

#### P-values #####
Pvalue_data=data.frame(Pvalue=NULL,Name=NULL,Original=NULL)
for(v in 1:15 ){
  SimPvalue=data.frame(Pvalue=sapply(1:500,function(REP) summary(Sim_fit[[REP]])$coefficients[v+1,"Pr(>|z|)"]),
                       Name=Variable_Name[v],Original=summary(Ori_fit)$coefficients[v+1,"Pr(>|z|)"])
  
  Pvalue_data=rbind(Pvalue_data,SimPvalue)
  
}
freq=sapply(1:15, function(i) mean(Pvalue_data$Pvalue[Pvalue_data$Name==Variable_Name[i]]<0.05,na.rm=TRUE) )
result$significance_freq=freq
write_xlsx(result,file=paste(Output_Path, "Surgery_ori_table.xlsx",sep=""))
save(result,file=paste(Output_Path, "table_Surgery.Rdata",sep=""))
################################################################################






# for(v in 1:N.VAR){
#   
#   SimHR=data.frame(HR=sapply(1:500,function(REP)  exp(coef(Sim_fit[[REP]])[v+1])),Name=Variable_Name[v])
#   Plot_List[[v]]=ggplot(SimHR, aes(x=Name, y=HR)) + theme(
#     axis.text.x=element_blank() ,
#     axis.title.x = element_text(color="blue", size=8, face="bold"),
#     axis.title.y = element_text(color="#993333", size=8, face="bold")
#   )+
#     geom_boxplot()+xlab(Variable_Name[v])+ylab("Odds Ratio")+geom_hline(yintercept=exp(coef(Ori_fit)[v+1]), linetype="dashed", 
#                                                                           color = "red", size=2)
# }
# 
# ggarrange(plotlist=Plot_List,nrow = 4,ncol=4)
# 

