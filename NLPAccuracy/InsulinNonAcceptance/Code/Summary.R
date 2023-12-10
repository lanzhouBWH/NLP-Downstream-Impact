####### Loading Required Libraries #############################################
library(haven)
library(survival)
library(ggplot2)
library(ggpubr)
library(Publish)
library(openxlsx2)
################################################################################


####### Defining Paths #########################################################
Data_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/InsulinNonAcceptance/Data/"
Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/InsulinNonAcceptance/Output/"
Code_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/InsulinNonAcceptance/Code/"
################################################################################

####### Loading the data #######################################################
data <- read_sas(paste(Data_Path,"prscore.sas7bdat",sep=""), NULL)
data2 <- read_sas(paste(Data_Path,"prscoreshort.sas7bdat",sep=""), NULL)
################################################################################

####### Original Fitting Result ###################################################
Variable_Name=c("Age, per 10-year increase",
                "Female Sex",
                "Government Insurance",
                "Median household income by zip code, per $10 000 increase",
                "Married",
                "White race (vs. all non-whites)",
                "English is the primary Language",
                "Year of study entry",
                "Presence of diabetes complications",
                "Charlson Co-morbidity Index",
                "BMI",
                "Treatment by an endocrinologist",
                "Number of non-insulin diabetes medications",
                "Basline hA1c",
                "Insulin therapy Decline")
data$event=(data$Censored==0)*1 ### Note R need event variable to be in the model
Ori_fit<-coxph(Surv(TimeToA1cControl,event) ~ age+Female+GovIns+income1000+Married+White+EngSp+StudyYear+DMComplications+CCI+BMIDot+
                  EndoDmClinic+ NumMeds +BaselineA1c+insDecline
                , weights=InvProbability,data,na.action = na.omit) 
result=cbind(Variable_Name,publish(Ori_fit)$regressionTable[,3:5])

################################################################################

###### Simulation Fitting Result ###############################################
Sim_fit=list()
for(REP in 1:500){
  remove(fit)
  load(file=paste(Output_Path,"Cox_Sensitivity_",REP,".Rdata",sep=""))
  Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/InsulinNonAcceptance/Output/"
  Sim_fit[[REP]]<-fit_prop
}
################################################################################

##### Visualize if point-wise estimation is accurate ############################
N.VAR=15
Plot_List=list()
Non_Clinical_index=1:8
NLP=15
Clinical_index=setdiff(1:15,c(Non_Clinical_index,NLP))


#### Hazard Ratio Summary ######################################################
HR_data=data.frame(HR=NULL,Name=NULL,Original=NULL)
for(v in 1:15 ){
  SimHR=data.frame(HR=sapply(1:500,function(REP) exp(Sim_fit[[REP]]$coefficients)[v]),Original=exp(Ori_fit$coefficients[v]),Name=Variable_Name[v])
  HR_data=rbind(HR_data,SimHR)
}

Perc_Change=sapply(1:15, function(i) mean((HR_data$HR[HR_data$Name==Variable_Name[i]]-HR_data$Original[HR_data$Name==Variable_Name[i]])/
                                            HR_data$Original[HR_data$Name==Variable_Name[i]],na.rm=TRUE) )
result$Perc_Change=Perc_Change

i=15
quantile((HR_data$HR[HR_data$Name==Variable_Name[i]]-HR_data$Original[HR_data$Name==Variable_Name[i]])/
           HR_data$Original[HR_data$Name==Variable_Name[i]])

tiff(paste(Output_Path, "insulin.tiff",sep=""),width=900,height=900)
ggplot(HR_data, aes(y=HR,fill=Name)) + theme(
  axis.text.x=element_blank() ,
  #axis.title.x = element_text(color="blue", size=10, face="bold"),
  axis.text.y = element_text(size = 20),
  axis.title.y = element_text(color="#993333", size=20, face="bold")
)+theme(legend.position = "none")+geom_boxplot()+ylab("Hazard Ratio")+geom_hline(aes(yintercept=Original), linetype="dashed", color = "red", size=1)+
  facet_wrap(~Name,nrow = 4,labeller = labeller(Name = label_wrap_gen(width = 20)),scales = "free")+
  theme(
    strip.text.x = element_text(
      size = 15
    ))
dev.off()
################################################################################


#### P-values #####
Pvalue_data=data.frame(Pvalue=NULL,Name=NULL,Original=NULL)
for(v in 1:15){
  SimPvalue=data.frame(Pvalue=sapply(1:500,function(REP) summary(Sim_fit[[REP]])$coefficients[v,"Pr(>|z|)"]),
                   Name=Variable_Name[v],Original=summary(Ori_fit)$coefficients[v,"Pr(>|z|)"])
  
  Pvalue_data=rbind(Pvalue_data,SimPvalue)
  
}

freq=sapply(1:15, function(i) mean(Pvalue_data$Pvalue[Pvalue_data$Name==Variable_Name[i]]<0.05) )
result$significance_freq=freq
write_xlsx(result,file=paste(Output_Path, "ori_table.xlsx",sep=""))
save(result,file=paste(Output_Path, "table.Rdata",sep=""))
################################################################################

