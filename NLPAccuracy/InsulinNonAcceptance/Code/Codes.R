REP=as.numeric(Sys.getenv('LSB_JOBINDEX'))
set.seed(REP)
####### Loading Required Libraries #############################################
library(haven)
library(survival)
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

####### Manipulate data ########################################################
PPV=0.95
NPV=1

P.index=which(data$insDecline==1)
N.index=which(data$insDecline==0)

Choose.P=P.index[sapply(1:length(P.index),function(i) sample(c(0,1),1,c(PPV,1-PPV),replace=TRUE))==1]
Choose.N=N.index[sapply(1:length(N.index),function(i) sample(c(0,1),1,c(NPV,1-NPV),replace=TRUE))==1]

data$insDecline[Choose.P]=0
data$insDecline[Choose.N]=1
################################################################################


data$event=(data$Censored==0)*1 ### Note R need event variable to be in the model
fit_prop<-coxph(Surv(TimeToA1cControl,event) ~ age+Female+GovIns+income1000+Married+White+EngSp+StudyYear+DMComplications+CCI+BMIDot+
             EndoDmClinic+ NumMeds +BaselineA1c+insDecline
           , weights=InvProbability,data,ties=c("breslow"),na.action = na.omit) 

fit<-coxph(Surv(TimeToA1cControl,event) ~ age+Female+GovIns+income1000+Married+White+EngSp+StudyYear+DMComplications+CCI+BMIDot+
                  EndoDmClinic+ NumMeds +BaselineA1c+insDecline
                , data,ties=c("breslow"),na.action = na.omit) 

save.image(file=paste(Output_Path,"Cox_Sensitivity_",REP,".Rdata",sep=""))


