REP=as.numeric(Sys.getenv('LSB_JOBINDEX'))
set.seed(REP)
####### Loading Required Libraries #############################################
library(haven)
library(survival)
################################################################################


####### Defining Paths #########################################################
Data_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Data/"
Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Output/"
Code_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Code/"
################################################################################

####### Loading the data #######################################################
data <- read_sas(paste(Data_Path,"statindeclinetbl15wrace.sas7bdat",sep=""), NULL)
################################################################################

####### Manipulate data ########################################################
PPV=0.778
NPV=3949/(3949+5)

P.index=which(data$statinDeclined==1)
N.index=which(data$statinDeclined==0)

Choose.P=P.index[sapply(1:length(P.index),function(i) sample(c(0,1),1,c(PPV,1-PPV),replace=TRUE))==1]
Choose.N=N.index[sapply(1:length(N.index),function(i) sample(c(0,1),1,c(NPV,1-NPV),replace=TRUE))==1]

data$statinDeclined[Choose.P]=0
data$statinDeclined[Choose.N]=1

################################################################################


# Fit a Cox model 
data$event=(data$daysToLdl100Censored==0)*1 ### Note R need event variable to be in the model
fit<-coxph(Surv(daysToLdl100,event) ~ studyEntryAge +dateStudyEntryYear+
        baselineLdl10 +demoIncome10k +cci+
        demoDrugAllergy +demoEnglish+ demoMarried +
        demoGovIns +demoSmoker+ ezetimibeRx +diagCAD+
        diagCVA+ diagPVD+ diagDM+ diagLDL+ demoFamilyDM+
        demoFamilyCVD+demoFemale+ statinDeclined+
        RaceAsian +RaceBlack+ RaceOther+ RaceHispanic, data,ties=c("breslow"),id=statinRecommenderProviderId,na.action = na.omit) 

save.image(file=paste(Output_Path,"Cox_Sensitivity_",REP,".Rdata",sep=""))
