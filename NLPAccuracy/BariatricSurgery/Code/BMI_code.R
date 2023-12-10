REP=as.numeric(Sys.getenv('LSB_JOBINDEX'))
set.seed(REP)
####### Loading Required Libraries #############################################
library(haven)
library(survival)
library(lme4)
################################################################################


####### Defining Paths #########################################################
Data_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Data/"
Output_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Output/"
Code_Path="/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Code/"
################################################################################

####### Loading the data #######################################################
data <- read_sas(paste(Data_Path,"output_project135ad_annualized_1.sas7bdat",sep=""), NULL)
################################################################################


####### Manipulate data ########################################################
PPV=0.758
NPV=1

P.index=which(data$Predictor_DiscBin_Project135ad==1)
N.index=which(data$Predictor_DiscBin_Project135ad==0)

Choose.P=P.index[sapply(1:length(P.index),function(i) sample(c(0,1),1,c(PPV,1-PPV),replace=TRUE))==1]
Choose.N=N.index[sapply(1:length(N.index),function(i) sample(c(0,1),1,c(NPV,1-NPV),replace=TRUE))==1]

data$Predictor_DiscBin_Project135ad[Choose.P]=0
data$Predictor_DiscBin_Project135ad[Choose.N]=1
################################################################################


### Fit a mixed model
fit<-lmer(PrimaryOutcome_BMIchange ~ Predictor_DiscBin_Project135ad +Female+ 
          AgeAtStudyEntry +RaceWhite +Married+ BMIbaseline+ EnglishPrimaryLanguage+ 
          GovernmentInsurance+ Income1000+ CCI+ DM+ HTN+ CAD+ CVA+ Smoking+ (1 | provIDmostNotes), data = data)


save.image(file=paste(Output_Path,"BMI_Sensitivity_",REP,".Rdata",sep=""))

