load("/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/InsulinNonAcceptance/Output/table.Rdata")
Study1=result[-15,]
Study1=result
load("/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Output/table_BMI.Rdata")
Study2A=result[-1,]
Study2A=result
load("/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/BariatricSurgery/Output/table_Surgery.Rdata")
Study2B=result[-15,]
Study2B=result
load("/data/cci_radiology/CCI/Alex Turchin/NLPAccuracy/StatinNonAcceptance/Output/table.Rdata")
Study3=result[-20,]
Study3=result

OutputTable1=data.frame('Reference'=c("Turchin et al., 2020 ","Chang et al., 2021","Chang et al., 2021","Brown et al., 2023"),
                        'Outcome'=c("the time to HbA1c control","change in BMI", "receipt of bariatric surgery","the time to LDL control"),
                        'Confounders'=c(14,14,14,23),	
                        'Changed direction	Mean percentage change of the point estimate (IQR)'=NA,
                        'Significance acquired, mean percentage simulations'=NA,
                        'Significance eliminated, mean percentage simulations'=NA)

OutputTable1$Changed.direction.Mean.percentage.change.of.the.point.estimate..IQR.[1]<-paste(round(mean(Study1$Perc_Change)*100,3),"% (",
                                                                                            round(quantile(Study1$Perc_Change,0.25)*100,3),"% , ",
                                                                                      round(quantile(Study1$Perc_Change,0.75)*100,3),"%)",
                                                                                       sep="")
OutputTable1$Changed.direction.Mean.percentage.change.of.the.point.estimate..IQR.[2]<-paste(round(mean(Study2A$Perc_Change)*100,3),"% (",
                                                                                            round(quantile(Study2A$Perc_Change,0.25)*100,3),"% , ",
                                                                                            round(quantile(Study2A$Perc_Change,0.75)*100,3),"%)",
                                                                                            sep="")
OutputTable1$Changed.direction.Mean.percentage.change.of.the.point.estimate..IQR.[3]<-paste(round(mean(Study2B$Perc_Change)*100,3),"% (",
                                                                                            round(quantile(Study2B$Perc_Change,0.25)*100,3),"% , ",
                                                                                            round(quantile(Study2B$Perc_Change,0.75)*100,3),"%)",
                                                                                            sep="")
OutputTable1$Changed.direction.Mean.percentage.change.of.the.point.estimate..IQR.[4]<-paste(round(mean(Study3$Perc_Change)*100,3),"% (",
                                                                                            round(quantile(Study3$Perc_Change,0.25)*100,3),"% , ",
                                                                                            round(quantile(Study3$Perc_Change,0.75)*100,3),"%)",
                                                                                            sep="")



