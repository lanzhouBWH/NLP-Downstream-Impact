# NLP-Downstream-Impact

Authors: Zhou Lan, PhD and Alexander Turchin, MD, MS

Instituite: Brigham and Women's Hospital, Harvard Medical School

This repository provides R codes (.R) and job scheduler codes (.lsf) for the Monte Carlo experiments to analyze Impact of Possible Errors in NLP-Derived Data on Downstream Epidemiologic Analysis. The codes are in the folder of "NLPAccuracy". In this folder, in contains three sub folders for the three participating studies.

| Reference | NLP-Derived Medical Variable | Outcome | Statistical Model | PPV | NPV | Sensitivity |
| --------- | ---------------------------- | ------- | ----------------- | --- | --- | ----------- |
| Turchin et al., 2020 | Non-acceptance of insulin therapy by patients | Time to HbA1c < 7.0% | Propensity score weighted Cox model (R package "survival" version 3.3-1 ) | 0.95 | 0.99 | 1.00 |
| Chang et al., 2021 | Patient-provider discussions of bariatric surgery | Change in BMI | Linear mixed model | 0.76 | 0.99 | 0.89 |
|  |  | Receipt of bariatric surgery | Logistic regression (R package "stats" version 4.2.1) |  |  |  |
| Brown et al., 2023 | Non-acceptance of statin therapy by patients | Time to LDL < 100 mg/dL | Cox model (R package "survival" version 3.3-1) | 0.78 | 0.99 | 0.88 |

According to our institutional policy, we are not allowed to share patient-level data. Our code provides procedures if the readers may specify the data at their ends.

### Turchin et al., 2020 (InsulinNonAcceptance)
This folder contain the codes for the Monte Carlo simulations of Turchin et al., 2020. 
a) "Codes.R": The main codes for Propensity score weighted Cox model.
b) "Codes.lsf": The job scheduler script for "Codes.R"
c) "Summary.R": Summarization codes for the results.

### Brown et al., 2023 (BariatricSurgery)
This folder contain the codes for the Monte Carlo simulations of Chang et al., 2021 
a) "BMI_Codes.R": The main codes for change in BMI using linear mixed model
b) "BMI_Codes.lsf": The job scheduler script for "BMI_Codes.R"
c) "BMI_summary.R": The summarization codes for the results of change in BMI.
d) "Surgery_code.R": The main codes for receipt of bariatric surgery using logistic regression
d) "Surgery_code.lsf": he job scheduler script for "Surgery_code.R"
d) "Surgery_summary.R": The summarization codes for the results of receipt of bariatric surgery.

### Brown et al., 2023 (InsulinNonAcceptance)
This folder contain the codes for the Monte Carlo simulations of Brown et al., 2023. 
a) "Cox_Sensitivity_Analysis.R": The main codes for Cox model.
b) "Cox_Sensitivity_Analysis.lsf": The job scheduler script for "Codes.R"
c) "SummaryCox_Sensitivity_Analysis.R": Summarization codes for the results.



