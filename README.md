# covid19_vaccination_letters

These are the files used for the analysis of the paper "Evaluation of the Effectiveness of COVID-19 Vaccination Appointment Letters on Uptake Across Sociodemographic Groups: A Regression Discontinuity Analysis in Sweden" (summary found below).

- Varotsis_et_al_Vaccination_Letter_Data_Processing_Table1.R: This file contains all data processing and preparation, as well as the code for generating Table 1.
- Varotsis_et_al_Vaccination_Letter_Primary_Secondary_Sensitivity_Analysis.R: This file includes the code for the primary, secondary, and sensitivity analyses. It also contains the code for generating Figure 1, Figure 2, Figure 3, Supplementary Table 1, Supplementary Table 2, and Supplementary Table 3.
- Varotsis_et_al_Vaccination_Letter_Spillover_Analysis.R: This file further processes the data for the spillover analysis and generates the results of the spillover analysis.

To obtain the results for the negative control analysis for the neighboring counties, use the file Varotsis_et_al_Vaccination_Letter_Primary_Secondary_Sensitivity_Analysis.R. In line 50, set the variable ‘County_selection’ to ‘Gävle’ to get the results for Gävleborg County or to ‘Stockholm’ to get the results for Stockholm County.

Summary below:

Evaluation of the effectiveness of COVID-19 vaccination appointment letters on uptake across sociodemographic groups: A regression discontinuity analysis in Sweden
Georgios Varotsis, MSc, Ulf Hammar, BSc, Carl Bonander, PhD, Per Lundmark, PhD, Beatrice Kennedy, PhD, Maria F. Gomez, MD, Mats Martinell, MD, Oliver J. Dyar, MD, Anna Sarkadi, MD, Robert Kristiansson, MD, Helena Svaleryd, PhD, Tove Fall, PhD

Summary  
Background Ensuring high vaccination coverage is vital, particularly during a pandemic and across sociodemographic groups with reportedly low uptake and high hesitancy. However, assessing the impact of vaccination campaigns through observational data presents challenges. To address these, our study employs a quasi-experimental methodology to evaluate the effect of pre-booked appointment letters on COVID-19 vaccine uptake across sociodemographic groups. 

Methods In Uppsala County, Sweden, residents born between 1962-1971 received pre-booked COVID-19 vaccination letters starting May 24, 2021, while younger residents were prompted to self-book via SMS starting June 7, 2021. Using a Regression Discontinuity Design, we leveraged the intervention cutoff at year of birth 1971 to assess the effectiveness of the letters in increasing vaccine uptake compared to the SMS campaign across sociodemographic groups. Our analysis included 96 194 individuals born between 1962-1981, with vaccination within 90 days post-eligibility served as the primary outcome. We also assessed household spillover effects and performed negative control analyses using the neighbouring counties.

Findings Adults just above the cutoff had 30% higher odds (95% CI 10%– 53%) of receiving vaccination than those just below. Subgroup analyses supported effects across most sociodemographic strata, one exception being those with university education. No effects were found in the negative control counties, nor were there household spillover effects. The estimate from a linear probability model indicated the policy effect increased vaccination coverage by 1·97 percentage points (95% CI 0·45–3·50).

Interpretation Pre-booked appointment letters are effective at boosting COVID-19 vaccination uptake, particularly in sociodemographic groups with low immunization coverage and elevated vaccine hesitancy, suggesting they may contribute to equitable vaccine distribution during a pandemic.

Keywords: COVID-19; Immunization Programs; Appointments and Schedules; Sociodemographic Factors; Regression Discontinuity


