# title: "Evaluation of the effectiveness of COVID-19 vaccination appointment letters on uptake across sociodemographic groups: A regression discontinuity analysis in Sweden"
# subtitle: "Script for the Spillover Analysis in Uppsala"
#
# author: "Georgios Varotsis, MSc"
# contact e-mail: georgios.varotsis@medsci.uu.se
# last updated: "2024-06-28"


############
############
############
############
##### # # # # LOAD LIBRARIES # # # # ##### ----------------------------------------------
library(lmtest)
library(tidyverse)
library(broom)
# non parametric discontinuity
library(rdrobust)
library(rddensity)
library(modelsummary)
library(data.table)
library(rio)

library(survey)
library(rddapp)
library(sandwich)
library(lmtest)
library(splines)
library(epiDisplay)
library(viridis)
library(scales)
library(rdrobust)
library(rdrobust)
library(splines)
library(epiDisplay)
library(forcats)

############
############
############
############
##### # # # # DATA PREPARATION # # # # ##### ----------------------------------------------

County_selection <- 'Spillover - Uppsala'

# Chose file based on the variable
file_to_import <- "//directory/Spillover_data_Uppsala_20240313.csv"
county_code <- 3
plot_title <- "Spillover - Uppsala County"

main_data_tbl <-  fread(file_to_import)
main_data_tbl$V1 <- NULL

main_data_tbl$First_dose_influenced <-
  as.Date(main_data_tbl$First_dose_influenced)
main_data_tbl$vacc_date_opening_p_age_influenced    <-
  as.Date(main_data_tbl$vacc_date_opening_p_age_influenced)

# make the forcing variable the right one
main_data_tbl <- main_data_tbl %>%
  dplyr::select(-Letter_received_influenced) %>%
  rename(Letter_received = Letter_received_influencer)
colnames(main_data_tbl) <-
  gsub('_influenced', '', colnames(main_data_tbl))


############
############
############
############
# Population addition code

pop <-  fread("//directory/Population_PersonNr_20211231.csv")

# you need the population of all partnered individuals
pop <-  pop %>%
  filter(P1105_LopNr_PersonNr %in% main_data_tbl$P1105_LopNr_PersonNr)

pop <-  pop %>%
  left_join(
    main_data_tbl %>%
      dplyr::select(P1105_LopNr_PersonNr, FodelseArMan_influencer),
    by = 'P1105_LopNr_PersonNr'
  )


# recreate the same time variables for the pop file
pop$Birth_year <- substr(pop$FodelseArMan, 1, 4)
pop$Birth_month_influencer <- substr(pop$FodelseArMan, 5, 6)

pop$Birth_year_influencer <-
  substr(pop$FodelseArMan_influencer, 1, 4)
pop$Birth_month_influencer <-
  substr(pop$FodelseArMan_influencer, 5, 6)

# Convert year and month to date ( assuming day is the first day of the month)
pop$birth_y_m_D <- as.Date(paste0(pop$Birth_year, '-',
                                  pop$Birth_month, '-' ,
                                  '01'),
                           format = '%Y-%m-%d')

pop$BirthYM_Cat <-
  paste0(substr(pop$FodelseArMan, 1, 4),
         '-',
         substr(pop$FodelseArMan, 5, 6))

pop$BirthYM_Cat <- factor(pop$BirthYM_Cat,
                          ordered = T,
                          levels = unique(pop$BirthYM_Cat[order(pop$birth_y_m_D)]))



pop$birth_y_m_D_influencer <-
  as.Date(
    paste0(
      pop$Birth_year_influencer,
      '-',
      pop$Birth_month_influencer,
      '-' ,
      '01'
    ),
    format = '%Y-%m-%d'
  )

pop$BirthYM_Cat_influencer <-
  paste0(
    substr(pop$FodelseArMan_influencer, 1, 4),
    '-',
    substr(pop$FodelseArMan_influencer, 5, 6)
  )

pop$BirthYM_Cat_influencer <- factor(pop$BirthYM_Cat_influencer)


############
############
############
############
# here you need to count the population
pop_per_month_cat <- pop %>%
  group_by(BirthYM_Cat_influencer) %>%
  summarise(population = n()) %>%
  ungroup() %>%
  arrange(BirthYM_Cat_influencer)

# for the following calculation you need the education, gender, foreign born salary and gender to do the manipulations
pop_for_interaction_models <- pop %>%
  left_join(
    main_data_tbl %>% dplyr::select(
      P1105_LopNr_PersonNr,
      Gender,
      Education,
      Ind_slr_cat,
      High_Low_Cntr,
      Educ_Gender,
      Foreign_vs_Sweden
    )
  )

pop_per_month_cat_education <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat_influencer, Education) %>%
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_country <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat_influencer, Foreign_vs_Sweden) %>%  #
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_salary <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat_influencer, Ind_slr_cat) %>%
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_gender <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat_influencer, Gender) %>%
  summarise(population = n()) %>%
  ungroup()


pop_per_month_cat_education_gender <-
  pop_for_interaction_models %>%
  group_by(BirthYM_Cat_influencer, Educ_Gender) %>%
  summarise(population = n()) %>%
  ungroup()

# I cant join the two dataset because of incompatible factors so I have to retransform them to character
main_data_tbl$BirthYM_Cat_influencer <-
  as.character(main_data_tbl$BirthYM_Cat)

# Fix the letter_received variable so we also see what happened to the above 60yo
main_data_tbl <- main_data_tbl %>%
  rename(
    Birth_year = Age_year,
    Birth_month = Age_month,
    Birth_year_influencer = Age_year_influencer,
    Birth_month_influencer = Age_month_influencer
  ) %>%
  # create a round age that rounds the values DOWN
  mutate(
    Age_Round = floor(age_for_model),
    Age_Round_influencer = floor(age_for_model_influencer)
  )


############
############
############
############
# Convert year and month to date ( assuming day is the first day of the month)
main_data_tbl$birth_y_m_D <-
  as.Date(
    paste0(
      main_data_tbl$Birth_year,
      '-',
      main_data_tbl$Birth_month,
      '-' ,
      '01'
    ),
    format = '%Y-%m-%d'
  )

main_data_tbl$BirthYM_Cat <-
  paste0(
    substr(main_data_tbl$FodelseArMan, 1, 4),
    '-',
    substr(main_data_tbl$FodelseArMan, 5, 6)
  )

main_data_tbl$BirthYM_Cat <- factor(
  main_data_tbl$BirthYM_Cat,
  ordered = T,
  levels = unique(main_data_tbl$BirthYM_Cat[order(main_data_tbl$birth_y_m_D)])
)


main_data_tbl$birth_y_m_D_influencer <-
  as.Date(
    paste0(
      main_data_tbl$Birth_year_influencer,
      '-',
      main_data_tbl$Birth_month,
      '-' ,
      '01'
    ),
    format = '%Y-%m-%d'
  )

main_data_tbl <- main_data_tbl %>%
  arrange(FodelseArMan_influencer)
main_data_tbl$BirthYM_Cat_influencer <-
  paste0(
    substr(main_data_tbl$FodelseArMan_influencer, 1, 4),
    '-',
    substr(main_data_tbl$FodelseArMan_influencer, 5, 6)
  )

main_data_tbl$BirthYM_Cat_influencer <-
  factor(main_data_tbl$BirthYM_Cat_influencer)

#
main_data_tbl$Year_Nmonth <-
  paste(main_data_tbl$Birth_year,
        (main_data_tbl$Birth_month %/% 2) + 1,
        sep = '-')

main_data_tbl$Year_Month <-
  paste(main_data_tbl$Birth_year,
        sprintf('%02d', main_data_tbl$Birth_month),
        sep = '-')
main_data_tbl$bi_month <-
  cut(as.Date(
    paste(
      main_data_tbl$Birth_year,
      main_data_tbl$Birth_month,
      '01',
      sep = '-'
    )
  ),
  breaks = '2 month',
  labels = F)

main_data_tbl$bi_month <-
  factor(main_data_tbl$bi_month, levels = unique(main_data_tbl$bi_month))

main_data_tbl$bi_month_influencer <-
  cut(as.Date(
    paste(
      main_data_tbl$Birth_year_influencer,
      main_data_tbl$Birth_month_influencer,
      '01',
      sep = '-'
    )
  ),
  breaks = '2 month',
  labels = F)

main_data_tbl$bi_month_influencer <-
  factor(main_data_tbl$bi_month_influencer,
         levels = unique(main_data_tbl$bi_month_influencer))


main_data_tbl_sum_90 <- main_data_tbl %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(
    Letter_received,
    Birth_year_influencer,
    Birth_month_influencer,
    BirthYM_Cat_influencer
  ) %>%
  
  summarize(pop_vaxed = n()) %>%
  ungroup()


main_data_tbl_sum_90 <- main_data_tbl_sum_90 %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat_influencer')


main_data_tbl_sum_90$prop_vaxed <-
  (main_data_tbl_sum_90$pop_vaxed / main_data_tbl_sum_90$population)

# add age_for_model
main_data_tbl_sum_90 <- main_data_tbl_sum_90 %>%
  mutate(
    age_for_model_influencer = (2021 - Birth_year_influencer) + (6 - Birth_month_influencer) /
      12
  )



############
############
############
############
# model the outcome for the regressions
main_data_tbl_logit <- main_data_tbl %>%
  mutate(
    Vax_within_90 = case_when(Vax_within_90 == 'Yes' ~ 1 ,
                              Vax_within_90 == 'No' ~ 0 ,
                              TRUE ~ NA),
    Letter_received =  case_when(
      Letter_received == 'Letter' ~ 1 ,
      Letter_received == 'No letter' ~ 0 ,
      TRUE ~ NA
    ),
    
    Education = factor(
      Education,
      levels = c('Primary School', 'Gymnasium', 'University')
    ),
    
    High_Low_Cntr  = factor(High_Low_Cntr,
                            levels = c('Middle_Low', 'High')),
    
  )



############
############
############
############
##### # # # # DISCONTINUITY ANALYSIS # # # # ##### ----------------------------------------------


############
############
############
############
library(rddapp)
bandwidth_imbens <-
  rddapp::rd_est(
    Vax_within_90  ~  age_for_model_influencer |
      Letter_received,
    # age_for_model_influencer | Letter_received,
    data = main_data_tbl_logit,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    # geq means the treatment is assigned if 'x' is greater than or equal to its cutoff
    # verbose = TRUE,
    # se.type = 'HC3',
    bw = 'IK12'
  ) # IK12 imbens from 2012, IK09 Imbens from 2009

selected_bandwidth_imbens <- bandwidth_imbens$bw[4]


############
############
############
############
library(rdrobust)
bandwidth_CalonicoCattaneo <-
  rdbwselect(
    y = main_data_tbl_logit$Vax_within_90,
    x = main_data_tbl_logit$age_for_model_influencer,
    c = 49.5
  )

selected_bandwidth_Calonico <- bandwidth_CalonicoCattaneo$bws[2]

# selected_bandwidth_imbens <- bandwidth_imbens$bw[4]

############
############
############
############
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth <- selected_bandwidth_imbens


############
############
############
############
# Epanechnikov kernels

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK <-
  Epanechnikov_kernel_IK(main_data_tbl_logit$age_for_model_influencer)


# keep also the weight calculation for Calonico
Epanechnikov_kernel_CCT <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_Calonico) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_CCT <-
  Epanechnikov_kernel_CCT(main_data_tbl_logit$age_for_model_influencer)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model <- weights_Epanechnikov_IK





############
############
############
############
# Imbens and Kalyanaraman (IK)

# Fit the logistic regression model with the kernel weights
library(survey)
model_simple_Kernels_design <-
  svydesign(id = ~ 1,
            data = main_data_tbl_logit,
            weights =  ~ weights_4_model)
model_simple_Kernels <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(
      age_for_model_influencer ,
      knots = 49.5,
      degree = 1
    ),
    design = model_simple_Kernels_design,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction <-
  logistic.display(model_simple_Kernels)
or_main_model_no_interaction <-
  or_main_model_no_interaction$table[1, 1]

results_logreg_epanechnikov <-
  logistic.display(model_simple_Kernels)
OR_epanechnikov <- results_logreg_epanechnikov$table[1, 1]
CI_low_epanechnikov <- results_logreg_epanechnikov$table[1, 2]
CI__high_epanechnikov <- results_logreg_epanechnikov$table[1, 3]
pvalue_epanechnikov <- results_logreg_epanechnikov$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels <-
  coeftest(model_simple_Kernels, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels <- wald_test_covariates_kernels[2, 4]

# isolate robustified confidence intervals
library(broom)
library(lmtest)
model_for_robust_rbst  <-
  broom::tidy(coeftest(model_simple_Kernels, vcov = vcovHC), conf.int = TRUE)
CI_low_epanechnikov_rbst <- exp(model_for_robust_rbst$conf.low)[2]
CI__high_epanechnikov_rbst <-
  exp(model_for_robust_rbst$conf.high)[2]

# isolate number of observations
number_obs_logreg <- as.numeric((nobs(model_simple_Kernels)))




############
############
############
############
# Calonico, Cattaneo and Titiunik (CCT)


# Fit the logistic regression model with the kernel weights

library(survey)
model_simple_Kernels_design_CCT <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit,
    weights =  ~ weights_Epanechnikov_CCT
  )
model_simple_Kernels_CCT <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(
      age_for_model_influencer ,
      knots = 49.5,
      degree = 1
    ),
    design = model_simple_Kernels_design_CCT,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_CCT <-
  logistic.display(model_simple_Kernels_CCT)
or_main_model_no_interaction_CCT <-
  or_main_model_no_interaction_CCT$table[1, 1]

results_logreg_epanechnikov_CCT <-
  logistic.display(model_simple_Kernels_CCT)
OR_epanechnikov_CCT <- results_logreg_epanechnikov_CCT$table[1, 1]
CI_low_epanechnikov_CCT <-
  results_logreg_epanechnikov_CCT$table[1, 2]
CI__high_epanechnikov_CCT <-
  results_logreg_epanechnikov_CCT$table[1, 3]
pvalue_epanechnikov_CCT <-
  results_logreg_epanechnikov_CCT$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_CCT <-
  coeftest(model_simple_Kernels_CCT, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_CCT <- wald_test_covariates_kernels_CCT[2, 4]


# isolate robustified confidence intervals
library(broom)
library(lmtest)
model_for_robust_CTT_rbst  <-
  broom::tidy(coeftest(model_simple_Kernels_CCT, vcov = vcovHC), conf.int = TRUE)
CI_low_epanechnikov_CCT_rbst <-
  exp(model_for_robust_CTT_rbst$conf.low)[2]
CI__high_epanechnikov_CCT_rbst <-
  exp(model_for_robust_CTT_rbst$conf.high)[2]
# isolate number of observations
number_obs_logreg_CCT <-
  as.numeric((nobs(model_simple_Kernels_CCT)))





############
############
############
############
##### # # # # DISCONTINUITY ANALYSIS with rdrobust# # # # ##### ----------------------------------------------

library(rdrobust)
library(splines)
library(epiDisplay)

rd_analysis <- rdrobust(
  y = main_data_tbl_logit$Vax_within_90,
  x = main_data_tbl_logit$age_for_model_influencer,
  c = 49.5,
  h = selected_bandwidth_imbens,
  p = 1,
  kernel = 'epanechnikov'
)

# rd_analysis
rd_analysis %>%
  summary()

coeff_main_rd <- rd_analysis$coef[3]
se_main_rd <- rd_analysis$se[3]
pvalue_main_rd <- rd_analysis$pv[1]
pvalue_main_rd_robust <- rd_analysis$pv[3]

CI95_low_main_rd <- rd_analysis$ci[3, 1]
CI95_high_main_rd <- rd_analysis$ci[3, 2]

nm_obs_rdrobust <- rd_analysis$N[1] + rd_analysis$N[2]
selected_bndwdth_rdrob <- rd_analysis$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob <- rd_analysis$bws[2, 1]
