# title: "Evaluation of the effectiveness of COVID-19 vaccination appointment letters on uptake across sociodemographic groups: A regression discontinuity analysis in Sweden"
# subtitle: "Script for Discontinuity Plots, Primary, Secondary and Sensitivity analysis"
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
##### # # # # SELECT COUNTY # # # # ##### ----------------------------------------------

# IMPORTANT
# Select the county here
# options are Uppsala (main analysis) , Gävle (negative control analysis) and Stockholm (negative control analysis)
County_selection <- 'Uppsala' # options Uppsala, Gävle or Stockholm

# Chose file based on the variable
if (County_selection == 'Uppsala') {
  file_to_import <- "//directory/main_data_tbl_Uppsala_20240514.csv"
  county_code <- 3
  plot_title <- "Uppsala County"
  
  
} else if (County_selection == 'Gävle') {
  file_to_import <- "//directory/main_data_tbl_Gavle_20240514.csv"
  county_code <- 21
  plot_title <- "Gävle County"
  
  
} else if (County_selection == 'Stockholm') {
  file_to_import <- "//directory/main_data_tbl_Stockholm_20240514.csv"
  county_code <- 1
  plot_title <- "Stockholm County"
  
  
} else {
  file_to_import <- ""
  county_code <- 0
  plot_title <- ""
}

main_data_tbl <-  fread(file_to_import)
main_data_tbl$V1 <- NULL


main_data_tbl$First_dose <- as.Date(main_data_tbl$First_dose)
main_data_tbl$vacc_date_opening_p_age    <-
  as.Date(main_data_tbl$vacc_date_opening_p_age)






############
############
############
############
##### # # # # DISCONTUITY PLOT # # # # ##### ----------------------------------------------


data_for_text_label <-
  data.frame(
    Letter_received = c('Letter', 'No letter'),
    age_for_model = c(55, 44),
    label = c('Letter notification', 'Self-booking')
  )

main_model_plotRD <-
  ggplot(
    main_data_tbl_sum_90,
    aes(
      x = age_for_model,
      y = prop_vaxed,
      fill = Letter_received,
      color = Letter_received
    )
  ) +
  geom_vline(xintercept = 49.45, linetype = 1) +
  
  
  geom_text(
    data = data_for_text_label,
    aes(
      x = age_for_model,
      y = 0.99,
      fill = Letter_received,
      label = label,
      color = Letter_received
    ),
    size = 10
  ) +
  geom_point(size = 2.2, alpha = 0.5) +
  
  # Add a line based on a linear model for the people born in 1971 and before
  geom_smooth(
    data = filter(main_data_tbl_sum_90, Birth_year <= 1971),
    formula = 'y~x',
    method = "lm"
  ) +
  
  #   # Add a line based on a linear model for the people born after 1971
  geom_smooth(
    data = filter(main_data_tbl_sum_90, Birth_year > 1971),
    formula = 'y~x',
    #
    method = "lm"
  ) +
  
  scale_y_continuous(labels = scales::percent, limits = c(NA, 1)) +
  
  scale_color_manual(
    values = c('dodgerblue3', 'red3'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  scale_fill_manual(
    values = c('dodgerblue1', 'red1'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  
  theme_classic() +
  theme(
    axis.text =  element_text(size = 16),
    axis.title =  element_text(size = 20),
    plot.subtitle  =  element_text(size = 20),
    strip.text = element_text(size = 20),
    legend.position = 'none'
  ) +
  
  labs(x = "Age (years)"
       ,
       y = ''
       ,
       color = "Intervention:",
       
       fill = "Intervention:")

main_model_plotRD





############
############
############
############
##### # # # # DISCONTUITY PLOT (zoom in around the cutoff) # # # # ##### ----------------------------------------------


# set the bandwidth
bandwidth_plot <- 3

ggplot(
  main_data_tbl_sum_90,
  aes(
    x = age_for_model,
    y = prop_vaxed,
    fill =  Letter_received,
    color = Letter_received
  )
) +
  geom_vline(xintercept = 49.5) +
  
  geom_point(size = 0.8, alpha = 0.3) +
  
  # ZOOM OUT
  geom_smooth(
    data = filter(main_data_tbl_sum_90, Birth_year <= 1971),
    method = "lm",
    size = 0.8,
    linetype = 2,
    se = F,
    color = 'dodgerblue1',
    alpha = 0.3
  ) +
  
  #   # Add a line based on a linear model for the people born after 1971
  geom_smooth(
    data = filter(main_data_tbl_sum_90, Birth_year > 1971),
    method = "lm",
    size = 0.8,
    linetype = 2,
    se = F,
    color = 'red1',
    alpha = 0.3
  ) +
  
  # Zoom IN
  geom_smooth(
    data = filter(
      main_data_tbl_sum_90,
      Birth_year >= (1971 - bandwidth_plot) &
        Birth_year <= 1971
    ),
    method = "lm"
  ) +
  
  geom_smooth(
    data = filter(
      main_data_tbl_sum_90,
      Birth_year > 1971 &
        Birth_year <= (1971 + bandwidth_plot)
    ),
    method = "lm"
  ) +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_color_manual(
    values = c('dodgerblue3', 'red3'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  scale_fill_manual(
    values = c('dodgerblue1', 'red1'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  
  labs(
    x = "Age (years)"
    ,
    y = "Proportion vaccinated within 90 days of the opening"
    ,
    color = "Intervention:",
    title = paste0(plot_title, ' (narrow bandwidth)'),
    fill = "Intervention:"
  ) +
  theme_classic()





############
############
############
############
##### # # # # BANDWIDTH ESTIMATION # # # # ##### ----------------------------------------------


############
############
############
############
# Method based on Imbens-Kalyanaraman method
bandwidth_imbens <-
  rddapp::rd_est(
    Vax_within_90  ~  age_for_model | Letter_received,
    data = main_data_tbl_logit,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    bw = 'IK12'
  )

selected_bandwidth_imbens <- bandwidth_imbens$bw[4]

bandwidth_imbens_ahead <-
  rddapp::rd_est(
    Vax_within_90  ~  age_for_model | Letter_received,
    data = main_data_tbl_logit_ahead_of_time_out,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    bw = 'IK12'
  )

selected_bandwidth_imbens_ahead <- bandwidth_imbens_ahead$bw[4]




############
############
############
############
bandwidth_CalonicoCattaneo <-
  rdbwselect(y = main_data_tbl_logit$Vax_within_90,
             x = main_data_tbl_logit$age_for_model,
             c = 49.5)

selected_bandwidth_Calonico <- bandwidth_CalonicoCattaneo$bws[2]


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
##### # # # # EPANECHNIKOV KERNEL ESTIMATION # # # # ##### ----------------------------------------------


# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK <-
  Epanechnikov_kernel_IK(main_data_tbl_logit$age_for_model)


Epanechnikov_kernel_IK_ahead <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_ahead) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

weights_Epanechnikov_IK_ahead <-
  Epanechnikov_kernel_IK_ahead(main_data_tbl_logit_ahead_of_time_out$age_for_model)


# keep also the weight calculation for Calonico
Epanechnikov_kernel_CCT <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_Calonico) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

############
############
############
############
# Apply the kernel weight function to the age variable
weights_Epanechnikov_CCT <-
  Epanechnikov_kernel_CCT(main_data_tbl_logit$age_for_model)


############
############
############
############
# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model <- weights_Epanechnikov_IK
weights_4_model_ahead <- weights_Epanechnikov_IK_ahead







############
############
############
############
##### # # # # PRIMARY ANALYSIS - Imbens and Kalyanaraman (IK) DISCONTINUITY ANALYSIS # # # # ##### ----------------------------------------------

# Fit the logistic regression model with the kernel weights
bandwidth_imbens <-
  rddapp::rd_est(
    Vax_within_90  ~  age_for_model | Letter_received,
    data = main_data_tbl_logit,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    bw = 'IK12'
  )

selected_bandwidth_imbens <- bandwidth_imbens$bw[4]

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
# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK <-
  Epanechnikov_kernel_IK(main_data_tbl_logit$age_for_model)

############
############
############
############
# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model <- weights_Epanechnikov_IK

############
############
############
############
model_simple_Kernels_design <-
  svydesign(id = ~ 1,
            data = main_data_tbl_logit,
            weights =  ~ weights_4_model)
model_simple_Kernels <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design,
    family = binomial(link = 'logit')
  )

model_simple_Kernels_linear <-
  svyglm(Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
         design = model_simple_Kernels_design)

summary(model_simple_Kernels_linear)

logistic.display(model_simple_Kernels)

############
############
############
############
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

############
############
############
############
# Apply robust standard errors using the sandwich library

# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels <-
  coeftest(model_simple_Kernels, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels <- wald_test_covariates_kernels[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst  <-
  broom::tidy(coeftest(model_simple_Kernels, vcov = vcovHC), conf.int = TRUE)
CI_low_epanechnikov_rbst <- exp(model_for_robust_rbst$conf.low)[2]
CI__high_epanechnikov_rbst <-
  exp(model_for_robust_rbst$conf.high)[2]

model_for_robust_rbst_Uppsala <- model_for_robust_rbst
model_simple_Kernels_Uppsala <- model_simple_Kernels

# isolate number of observations
number_obs_logreg <- as.numeric((nobs(model_simple_Kernels)))

############
############
############
############
# keep main intercept and its std.error from the robust
main_log_OR <- exp(summary(model_simple_Kernels)$coefficients[2, 1])
main_log_SD <- exp(model_for_robust_rbst$std.error[2])

main_OR_SD <- data.frame(
  model = County_selection,
  OR = main_log_OR,
  SD = main_log_SD,
  CI_rob_low = CI_low_epanechnikov_rbst,
  CI_rob_high = CI__high_epanechnikov_rbst
)

results_table_Main_Log <- data.frame(
  Model = c('Epanechnikov_IK'),
  OR = c(OR_epanechnikov),
  CI_L = c(CI_low_epanechnikov_rbst),
  CI_H = c(CI__high_epanechnikov_rbst),
  P = c(pvalue_epanechnikov),
  P_Robust = c(p_value_wald_kernels),
  P_intrctn = c(''),
  nm_obs = c(number_obs_logreg),
  bandwidth = c(selected_bandwidth_imbens)
)





############
############
############
############
##### # # # # SENSITIVITY - Calonico, Cattaneo and Titiunik (CCT) DISCONTINUITY ANALYSIS # # # # ##### ----------------------------------------------


# Fit the logistic regression model with the kernel weights

# library(survey)
model_simple_Kernels_design_CCT <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit,
    weights =  ~ weights_Epanechnikov_CCT
  )
model_simple_Kernels_CCT <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
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


############
############
############
############
# Apply robust standard errors using the sandwich library

# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_CCT <-
  coeftest(model_simple_Kernels_CCT, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_CCT <- wald_test_covariates_kernels_CCT[2, 4]

# isolate robustified confidence intervals
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
##### # # # # SENSITIVITY - Imbens and Kalyanaraman (IK) DISCONTINUITY ANALYSIS EXCLUDING INDIVIDUALS VACCINATED AHEAD OF TIME # # # # ##### ----------------------------------------------

model_simple_Kernels_design_ahead <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit_ahead_of_time_out,
    weights =  ~ weights_4_model_ahead
  )
model_simple_Kernels_ahead <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_ahead,
    family = binomial(link = 'logit')
  )

logistic.display(model_simple_Kernels_ahead)

or_main_model_no_interaction_ahead <-
  logistic.display(model_simple_Kernels_ahead)
or_main_model_no_interaction_ahead <-
  or_main_model_no_interaction_ahead$table[1, 1]

results_logreg_epanechnikov_ahead <-
  logistic.display(model_simple_Kernels_ahead)
OR_epanechnikov_ahead <-
  results_logreg_epanechnikov_ahead$table[1, 1]
CI_low_epanechnikov_ahead <-
  results_logreg_epanechnikov_ahead$table[1, 2]
CI__high_epanechnikov_ahead <-
  results_logreg_epanechnikov_ahead$table[1, 3]
pvalue_epanechnikov_ahead <-
  results_logreg_epanechnikov_ahead$table[1, 4]

############
############
############
############
# Apply robust standard errors using the sandwich library

# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_ahead <-
  coeftest(model_simple_Kernels_ahead, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_ahead <-
  wald_test_covariates_kernels_ahead[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst_ahead  <-
  broom::tidy(coeftest(model_simple_Kernels_ahead, vcov = vcovHC), conf.int = TRUE)
CI_low_epanechnikov_rbst_ahead <-
  exp(model_for_robust_rbst_ahead$conf.low)[2]
CI__high_epanechnikov_rbst_ahead <-
  exp(model_for_robust_rbst_ahead$conf.high)[2]

model_for_robust_rbst_Uppsala_ahead <- model_for_robust_rbst_ahead
model_simple_Kernels_Uppsala_ahead <- model_simple_Kernels_ahead

# isolate number of observations
number_obs_logreg_ahead <-
  as.numeric((nobs(model_simple_Kernels_ahead)))

############
############
############
############
# keep main intercept and its std.error from the robust
main_log_OR_ahead <-
  exp(summary(model_simple_Kernels_ahead)$coefficients[2, 1])
main_log_SD_ahead <- exp(model_for_robust_rbst_ahead$std.error[2])

main_OR_SD_ahead <- data.frame(
  model = County_selection,
  OR = main_log_OR_ahead,
  SD = main_log_SD_ahead,
  CI_rob_low = CI_low_epanechnikov_rbst_ahead,
  CI_rob_high = CI__high_epanechnikov_rbst_ahead
)


############
############
############
############
##### # # # # PRIMARY ANALYSIS - EDUCATION # # # # ##### ----------------------------------------------

main_data_tbl_logit <- main_data_tbl_logit %>%
  mutate(
    Education_simplified = case_when(
      Education == 'University' ~ 'University',
      Education == 'Primary School' ~ 'Primary',
      Education == 'Gymnasium' ~ 'Gymnasium',
      
      TRUE ~ NA
    )
  ) %>%
  mutate_at(c('Education_simplified'), as.factor)

# str(main_data_tbl_logit$Education_simplified)



# attention do not make it into an ordered factor!! the glm model might get confused and give polynomials instead
main_data_tbl_logit$Education_simplified <-
  factor(
    main_data_tbl_logit$Education_simplified,
    ordered = F,
    levels = c('Primary', 'Gymnasium', 'University')
  )
############
############
############
############
# STEP 1
# HIGHEST CATEGORY (University)

model_simple_Kernels_Education_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(Education_simplified = factor(
        Education_simplified,
        levels = c('University', 'Gymnasium', 'Primary')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Education <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + Education_simplified + Education_simplified:Letter_received,
    design = model_simple_Kernels_Education_design,
    family = binomial(link = 'logit')
  )



# Check results
results_logreg_education <-
  logistic.display(model_simple_Kernels_Education)

OR_education_main <- results_logreg_education$table[1, 1]
CI_low_education_main <- results_logreg_education$table[1, 2]
CI_high_education_main <- results_logreg_education$table[1, 3]
pvalue_education_main <- results_logreg_education$table[1, 4]


############
############
############
############
# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_education <-
  coeftest(model_simple_Kernels_Education, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_education_Uni <-
  wald_test_covariates_kernels_inter_education[2, 'Pr(>|z|)']

# isolate robustified confidence intervals
model_for_robust_education <-
  broom::tidy(coeftest(model_simple_Kernels_Education, vcov = vcovHC),
              conf.int = TRUE)
CI_low_education_main_rbst <-
  exp(model_for_robust_education$conf.low)[2]
CI_high_education_main_rbst <-
  exp(model_for_robust_education$conf.high)[2]
OR_robust_NOT_FOR_USE_JUST_FOR_COMPARISON_educ <-
  exp(model_for_robust_education$estimate)[2]

# isolate number of observations
number_obs_inter_education_uni <-
  (nobs(model_simple_Kernels_Education))

# the the p-value here should be the same for all switches
model_interaction_Education_for_Robust_uni <-
  car::linearHypothesis(
    model_simple_Kernels_Education,
    c(
      'Letter_received:Education_simplifiedGymnasium',
      'Letter_received:Education_simplifiedPrimary'
    ),
    cluster = NULL,
    
    vcov = vcovHC(model_simple_Kernels_Education, type = 'HC3')
  )

pvalue_interaction_Robust_education_uni <-
  model_interaction_Education_for_Robust_uni[2, 'Pr(>Chisq)']


############
############
############
############
# STEP 2
# MIDDLE CATEGORY (Gymnasium)


model_simple_Kernels_Education_Gymn_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(Education_simplified = factor(
        Education_simplified,
        levels = c('Gymnasium', 'University', 'Primary')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Education_Gymn <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + Education_simplified + Education_simplified:Letter_received,
    design = model_simple_Kernels_Education_Gymn_design,
    family = binomial(link = 'logit')
  )


# Check results
results_logreg_Education_Gymn <-
  logistic.display(model_simple_Kernels_Education_Gymn)

OR_Education_Gymn <- results_logreg_Education_Gymn$table[1, 1]
CI_low_Education_Gymn <- results_logreg_Education_Gymn$table[1, 2]
CI__high_Education_Gymn <- results_logreg_Education_Gymn$table[1, 3]
pvalue_Education_Gymn <- results_logreg_Education_Gymn$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_country_Gymn <-
  coeftest(model_simple_Kernels_Education_Gymn, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_country_Gymn <-
  wald_test_covariates_kernels_inter_country_Gymn[2, 4]



# isolate robustified confidence intervals
model_for_robust_education_gymn <-
  broom::tidy(coeftest(model_simple_Kernels_Education_Gymn, vcov = vcovHC),
              conf.int = TRUE)
CI_low_Education_Gymn_rbst <-
  exp(model_for_robust_education_gymn$conf.low)[2]
CI__high_Education_Gymn_rbst <-
  exp(model_for_robust_education_gymn$conf.high)[2]

# isolate number of observations
number_obs_inter_education_uni_Gymn <-
  (nobs(model_simple_Kernels_Education_Gymn))

model_interaction_education_gymn <-
  car::linearHypothesis(
    model_simple_Kernels_Education_Gymn,
    c(
      'Letter_received:Education_simplifiedPrimary',
      'Letter_received:Education_simplifiedUniversity'
    ),
    cluster = NULL,
    
    vcov = vcovHC(model_simple_Kernels_Education_Gymn, type = 'HC3')
  )

pvalue_interaction_Robust_education_gymn <-
  model_interaction_education_gymn[2, 'Pr(>Chisq)']


############
############
############
############
# STEP 3
# LOW CATEGORY (Primary School)


model_simple_Kernels_Education_Primary_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(Education_simplified = factor(
        Education_simplified,
        levels = c('Primary', 'University', 'Gymnasium')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Education_Primary <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + Education_simplified + Education_simplified:Letter_received,
    design = model_simple_Kernels_Education_Primary_design,
    family = binomial(link = 'logit')
  )


# Check results
results_logreg_Education_Primary <-
  logistic.display(model_simple_Kernels_Education_Primary)

OR_Education_Primary <- results_logreg_Education_Primary$table[1, 1]
CI_low_Education_Primary <-
  results_logreg_Education_Primary$table[1, 2]
CI__high_Education_Primary <-
  results_logreg_Education_Primary$table[1, 3]
pvalue_Education_Primary <-
  results_logreg_Education_Primary$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_country_Primary <-
  coeftest(model_simple_Kernels_Education_Primary, vcov = vcovHC)
exp(0.81)

# isolate p-value of interest
p_value_wald_kernels_inter_country_Primary <-
  wald_test_covariates_kernels_inter_country_Primary[2, 4]


# isolate robustified confidence intervals
model_for_robust_education_prim <-
  broom::tidy(coeftest(model_simple_Kernels_Education_Primary, vcov = vcovHC),
              conf.int = TRUE)
CI_low_Education_Primary_rbst <-
  exp(model_for_robust_education_prim$conf.low)[2]
CI__high_Education_Primary_rbst <-
  exp(model_for_robust_education_prim$conf.high)[2]

# isolate number of observations
number_obs_inter_Education_prim <-
  (nobs(model_simple_Kernels_Education_Primary))

# the the p-value here should be the same for all switches
model_interaction_education_primary <-
  car::linearHypothesis(
    model_simple_Kernels_Education_Primary,
    c(
      'Letter_received:Education_simplifiedGymnasium',
      'Letter_received:Education_simplifiedUniversity'
    ),
    cluster = NULL,
    
    vcov = vcovHC(model_simple_Kernels_Education_Primary, type = 'HC3')
  )

pvalue_interaction_Robust_education_primary <-
  model_interaction_education_primary[2, 'Pr(>Chisq)']

############
############
############
############
# Make the table

results_table_Education <- data.frame(
  # Model = c('Edu_Uni','Edu_Gymns','Edu_Primr'),
  Model = c('University studies', 'Secondary school', 'Primary school'),
  OR = c(OR_education_main, OR_Education_Gymn, OR_Education_Primary),
  CI_L = c(
    CI_low_education_main_rbst,
    CI_low_Education_Gymn_rbst,
    CI_low_Education_Primary_rbst
  ),
  CI_H = c(
    CI_high_education_main_rbst,
    CI__high_Education_Gymn_rbst,
    CI__high_Education_Primary_rbst
  ),
  P = c(
    pvalue_education_main,
    pvalue_Education_Gymn,
    pvalue_Education_Primary
  ),
  P_Robust = c(
    p_value_wald_kernels_inter_education_Uni,
    p_value_wald_kernels_inter_country_Gymn,
    p_value_wald_kernels_inter_country_Primary
  ),
  P_intrctn = c(
    pvalue_interaction_Robust_education_uni,
    pvalue_interaction_Robust_education_gymn,
    pvalue_interaction_Robust_education_primary
  ),
  nm_obs = c(
    number_obs_inter_education_uni,
    number_obs_inter_education_uni_Gymn,
    number_obs_inter_Education_prim
  ),
  bandwidth = c(optimal_bandwidth, optimal_bandwidth, optimal_bandwidth)
  
)


############
############
############
############
# Make the discontinuity plot

main_data_tbl_sum_90_Education <- main_data_tbl %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D,
           Education) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_per_month_cat_education$BirthYM_Cat <-
  as.character(pop_per_month_cat_education$BirthYM_Cat)
main_data_tbl_sum_90_Education$BirthYM_Cat <-
  as.character(main_data_tbl_sum_90_Education$BirthYM_Cat)

# bring in the total pop
main_data_tbl_sum_90_Education <-
  main_data_tbl_sum_90_Education %>%
  left_join(pop_per_month_cat_education, by = c('Education', 'BirthYM_Cat'))

main_data_tbl_sum_90_Education$prop_vaxed <-
  (
    main_data_tbl_sum_90_Education$pop_vaxed / main_data_tbl_sum_90_Education$population
  )

# add age_for_model
main_data_tbl_sum_90_Education <-
  main_data_tbl_sum_90_Education %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)# %>%

# recreate the ordered factor for birth year cat
main_data_tbl_sum_90_Education$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_90_Education$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_90_Education$BirthYM_Cat[order(main_data_tbl_sum_90_Education$birth_y_m_D)])
  )

main_data_tbl_sum_90_Education <-
  main_data_tbl_sum_90_Education %>%
  dplyr::filter(!is.na(Education))

# change order of faceted variables
main_data_tbl_sum_90_Education$Education <- factor(
  main_data_tbl_sum_90_Education$Education,
  ordered = T,
  levels = c('Primary School', 'Gymnasium', 'University')
)


############
############
############
############
# The plot
education_model_plotRD <-
  ggplot(
    main_data_tbl_sum_90_Education,
    aes(
      x = age_for_model,
      y = prop_vaxed,
      # group = Education,
      fill = Letter_received,
      color = Letter_received
    )
  ) +
  geom_vline(xintercept = 49.5, linetype = 1) +
  
  geom_point(size = 0.8, alpha = 0.3) +
  
  geom_smooth(
    data = filter(main_data_tbl_sum_90_Education, Birth_year <= 1971),
    method = "lm"
  ) +
  geom_smooth(
    data = filter(main_data_tbl_sum_90_Education, Birth_year > 1971),
    method = "lm"
  ) +
  
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    values = c('dodgerblue3', 'red3'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  scale_fill_manual(
    values = c('dodgerblue1', 'red1'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  
  coord_cartesian (ylim = c(NA, 1)) +
  labs(
    x = "Age (years)"
    ,
    y = ''
    ,
    color = "Intervention:",
    subtitle = 'c) Education Attained',
    
    fill = " "
  ) +
  theme_classic() +
  theme(
    axis.text =  element_text(size = 12),
    axis.title =  element_text(size = 16),
    plot.subtitle  =  element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = 'none'
  ) +
  facet_wrap(. ~ Education)

education_model_plotRD



############
############
############
############
##### # # # # PRIMARY ANALYSIS - SALARY # # # # ##### ----------------------------------------------

############
############
############
############
# STEP 1
# High CATEGORY (High Salary)

model_simple_Kernels_Salary_design <-
  svydesign(id = ~ 1,
            data = main_data_tbl_logit,
            weights =  ~ weights_4_model)
model_simple_Kernels_Salary <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + Ind_slr_cat + Ind_slr_cat:Letter_received,
    design = model_simple_Kernels_Salary_design,
    family = binomial(link = 'logit')
  )


# Check results
results_logreg_salary <-
  logistic.display(model_simple_Kernels_Salary)

OR_salary_main <- results_logreg_salary$table[1, 1]
CI_low_salary_main <- results_logreg_salary$table[1, 2]
CI_high_salary_main <- results_logreg_salary$table[1, 3]
pvalue_salary_main <- results_logreg_salary$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_Salary_main <-
  coeftest(model_simple_Kernels_Salary, vcov = vcovHC)

# Get the robustified pvalue for Female
p_value_wald_kernels_inter_Salary_main_robustified_High <-
  wald_test_covariates_kernels_inter_Salary_main[2, 4]

# Get the robustified pvalue for interaction
p_value_wald_kernels_inter_Salary_main_robustified_interaction <-
  wald_test_covariates_kernels_inter_Salary_main[6, 4]



# isolate robustified confidence intervals
model_for_robust_salary <-
  broom::tidy(coeftest(model_simple_Kernels_Salary, vcov = vcovHC),
              conf.int = TRUE)
CI_low_salary_main_rbst <- exp(model_for_robust_salary$conf.low)[2]
CI_high_salary_main_rbst <-
  exp(model_for_robust_salary$conf.high)[2]

number_obs_inter_Salary_high <-  (nobs(model_simple_Kernels_Salary))


############
############
############
############
# STEP 2
# LOW CATEGORY (Low Salary)

model_simple_Kernels_Salary_low_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(Ind_slr_cat = fct_rev(Ind_slr_cat)),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Salary_low <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + Ind_slr_cat + Ind_slr_cat:Letter_received,
    design = model_simple_Kernels_Salary_low_design,
    family = binomial(link = 'logit')
  )


# Check results
results_logreg_Salary_low <-
  logistic.display(model_simple_Kernels_Salary_low)

OR_Salary_low <- results_logreg_Salary_low$table[1, 1]
CI_low_Salary_low <- results_logreg_Salary_low$table[1, 2]
CI_high_Salary_low <- results_logreg_Salary_low$table[1, 3]
pvalue_Salary_low <- results_logreg_Salary_low$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_Salary_low <-
  coeftest(model_simple_Kernels_Salary_low, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_Salary_main_robustified_Low <-
  wald_test_covariates_kernels_inter_Salary_low[2, 4]

# isolate robustified confidence intervals
model_for_robust_salary_low <-
  broom::tidy(coeftest(model_simple_Kernels_Salary_low, vcov = vcovHC),
              conf.int = TRUE)
CI_low_Salary_low_rbst <-
  exp(model_for_robust_salary_low$conf.low)[2]
CI_high_Salary_low_rbst <-
  exp(model_for_robust_salary_low$conf.high)[2]

# isolate number of observations
number_obs_inter_Salary_low <-
  (nobs(model_simple_Kernels_Salary_low))

# Get AGAIN the robustified pvalue for interaction
p_value_wald_kernels_inter_Salary_Low_robustified_interaction <-
  wald_test_covariates_kernels_inter_Salary_low[6, 4]



############
############
############
############
results_table_Salary <- data.frame(
  Model = c('Salr_High', 'Salr_Low'),
  OR = c(OR_salary_main, OR_Salary_low),
  CI_L = c(CI_low_salary_main_rbst, CI_low_Salary_low_rbst),
  CI_H = c(CI_high_salary_main_rbst, CI_high_Salary_low_rbst),
  P = c(pvalue_salary_main, pvalue_Salary_low),
  P_Robust = c(
    p_value_wald_kernels_inter_Salary_main_robustified_High,
    p_value_wald_kernels_inter_Salary_main_robustified_Low
  ),
  P_intrctn = c(
    p_value_wald_kernels_inter_Salary_main_robustified_interaction,
    p_value_wald_kernels_inter_Salary_Low_robustified_interaction
  ),
  nm_obs = c(number_obs_inter_Salary_high, number_obs_inter_Salary_low),
  bandwidth = c(optimal_bandwidth, optimal_bandwidth)
  
)




############
############
############
############
# Make discontinuity plot

main_data_tbl_sum_90_Salary <- main_data_tbl %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D,
           Ind_slr_cat) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_per_month_cat_salary$BirthYM_Cat <-
  as.character(pop_per_month_cat_salary$BirthYM_Cat)
main_data_tbl_sum_90_Salary$BirthYM_Cat <-
  as.character(main_data_tbl_sum_90_Salary$BirthYM_Cat)


# bring in the total pop
main_data_tbl_sum_90_Salary <- main_data_tbl_sum_90_Salary %>%
  left_join(pop_per_month_cat_salary, by = c('Ind_slr_cat', 'BirthYM_Cat'))

main_data_tbl_sum_90_Salary$prop_vaxed <-
  (main_data_tbl_sum_90_Salary$pop_vaxed / main_data_tbl_sum_90_Salary$population)


# add age_for_model
main_data_tbl_sum_90_Salary <- main_data_tbl_sum_90_Salary %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)# %>%


# recreate the ordered factor for birth year cat
main_data_tbl_sum_90_Salary$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_90_Salary$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_90_Salary$BirthYM_Cat[order(main_data_tbl_sum_90_Salary$birth_y_m_D)])
  )


main_data_tbl_sum_90_Salary <- main_data_tbl_sum_90_Salary %>%
  dplyr::filter(!is.na(Ind_slr_cat))


# change order of faceted variables
main_data_tbl_sum_90_Salary$Ind_slr_cat <- factor(
  main_data_tbl_sum_90_Salary$Ind_slr_cat,
  ordered = T,
  levels = c('Low', 'High')
)



############
############
############
############
# The plot
ggplot(
  main_data_tbl_sum_90_Salary,
  aes(
    x = age_for_model,
    y = prop_vaxed,
    fill = Letter_received,
    color = Letter_received
  )
) +
  geom_vline(xintercept = 49.5) +
  
  geom_point(size = 0.8, alpha = 0.3) +
  
  geom_smooth(
    data = filter(main_data_tbl_sum_90_Salary, Birth_year <= 1971),
    method = "lm"
  ) +
  geom_smooth(data = filter(main_data_tbl_sum_90_Salary, Birth_year > 1971),
              method = "lm") +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_color_manual(
    values = c('dodgerblue3', 'red3'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  scale_fill_manual(
    values = c('dodgerblue1', 'red1'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  coord_cartesian (ylim = c(NA, 1)) +
  labs(
    x = "Age (years)"
    ,
    y = ''
    ,
    color = "Intervention:",
    subtitle = 'd) Disposable Income',
    
    fill = " "
  ) +
  theme_classic() +
  theme(
    axis.text =  element_text(size = 12),
    axis.title =  element_text(size = 16),
    plot.subtitle  =  element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = 'none'
  ) +
  
  facet_wrap(Ind_slr_cat ~ .)




############
############
############
############
##### # # # # PRIMARY ANALYSIS - SEX # # # # ##### ----------------------------------------------


############
############
############
############
# STEP 1
# High CATEGORY (Female)

model_simple_Kernels_sex_design <-
  svydesign(id = ~ 1,
            data = main_data_tbl_logit,
            weights =  ~ weights_4_model)
model_simple_Kernels_sex <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + sex + sex:Letter_received,
    design = model_simple_Kernels_sex_design,
    family = binomial(link = 'logit')
  )

# Check results
results_logreg_sex <- logistic.display(model_simple_Kernels_sex)

OR_sex_main <- results_logreg_sex$table[1, 1]
CI_low_sex_main <- results_logreg_sex$table[1, 2]
CI_high_sex_main <- results_logreg_sex$table[1, 3]
pvalue_sex_main <- results_logreg_sex$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_sex_main <-
  coeftest(model_simple_Kernels_sex, vcov = vcovHC)

number_obs_inter_sex_female <-  (nobs(model_simple_Kernels_sex))

# Get the robustified pvalue for Female
p_value_wald_kernels_inter_sex_main_robustified_Female <-
  wald_test_covariates_kernels_inter_sex_main[2, 4]

# isolate robustified confidence intervals
model_for_robust_sex <-
  broom::tidy(coeftest(model_simple_Kernels_sex, vcov = vcovHC), conf.int = TRUE)
CI_low_sex_main_rbst <- exp(model_for_robust_sex$conf.low)[2]
CI_high_sex_main_rbst <- exp(model_for_robust_sex$conf.high)[2]

# Get the robustified pvalue for interaction
p_value_wald_kernels_inter_sex_main_robustified_interaction <-
  wald_test_covariates_kernels_inter_sex_main[6, 4]




############
############
############
############
# STEP 2
# LOW CATEGORY (Male)

model_simple_Kernels_sex_male_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(sex = fct_rev(sex)),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_sex_male <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + sex + sex:Letter_received,
    design = model_simple_Kernels_sex_male_design,
    family = binomial(link = 'logit')
  )



# Check results
results_logreg_sex_male <-
  logistic.display(model_simple_Kernels_sex_male)

OR_sex_male <- results_logreg_sex_male$table[1, 1]
CI_low_sex_male <- results_logreg_sex_male$table[1, 2]
CI__high_sex_male <- results_logreg_sex_male$table[1, 3]
pvalue_sex_male <- results_logreg_sex_male$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_sex_male <-
  coeftest(model_simple_Kernels_sex_male, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_sex_main_robustified_Male <-
  wald_test_covariates_kernels_inter_sex_male[2, 4]



# isolate robustified confidence intervals
model_for_robust_sex_male <-
  broom::tidy(coeftest(model_simple_Kernels_sex_male, vcov = vcovHC),
              conf.int = TRUE)
CI_low_sex_male_rbst <- exp(model_for_robust_sex_male$conf.low)[2]
CI__high_sex_male_rbst <-
  exp(model_for_robust_sex_male$conf.high)[2]

# isolate number of observations
number_obs_inter_sex_male <-  (nobs(model_simple_Kernels_sex_male))

# Get AGAIN the robustified pvalue for interaction
p_value_wald_kernels_inter_sex_MALE__robustified_interaction <-
  wald_test_covariates_kernels_inter_sex_male[6, 4]


############
############
############
############
results_table_sex <- data.frame(
  Model = c('Female', 'Male'),
  OR = c(OR_sex_main, OR_sex_male),
  CI_L = c(CI_low_sex_main_rbst, CI_low_sex_male_rbst),
  CI_H = c(CI_high_sex_main_rbst, CI__high_sex_male_rbst),
  P = c(pvalue_sex_main, pvalue_sex_male),
  P_Robust = c(
    p_value_wald_kernels_inter_sex_main_robustified_Female,
    p_value_wald_kernels_inter_sex_main_robustified_Male
  ),
  P_intrctn = c(
    p_value_wald_kernels_inter_sex_main_robustified_interaction,
    p_value_wald_kernels_inter_sex_MALE__robustified_interaction
  ),
  nm_obs = c(number_obs_inter_sex_female, number_obs_inter_sex_male),
  bandwidth = c(optimal_bandwidth, optimal_bandwidth)
  
)


############
############
############
############
# Make discontinuity plot

main_data_tbl_sum_90_sex <- main_data_tbl %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D,
           sex) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_per_month_cat_sex$BirthYM_Cat <-
  as.character(pop_per_month_cat_sex$BirthYM_Cat)
main_data_tbl_sum_90_sex$BirthYM_Cat <-
  as.character(main_data_tbl_sum_90_sex$BirthYM_Cat)


# bring in the total pop
main_data_tbl_sum_90_sex <- main_data_tbl_sum_90_sex %>%
  left_join(pop_per_month_cat_sex, by = c('sex', 'BirthYM_Cat'))

main_data_tbl_sum_90_sex$prop_vaxed <-
  (main_data_tbl_sum_90_sex$pop_vaxed / main_data_tbl_sum_90_sex$population)


# add age_for_model
main_data_tbl_sum_90_sex <- main_data_tbl_sum_90_sex %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)# %>%


# recreate the ordered factor for birth year cat
main_data_tbl_sum_90_sex$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_90_sex$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_90_sex$BirthYM_Cat[order(main_data_tbl_sum_90_sex$birth_y_m_D)])
  )


main_data_tbl_sum_90_sex <- main_data_tbl_sum_90_sex %>%
  dplyr::filter(!is.na(sex))


# change order of faceted variables
main_data_tbl_sum_90_sex$sex <- factor(
  main_data_tbl_sum_90_sex$sex,
  ordered = T,
  levels = c('Female', 'Male')
)

############
############
############
############
# The plot
ggplot(
  main_data_tbl_sum_90_sex,
  aes(
    x = age_for_model,
    y = prop_vaxed,
    fill = Letter_received,
    color = Letter_received
  )
) +
  geom_vline(xintercept = 49.5) +
  
  geom_point(size = 0.8, alpha = 0.3) +
  
  geom_smooth(data = filter(main_data_tbl_sum_90_sex, Birth_year <= 1971),
              method = "lm") +
  geom_smooth(data = filter(main_data_tbl_sum_90_sex, Birth_year > 1971),
              method = "lm") +
  
  scale_y_continuous(labels = scales::percent) +
  
  
  scale_color_manual(
    values = c('dodgerblue3', 'red3'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  scale_fill_manual(
    values = c('dodgerblue1', 'red1'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  coord_cartesian (ylim = c(NA, 1)) +
  labs(
    x = ''
    ,
    y = ''
    ,
    color = "Intervention:",
    
    subtitle = 'b) Sex',
    
    fill = " "
  ) +
  theme_classic() +
  theme(
    axis.text =  element_text(size = 12),
    axis.title =  element_text(size = 16),
    plot.subtitle  =  element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = 'none'
  ) +
  
  facet_wrap(sex ~ .)





############
############
############
############
##### # # # # PRIMARY ANALYSIS - TRUST # # # # ##### ----------------------------------------------

############
############
############
############
# STEP 1
# HIGHEST CATEGORY (SWEDEN)

model_simple_Kernels_design_Country_trust <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(trust_catgr = factor(
        trust_catgr, levels = c('Sweden', 'High', 'Low')
      )),
    weights =  ~ weights_4_model
  )

model_simple_Kernels_Country_trust <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + trust_catgr + trust_catgr:Letter_received,
    design = model_simple_Kernels_design_Country_trust,
    family = binomial(link = 'logit')
  )


# Check results
results_logreg_birthcountry_trust <-
  logistic.display(model_simple_Kernels_Country_trust)


OR_birthcoutnry_main_trust <-
  results_logreg_birthcountry_trust$table[1, 1]
CI_low_birthcoutnry_main_trust <-
  results_logreg_birthcountry_trust$table[1, 2]
CI__high_birthcoutnry_main_trust <-
  results_logreg_birthcountry_trust$table[1, 3]
pvalue_birthcoutnry_main_trust <-
  results_logreg_birthcountry_trust$table[1, 4]


# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_country_trust <-
  coeftest(model_simple_Kernels_Country_trust, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_country_trust <-
  wald_test_covariates_kernels_inter_country_trust[2, 4]


# isolate robustified confidence intervals
model_for_robust_country_trust <-
  broom::tidy(coeftest(model_simple_Kernels_Country_trust, vcov = vcovHC),
              conf.int = TRUE)
CI_low_birthcoutnry_main_trust_rbst <-
  exp(model_for_robust_country_trust$conf.low)[2]
CI__high_birthcoutnry_main_trust_rbst <-
  exp(model_for_robust_country_trust$conf.high)[2]
OR_robust_NOT_FOR_USE_JUST_FOR_COMPARISON_country <-
  exp(model_for_robust_country_trust$estimate)[2]

# isolate number of observations
number_obs_inter_country_trust <-
  (nobs(model_simple_Kernels_Country_trust))

# the the p-value here should be the same for all switches
model_interaction_country_trust_for_Robust <-
  car::linearHypothesis(
    model_simple_Kernels_Country_trust,
    c(
      'Letter_received:trust_catgrHigh',
      'Letter_received:trust_catgrLow'
    ),
    cluster = NULL,
    vcov = vcovHC(model_simple_Kernels_Country_trust, type = 'HC3')
  )

pvalue_interaction_country_Trust_for_Robust_sweden <-
  model_interaction_country_trust_for_Robust[2, 'Pr(>Chisq)']

############
############
############
############
# STEP 2
# MIDDLE CATEGORY (High)


model_simple_Kernels_Country_High_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(trust_catgr = factor(
        trust_catgr, levels = c('High', 'Sweden', 'Low')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Country_High <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + trust_catgr + trust_catgr:Letter_received,
    design = model_simple_Kernels_Country_High_design,
    family = binomial(link = 'logit')
  )


# Check results
results_logreg_birthcountry_High <-
  logistic.display(model_simple_Kernels_Country_High)

OR_birthcoutnry_High <- results_logreg_birthcountry_High$table[1, 1]
CI_low_birthcoutnry_High <-
  results_logreg_birthcountry_High$table[1, 2]
CI__high_birthcoutnry_High <-
  results_logreg_birthcountry_High$table[1, 3]
pvalue_birthcoutnry_High <-
  results_logreg_birthcountry_High$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_country_High <-
  coeftest(model_simple_Kernels_Country_High, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_country_High <-
  wald_test_covariates_kernels_inter_country_High[2, 4]

# isolate robustified confidence intervals
model_for_robust_country_high_trust <-
  broom::tidy(coeftest(model_simple_Kernels_Country_High, vcov = vcovHC),
              conf.int = TRUE)
CI_low_birthcoutnry_High_rbst <-
  exp(model_for_robust_country_high_trust$conf.low)[2]
CI__high_birthcoutnry_High_rbst <-
  exp(model_for_robust_country_high_trust$conf.high)[2]

# isolate number of observations
number_obs_inter_country_High <-
  (nobs(model_simple_Kernels_Country_High))

# the the p-value here should be the same for all switches

model_interaction_country_for_Robust_High <-
  car::linearHypothesis(
    model_simple_Kernels_Country_High,
    c(
      'Letter_received:trust_catgrSweden',
      'Letter_received:trust_catgrLow'
    ),
    cluster = NULL,
    
    vcov = vcovHC(model_simple_Kernels_Country_High, type = 'HC3')
  )

pvalue_interaction_country_for_Robust_high <-
  model_interaction_country_for_Robust_High[2, 'Pr(>Chisq)']


############
############
############
############
# STEP 3
# LOW CATEGORY (Low)

model_simple_Kernels_Country_Low_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(trust_catgr = factor(
        trust_catgr, levels = c('Low', 'Sweden', 'High')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Country_Low <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + trust_catgr + trust_catgr:Letter_received,
    design = model_simple_Kernels_Country_Low_design,
    family = binomial(link = 'logit')
  )



# Check results
results_logreg_birthcountry_Low <-
  logistic.display(model_simple_Kernels_Country_Low)

OR_birthcoutnry_Low <- results_logreg_birthcountry_Low$table[1, 1]
CI_low_birthcoutnry_Low <-
  results_logreg_birthcountry_Low$table[1, 2]
CI__high_birthcoutnry_Low <-
  results_logreg_birthcountry_Low$table[1, 3]
pvalue_birthcoutnry_Low <-
  results_logreg_birthcountry_Low$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_country_Low <-
  coeftest(model_simple_Kernels_Country_Low, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_country_Low <-
  wald_test_covariates_kernels_inter_country_Low[2, 4]

# ISOLATE robustified confidence intervals
model_for_robust_country_low <-
  broom::tidy(coeftest(model_simple_Kernels_Country_Low, vcov = vcovHC),
              conf.int = TRUE)
CI_low_birthcoutnry_Low_rbst <-
  exp(model_for_robust_country_low$conf.low)[2]
CI__high_birthcoutnry_Low_rbst <-
  exp(model_for_robust_country_low$conf.high)[2]


# isolate number of observations
number_obs_inter_country_Low <-
  (nobs(model_simple_Kernels_Country_Low))

model_interaction_country_for_Robust_Low <-
  car::linearHypothesis(
    model_simple_Kernels_Country_Low,
    c(
      'Letter_received:trust_catgrSweden',
      'Letter_received:trust_catgrHigh'
    ),
    cluster = NULL,
    
    vcov = vcovHC(model_simple_Kernels_Country_Low, type = 'HC3')
  )

pvalue_interaction_country_for_Robust_low <-
  model_interaction_country_for_Robust_Low[2, 'Pr(>Chisq)']

############
############
############
############
results_Birth_Country_Trust <- data.frame(
  Model = c('Sweden (Trust)', 'High trust', 'Low trust'),
  
  OR = c(
    OR_birthcoutnry_main_trust,
    OR_birthcoutnry_High,
    OR_birthcoutnry_Low
  ),
  CI_L = c(
    CI_low_birthcoutnry_main_trust_rbst,
    CI_low_birthcoutnry_High_rbst,
    CI_low_birthcoutnry_Low_rbst
  ),
  CI_H = c(
    CI__high_birthcoutnry_main_trust_rbst,
    CI__high_birthcoutnry_High_rbst,
    CI__high_birthcoutnry_Low_rbst
  ),
  P = c(
    pvalue_birthcoutnry_main_trust,
    pvalue_birthcoutnry_High,
    pvalue_birthcoutnry_Low
  ),
  P_Robust = c(
    p_value_wald_kernels_inter_country_trust,
    p_value_wald_kernels_inter_country_High,
    p_value_wald_kernels_inter_country_Low
  ),
  P_intrctn = c(
    pvalue_interaction_country_Trust_for_Robust_sweden,
    pvalue_interaction_country_for_Robust_low,
    pvalue_interaction_country_for_Robust_high
  ),
  nm_obs = c(
    number_obs_inter_country_trust,
    number_obs_inter_country_High,
    number_obs_inter_country_Low
  ),
  bandwidth = c(optimal_bandwidth, optimal_bandwidth, optimal_bandwidth)
  
)





############
############
############
############
# Make discontinuity plot

main_data_tbl_sum_90_Trust <- main_data_tbl %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D,
           trust_catgr) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_per_month_cat_trust$BirthYM_Cat <-
  as.character(pop_per_month_cat_trust$BirthYM_Cat)
main_data_tbl_sum_90_Trust$BirthYM_Cat <-
  as.character(main_data_tbl_sum_90_Trust$BirthYM_Cat)

# bring in the total pop
main_data_tbl_sum_90_Trust <- main_data_tbl_sum_90_Trust %>%
  left_join(pop_per_month_cat_trust, by = c('trust_catgr', 'BirthYM_Cat'))

main_data_tbl_sum_90_Trust$prop_vaxed <-
  (main_data_tbl_sum_90_Trust$pop_vaxed / main_data_tbl_sum_90_Trust$population)


# add age_for_model
main_data_tbl_sum_90_Trust <- main_data_tbl_sum_90_Trust %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)# %>%


# recreate the ordered factor for birth year cat
main_data_tbl_sum_90_Trust$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_90_Trust$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_90_Trust$BirthYM_Cat[order(main_data_tbl_sum_90_Trust$birth_y_m_D)])
  )


main_data_tbl_sum_90_Trust <- main_data_tbl_sum_90_Trust %>%
  dplyr::filter(!is.na(trust_catgr))



# change order of faceted variables
main_data_tbl_sum_90_Trust$trust_catgr <- factor(
  main_data_tbl_sum_90_Trust$trust_catgr,
  ordered = T,
  levels = c('Sweden', 'Low', 'High')
)


############
############
############
############
# The plot
ggplot(
  main_data_tbl_sum_90_Trust,
  aes(
    x = age_for_model,
    y = prop_vaxed,
    fill = Letter_received,
    color = Letter_received
  )
) +
  geom_vline(xintercept = 49.5) +
  
  geom_point(size = 0.8, alpha = 0.3) +
  
  geom_smooth(data = filter(main_data_tbl_sum_90_Trust, Birth_year <= 1971),
              method = "lm") +
  geom_smooth(data = filter(main_data_tbl_sum_90_Trust, Birth_year > 1971),
              method = "lm") +
  
  scale_y_continuous(labels = scales::percent) +
  
  coord_cartesian (ylim = c(NA, 1)) +
  labs(
    x = ''
    ,
    y = ''
    ,
    color = "Intervention:",
    subtitle = 'a) Trust Level of Birth Country',
    
    fill = " "
  ) +
  theme_classic() +
  theme(
    axis.text =  element_text(size = 12),
    axis.title =  element_text(size = 16),
    plot.subtitle  =  element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = 'none'
  ) +
  
  
  scale_color_manual(
    values = c('dodgerblue3', 'red3'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  scale_fill_manual(
    values = c('dodgerblue1', 'red1'),
    labels = c('Letter notification', 'Self-booking')
  ) +
  
  facet_wrap(trust_catgr ~ .)





############
############
############
############
##### # # # # PRIMARY ANALYSIS - LENGTH OF RESIDENCE # # # # ##### ----------------------------------------------

############
############
############
############
# STEP 1
# HIGHEST CATEGORY (SWEDEN)

model_simple_Kernels_design_Stay <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(length_stay_cat = factor(
        length_stay_cat, levels = c('Born in Sweden', 'Long', 'Short')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Stay <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + length_stay_cat + length_stay_cat:Letter_received,
    design = model_simple_Kernels_design_Stay,
    family = binomial(link = 'logit')
  )
# Check results
results_logreg_Stay <- logistic.display(model_simple_Kernels_Stay)

OR_Stay_main <- results_logreg_Stay$table[1, 1]
CI_low_Stay_main <- results_logreg_Stay$table[1, 2]
CI__high_Stay_main <- results_logreg_Stay$table[1, 3]
pvalue_Stay_main <- results_logreg_Stay$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_Stay <-
  coeftest(model_simple_Kernels_Stay, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_Stay <-
  wald_test_covariates_kernels_inter_Stay[2, 4]
# isolate robustified confidence intervals
model_for_robust_Stay <-
  broom::tidy(coeftest(model_simple_Kernels_Stay, vcov = vcovHC), conf.int = TRUE)
CI_low_Stay_main_rbst <- exp(model_for_robust_Stay$conf.low)[2]
CI__high_Stay_main_rbst <- exp(model_for_robust_Stay$conf.high)[2]
OR_robust_NOT_FOR_USE_JUST_FOR_COMPARISON_Stay <-
  exp(model_for_robust_Stay$estimate)[2]

# isolate number of observations
number_obs_inter_Stay <- (nobs(model_simple_Kernels_Stay))

# the the p-value here should be the same for all switches
model_interaction_Stay_for_Robust <-
  car::linearHypothesis(
    model_simple_Kernels_Stay,
    c(
      'Letter_received:length_stay_catLong',
      'Letter_received:length_stay_catShort'
    ),
    cluster = NULL,
    vcov = vcovHC(model_simple_Kernels_Stay, type = 'HC3')
  )
pvalue_interaction_Stay_for_Robust_sweden <-
  model_interaction_Stay_for_Robust[2, 'Pr(>Chisq)']



############
############
############
############
# STEP 2
# MIDDLE CATEGORY (Long)


model_simple_Kernels_Stay_Long_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(length_stay_cat = factor(
        length_stay_cat, levels = c('Long', 'Born in Sweden', 'Short')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Stay_Long <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + length_stay_cat + length_stay_cat:Letter_received,
    design = model_simple_Kernels_Stay_Long_design,
    family = binomial(link = 'logit')
  )


# Check results
results_logreg_Stay_Long <-
  logistic.display(model_simple_Kernels_Stay_Long)

OR_Stay_Long <- results_logreg_Stay_Long$table[1, 1]
CI_low_Stay_Long <- results_logreg_Stay_Long$table[1, 2]
CI__high_Stay_Long <- results_logreg_Stay_Long$table[1, 3]
pvalue_Stay_Long <- results_logreg_Stay_Long$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_Stay_Long <-
  coeftest(model_simple_Kernels_Stay_Long, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_Stay_Long <-
  wald_test_covariates_kernels_inter_Stay_Long[2, 4]

# isolate robustified confidence intervals
model_for_robust_Stay_high <-
  broom::tidy(coeftest(model_simple_Kernels_Stay_Long, vcov = vcovHC),
              conf.int = TRUE)
CI_low_Stay_Long_rbst <- exp(model_for_robust_Stay_high$conf.low)[2]
CI__high_Stay_Long_rbst <-
  exp(model_for_robust_Stay_high$conf.high)[2]

# isolate number of observations
number_obs_inter_Stay_Long <- (nobs(model_simple_Kernels_Stay_Long))

model_interaction_Stay_for_Robust_High <-
  car::linearHypothesis(
    model_simple_Kernels_Stay_Long,
    c(
      'Letter_received:length_stay_catBorn in Sweden',
      'Letter_received:length_stay_catShort'
    ),
    cluster = NULL,
    
    vcov = vcovHC(model_simple_Kernels_Stay_Long, type = 'HC3')
  )

pvalue_interaction_Stay_for_Robust_high <-
  model_interaction_Stay_for_Robust_High[2, 'Pr(>Chisq)']


############
############
############
############
# STEP 3
# LOW CATEGORY (Short)

model_simple_Kernels_Stay_Short_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(length_stay_cat = factor(
        length_stay_cat, levels = c('Short', 'Born in Sweden', 'Long')
      )),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Stay_Short <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + length_stay_cat + length_stay_cat:Letter_received,
    design = model_simple_Kernels_Stay_Short_design,
    family = binomial(link = 'logit')
  )



# Check results
results_logreg_Stay_Short <-
  logistic.display(model_simple_Kernels_Stay_Short)

OR_Stay_Short <- results_logreg_Stay_Short$table[1, 1]
CI_low_Stay_Short <- results_logreg_Stay_Short$table[1, 2]
CI__high_Stay_Short <- results_logreg_Stay_Short$table[1, 3]
pvalue_Stay_Short <- results_logreg_Stay_Short$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_Stay_Short <-
  coeftest(model_simple_Kernels_Stay_Short, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_Stay_Short <-
  wald_test_covariates_kernels_inter_Stay_Short[2, 4]

# ISOLATE robustified confidence intervals
model_for_robust_Stay_Short <-
  broom::tidy(coeftest(model_simple_Kernels_Stay_Short, vcov = vcovHC),
              conf.int = TRUE)
CI_low_Stay_Short_rbst <-
  exp(model_for_robust_Stay_Short$conf.low)[2]
CI__high_Stay_Short_rbst <-
  exp(model_for_robust_Stay_Short$conf.high)[2]


# isolate number of observations
number_obs_inter_Stay_Short <-
  (nobs(model_simple_Kernels_Stay_Short))

model_interaction_Stay_for_Robust_LM <-
  car::linearHypothesis(
    model_simple_Kernels_Stay_Short,
    c(
      'Letter_received:length_stay_catBorn in Sweden',
      'Letter_received:length_stay_catLong'
    ),
    cluster = NULL,
    
    vcov = vcovHC(model_simple_Kernels_Stay_Short, type = 'HC3')
  )

pvalue_interaction_Stay_for_Robust_low <-
  model_interaction_Stay_for_Robust_LM[2, 'Pr(>Chisq)']


############
############
############
############
results_Length_Stay <- data.frame(
  Model = c('Born in Sweden', 'Long length', 'Short length'),
  
  OR = c(OR_Stay_main, OR_Stay_Long, OR_Stay_Short),
  CI_L = c(
    CI_low_Stay_main_rbst,
    CI_low_Stay_Long_rbst,
    CI_low_Stay_Short_rbst
  ),
  CI_H = c(
    CI__high_Stay_main_rbst,
    CI__high_Stay_Long_rbst,
    CI__high_Stay_Short_rbst
  ),
  P = c(pvalue_Stay_main, pvalue_Stay_Long, pvalue_Stay_Short),
  P_Robust = c(
    p_value_wald_kernels_inter_Stay,
    p_value_wald_kernels_inter_Stay_Long,
    p_value_wald_kernels_inter_Stay_Short
  ),
  P_intrctn = c(
    pvalue_interaction_Stay_for_Robust_sweden,
    pvalue_interaction_Stay_for_Robust_low,
    pvalue_interaction_Stay_for_Robust_high
  ),
  nm_obs = c(
    number_obs_inter_Stay,
    number_obs_inter_Stay_Long,
    number_obs_inter_Stay_Short
  ),
  bandwidth = c(optimal_bandwidth, optimal_bandwidth, optimal_bandwidth)
  
)


############
############
############
############
##### # # # # PRIMARY ANALYSIS - MEDICAL RISK # # # # ##### ----------------------------------------------



############
############
############
############
# STEP 1
# High CATEGORY (Low medical risk)

model_simple_Kernels_Risk_design <-
  svydesign(id = ~ 1,
            data = main_data_tbl_logit,
            weights =  ~ weights_4_model)
model_simple_Kernels_Risk <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + Risk_group + Risk_group:Letter_received,
    design = model_simple_Kernels_Risk_design,
    family = binomial(link = 'logit')
  )

# Check results
results_logreg_Risk <- logistic.display(model_simple_Kernels_Risk)
# summary(model_simple_Kernels_Risk)

OR_Risk_main <- results_logreg_Risk$table[1, 1]
CI_low_Risk_main <- results_logreg_Risk$table[1, 2]
CI_high_Risk_main <- results_logreg_Risk$table[1, 3]
pvalue_Risk_main <- results_logreg_Risk$table[1, 4]


# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_Risk_main <-
  coeftest(model_simple_Kernels_Risk, vcov = vcovHC)

number_obs_inter_Risk_No_risk <-  (nobs(model_simple_Kernels_Risk))

# Get the robustified pvalue for No_risk
p_value_wald_kernels_inter_Risk_main_robustified_No_risk <-
  wald_test_covariates_kernels_inter_Risk_main[2, 4]

# isolate robustified confidence intervals
model_for_robust_Risk <-
  broom::tidy(coeftest(model_simple_Kernels_Risk, vcov = vcovHC), conf.int = TRUE)
CI_low_Risk_main_rbst <- exp(model_for_robust_Risk$conf.low)[2]
CI_high_Risk_main_rbst <- exp(model_for_robust_Risk$conf.high)[2]

# Get the robustified pvalue for interaction
p_value_wald_kernels_inter_Risk_main_robustified_interaction <-
  wald_test_covariates_kernels_inter_Risk_main[6, 4]




############
############
############
############
# STEP 2
# LOW CATEGORY (High medical risk)
model_simple_Kernels_Risk_Yes_risk_design <-
  svydesign(
    id = ~ 1,
    data = main_data_tbl_logit %>%
      mutate(Risk_group = fct_rev(Risk_group)),
    weights =  ~ weights_4_model
  )
model_simple_Kernels_Risk_Yes_risk <-
  svyglm(
    Vax_within_90  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1) + Risk_group + Risk_group:Letter_received,
    design = model_simple_Kernels_Risk_Yes_risk_design,
    family = binomial(link = 'logit')
  )

# Check results
results_logreg_Risk_Yes_risk <-
  logistic.display(model_simple_Kernels_Risk_Yes_risk)

OR_Risk_Yes_risk <- results_logreg_Risk_Yes_risk$table[1, 1]
CI_low_Risk_Yes_risk <- results_logreg_Risk_Yes_risk$table[1, 2]
CI__high_Risk_Yes_risk <- results_logreg_Risk_Yes_risk$table[1, 3]
pvalue_Risk_Yes_risk <- results_logreg_Risk_Yes_risk$table[1, 4]

# WALD
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_inter_Risk_Yes_risk <-
  coeftest(model_simple_Kernels_Risk_Yes_risk, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_inter_Risk_main_robustified_Yes_risk <-
  wald_test_covariates_kernels_inter_Risk_Yes_risk[2, 4]

# isolate robustified confidence intervals
model_for_robust_Risk_Yes_risk <-
  broom::tidy(coeftest(model_simple_Kernels_Risk_Yes_risk, vcov = vcovHC),
              conf.int = TRUE)
CI_low_Risk_Yes_risk_rbst <-
  exp(model_for_robust_Risk_Yes_risk$conf.low)[2]
CI__high_Risk_Yes_risk_rbst <-
  exp(model_for_robust_Risk_Yes_risk$conf.high)[2]

# isolate number of observations
number_obs_inter_Risk_Yes_risk <-
  (nobs(model_simple_Kernels_Risk_Yes_risk))

# Get AGAIN the robustified pvalue for interaction
p_value_wald_kernels_inter_Risk_Yes_risk__robustified_interaction <-
  wald_test_covariates_kernels_inter_Risk_Yes_risk[6, 4]


############
############
############
############
results_Risk_group <- data.frame(
  Model = c('Low medical risk', 'High medical risk'),
  OR = c(OR_Risk_main, OR_Risk_Yes_risk),
  CI_L = c(CI_low_Risk_main_rbst, CI_low_Risk_Yes_risk_rbst),
  CI_H = c(CI_high_Risk_main_rbst, CI__high_Risk_Yes_risk_rbst),
  P = c(pvalue_Risk_main, pvalue_Risk_Yes_risk),
  P_Robust = c(
    p_value_wald_kernels_inter_Risk_main_robustified_No_risk,
    p_value_wald_kernels_inter_Risk_main_robustified_Yes_risk
  ),
  P_intrctn = c(
    p_value_wald_kernels_inter_Risk_main_robustified_interaction,
    p_value_wald_kernels_inter_Risk_Yes_risk__robustified_interaction
  ),
  nm_obs = c(
    number_obs_inter_Risk_No_risk,
    number_obs_inter_Risk_Yes_risk
  ),
  bandwidth = c(optimal_bandwidth, optimal_bandwidth)
  
)




############
############
############
############
##### # # # # SENSITIVITY - DISTRIBUTION OF VARIABLES AROUND THE CUTOFF # # # # ##### ----------------------------------------------



############
############
############
############
# Adjust the variables for these sensitivity analyses

data_Sens_Distr <-  main_data_tbl_logit %>%
  mutate(
    sex_Sens = case_when(sex == 'Male' ~ 1 ,
                         sex == 'Female' ~ 0 ,
                         TRUE ~ NA),
    
    University_Sens = case_when(
      Education  == 'University' ~ 1 ,
      Education == 'Gymnasium' ~ 0 ,
      Education == 'Primary School' ~ 0 ,
      TRUE ~ NA
    ),
    
    Prim_Sens = case_when(
      Education == 'Primary School' ~ 1 ,
      Education  == 'Gymnasium' ~ 0 ,
      Education == 'University' ~ 0 ,
      TRUE ~ NA
    ),
    
    Salar_Sens = case_when(Ind_slr_cat  == 'High' ~ 1 ,
                           Ind_slr_cat == 'Low' ~ 0 ,
                           TRUE ~ NA),
    
    Risk_Sens = case_when(Risk_group == 'Yes' ~ 1 ,
                          Risk_group == 'No' ~ 0 ,
                          TRUE ~ NA),
    
    Ahead_Sched_Sens = case_when(Vax_within_0  == 'Yes' ~ 1 ,
                                 Vax_within_0 == 'No' ~ 0 ,
                                 TRUE ~ NA),
    
    Long_stay_Sens = case_when(
      length_stay_cat == 'Long' ~ 1 ,
      length_stay_cat == 'Born in Sweden' ~ 0 ,
      length_stay_cat == 'Short' ~ 0 ,
      TRUE ~ NA
    ),
    
    Short_stay_Sens = case_when(
      length_stay_cat == 'Short' ~ 1 ,
      length_stay_cat == 'Born in Sweden' ~ 0 ,
      length_stay_cat == 'Long' ~ 0 ,
      TRUE ~ NA
    ),
    
    
    High_trust_Sens = case_when(
      trust_catgr == 'High' ~ 1 ,
      trust_catgr == 'Sweden' ~ 0 ,
      trust_catgr == 'Low' ~ 0 ,
      
      TRUE ~ NA
    ),
    
    
    Low_trust_Sens = case_when(
      trust_catgr == 'Low' ~ 1 ,
      trust_catgr == 'Sweden' ~ 0 ,
      trust_catgr == 'High' ~ 0 ,
      
      TRUE ~ NA
    )
  )


############  ############
############  ############
############  ############
############  ############
# CUT OFF SEX
bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    sex_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    sex_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]

results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]
model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Male <- data.frame(
  Cutoff = 'Male',
  OR = round(main_log_OR_Sens_Distri, digits = 2),
  SD = round(main_log_SD_Sens_Distri, digits = 2),
  CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                       2),
  CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                        2),
  pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                       4)
)


rm(main_log_OR_Sens_Distri)
rm(main_log_SD_Sens_Distri)
rm(CI_low_epanechnikov_rbst_Sens_Distri)
rm(CI__high_epanechnikov_rbst_Sens_Distri)
rm(p_value_wald_kernels_Sens_Distri)


############  ############
############  ############
############  ############
############  ############
# CUT OFF UNIVERSITY EDUCATION

bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    University_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

# library(survey)
model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    University_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]

results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]


model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Univ <- data.frame(
  Cutoff = 'University',
  OR = round(main_log_OR_Sens_Distri, digits = 2),
  SD = round(main_log_SD_Sens_Distri, digits = 2),
  CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                       2),
  CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                        2),
  pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                       4)
  
)

rm(main_log_OR_Sens_Distri)
rm(main_log_SD_Sens_Distri)
rm(CI_low_epanechnikov_rbst_Sens_Distri)
rm(CI__high_epanechnikov_rbst_Sens_Distri)
rm(p_value_wald_kernels_Sens_Distri)


############  ############
############  ############
############  ############
############  ############
# CUT OFF PRIMARY SCHOOL EDUCATION

bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    Prim_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    Prim_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]


results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]
# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]

model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Primary <- data.frame(
  Cutoff = 'Primary',
  OR = round(main_log_OR_Sens_Distri, digits =
               2),
  SD = round(main_log_SD_Sens_Distri, digits = 2),
  CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                       2),
  CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                        2),
  pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                       4)
  
)

rm(main_log_OR_Sens_Distri)
rm(main_log_SD_Sens_Distri)
rm(CI_low_epanechnikov_rbst_Sens_Distri)
rm(CI__high_epanechnikov_rbst_Sens_Distri)
rm(p_value_wald_kernels_Sens_Distri)


############  ############
############  ############
############  ############
############  ############
# CUT OFF HIGH SALARY

bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    Salar_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    Salar_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]

results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]

model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Salary <- data.frame(
  Cutoff = 'High salary',
  OR = round(main_log_OR_Sens_Distri, digits =
               2),
  SD = round(main_log_SD_Sens_Distri, digits = 2),
  CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                       2),
  CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                        2),
  pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                       4)
)


############  ############
############  ############
############  ############
############  ############
# CUT OFF MEDICAL RISK

bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    Risk_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

# library(survey)
model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    Risk_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]

results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]
# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]


model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Risk <- data.frame(
  Cutoff = 'High risk',
  OR = round(main_log_OR_Sens_Distri, digits = 2),
  SD = round(main_log_SD_Sens_Distri, digits = 2),
  CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                       2),
  CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                        2),
  pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                       4)
)


############  ############
############  ############
############  ############
############  ############
# CUT OFF INDIVIDUALS VACCINATED AHEAD OF TIME

bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    Ahead_Sched_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    Ahead_Sched_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]


results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]

model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])
main_OR_SD_Sens_Distri_Ahead <-
  data.frame(
    Cutoff = 'Ahead of schedule',
    OR = round(main_log_OR_Sens_Distri, digits = 2),
    SD = round(main_log_SD_Sens_Distri, digits = 2),
    CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                         2),
    CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                          2),
    pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                         4)
  )


############  ############
############  ############
############  ############
############  ############
# CUT OFF LONG STAY
bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    Long_stay_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    Long_stay_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]

results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]

model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Length_long <-
  data.frame(
    Cutoff = 'Long stay',
    OR = round(main_log_OR_Sens_Distri, digits = 2),
    SD = round(main_log_SD_Sens_Distri, digits = 2),
    CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                         2),
    CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                          2),
    pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                         4)
  )


############  ############
############  ############
############  ############
############  ############
# CUT OFF SHORT STAY
bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    Short_stay_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    Short_stay_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]

results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]

model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])
main_OR_SD_Sens_Distri_Length_short <-
  data.frame(
    Cutoff = 'Short stay',
    OR = round(main_log_OR_Sens_Distri, digits =
                 2),
    SD = round(main_log_SD_Sens_Distri, digits = 2),
    CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                         2),
    CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                          2),
    pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                         4)
  )



############  ############
############  ############
############  ############
############  ############
# CUT OFF HIGH TRUST
bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    High_trust_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    High_trust_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]


results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]
model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Trust_high <-
  data.frame(
    Cutoff = 'High trust',
    OR = round(main_log_OR_Sens_Distri, digits =
                 2),
    SD = round(main_log_SD_Sens_Distri, digits = 2),
    CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                         2),
    CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                          2),
    pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                         4)
    
  )


############  ############
############  ############
############  ############
############  ############
# CUT OFF LOW TRUST

bandwidth_imbens_Sens_Distri <-
  rddapp::rd_est(
    Low_trust_Sens  ~  age_for_model | Letter_received,
    data = data_Sens_Distr,
    cutpoint = 49.5,
    kernel = 'epanechnikov',
    t.design = 'geq',
    
    
    bw = 'IK12'
  )

selected_bandwidth_imbens_Sens_Distri <-
  bandwidth_imbens_Sens_Distri$bw[4]
# HERE SELECT THE BANDWIDTH FOR THE MAIN MODEL
optimal_bandwidth_Sens_Distri <-
  selected_bandwidth_imbens_Sens_Distri

# write an Epanechnikov_kernel that will assign weights (weight function)
Epanechnikov_kernel_IK_Sens_Distri <-
  function(x, xi = 49.5, bandwidth = selected_bandwidth_imbens_Sens_Distri) {
    u <- abs((x - xi) / bandwidth)
    return(ifelse(u <= 1, 3 / 4 * (1 - u ^ 2), 0))
  }

# Apply the kernel weight function to the age variable
weights_Epanechnikov_IK_Sens_Distri <-
  Epanechnikov_kernel_IK_Sens_Distri(data_Sens_Distr$age_for_model)

# HERE SELECT THE WEIGHT FOR THE MAIN MODELS
weights_4_model_Sens_Distri <- weights_Epanechnikov_IK_Sens_Distri

model_simple_Kernels_design_Sens_Distri <-
  svydesign(
    id = ~ 1,
    data = data_Sens_Distr,
    weights =  ~ weights_4_model_Sens_Distri
  )
model_simple_Kernels_Sens_Distri <-
  svyglm(
    Low_trust_Sens  ~ Letter_received +  bs(age_for_model , knots = 49.5, degree = 1),
    design = model_simple_Kernels_design_Sens_Distri,
    family = binomial(link = 'logit')
  )

or_main_model_no_interaction_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
or_main_model_no_interaction_Sens_Distri <-
  or_main_model_no_interaction_Sens_Distri$table[1, 1]

results_logreg_epanechnikov_Sens_Distri <-
  logistic.display(model_simple_Kernels_Sens_Distri)
OR_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 1]
CI_low_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 2]
CI__high_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 3]
pvalue_epanechnikov_Sens_Distri <-
  results_logreg_epanechnikov_Sens_Distri$table[1, 4]

# Apply robust standard errors using the sandwich library
# Perform Wald test with robust standard errors for each covariate
wald_test_covariates_kernels_Sens_Distri <-
  coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC)

# isolate p-value of interest
p_value_wald_kernels_Sens_Distri <-
  wald_test_covariates_kernels_Sens_Distri[2, 4]

# isolate robustified confidence intervals
model_for_robust_rbst_Sens_Distri  <-
  broom::tidy(coeftest(model_simple_Kernels_Sens_Distri, vcov = vcovHC),
              conf.int = TRUE)
CI_low_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.low)[2]
CI__high_epanechnikov_rbst_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$conf.high)[2]

model_for_robust_rbst_Uppsala_Sens_Distri <-
  model_for_robust_rbst_Sens_Distri
model_simple_Kernels_Uppsala_Sens_Distri <-
  model_simple_Kernels_Sens_Distri

# isolate number of observations
number_obs_logreg_Sens_Distri <-
  as.numeric((nobs(model_simple_Kernels_Sens_Distri)))

# keep main intercept and its std.error from the robust
main_log_OR_Sens_Distri <-
  exp(summary(model_simple_Kernels_Sens_Distri)$coefficients[2, 1])
main_log_SD_Sens_Distri <-
  exp(model_for_robust_rbst_Sens_Distri$std.error[2])

main_OR_SD_Sens_Distri_Trust_low <-
  data.frame(
    Cutoff = 'Low trust',
    OR = round(main_log_OR_Sens_Distri, digits =
                 2),
    SD = round(main_log_SD_Sens_Distri, digits = 2),
    CI_rob_low = round(CI_low_epanechnikov_rbst_Sens_Distri, digits =
                         2),
    CI_rob_high = round(CI__high_epanechnikov_rbst_Sens_Distri, digits =
                          2),
    pvalue_rob = round(p_value_wald_kernels_Sens_Distri, digits =
                         4)
  )

############  ############
############  ############
############  ############
############  ############
# CUT OFF BRING ALL TOGETHER
sensitivity_distribution_cutoff <-
  rbind(
    main_OR_SD_Sens_Distri_Male,
    main_OR_SD_Sens_Distri_Univ,
    main_OR_SD_Sens_Distri_Primary,
    main_OR_SD_Sens_Distri_Salary,
    main_OR_SD_Sens_Distri_Risk,
    main_OR_SD_Sens_Distri_Ahead,
    main_OR_SD_Sens_Distri_Length_long,
    main_OR_SD_Sens_Distri_Length_short,
    main_OR_SD_Sens_Distri_Trust_high,
    main_OR_SD_Sens_Distri_Trust_low
  )



############
############
############
############
##### # # # # SECONDARY ANALYSIS - rdrobust with Imbens and Kalyanaraman  # # # # ##### ----------------------------------------------

rd_analysis <- rdrobust(
  y = main_data_tbl_logit$Vax_within_90,
  x = main_data_tbl_logit$age_for_model,
  c = 49.5,
  h = selected_bandwidth_imbens,
  
  p = 1,
  kernel = 'epanechnikov'
)

############
############
############
############
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


results_table_Main_rdrobust <- data.frame(
  Model = c('rdrobust'),
  Coeff = c(coeff_main_rd),
  CI_L = c(CI95_low_main_rd),
  CI_H = c(CI95_high_main_rd),
  P = c(pvalue_main_rd),
  P_Robust = c(pvalue_main_rd_robust),
  
  P_intrctn = c(NA),
  
  nm_obs = c(nm_obs_rdrobust),
  bandwidth = c(selected_bndwdth_rdrob),
  bndw_bias_correct = c(selected_bndwdth_Bias_Corrected_rdrob)
  
)




############
############
############
############
##### # # # # SENSITIVITY - rdrobust with Calonico, Cattaneo and Titiunik  # # # # ##### ----------------------------------------------

rd_analysis_CCT <- rdrobust(
  y = main_data_tbl_logit$Vax_within_90,
  x = main_data_tbl_logit$age_for_model,
  c = 49.5,
  h = selected_bandwidth_Calonico,
  
  p = 1,
  kernel = 'epanechnikov'
)

############
############
############
############
rd_analysis_CCT %>%
  summary()

coeff_main_rd_CCT <- rd_analysis_CCT$coef[3]
se_main_rd_CCT <- rd_analysis_CCT$se[3]
pvalue_main_rd_CCT <- rd_analysis_CCT$pv[1]
pvalue_main_rd_robust_CCT <- rd_analysis_CCT$pv[3]

CI95_low_main_rd_CCT <- rd_analysis_CCT$ci[3, 1]
CI95_high_main_rd_CCT <- rd_analysis_CCT$ci[3, 2]

nm_obs_rdrobust_CCT <- rd_analysis_CCT$N[1] + rd_analysis_CCT$N[2]
selected_bndwdth_rdrob_CCT <- rd_analysis_CCT$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_CCT <-
  rd_analysis_CCT$bws[2, 1]




############
############
############
############
##### # # # # SENSITIVITY - rdrobust without individuals ahead of time  # # # # ##### ----------------------------------------------

rd_analysis_ahead <-
  rdrobust(
    y = main_data_tbl_logit_ahead_of_time_out$Vax_within_90,
    x = main_data_tbl_logit_ahead_of_time_out$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens_ahead,
    
    p = 1,
    kernel = 'epanechnikov'
  )

############
############
############
############
rd_analysis_ahead %>%
  summary()

coeff_main_rd_ahead <- rd_analysis_ahead$coef[3]
se_main_rd_ahead <- rd_analysis_ahead$se[3]
pvalue_main_rd_ahead <- rd_analysis_ahead$pv[1]
pvalue_main_rd_robust_ahead <- rd_analysis_ahead$pv[3]

CI95_low_main_rd_ahead <- rd_analysis_ahead$ci[3, 1]
CI95_high_main_rd_ahead <- rd_analysis_ahead$ci[3, 2]

nm_obs_rdrobust_ahead <-
  rd_analysis_ahead$N[1] + rd_analysis_ahead$N[2]
selected_bndwdth_rdrob_ahead <- rd_analysis_ahead$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_ahead <-
  rd_analysis_ahead$bws[2, 1]


############
############
############
############
##### # # # # SECONDARY ANALYSIS - rdrobust for EDUCATION  # # # # ##### ----------------------------------------------

############
############
############
############
# STEP 1
# split the data into the country categories

main_data_tbl_logit_Edu_Prim <- main_data_tbl_logit %>%
  filter(Education == 'Primary School')

main_data_tbl_logit_Edu_Gymn <- main_data_tbl_logit %>%
  filter(Education == 'Gymnasium')

main_data_tbl_logit_Edu_Uni <- main_data_tbl_logit %>%
  filter(Education == 'University')


############
############
############
############
# STEP 2
# run the rd robust for eahc of the split categories without interaction
rd_analysis_edu_Prim <-
  rdrobust(
    y = main_data_tbl_logit_Edu_Prim$Vax_within_90,
    x = main_data_tbl_logit_Edu_Prim$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )



rd_analysis_edu_Gymn <-
  rdrobust(
    y = main_data_tbl_logit_Edu_Gymn$Vax_within_90,
    x = main_data_tbl_logit_Edu_Gymn$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )


rd_analysis_edu_Uni <-
  rdrobust(
    y = main_data_tbl_logit_Edu_Uni$Vax_within_90,
    x = main_data_tbl_logit_Edu_Uni$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )


############
############
############
############
# STEP 3
# save the betas and se

Primary_beta <- rd_analysis_edu_Prim$coef[3]
Primary_se <- rd_analysis_edu_Prim$se[3]
pvalue_primary <- rd_analysis_edu_Prim$pv[1]
CI95_low_primary <- rd_analysis_edu_Prim$ci[3, 1]
CI95_high_primary <- rd_analysis_edu_Prim$ci[3, 2]

pvalue_Primar_robust <- rd_analysis_edu_Prim$pv[3]
nm_obs_rdrobust_Primar <-
  rd_analysis_edu_Prim$N[1] + rd_analysis_edu_Prim$N[2]
selected_bndwdth_rdrob_Primar <- rd_analysis_edu_Prim$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Primar <-
  rd_analysis_edu_Prim$bws[2, 1]



Gymnasium_beta <- rd_analysis_edu_Gymn$coef[3]
Gymnasium_se <- rd_analysis_edu_Gymn$se[3]
pvalue_gymnas <- rd_analysis_edu_Gymn$pv[1]
CI95_low_gymnas <- rd_analysis_edu_Gymn$ci[3, 1]
CI95_high_gymnas <- rd_analysis_edu_Gymn$ci[3, 2]

pvalue_Gymn_robust <- rd_analysis_edu_Gymn$pv[3]
nm_obs_rdrobust_Gymn <-
  rd_analysis_edu_Gymn$N[1] + rd_analysis_edu_Gymn$N[2]
selected_bndwdth_rdrob_Gymn <- rd_analysis_edu_Gymn$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Gymn <-
  rd_analysis_edu_Gymn$bws[2, 1]



University_beta <- rd_analysis_edu_Uni$coef[3]
University_se <- rd_analysis_edu_Uni$se[3]
pvalue_University <- rd_analysis_edu_Uni$pv[1]
CI95_low_University <- rd_analysis_edu_Uni$ci[3, 1]
CI95_high_University <- rd_analysis_edu_Uni$ci[3, 2]

pvalue_Uni_robust <- rd_analysis_edu_Uni$pv[3]
nm_obs_rdrobust_Uni <-
  rd_analysis_edu_Uni$N[1] + rd_analysis_edu_Uni$N[2]
selected_bndwdth_rdrob_Uni <- rd_analysis_edu_Uni$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Uni <-
  rd_analysis_edu_Uni$bws[2, 1]



############
############
############
############
# STEP 4
# run the z data
beta_mean_edu = (Primary_beta + Gymnasium_beta + University_beta) / 3

chi2_education <-
  (((Primary_beta - beta_mean_edu) / (sqrt(Primary_se ^ 2))) ^ 2) + (((Gymnasium_beta -
                                                                         beta_mean_edu) /
                                                                        (sqrt(Gymnasium_se ^ 2))) ^ 2) + (((University_beta - beta_mean_edu) / (sqrt(University_se ^
                                                                                                                                                       2))) ^ 2)
p_value_educ <- (1 - pchisq(chi2_education, df = 2))



############
############
############
############
# Step 5
# Make the table

results_table_Education_rdrobust <- data.frame(
  Model = c('University studies', 'Secondary school', 'Primary school'),
  Coeff = c(University_beta, Gymnasium_beta, Primary_beta),
  CI_L = c(CI95_low_University, CI95_low_gymnas, CI95_low_primary),
  CI_H = c(CI95_high_University, CI95_high_gymnas, CI95_high_primary),
  P = c(pvalue_University, pvalue_gymnas, pvalue_primary),
  P_Robust = c(pvalue_Uni_robust, pvalue_Gymn_robust, pvalue_Primar_robust),
  
  P_intrctn = c(p_value_educ, p_value_educ, p_value_educ),
  
  nm_obs = c(
    nm_obs_rdrobust_Uni,
    nm_obs_rdrobust_Gymn,
    nm_obs_rdrobust_Primar
  ),
  bandwidth = c(
    selected_bndwdth_rdrob_Uni,
    selected_bndwdth_rdrob_Gymn,
    selected_bndwdth_rdrob_Primar
  ),
  bndw_bias_correct = c(
    selected_bndwdth_Bias_Corrected_rdrob_Uni,
    selected_bndwdth_Bias_Corrected_rdrob_Gymn,
    selected_bndwdth_Bias_Corrected_rdrob_Primar
  )
  
  
)



############
############
############
############
##### # # # # SECONDARY ANALYSIS - rdrobust for SALARY  # # # # ##### ----------------------------------------------


############
############
############
############
# STEP 1
# split the data into the country categories

main_data_tbl_logit_low_salary <- main_data_tbl_logit %>%
  filter(Ind_slr_cat == 'Low')



main_data_tbl_logit_high_salary <- main_data_tbl_logit %>%
  filter(Ind_slr_cat == 'High')

############
############
############
############
# STEP 2
# run the rd robust for eahc of the split categories without interaction
rd_analysis_salary_low <-
  rdrobust(
    y = main_data_tbl_logit_low_salary$Vax_within_90,
    x = main_data_tbl_logit_low_salary$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )



rd_analysis_salary_high <-
  rdrobust(
    y = main_data_tbl_logit_high_salary$Vax_within_90,
    x = main_data_tbl_logit_high_salary$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )

############
############
############
############
# STEP 3
# save the betas and se

low_salary_beta <- rd_analysis_salary_low$coef[3]
low_salary_se <- rd_analysis_salary_low$se[3]
pvalue_salary_low <- rd_analysis_salary_low$pv[1]

CI95_low_salary_low <- rd_analysis_salary_low$ci[3, 1]
CI95_high_salary_low <- rd_analysis_salary_low$ci[3, 2]

pvalue_Low_Sal_robust <- rd_analysis_salary_low$pv[3]
nm_obs_rdrobust_Low_Sal <-
  rd_analysis_salary_low$N[1] + rd_analysis_salary_low$N[2]
selected_bndwdth_rdrob_Low_Sal <- rd_analysis_salary_low$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Low_Sal <-
  rd_analysis_salary_low$bws[2, 1]



high_salary_beta <- rd_analysis_salary_high$coef[3]
high_salary_se <- rd_analysis_salary_high$se[3]
pvalue_high_salary <- rd_analysis_salary_high$pv[1]

CI95_low_high_salary <- rd_analysis_salary_high$ci[3, 1]
CI95_high_high_salary <- rd_analysis_salary_high$ci[3, 2]

pvalue_High_Sal_robust <- rd_analysis_salary_high$pv[3]
nm_obs_rdrobust_High_Sal <-
  rd_analysis_salary_high$N[1] + rd_analysis_salary_high$N[2]
selected_bndwdth_rdrob_High_Sal <- rd_analysis_salary_high$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_High_Sal <-
  rd_analysis_salary_high$bws[2, 1]




############
############
############
############
# STEP 4
# run the z data
z_salary <-
  (low_salary_beta - high_salary_beta) / (sqrt(low_salary_se ^ 2 + high_salary_se ^
                                                 2))
pvalue_salary_rd <- (1 - pnorm(abs(z_salary))) * 2


beta_salary <- (high_salary_beta + low_salary_beta) / 2

############
############
############
############
# Step 5
# Make the table

results_table_Salary_rdrobust <- data.frame(
  Model = c('Salr_High', 'Salr_Low'),
  Coeff = c(high_salary_beta, low_salary_beta),
  CI_L = c(CI95_low_high_salary, CI95_low_salary_low),
  CI_H = c(CI95_high_high_salary, CI95_high_salary_low),
  P = c(pvalue_high_salary, pvalue_salary_low),
  P_Robust = c(pvalue_High_Sal_robust, pvalue_Low_Sal_robust),
  
  P_intrctn = c(pvalue_salary_rd, pvalue_salary_rd),
  
  nm_obs = c(nm_obs_rdrobust_High_Sal, nm_obs_rdrobust_Low_Sal),
  bandwidth = c(
    selected_bndwdth_rdrob_High_Sal,
    selected_bndwdth_rdrob_Low_Sal
  ),
  bndw_bias_correct = c(
    selected_bndwdth_Bias_Corrected_rdrob_High_Sal,
    selected_bndwdth_Bias_Corrected_rdrob_Low_Sal
  )
  
)





############
############
############
############
##### # # # # SECONDARY ANALYSIS - rdrobust for SEX  # # # # ##### ----------------------------------------------



############
############
############
############
# STEP 1
# split the data into the country categories
main_data_tbl_logit_female <- main_data_tbl_logit %>%
  filter(sex == 'Female')



main_data_tbl_logit_male <- main_data_tbl_logit %>%
  filter(sex == 'Male')

############
############
############
############
# STEP 2
# run the rd robust for eahc of the split categories without interaction
rd_analysis_female <-
  rdrobust(
    y = main_data_tbl_logit_female$Vax_within_90,
    x = main_data_tbl_logit_female$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )



rd_analysis_male <-
  rdrobust(
    y = main_data_tbl_logit_male$Vax_within_90,
    x = main_data_tbl_logit_male$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )

############
############
############
############
# STEP 3
# save the betas and se

female_beta <- rd_analysis_female$coef[3]
female_se <- rd_analysis_female$se[3]
pvalue_female <- rd_analysis_female$pv[1]

CI95_low_female <- rd_analysis_female$ci[3, 1]
CI95_high_female <- rd_analysis_female$ci[3, 2]

pvalue_Female_robust <- rd_analysis_female$pv[3]
nm_obs_rdrobust_Female <-
  rd_analysis_female$N[1] + rd_analysis_female$N[2]
selected_bndwdth_rdrob_Female <- rd_analysis_female$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Female <-
  rd_analysis_female$bws[2, 1]



male_beta <- rd_analysis_male$coef[3]
male_se <- rd_analysis_male$se[3]
pvalue_male <- rd_analysis_male$pv[1]

CI95_low_male <- rd_analysis_male$ci[3, 1]
CI95_high_male <- rd_analysis_male$ci[3, 2]


pvalue_Male_robust <- rd_analysis_male$pv[3]
nm_obs_rdrobust_Male <-
  rd_analysis_male$N[1] + rd_analysis_male$N[2]
selected_bndwdth_rdrob_Male <- rd_analysis_male$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Male <-
  rd_analysis_male$bws[2, 1]



############
############
############
############
# STEP 4
# run the z data
z_sex <-
  (female_beta - male_beta) / (sqrt(female_se ^ 2 + male_se ^ 2))
pvalue_sex_rd <- (1 - pnorm(abs(z_sex))) * 2
beta_sex <- (female_beta + male_beta) / 2



############
############
############
############
# STEP 5
results_table_sex_rdrobust <- data.frame(
  Model = c('Female', 'Male'),
  Coeff = c(female_beta, male_beta),
  CI_L = c(CI95_low_female, CI95_low_male),
  CI_H = c(CI95_high_female, CI95_high_male),
  P = c(pvalue_female, pvalue_male),
  P_Robust = c(pvalue_Female_robust, pvalue_Male_robust),
  
  P_intrctn = c(pvalue_sex_rd, pvalue_sex_rd),
  
  nm_obs = c(nm_obs_rdrobust_Female, nm_obs_rdrobust_Male),
  bandwidth = c(selected_bndwdth_rdrob_Female, selected_bndwdth_rdrob_Male),
  bndw_bias_correct = c(
    selected_bndwdth_Bias_Corrected_rdrob_Female,
    selected_bndwdth_Bias_Corrected_rdrob_Male
  )
  
)



############
############
############
############
##### # # # # SECONDARY ANALYSIS - rdrobust for TRUST  # # # # ##### ----------------------------------------------


############
############
############
############
# STEP 1
# split the data into the country categories

main_data_tbl_logit_Low_Trust <- main_data_tbl_logit %>%
  filter(trust_catgr == 'Low')

main_data_tbl_logit_High_Trust <- main_data_tbl_logit %>%
  filter(trust_catgr == 'High')

main_data_tbl_logit_Sweden_Trust <- main_data_tbl_logit %>%
  filter(trust_catgr == 'Sweden')

############
############
############
############
# STEP 2
# run the rd robust for each of the split categories without interaction
rd_analysis_country_Low_Trust <-
  rdrobust(
    y = main_data_tbl_logit_Low_Trust$Vax_within_90,
    x = main_data_tbl_logit_Low_Trust$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )



rd_analysis_country_High_Trust <-
  rdrobust(
    y = main_data_tbl_logit_High_Trust$Vax_within_90,
    x = main_data_tbl_logit_High_Trust$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )


rd_analysis_country_Sweden_Trust <-
  rdrobust(
    y = main_data_tbl_logit_Sweden_Trust$Vax_within_90,
    x = main_data_tbl_logit_Sweden_Trust$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )



############
############
############
############
# STEP 3
# save the betas and se

Low_Trust_beta <- rd_analysis_country_Low_Trust$coef[3]
Low_Trust_se <- rd_analysis_country_Low_Trust$se[3]
pvalue_Low_Trust <- rd_analysis_country_Low_Trust$pv[1]

pvalue_Low_Trust_robust <- rd_analysis_country_Low_Trust$pv[3]
nm_obs_rdrobust_Low_Trust <-
  rd_analysis_country_Low_Trust$N[1] + rd_analysis_country_Low_Trust$N[2]
selected_bndwdth_rdrob_Low_Trust <-
  rd_analysis_country_Low_Trust$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Low_Trust <-
  rd_analysis_country_Low_Trust$bws[2, 1]

CI95_low_Low_Trust <- rd_analysis_country_Low_Trust$ci[3, 1]
CI95_high_Low_Trust <- rd_analysis_country_Low_Trust$ci[3, 2]



High_Trust_beta <- rd_analysis_country_High_Trust$coef[3]
High_Trust_se <- rd_analysis_country_High_Trust$se[3]
pvalue_High_Trust <- rd_analysis_country_High_Trust$pv[1]

pvalue_High_Trust_robust <- rd_analysis_country_High_Trust$pv[3]
nm_obs_rdrobust_High <-
  rd_analysis_country_High_Trust$N[1] + rd_analysis_country_High_Trust$N[2]
selected_bndwdth_rdrob_High <-
  rd_analysis_country_High_Trust$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_High <-
  rd_analysis_country_High_Trust$bws[2, 1]


CI95_low_High_Trust <- rd_analysis_country_High_Trust$ci[3, 1]
CI95_high_High_Trust <- rd_analysis_country_High_Trust$ci[3, 2]




Sweden_Trust_country_beta <-
  rd_analysis_country_Sweden_Trust$coef[3]
Sweden_Trust_country_se <- rd_analysis_country_Sweden_Trust$se[3]
pvalue_Sweden_Trust <- rd_analysis_country_Sweden_Trust$pv[1]

CI95_low_Sweden_Trust <- rd_analysis_country_Sweden_Trust$ci[3, 1]
CI95_high_Sweden_Trust <- rd_analysis_country_Sweden_Trust$ci[3, 2]



pvalue_Sweden_Trust_robust <- rd_analysis_country_Sweden_Trust$pv[3]
nm_obs_rdrobust_Sweden_Trust <-
  rd_analysis_country_Sweden_Trust$N[1] + rd_analysis_country_Sweden_Trust$N[2]
selected_bndwdth_rdrob_Sweden_Trust <-
  rd_analysis_country_Sweden_Trust$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Sweden_Trust <-
  rd_analysis_country_Sweden_Trust$bws[2, 1]


############
############
############
############
# STEP 4
# run the z data
beta_mean_country_trust = (Low_Trust_beta + High_Trust_beta + Sweden_Trust_country_beta) /
  3

chi2_country_trust <-
  (((Low_Trust_beta - beta_mean_country_trust) / (sqrt(Low_Trust_se ^ 2))) ^
     2) + (((
       High_Trust_beta - beta_mean_country_trust
     ) /
       (sqrt(High_Trust_se ^ 2))) ^ 2) + (((Sweden_Trust_country_beta - beta_mean_country_trust) /
                                             (sqrt(Sweden_Trust_country_se ^ 2))
       ) ^ 2)

pvalue_country_rd_trust <- (1 - pchisq(chi2_country_trust, df = 2))





############
############
############
############
# Step 5
# Make the table

results_table_Trust_rdrobust <- data.frame(
  Model = c('Sweden (Trust)', 'High trust', 'Low trust'),
  Coeff = c(Sweden_Trust_country_beta, High_Trust_beta, Low_Trust_beta),
  CI_L = c(
    CI95_low_Sweden_Trust,
    CI95_low_High_Trust,
    CI95_low_Low_Trust
  ),
  CI_H = c(
    CI95_high_Sweden_Trust,
    CI95_high_High_Trust,
    CI95_high_Low_Trust
  ),
  P = c(pvalue_Sweden_Trust, pvalue_High_Trust, pvalue_Low_Trust),
  P_Robust = c(
    pvalue_Sweden_Trust_robust,
    pvalue_High_Trust_robust,
    pvalue_Low_Trust_robust
  ),
  
  P_intrctn = c(
    pvalue_country_rd_trust,
    pvalue_country_rd_trust,
    pvalue_country_rd_trust
  ),
  
  nm_obs = c(
    nm_obs_rdrobust_Sweden_Trust,
    nm_obs_rdrobust_High,
    nm_obs_rdrobust_Low_Trust
  ),
  bandwidth = c(
    selected_bndwdth_rdrob_Sweden_Trust,
    selected_bndwdth_rdrob_High,
    selected_bndwdth_rdrob_Low_Trust
  ),
  bndw_bias_correct = c(
    selected_bndwdth_Bias_Corrected_rdrob_Sweden_Trust,
    selected_bndwdth_Bias_Corrected_rdrob_High,
    selected_bndwdth_Bias_Corrected_rdrob_Low_Trust
  )
  
  
)





############
############
############
############
##### # # # # SECONDARY ANALYSIS - rdrobust for LENGTH OF RESIDENCE  # # # # ##### ----------------------------------------------

############
############
############
############
# STEP 1
# split the data into the country categories

main_data_tbl_logit_Short_Stay <- main_data_tbl_logit %>%
  filter(length_stay_cat == 'Short')

main_data_tbl_logit_Long_Stay <- main_data_tbl_logit %>%
  filter(length_stay_cat == 'Long')

main_data_tbl_logit_Sweden_stay <- main_data_tbl_logit %>%
  filter(length_stay_cat == 'Born in Sweden')

############
############
############
############
# STEP 2
# run the rd robust for eahc of the split categories without interaction
rd_analysis_Short_Stay <-
  rdrobust(
    y = main_data_tbl_logit_Short_Stay$Vax_within_90,
    x = main_data_tbl_logit_Short_Stay$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )



rd_analysis_Long_Stay <-
  rdrobust(
    y = main_data_tbl_logit_Long_Stay$Vax_within_90,
    x = main_data_tbl_logit_Long_Stay$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )


rd_analysis_Sweden_stay <-
  rdrobust(
    y = main_data_tbl_logit_Sweden_stay$Vax_within_90,
    x = main_data_tbl_logit_Sweden_stay$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )


############
############
############
############
# STEP 3
# save the betas and se

Short_stay_beta <- rd_analysis_Short_Stay$coef[3]
Short_stay_se <- rd_analysis_Short_Stay$se[3]
pvalue_Short_stay <- rd_analysis_Short_Stay$pv[1]

CI95_low_Short_stay <- rd_analysis_Short_Stay$ci[3, 1]
CI95_high_Short_stay <- rd_analysis_Short_Stay$ci[3, 2]

pvalue_Short_stay_robust <- rd_analysis_Short_Stay$pv[3]
nm_obs_rdrobust_Short_stay <-
  rd_analysis_Short_Stay$N[1] + rd_analysis_Short_Stay$N[2]
selected_bndwdth_rdrob_Short_stay <-
  rd_analysis_Short_Stay$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Short_stay <-
  rd_analysis_Short_Stay$bws[2, 1]




Long_stay_beta <- rd_analysis_Long_Stay$coef[3]
Long_stay_se <- rd_analysis_Long_Stay$se[3]
pvalue_Long_stay <- rd_analysis_Long_Stay$pv[1]
CI95_low_Long_stay <- rd_analysis_Long_Stay$ci[3, 1]
CI95_high_Long_stay <- rd_analysis_Long_Stay$ci[3, 2]

pvalue_Long_stay_robust <- rd_analysis_Long_Stay$pv[3]
nm_obs_rdrobust_Long_stay <-
  rd_analysis_Long_Stay$N[1] + rd_analysis_Long_Stay$N[2]
selected_bndwdth_rdrob_Long_stay <- rd_analysis_Long_Stay$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Long_stay <-
  rd_analysis_Long_Stay$bws[2, 1]



Sweden_stay_beta <- rd_analysis_Sweden_stay$coef[3]
Sweden_stay_se <- rd_analysis_Sweden_stay$se[3]
pvalue_Sweden_stay <- rd_analysis_Sweden_stay$pv[1]

CI95_low_Sweden_stay <- rd_analysis_Sweden_stay$ci[3, 1]
CI95_high_Sweden_stay <- rd_analysis_Sweden_stay$ci[3, 2]

pvalue_Sweden_stay_robust <- rd_analysis_Sweden_stay$pv[3]
nm_obs_rdrobust_Sweden_stay <-
  rd_analysis_Sweden_stay$N[1] + rd_analysis_Sweden_stay$N[2]
selected_bndwdth_rdrob_Sweden_stay <-
  rd_analysis_Sweden_stay$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_Sweden_stay <-
  rd_analysis_Sweden_stay$bws[2, 1]



############
############
############
############
# STEP 4
# run the z data
beta_mean_stay = (Short_stay_beta + Long_stay_beta + Sweden_stay_beta) /
  3

chi2_stay <-
  (((Short_stay_beta - beta_mean_stay) / (sqrt(Short_stay_se ^ 2))) ^ 2) +
  (((Long_stay_beta - beta_mean_stay) /
      (sqrt(Long_stay_se ^ 2))) ^ 2) + (((Sweden_stay_beta - beta_mean_stay) /
                                           (sqrt(Sweden_stay_se ^ 2))) ^ 2)
p_value_stay <- (1 - pchisq(chi2_stay, df = 2))


############
############
############
############
# Step 5
# Make the table

results_table_Stay_rdrobust <- data.frame(
  Model = c('Born in Sweden (No migration)', 'Long', 'Short'),
  Coeff = c(Sweden_stay_beta, Long_stay_beta, Short_stay_beta),
  CI_L = c(
    CI95_low_Sweden_stay,
    CI95_low_Long_stay,
    CI95_low_Short_stay
  ),
  CI_H = c(
    CI95_high_Sweden_stay,
    CI95_high_Long_stay,
    CI95_high_Short_stay
  ),
  P = c(pvalue_Sweden_stay, pvalue_Long_stay, pvalue_Short_stay),
  P_Robust = c(
    pvalue_Sweden_stay_robust,
    pvalue_Long_stay_robust,
    pvalue_Short_stay_robust
  ),
  
  P_intrctn = c(p_value_stay, p_value_stay, p_value_stay),
  
  nm_obs = c(
    nm_obs_rdrobust_Sweden_stay,
    nm_obs_rdrobust_Long_stay,
    nm_obs_rdrobust_Short_stay
  ),
  bandwidth = c(
    selected_bndwdth_rdrob_Sweden_stay,
    selected_bndwdth_rdrob_Long_stay,
    selected_bndwdth_rdrob_Short_stay
  ),
  bndw_bias_correct = c(
    selected_bndwdth_Bias_Corrected_rdrob_Sweden_stay,
    selected_bndwdth_Bias_Corrected_rdrob_Long_stay,
    selected_bndwdth_Bias_Corrected_rdrob_Short_stay
  )
  
  
)


############
############
############
############
##### # # # # SECONDARY ANALYSIS - rdrobust for MEDICAL RISK GROUP  # # # # ##### ----------------------------------------------



############
############
############
############
# STEP 1
# split the data into the country categories

main_data_tbl_logit_no_risk <- main_data_tbl_logit %>%
  filter(Risk_group == 'No')

main_data_tbl_logit_yes_risk <- main_data_tbl_logit %>%
  filter(Risk_group == 'Yes')

############
############
############
############
# STEP 2
# run the rd robust for eahc of the split categories without interaction
rd_analysis_no_risk <-
  rdrobust(
    y = main_data_tbl_logit_no_risk$Vax_within_90,
    x = main_data_tbl_logit_no_risk$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )



rd_analysis_yes_risk <-
  rdrobust(
    y = main_data_tbl_logit_yes_risk$Vax_within_90,
    x = main_data_tbl_logit_yes_risk$age_for_model,
    c = 49.5,
    h = selected_bandwidth_imbens,
    p = 1,
    kernel = 'epanechnikov'
  )

############
############
############
############
# STEP 3
# save the betas and se

no_risk_beta <- rd_analysis_no_risk$coef[3]
no_risk_se <- rd_analysis_no_risk$se[3]
pvalue_no_risk <- rd_analysis_no_risk$pv[1]


CI95_low_no_risk <- rd_analysis_no_risk$ci[3, 1]
CI95_high_no_risk <- rd_analysis_no_risk$ci[3, 2]

pvalue_no_risk_robust <- rd_analysis_no_risk$pv[3]
nm_obs_rdrobust_no_risk <-
  rd_analysis_no_risk$N[1] + rd_analysis_no_risk$N[2]
selected_bndwdth_rdrob_no_risk <- rd_analysis_no_risk$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_no_risk <-
  rd_analysis_no_risk$bws[2, 1]



yes_risk_beta <- rd_analysis_yes_risk$coef[3]
yes_risk_se <- rd_analysis_yes_risk$se[3]
pvalue_yes_risk <- rd_analysis_yes_risk$pv[1]


CI95_low_yes_risk <- rd_analysis_yes_risk$ci[3, 1]
CI95_high_yes_risk <- rd_analysis_yes_risk$ci[3, 2]


pvalue_yes_risk_robust <- rd_analysis_yes_risk$pv[3]
nm_obs_rdrobust_yes_risk <-
  rd_analysis_yes_risk$N[1] + rd_analysis_yes_risk$N[2]
selected_bndwdth_rdrob_yes_risk <- rd_analysis_yes_risk$bws[1, 1]
selected_bndwdth_Bias_Corrected_rdrob_yes_risk <-
  rd_analysis_yes_risk$bws[2, 1]



############
############
############
############
# STEP 4
# run the z data
z_Risk_group <-
  (no_risk_beta - yes_risk_beta) / (sqrt(no_risk_se ^ 2 + yes_risk_se ^ 2))
pvalue_Risk_group_rd <- (1 - pnorm(abs(z_Risk_group))) * 2
beta_Risk_group <- (no_risk_beta + yes_risk_beta) / 2




############
############
############
############
# STEP 5
results_table_Risk_rdrobust <- data.frame(
  Model = c('Low medical risk', 'High medical risk'),
  Coeff = c(no_risk_beta, yes_risk_beta),
  CI_L = c(CI95_low_no_risk, CI95_low_yes_risk),
  CI_H = c(CI95_high_no_risk, CI95_high_yes_risk),
  P = c(pvalue_no_risk, pvalue_yes_risk),
  P_Robust = c(pvalue_no_risk_robust, pvalue_yes_risk_robust),
  
  P_intrctn = c(pvalue_Risk_group_rd, pvalue_Risk_group_rd),
  
  nm_obs = c(nm_obs_rdrobust_no_risk, nm_obs_rdrobust_yes_risk),
  bandwidth = c(
    selected_bndwdth_rdrob_no_risk,
    selected_bndwdth_rdrob_yes_risk
  ),
  bndw_bias_correct = c(
    selected_bndwdth_Bias_Corrected_rdrob_no_risk,
    selected_bndwdth_Bias_Corrected_rdrob_yes_risk
  )
  
)




############
############
############
############
##### # # # # PRIMARY & SECONDARY ANALYSIS - FOREST PLOT # # # # ##### ----------------------------------------------


############
############
############
############
results_table_All_Logreg <- rbind(
  results_table_Main_Log,
  results_table_Education ,
  results_table_Salary,
  results_table_sex,
  results_Birth_Country_Trust,
  results_Length_Stay,
  results_Risk_group
  
)
results_table_All_Logreg$OR <-
  round(results_table_All_Logreg$OR, digits = 2)
results_table_All_Logreg$CI_L <-
  round(results_table_All_Logreg$CI_L, digits = 2)
results_table_All_Logreg$CI_H <-
  round(results_table_All_Logreg$CI_H, digits = 2)
results_table_All_Logreg$P <-
  round(results_table_All_Logreg$P, digits = 4)
results_table_All_Logreg$P_Robust <-
  round(results_table_All_Logreg$P_Robust, digits = 4)
results_table_All_Logreg$P_intrctn <-
  as.numeric(results_table_All_Logreg$P_intrctn)
results_table_All_Logreg$P_intrctn <-
  round(results_table_All_Logreg$P_intrctn, digits = 4)
results_table_All_Logreg$bandwidth <-
  round(results_table_All_Logreg$bandwidth, digits = 2)





############
############
############
############
results_table_All_rdrobust <- rbind(
  results_table_Main_rdrobust,
  results_table_Education_rdrobust ,
  results_table_Salary_rdrobust,
  results_table_sex_rdrobust,
  results_table_Trust_rdrobust,
  results_table_Stay_rdrobust,
  results_table_Risk_rdrobust
)

results_table_All_rdrobust$Coeff <-
  round(results_table_All_rdrobust$Coeff * 100, digits = 2)
results_table_All_rdrobust$CI_L <-
  round(results_table_All_rdrobust$CI_L * 100, digits = 2)
results_table_All_rdrobust$CI_H <-
  round(results_table_All_rdrobust$CI_H * 100, digits = 2)
results_table_All_rdrobust$P <-
  round(results_table_All_rdrobust$P, digits = 4)
results_table_All_rdrobust$P_Robust <-
  round(results_table_All_rdrobust$P_Robust, digits = 4)
results_table_All_rdrobust$P_intrctn <-
  round(results_table_All_rdrobust$P_intrctn, digits = 4)
results_table_All_rdrobust$bandwidth <-
  round(results_table_All_rdrobust$bandwidth, digits = 2)
results_table_All_rdrobust$bndw_bias_correct <-
  round(results_table_All_rdrobust$bndw_bias_correct, digits = 2)

colnames(results_table_All_rdrobust) <-
  c(
    "Model"      ,
    "Coeff(%)"        ,
    "CI_L(%)" ,
    "CI_H(%)"  ,
    "P"    ,
    "P_Robust"     ,
    "P_intrctn"    ,
    "nm_obs"        ,
    "bandwidth"   ,
    "bndw_unbiased"
  )


############
############
############
############
# Odds Ratios
results_table_All_Logreg_forsest <- results_table_All_Logreg %>%
  dplyr::select(-P, -nm_obs, -bandwidth)

results_table_All_Logreg_forsest$Model[results_table_All_Logreg_forsest$Model ==
                                         'Epanechnikov_IK'] <-
  'Main'

############
############
############
############
# PERCENTAGES
results_table_All_rdrobust_forest <- results_table_All_rdrobust

results_table_All_rdrobust_forest <- results_table_All_rdrobust %>%
  dplyr::select(-P, -nm_obs, -bandwidth, -bndw_unbiased)

colnames(results_table_All_rdrobust_forest) <-
  c('Model', 'Coeff', 'CI_L', 'CI_H', 'P_Robust', 'P_intrctn')

results_table_All_rdrobust_forest$Model[results_table_All_rdrobust_forest$Model ==
                                          'rdrobust'] <- 'Main'


results_table_All_Logreg_forsest$Model <-
  factor(results_table_All_rdrobust_forest$Model, levels = desired_order)
results_table_All_rdrobust_forest$Model <-
  factor(results_table_All_rdrobust_forest$Model, levels = desired_order)


results_table_All_Logreg_forsest$category <- c(
  'Main',
  rep('Education Attained', 3),
  rep('Disposable Income', 2),
  rep('Biological Sex', 2),
  rep('Trust Level of Birth Country', 3),
  rep('Duration of Residence', 3),
  rep('Medical Risk', 2)
)


results_table_All_rdrobust_forest$category <- c(
  'Main',
  rep('Education Attained', 3),
  rep('Disposable Income', 2),
  rep('Biological Sex', 2),
  rep('Trust Level of Birth Country', 3),
  rep('Duration of Residence', 3),
  rep('Medical Risk', 2)
)

library(ggthemes)

scaling_factor <- -1 / max(results_table_All_Logreg_forsest$OR)

value_multpl <- 0

# Forest Plot
forest_plot_main <- ggplot() +
  geom_vline(
    aes(xintercept = 0),
    size = .8,
    color = 'grey24',
    linetype = 'longdash'
  ) +
  
  
  # Odds Ratios
  geom_errorbarh(
    data = results_table_All_Logreg_forsest,
    position = position_nudge(y = 0.2),
    
    aes(
      xmin = (CI_L * 6) - 6,
      xmax = (CI_H * 6) - 6,
      y = Model
    ),
    height = 0.1,
    color = 'black'
  ) +
  
  
  geom_point(
    data = results_table_All_Logreg_forsest,
    position = position_nudge(y = 0.2),
    
    aes(x = (OR * 6) - 6, y = Model),
    size = 6,
    color = 'grey24'
  ) +
  
  geom_point(
    data = results_table_All_Logreg_forsest,
    position = position_nudge(y = 0.2),
    aes(x = (OR * 6) - 6, y = Model),
    size = 5.2,
    color = '#ed6b08'
  ) +
  
  
  # Percentage
  geom_errorbarh(
    position = position_nudge(y = -0.2),
    
    data = results_table_All_rdrobust_forest,
    aes(xmin = CI_L,
        xmax = CI_H,
        y = Model),
    height = 0.2,
    color = 'black'
  ) +
  
  
  geom_point(
    position = position_nudge(y = -0.2),
    
    data = results_table_All_rdrobust_forest,
    aes(x = Coeff, y = Model),
    size = 6,
    color = 'grey24'
  ) +
  
  
  geom_point(
    position = position_nudge(y = -0.2),
    
    data = results_table_All_rdrobust_forest,
    aes(x = Coeff, y = Model),
    size = 5.2,
    color = '#ffebd7'
  ) +
  
  
  scale_x_continuous(
    name = '',
    
    breaks = c(-6,-4,-2, 0, 2, 4, 6),
    labels =  paste0(c(-6,-4,-2, 0, 2, 4, 6), '%'),
    
    
    sec.axis = sec_axis ( ~ (. + 6) / 6,
                          breaks = c(0, 0.5, 1, 1.5, 2),
                          name = '')
  ) +
  
  
  labs(y = '') +
  
  theme_gdocs() +
  theme(
    legend.position = 'bottom',
    
    
    axis.ticks = element_line(color = 'black'),
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x =  element_blank(),
    
    panel.grid.major.y =  element_line(
      linetype = 'longdash',
      color = 'grey82',
      size = 0.8
    ),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  )
