# title: "Evaluation of the effectiveness of COVID-19 vaccination appointment letters on uptake across sociodemographic groups: A regression discontinuity analysis in Sweden"
# subtitle: "Script for Data Processing, Preparation, and Table 1 (Uppsala)"
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
##### # # # # PROCESS VARIABLES # # # # ##### ----------------------------------------------

############
############
############
############
# import Population_PersonNr_20211231.csv file
pop <-  fread("//directory/Population_PersonNr_20211231.csv")

foreign_swed <-  fread("//directory/Fodelseuppg_20211231.csv")


foreign_swed <- foreign_swed %>%
  mutate(Fodelseland = toupper(Fodelseland))


############
############
############
############
# # World Bank Ranking
library(utils)
world_bank <-
  read.table("//directory/OGHIST_world_bank_for_MONA",
             header = F,
             sep = '\t')


# Modify
colnames(world_bank) <- as.character(unlist(world_bank[1, ]))
world_bank <- world_bank[-1, ]

# Select 2021 values
world_bank <- world_bank %>%
  dplyr::select(3, 38)

# change column names
colnames(world_bank) <- c('Country_name_SV', 'WB_Rank_2020')

# replace the NA value of Venezual with the latest consisten value
world_bank$WB_Rank_2020[which(world_bank$Country_name_SV  == 'Venezuela, RB')] <-
  'UM'

############
############
############
############
# correct some country names so that they align to the Fodelseuppg_20211231.csv file
library(stringr)
world_bank <- world_bank %>%
  mutate(
    Country_name_SV = recode(
      Country_name_SV,
      'Ã–sterrike' = 'Österrike',
      'Bahamas, The' = 'Bahamas',
      'Brittiska JungfruÃ¶arna' = 'Brittiska Jungfruöarna',
      'Brunei Darussalam' = 'BRUNEI',
      'CaymanÃ¶arna' = 'Caymanöarna',
      'KanalÃ¶arna' = 'Kanalöarna',
      'Kongo, Dem. Rep.' = 'Demokratiska republiken Kongo',
      'Kongo, Rep.' = 'Kongo',
      'Egypten, arabisk rep.' = 'Egypten',
      'FÃ¤rÃ¶arna' = 'Färöarna',
      'Cabo Verde' = 'KAP VERDE',
      'CuraÃ§ao' = 'Curacao',
      'Eswatini' = 'SWAZILAND',
      'Gambia, The' = 'Gambia',
      'GrÃ¶nland' = 'Grönland',
      'Storbritannien' = 'FÖRENADE KUNGARIKET',
      'Hong Kong SAR, Kina' = 'HONGKONG',
      'Iran, islamisk rep.' = 'IRAN',
      'Ã¶ av man' = 'Isle of Man',
      'Korea, Dem. Rep.' = 'Nordkorea',
      'Korea, rep.' = 'SYDKOREA',
      'Laos PDR' = 'LAOS',
      'Macao SAR, Kina' = 'Macao',
      'Italy' = 'ITALIEN',
      'Nordmakedonien' = 'NORDMAKEDONIEN',
      'Makedonien' = 'NORDMAKEDONIEN',
      'MarshallÃ¶arna' = 'MARSHALLÖARNA',
      'Mikronesien, Fed. Sts.' = 'MIKRONESIEN',
      'MoÃ§ambique' = 'MOÇAMBIQUE',
      'NederlÃ¤nderna' = 'NEDERLÄNDERNA',
      'RumÃ¤nien' = 'RUMÄNIEN',
      'Ryska Federationen' = 'RYSSLAND',
      'SÃ£o TomÃ© och PrÃ­ncipe' = 'SÃO TOMÉ OCH PRÍNCIPE',
      'Sint Maarten (nederlÃ¤ndska delen)' = 'Sint Maarten Neder',
      'St Kitts och Nevis'  = 'SAINT KITTS OCH NEVIS',
      'SalomonÃ¶arna' = 'SALOMONÖARNA',
      'sÃ¶dra Sudan' = 'SYDSUDAN',
      'St. Martin (franska delen)' = 'Sint Maarten Franska',
      'St. Vincent och Grenadinerna' = 'SAINT VINCENT OCH GRENADINERNA',
      'Syrien Arabrepubliken' = 'SYRIEN',
      'Sankta Lucia' = 'SAINT LUCIA',
      'Taiwan, Kina' = 'TAIWAN, PROVINS AV KINA',
      'Ã–sttimor' = 'ÖSTTIMOR',
      'Turks- och CaicosÃ¶arna' = 'Turks- och Caicosöarna',
      'FÃ¶renade arabemiraten' = 'Förenade arabemiraten',
      'FÃ¶renta staterna' = 'FÖRENTA STATERNA',
      'Venezuela, RB' = 'VENEZUELA',
      'JungfruÃ¶arna (USA)' = 'JUNGFRUÖARNA, BRITTISKA',
      
      # Gaza nad west bank are together in the world bank
      'VÃ¤stbanken och Gaza' = 'VÄSTBANKEN OCH GAZA',
      # "GAZA-OMRÅDET'
      # 'Vietnam' = 'VIETNAM, REP',
      'Jemen, Rep.' = 'Jemen'
    )
  ) %>%
  mutate(Country_name_SV = toupper(Country_name_SV))


############
############
############
############
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'VÄSTBANKEN'] <-
  'VÄSTBANKEN OCH GAZA'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'GAZA-OMRÅDET'] <-
  'VÄSTBANKEN OCH GAZA'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'PALESTINA'] <-
  'VÄSTBANKEN OCH GAZA'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'TYSKA DEM REP (DDR)'] <-
  'TYSKLAND'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'FRANSKA MAROCKO'] <-
  'MAROCKO'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'SOVJETUNIONEN'] <-
  'RYSSLAND'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'MALAJISKA FÖRBUNDET'] <-
  'MALAYSIA'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'SYDJEMEN'] <-
  'JEMEN'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'ITALY'] <-
  'ITALIEN'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'HELIGA STOLEN'] <-
  'ITALY'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'SIKKIM'] <-
  'INDIEN'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'VÄSTRA SAMOA'] <-
  'SAMOA'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'VIETNAM, REP'] <-
  'VIETNAM'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'MAKEDONIEN'] <-
  'NORDMAKEDONIEN'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'TJECKOSLOVAKIEN'] <-
  'TJECKIEN'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'JUGOSLAVIEN'] <-
  'SERBIEN'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'SERBIEN OCH MONTENEGRO'] <-
  'SERBIEN'
foreign_swed$Fodelseland[foreign_swed$Fodelseland == 'DANZIG'] <-
  'POLEN'


############
############
############
############
# set the missing ones
foreign_swed <- foreign_swed %>%
  mutate(
    Fodelseland_new = case_when(
      Fodelseland == 'STATSLÖS' | Fodelseland == 'OKÄNT' |
        Fodelseland == 'INT. NAT. TERRITORIUM' |
        Fodelseland == 'UPPGIFT SAKNAS'  |
        Fodelseland == 'UNDER UTREDNING' |
        Fodelseland == ''  ~ 'missing',
      TRUE ~ Fodelseland
    )
  )

############
############
############
############
# bring together with the foreign_swed
foreign_swed <- foreign_swed %>%
  rename(Country_name_SV = Fodelseland_new) %>%
  left_join(world_bank)

############
############
############
############
# there are some duplicates but they all have the same Country,  (5664115)
foreign_swed$Country_name_SV[foreign_swed$P1105_LopNr_PersonNr == 5664115] <-
  c('INDIEN', 'INDIEN')
foreign_swed$WB_Rank_2020[foreign_swed$P1105_LopNr_PersonNr == 5664115] <-
  c('LM', 'LM')

foreign_swed <- foreign_swed %>%
  distinct(P1105_LopNr_PersonNr, .keep_all = T)

# keep here all possible unique countries
all_unique_countries <- foreign_swed %>%
  distinct(Fodelseland) %>%
  arrange(Fodelseland)



############
############
############
############
# Risk group
# library(dplyr)
# bring in the comorbidities and high risk
risk_group <-  fread("//directory/risk2020s.csv")

# count the number of risk groups that each person belongs to
risk_group <- risk_group %>%
  dplyr::select(-year) %>%
  mutate(lopnr = as.character(lopnr)) %>%
  mutate(
    N_Risk_groups = rowSums(across(where(is.integer))),
    Risk_group = ifelse(N_Risk_groups >= 1, 'Yes', 'No'),
    lopnr = as.numeric(lopnr)
  )

# ATTENTION
# remember once you merge with the ongoing data to
# turn every unmatched individual to NO risk group
# like so:
risk_group <- risk_group %>%
  dplyr::select(lopnr, Risk_group)



############
############
############
############
# Length of stay
# bring in the comorbidities and high risk
length_stay <-  fread("//directory/immigranter_20221231.csv")

# convert the date into an actual date
length_stay$immigration_date <-
  paste0(
    substr(length_stay$datinv, 1, 4),
    '-',
    substr(length_stay$datinv, 5, 6),
    '-',
    substr(length_stay$datinv, 7, 8)
  )

length_stay$immigration_date <-
  as.Date(length_stay$immigration_date, format = '%Y-%m-%d')

# count length of stay as of December 31, 2020
length_stay$length_stay_days <-
  difftime(as.Date('2020-12-31', format = '%Y-%m-%d'),
           length_stay$immigration_date,
           units = 'days')
length_stay$length_stay_years <-
  as.numeric(difftime(
    as.Date('2020-12-31', format = '%Y-%m-%d'),
    length_stay$immigration_date,
    units = 'weeks'
  ) / 52)

# categorize this
length_stay <- length_stay %>%
  mutate(length_stay_cat = ifelse(length_stay_years <= 10, 'Short', 'Long')) %>%
  dplyr::select(P1105_LopNr_PersonNr, length_stay_cat, length_stay_years)


############
############
############
############
# trust in science per country
values_country <-
  fread("//directory/Hofstede_Welzel_country_region_indeces_20240313.txt")

# fix country names that have special Swedish characters that do not translate in the Windows system
values_country$country_in_data[values_country$country_in_data == 'JUNGFRUÃ–ARNA, BRITTISKA'] <-
  'JUNGFRUÖARNA, BRITTISKA'
values_country$country_in_data[values_country$country_in_data == 'VÃ„STBANKEN OCH GAZA'] <-
  'VÄSTBANKEN OCH GAZA'
values_country$country_in_data[values_country$country_in_data == 'NEDERLÃ„NDERNA'] <-
  'NEDERLÄNDERNA'
values_country$country_in_data[values_country$country_in_data == 'Ã–STERRIKE'] <-
  'ÖSTERRIKE'
values_country$country_in_data[values_country$country_in_data == 'FÃ–RENADE KUNGARIKET'] <-
  'FÖRENADE KUNGARIKET'
values_country$country_in_data[values_country$country_in_data == 'FÃ–RENTA STATERNA'] <-
  'FÖRENTA STATERNA'
values_country$country_in_data[values_country$country_in_data == 'RUMÃ„NIEN'] <-
  'RUMÄNIEN'
values_country$country_in_data[values_country$country_in_data == 'FÃ–RENADE ARABEMIRATEN'] <-
  'FÖRENADE ARABEMIRATEN'
values_country$country_in_data[values_country$country_in_data == 'MOÃ‡AMBIQUE'] <-
  'MOÇAMBIQUE'
values_country$country_in_data[values_country$country_in_data == 'SALOMONÃ–ARNA'] <-
  'SALOMONÖARNA'
values_country$country_in_data[values_country$country_in_data == 'SAO TOME OCH PRINCIPE'] <-
  'SALOMONÖARNA'
values_country$country_in_data[values_country$country_in_data == 'MARSHALLÃ–ARNA'] <-
  'MARSHALLÖARNA'

values_country_mean_median <- values_country %>%
  group_by(Classification) %>%
  summarize(
    md_Trust_per_cluster = median(Dis_Trust_Hofstede_Welzel, na.rm = T),
    mn_Trust_per_cluster = mean(Dis_Trust_Hofstede_Welzel, na.rm = T)
  )

# choose the median for the Trust index
# so for all countries that do not have a measurement use the mean of their group
# match with the mean
values_country <- values_country %>%
  left_join(values_country_mean_median, by = 'Classification') %>%
  mutate(Trust_Hfs_Wlzl = coalesce(Dis_Trust_Hofstede_Welzel, md_Trust_per_cluster)) %>%
  mutate(
    mn_Trust_overall = mean(Trust_Hfs_Wlzl, na.rm = T),
    md_Trust_overall = median(Trust_Hfs_Wlzl, na.rm = T)
  )

values_country <- values_country %>%
  # mutate(quantile_group = ntile(Trust_Hfs_Wlzl,3)) %>%
  mutate(quantile_group = ntile(Trust_Hfs_Wlzl, 2)) %>%
  mutate(trust_catgr = quantile_group) %>%
  group_by(trust_catgr) %>%
  mutate(mean_Trust_p_qntl = mean(Trust_Hfs_Wlzl)) %>%
  ungroup() #%>%

values_country$trust_catgr[values_country$English == 'Sweden'] <-
  'Sweden'
values_country$trust_catgr[values_country$trust_catgr == 1] <- 'Low'
values_country$trust_catgr[values_country$trust_catgr == 2] <-
  'High'

values_country <- values_country %>%
  dplyr::select(country_in_data, trust_catgr)




############
############
############
############
##### # # # # UPPSALA # # # # ##### ----------------------------------------------


ind_2020 <-
  fread("////directory/Individ_2020.csv")

ind_2020_uppsala <- ind_2020 %>%
  filter(Lan == 3)


############
############
############
############
# total uppsala population
population_uppsala <-
  length(unique(ind_2020_uppsala$P1105_LopNr_PersonNr))

############
############
############
############
# EDUCATION
main_data_uppsala <- ind_2020_uppsala %>%
  dplyr::select(
    P1105_LopNr_PersonNr,
    DispInk04 ,
    DispInkFam04,
    Lan,
    Civil,
    Ssyk4_2012_J16 ,
    Sun2020niva_old
  ) %>%
  mutate(
    Sun_name_catg = case_when(
      Sun2020niva_old == 0 |
        Sun2020niva_old == 1 |
        Sun2020niva_old == 2 ~ 'Primary School',
      Sun2020niva_old == 3 | Sun2020niva_old == 4 ~ 'Gymnasium',
      Sun2020niva_old == 5 |
        Sun2020niva_old == 6 | Sun2020niva_old == 7 ~ 'University',
      TRUE ~ NA
    )
  )

############
############
############
############
# OUTCOME DATA
fhm_nvr <-  fread("//directory/FHM_NVR_Covid_20230316.csv")

# your outcome here
fhm_nvr <- fhm_nvr %>%
  dplyr::select(P1105_Lopnr_PersonNr, vaccination_date) %>%
  rename(P1105_LopNr_PersonNr = P1105_Lopnr_PersonNr) %>%
  # keep the earliest vacc date per idnividual
  group_by(P1105_LopNr_PersonNr) %>%
  arrange(vaccination_date) %>%
  slice(1L) %>%
  rename(First_dose = vaccination_date)


main_data_uppsala <- main_data_uppsala %>%
  dplyr::left_join(fhm_nvr %>%
                     distinct(P1105_LopNr_PersonNr, First_dose)) %>%
  dplyr::left_join((
    pop %>%
      dplyr::select(P1105_LopNr_PersonNr, FodelseArMan, Kon) %>%
      distinct(P1105_LopNr_PersonNr, FodelseArMan, Kon)
  ))

# Now join it with the main dataset and remove any duplicates
main_data_uppsala <- main_data_uppsala %>%
  dplyr::left_join(foreign_swed)# %>%


############
############
############
############
# bring in the extra variables
main_data_uppsala <- main_data_uppsala %>%
  dplyr::left_join(values_country, by = c('Country_name_SV' = 'country_in_data')) %>%
  dplyr::left_join(risk_group, by = c('P1105_LopNr_PersonNr' = 'lopnr')) %>%
  dplyr::left_join(length_stay, by = 'P1105_LopNr_PersonNr')


# make the necessary corrections for the addition variables:
main_data_uppsala$length_stay_cat[main_data_uppsala$Country_name_SV == 'SVERIGE'] <-
  'Born in Sweden'
main_data_uppsala$Risk_group[is.na(main_data_uppsala$Risk_group)] <-
  'No'





############
############
############
############
##### # # # # GAVLE # # # # ##### ----------------------------------------------

ind_2020_gavle <- ind_2020 %>%
  filter(Lan == 21)

############
############
############
############
# EDUCATION
main_data_gavle <- ind_2020_gavle %>%
  dplyr::select(
    P1105_LopNr_PersonNr,
    DispInk04 ,
    DispInkFam04,
    Lan,
    Civil,
    Ssyk4_2012_J16 ,
    Sun2020niva_old
  ) %>%
  mutate(
    Sun_name_catg = case_when(
      Sun2020niva_old == 0 |
        Sun2020niva_old == 1 |
        Sun2020niva_old == 2 ~ 'Primary School',
      Sun2020niva_old == 3 | Sun2020niva_old == 4 ~ 'Gymnasium',
      Sun2020niva_old == 5 |
        Sun2020niva_old == 6 | Sun2020niva_old == 7 ~ 'University',
      TRUE ~ NA
    )
  )#

main_data_gavle <- main_data_gavle %>%
  dplyr::left_join(fhm_nvr %>%
                     distinct(P1105_LopNr_PersonNr, First_dose)) %>%
  dplyr::left_join((
    pop %>%
      dplyr::select(P1105_LopNr_PersonNr, FodelseArMan, Kon) %>%
      distinct(P1105_LopNr_PersonNr, FodelseArMan, Kon)
  ))


# Now join it with the main dataset and remove any duplicates
main_data_gavle <- main_data_gavle %>%
  dplyr::left_join(foreign_swed) %>%
  distinct(P1105_LopNr_PersonNr, .keep_all = T)

############
############
############
############
# bring in the extra variables
main_data_gavle <- main_data_gavle %>%
  dplyr::left_join(values_country, by = c('Country_name_SV' = 'country_in_data')) %>%
  dplyr::left_join(risk_group, by = c('P1105_LopNr_PersonNr' = 'lopnr')) %>%
  dplyr::left_join(length_stay, by = 'P1105_LopNr_PersonNr')


main_data_gavle$length_stay_cat[main_data_gavle$Country_name_SV == 'SVERIGE'] <-
  'Born in Sweden'
main_data_gavle$Risk_group[is.na(main_data_gavle$Risk_group)] <-
  'No'




############
############
############
############
##### # # # # STOCKHOLM # # # # ##### ----------------------------------------------

ind_2020_stockholm <- ind_2020 %>%
  filter(Lan == 1)

############
############
############
############
# EDUCATION
main_data_stockholm <- ind_2020_stockholm %>%
  dplyr::select(
    P1105_LopNr_PersonNr,
    DispInk04 ,
    DispInkFam04,
    Lan,
    Civil,
    Ssyk4_2012_J16 ,
    Sun2020niva_old
  ) %>%
  mutate(
    Sun_name_catg = case_when(
      Sun2020niva_old == 0 |
        Sun2020niva_old == 1 |
        Sun2020niva_old == 2 ~ 'Primary School',
      Sun2020niva_old == 3 | Sun2020niva_old == 4 ~ 'Gymnasium',
      Sun2020niva_old == 5 |
        Sun2020niva_old == 6 | Sun2020niva_old == 7 ~ 'University',
      TRUE ~ NA
    )
  )


main_data_stockholm <- main_data_stockholm %>%
  dplyr::left_join(fhm_nvr %>%
                     distinct(P1105_LopNr_PersonNr, First_dose)) %>%
  dplyr::left_join((
    pop %>%
      dplyr::select(P1105_LopNr_PersonNr, FodelseArMan, Kon) %>%
      distinct(P1105_LopNr_PersonNr, FodelseArMan, Kon)
  ))


# Now join it with the main dataset and remove any duplicates
main_data_stockholm <- main_data_stockholm %>%
  dplyr::left_join(foreign_swed)# %>%
# distinct(P1105_LopNr_PersonNr, .keep_all = T )


############
############
############
############
# bring in the extra variables
main_data_stockholm <- main_data_stockholm %>%
  dplyr::left_join(values_country, by = c('Country_name_SV' = 'country_in_data')) %>%
  dplyr::left_join(risk_group, by = c('P1105_LopNr_PersonNr' = 'lopnr')) %>%
  dplyr::left_join(length_stay, by = 'P1105_LopNr_PersonNr')


main_data_stockholm$length_stay_cat[main_data_stockholm$Country_name_SV == 'SVERIGE'] <-
  'Born in Sweden'

main_data_stockholm$Risk_group[is.na(main_data_stockholm$Risk_group)] <-
  'No'




############
############
############
############
##### # # # # SPILLOVER # # # # ##### ----------------------------------------------

ind_2020_Spillover <-  ind_2020

############
############
############
############
# EDUCATION
main_data_spillover <- ind_2020_Spillover %>%
  dplyr::select(
    P1105_LopNr_PersonNr,
    DispInk04 ,
    DispInkFam04,
    Lan,
    Civil,
    Ssyk4_2012_J16 ,
    Sun2020niva_old
  ) %>%
  mutate(
    Sun_name_catg = case_when(
      Sun2020niva_old == 0 |
        Sun2020niva_old == 1 |
        Sun2020niva_old == 2 ~ 'Primary School',
      Sun2020niva_old == 3 | Sun2020niva_old == 4 ~ 'Gymnasium',
      Sun2020niva_old == 5 |
        Sun2020niva_old == 6 | Sun2020niva_old == 7 ~ 'University',
      TRUE ~ NA
    )
  )


main_data_spillover <- main_data_spillover %>%
  dplyr::left_join(fhm_nvr %>%
                     distinct(P1105_LopNr_PersonNr, First_dose)) %>%
  dplyr::left_join((
    pop %>%
      dplyr::select(P1105_LopNr_PersonNr, FodelseArMan, Kon) %>%
      distinct(P1105_LopNr_PersonNr, FodelseArMan, Kon)
  ))

# Now join it with the main dataset and remove any duplicates
main_data_spillover <- main_data_spillover %>%
  dplyr::left_join(foreign_swed)# %>%

############
############
############
############
# bring in the extra variables
main_data_spillover <- main_data_spillover %>%
  dplyr::left_join(values_country, by = c('Country_name_SV' = 'country_in_data')) %>%
  dplyr::left_join(risk_group, by = c('P1105_LopNr_PersonNr' = 'lopnr')) %>%
  dplyr::left_join(length_stay, by = 'P1105_LopNr_PersonNr')



############
############
############
############
##### # # # # PREPARATION FOR TABLE 1 - UPPSALA # # # # ##### ----------------------------------------------

# Now we need to create the outcome: Did the individual receive the vaccination dose within 90 days after the vaccination opening date corresponding to their age group?

############
############
############
############
# CALCULATE AGE YEAR AND AGE MONTH Seperately
main_data_uppsala <- main_data_uppsala %>%
  mutate(
    Age_year = as.numeric(substr(FodelseArMan, 1, 4)),
    Age_month = as.numeric(substr(FodelseArMan, 5, 6)),
    age_for_model = (2021 - Age_year) + (6 - Age_month) / 12
  )


############
############
############
############
# this is the only filtering step other than the filtering based on age range
# save the numbers
ind_before_birthcountry_missing <-
  length(unique(main_data_uppsala$P1105_LopNr_PersonNr))
main_data_uppsala <- main_data_uppsala %>%
  # filter out missing Age entries
  filter(!is.na(FodelseArMan)) #%>%
ind_AFTER_birthcountry_missing <-
  length(unique(main_data_uppsala$P1105_LopNr_PersonNr))

############
############
############
############
main_data_uppsala <- main_data_uppsala %>%
  mutate(
    # OUTCOME - create all the data needed to see if each individual got the outcome (vaccination) with 90 days from the opening day
    First_dose = as.Date(First_dose, format = '%Y-%m-%d'),
    vacc_date_opening_p_age = case_when(
      Age_year >= 1962 &
        Age_year <= 1971  ~ as.Date('24-05-2021', format = '%d-%m-%Y'),
      Age_year >= 1972 &
        Age_year <= 1976  ~ as.Date('31-05-2021', format = '%d-%m-%Y'),
      Age_year >= 1977 &
        Age_year <= 1981  ~ as.Date('07-06-2021', format = '%d-%m-%Y'),
      TRUE ~ NA
    ),
    
    days_btwn_opening_and_vacc = as.numeric(First_dose - vacc_date_opening_p_age),
    
    vaccinated_90days = case_when(days_btwn_opening_and_vacc <= 90 ~ 'Yes',
                                  TRUE ~ 'No'),
    
    vaccinated_0days = case_when(days_btwn_opening_and_vacc < 0 ~ 'Yes',
                                 TRUE ~ 'No'),
    
    # CREATE THE EXPOSURE - the ones above 50 had the letter with the appointment
    Letter_received = case_when(
      Age_year >= 1962 & Age_year <= 1971 ~ 'Letter',
      Age_year >= 1972 ~  'No letter',
      TRUE ~ NA
    )
  )


############
############
############
############
# find the percentage of each age group in Uppsala county
percentage_of_targeted_group_population <- main_data_uppsala %>%
  filter(Letter_received == 'Letter') %>%
  summarize(sum_50_59 = n())

percentage_of_nonTarget_group_population <- main_data_uppsala %>%
  filter(Age_year >= 1972 & Age_year <= 1982) %>%
  summarize(sum_40_49 = n())

percentage_of_targeted_group_population_All_adult <-
  main_data_uppsala %>%
  filter(Age_year <= 2003) %>%
  summarize(sum_all = n())


# find percentage of people targeted by the letter intervention
percentage_between_50_59 <-
  (
    percentage_of_targeted_group_population$sum_50_59 / percentage_of_targeted_group_population_All_adult$sum_all
  ) * 100
percentage_between_40_49 <-
  (
    percentage_of_nonTarget_group_population$sum_50_59 / percentage_of_targeted_group_population_All_adult$sum_all
  ) * 100


############
############
############
############
# Make the data look nicer
main_data_uppsala_tbl <- main_data_uppsala %>%
  
  # create the category of the big age groups
  mutate(
    Letter_received = factor(Letter_received, levels = c('No letter', 'Letter')),
    
    Age_group = case_when(
      age_for_model >= 39.5 & age_for_model < 49.5 ~ '39.5-49.4',
      age_for_model >= 49.5 & age_for_model <= 59.4 ~ '49.5-59.4',
      TRUE ~ 'outside_range'
    ),
    
    Age_group = factor(Age_group, levels = c('39.5-49.4', '49.5-59.4')),
    
    Birth_year_group = case_when(
      Age_year >= 1962 & Age_year <= 1971 ~ '1962 - 1971',
      Age_year >= 1972 & Age_year <= 1981 ~ '1972 - 1981',
      TRUE ~ 'outside_range'
    ),
    
    Birth_year_group = factor(Birth_year_group, levels = c('1962 - 1971', '1972 - 1981')),
    
    Sun_name_catg = factor(
      Sun_name_catg,
      levels = c('Primary School', 'Gymnasium' , 'University')
    ),
    
    vaccinated_90days = factor(vaccinated_90days),
    vaccinated_0days = factor(vaccinated_0days),
    
    sex = case_when(Kon == 1 ~ 'Male',
                    Kon == 2 ~ 'Female',
                    TRUE ~ NA),
    sex = factor(sex),
    
    Foreign_vs_Sweden = case_when(
      Country_name_SV == 'SVERIGE' ~ 'Sweden' ,
      WB_Rank_2020 == 'UM' |
        WB_Rank_2020 == "LM" | WB_Rank_2020 == 'L' ~ 'Abroad_LM',
      WB_Rank_2020 == 'H' ~ 'Abroad_High',
      
      TRUE ~ NA
    ),
    Foreign_vs_Sweden = factor(
      Foreign_vs_Sweden,
      levels = c('Sweden', 'Abroad_LM', 'Abroad_High')
    ),
    
    High_Low_Cntr = case_when(
      WB_Rank_2020 == 'H' ~ 'High',
      WB_Rank_2020 == 'UM' |
        WB_Rank_2020 == "LM" | WB_Rank_2020 == 'L' ~ 'Middle_Low',
      TRUE ~ NA
    ),
    High_Low_Cntr = factor(High_Low_Cntr, levels = c('Middle_Low', 'High'))
  ) %>%
  
  rename(
    Education = Sun_name_catg,
    Vax_within_90 = vaccinated_90days,
    Vax_within_0 = vaccinated_0days,
    
    age_for_model = age_for_model
  )



############
############
############
############
# SALARY ESTIMATIONS
# UPPSALA
main_data_tbl_median_sal_uppsala <- main_data_uppsala_tbl %>%
  group_by(Age_year, sex) %>%
  summarize(md_sal_Ind_per_Birthyear = median(DispInk04)) %>%
  ungroup()

############
############
############
############
# merge it to the ongoing data
main_data_uppsala_tbl <- main_data_uppsala_tbl %>%
  left_join(main_data_tbl_median_sal_uppsala, by = c('Age_year', 'sex')) %>%
  mutate(
    Ind_slr_cat = case_when(
      DispInk04 >= md_sal_Ind_per_Birthyear ~ 'High',
      DispInk04 < md_sal_Ind_per_Birthyear ~ 'Low',
      TRUE ~ NA
    )
  )

############
############
############
############
# Create a variable that combines education and sex
main_data_uppsala_tbl <- main_data_uppsala_tbl %>%
  mutate(
    Educ_sex =   case_when(
      Education == 'Primary School' & sex == 'Female' ~ 'Primr_F',
      Education == 'Primary School' &
        sex == 'Male' ~ 'Primr_M',
      
      Education == 'Gymnasium' & sex == 'Female' ~ 'Gymns_F',
      Education == 'Gymnasium' & sex == 'Male' ~ 'Gymns_M',
      
      Education == 'University' & sex == 'Female' ~ 'Univ_F',
      Education == 'University' & sex == 'Male' ~ 'Univ_M',
      
      DispInk04 < md_sal_Ind_per_Birthyear ~ 'Low',
      TRUE ~ NA
    )
  )

############
############
############
############
# Filter
main_data_uppsala_tbl_restricted_39_59 <- main_data_uppsala_tbl %>%
  filter(Age_year <= (2021 - 40) & Age_year >= (2021 - 59))


############
############
############
############
# check the vaccination dates within your study population
main_data_uppsala_tbl_restricted_39_59 <-
  main_data_uppsala_tbl_restricted_39_59 %>%
  mutate(First_dose = case_when(days_btwn_opening_and_vacc <= 90 ~ First_dose,
                                TRUE ~ NA))





############
############
############
############
##### # # # # PREPARATION FOR SPILLOVER - UPPSALA # # # # ##### ----------------------------------------------


############
############
############
############
# CALCULATE AGE YEAR AND AGE MONTH Seperately
main_data_spillover <- main_data_spillover %>%
  mutate(
    Age_year = as.numeric(substr(FodelseArMan, 1, 4)),
    Age_month = as.numeric(substr(FodelseArMan, 5, 6)),
    age_for_model = (2021 - Age_year) + (6 - Age_month) / 12
  )


main_data_spillover <- main_data_spillover %>%
  
  
  mutate(
    # OUTCOME - create all the data needed to see if each individual got the outcome (vaccination) with 90 days from the opening day
    First_dose = as.Date(First_dose, format = '%Y-%m-%d'),
    vacc_date_opening_p_age = case_when(
      Age_year >= 1962 &
        Age_year <= 1971  ~ as.Date('24-05-2021', format = '%d-%m-%Y'),
      Age_year >= 1972 &
        Age_year <= 1976  ~ as.Date('31-05-2021', format = '%d-%m-%Y'),
      Age_year >= 1977 &
        Age_year <= 1981  ~ as.Date('07-06-2021', format = '%d-%m-%Y'),
      TRUE ~ NA
    ),
    
    days_btwn_opening_and_vacc = as.numeric(First_dose - vacc_date_opening_p_age),
    
    vaccinated_90days = case_when(days_btwn_opening_and_vacc <= 90 ~ 'Yes',
                                  TRUE ~ 'No'),
    
    
    vaccinated_0days = case_when(days_btwn_opening_and_vacc < 0 ~ 'Yes',
                                 TRUE ~ 'No'),
    
    # CREATE THE EXPOSURE - the ones above 50 had the letter with the appointment
    Letter_received = case_when(
      Age_year >= 1962 & Age_year <= 1971 ~ 'Letter',
      Age_year >= 1972 ~  'No letter',
      TRUE ~ NA
    )
  )



# MAke the data look nicer
main_data_spillover_tbl <- main_data_spillover %>%
  
  # create the category of the big age groups
  
  mutate(
    Letter_received = factor(Letter_received, levels = c('No letter', 'Letter')),
    
    Age_group = case_when(
      age_for_model >= 39.5 & age_for_model < 49.5 ~ '39.5-49.4',
      age_for_model >= 49.5 & age_for_model <= 59.4 ~ '49.5-59.4',
      TRUE ~ 'outside_range'
    ),
    
    Age_group = factor(Age_group, levels = c('39.5-49.4', '49.5-59.4')),
    
    Birth_year_group = case_when(
      Age_year >= 1962 & Age_year <= 1971 ~ '1962 - 1971',
      Age_year >= 1972 & Age_year <= 1981 ~ '1972 - 1981',
      TRUE ~ 'outside_range'
    ),
    
    Birth_year_group = factor(Birth_year_group, levels = c('1962 - 1971', '1972 - 1981')),
    
    
    vaccinated_90days = factor(vaccinated_90days),
    sex = case_when(Kon == 1 ~ 'Male',
                    Kon == 2 ~ 'Female',
                    TRUE ~ NA),
    sex = factor(sex)
    
    
  ) %>%
  
  rename(Vax_within_90 = vaccinated_90days,
         
         
         age_for_model = age_for_model)



# first identify individuals 40-50 as the main data
# then, match their partnership status
# then, match their partner's age
# keep only individuals with a partner aged between 40-60


############
############
############
############
# People influenced by spillover
main_data_Spillover_tbl_restricted_39_49 <-
  main_data_uppsala_tbl_restricted_39_59 %>%
  filter(Age_year <= (2021 - 40) & Age_year >= (2021 - 49)) %>%
  dplyr::select(
    P1105_LopNr_PersonNr      ,
    DispInk04                 ,
    Education                 ,
    Educ_sex,
    First_dose                ,
    Age_year                  ,
    Age_month,
    age_for_model                   ,
    vacc_date_opening_p_age   ,
    Vax_within_90             ,
    Letter_received           ,
    sex               ,
    FodelseArMan,
    Foreign_vs_Sweden         ,
    High_Low_Cntr      ,
    Ind_slr_cat,
    Birth_year_group,
    
    Risk_group,
    length_stay_cat,
    trust_catgr,
    
    Vax_within_0
    
  )

colnames(main_data_Spillover_tbl_restricted_39_49) <-
  paste0(colnames(main_data_Spillover_tbl_restricted_39_49),
         '_influenced')

############
############
############
############
# Find their partner's id
ind_RTB <-  fread("//directory/RTB2020.csv")

main_data_Spillover_tbl_restricted_39_49 <-
  main_data_Spillover_tbl_restricted_39_49 %>%
  left_join(ind_RTB,
            by = c('P1105_LopNr_PersonNr_influenced' = 'P1105_LopNr_PersonNr'))

############
############
############
############
# combine
main_data_Spillover_tbl_restricted_39_49 <-
  main_data_Spillover_tbl_restricted_39_49 %>%
  mutate(partner_ID = coalesce(P1105_LopNr_PersonNrSambo, P1105_LopNr_PersonNrMakPart))

############
############
############
############
# remove NAs, aka people who are not partnered
main_data_Spillover_tbl_restricted_39_49 <-
  main_data_Spillover_tbl_restricted_39_49 %>%
  filter(!is.na(partner_ID))




############
############
############
############
# Bring in the 'senders' of spillover
main_data_spillover_tbl_influencer <- main_data_spillover_tbl %>%
  dplyr::select(
    P1105_LopNr_PersonNr,
    Age_year,
    FodelseArMan,
    Age_month,
    age_for_model,
    Birth_year_group,
    Letter_received,
    Lan
  )

colnames(main_data_spillover_tbl_influencer) <-
  paste0(colnames(main_data_spillover_tbl_influencer), '_influencer')

# merge the two
main_data_Spillover_Partnered_Influencer <-
  main_data_Spillover_tbl_restricted_39_49 %>%
  left_join(
    main_data_spillover_tbl_influencer,
    by = c('partner_ID' = 'P1105_LopNr_PersonNr_influencer')
  )

# create a couple age difference to understand how things work
main_data_Spillover_Partnered_Influencer <-
  main_data_Spillover_Partnered_Influencer %>%
  mutate(pair_age_differ = age_for_model_influenced - age_for_model_influencer)

# remove rows from partners who did not leave in uppsala
main_data_Spillover_Partnered_Influencer <-
  main_data_Spillover_Partnered_Influencer %>%
  filter(Lan_influencer == 3)

############
############
############
############
# explore NAs
missing_partner_age <- main_data_Spillover_Partnered_Influencer %>%
  filter(is.na(age_for_model_influencer))

ids_missing_partners <- as.numeric(missing_partner_age$partner_ID)
ind_2020$P1105_LopNr_PersonNr <-
  as.numeric(ind_2020$P1105_LopNr_PersonNr)

non_matching_partners_ind <-  ind_2020 %>%
  filter(P1105_LopNr_PersonNr %in% missing_partner_age$partner_ID)


# exclude them for now since we dont have information from these people on the date of birth
Spillover_data <- main_data_Spillover_Partnered_Influencer %>%
  filter(!is.na(age_for_model_influencer))


# remove rows with partners outside the birth years 1962-1981
Spillover_data <- Spillover_data %>%
  filter(Age_year_influencer <= 1981 & Age_year_influencer >= 1962)





############
############
############
############
##### # # # # PREPARE THE OUTCOME VARIABLE # # # # ##### ----------------------------------------------


# Population addition code

pop <-  fread("//directory/Population_PersonNr_20211231.csv")

# keep only the ids that are found in the ind_2020 file
ind_2020 <-  fread("//directory/Individ_2020.csv")


############
############
############
############
# HERE YOU CHOOSE THE COUNTY
ind_2020 <- ind_2020 %>%
  filter(Lan == county_code)

pop <- pop %>%
  semi_join(ind_2020, by = 'P1105_LopNr_PersonNr')


# recreate the same time variables for the pop file
pop$Birth_year <- substr(pop$FodelseArMan, 1, 4)
pop$Birth_month <- substr(pop$FodelseArMan, 5, 6)

# keep only target birth years
pop <- pop %>%
  filter(Birth_year >= (2021 - 59),
         Birth_year <= (2021 - 40))


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

pop_per_month_cat <- pop %>%
  group_by(BirthYM_Cat) %>%
  summarise(population = n()) %>%
  ungroup() %>%
  arrange(BirthYM_Cat)


############
############
############
############
# for the following calculation you need the education, sex, foreign born salary and sex to do the manipulations
pop_for_interaction_models <- pop %>%
  left_join(
    main_data_tbl %>% dplyr::select(
      P1105_LopNr_PersonNr,
      sex,
      Education,
      Ind_slr_cat,
      High_Low_Cntr,
      Educ_sex,
      Foreign_vs_Sweden,
      trust_catgr,
      length_stay_cat,
      Risk_group
    )
  )

pop_per_month_cat_education <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, Education) %>%
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_country <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, Foreign_vs_Sweden) %>%  #
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_trust <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, trust_catgr) %>%  #
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_stay <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, length_stay_cat) %>%  #
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_Risk <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, Risk_group) %>%  #
  summarise(population = n()) %>%
  ungroup()


pop_per_month_cat_salary <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, Ind_slr_cat) %>%
  summarise(population = n()) %>%
  ungroup()

pop_per_month_cat_sex <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, sex) %>%
  summarise(population = n()) %>%
  ungroup()


pop_per_month_cat_education_sex <- pop_for_interaction_models %>%
  group_by(BirthYM_Cat, Educ_sex) %>%
  summarise(population = n()) %>%
  ungroup()

main_data_tbl$BirthYM_Cat <- as.character(main_data_tbl$BirthYM_Cat)
pop_per_month_cat$BirthYM_Cat <-
  as.character(pop_per_month_cat$BirthYM_Cat)



# Fix
main_data_tbl <- main_data_tbl %>%
  rename(Birth_year = Age_year,
         Birth_month = Age_month) %>%
  # create a round age that rounds the values DOWN
  mutate(Age_Round = floor(age_for_model))



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


############
############
############
############
#Two-month increment code
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


############
############
############
############
###
# 90 THRESHOLD
# You need to summarize the data per vaccinated_within_90 and per BirthYM_Cat
main_data_tbl_sum_90 <- main_data_tbl %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

# bring in the total pop
main_data_tbl_sum_90 <- main_data_tbl_sum_90 %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_90$prop_vaxed <-
  (main_data_tbl_sum_90$pop_vaxed / main_data_tbl_sum_90$population)

# add age_for_model
main_data_tbl_sum_90 <- main_data_tbl_sum_90 %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)# %>%


# recreate the ordered factor for birth year cat
main_data_tbl_sum_90$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_90$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_90$BirthYM_Cat[order(main_data_tbl_sum_90$birth_y_m_D)])
  )


############
############
############
############
#########
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
##### # # # # SENSITIVITY DATA # # # # ##### ----------------------------------------------



############
############
############
############
# create a subset of data after excluding people who got vaccianted ahead of time.
main_data_tbl_logit_ahead_of_time_out <- main_data_tbl_logit %>%
  filter(days_btwn_opening_and_vacc >= 0 |
           is.na(days_btwn_opening_and_vacc))

main_data_tbl_logit_ahead_of_time_out <-
  subset(
    main_data_tbl_logit,
    days_btwn_opening_and_vacc >= 0 |
      is.na(days_btwn_opening_and_vacc)
  )



############
############
############
############
# EDUCATION DISTRIBUTION AROUND THE CUTOFF
####
# FOR University Education

main_data_tbl_sum_Univ <- main_data_tbl %>%
  filter(Education == 'University') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_Univ$prop_vaxed <-
  (main_data_tbl_sum_Univ$pop_vaxed / main_data_tbl_sum_Univ$population)


# add age_for_model
main_data_tbl_sum_Univ <- main_data_tbl_sum_Univ %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_Univ$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_Univ$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_Univ$BirthYM_Cat[order(main_data_tbl_sum_Univ$birth_y_m_D)])
  )

############
############
############
############
####
# DO THE SAME FOR Upper secondary Education

main_data_tbl_sum_Gymn <- main_data_tbl %>%
  filter(Education == 'Gymnasium') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_Gymn$prop_vaxed <-
  (main_data_tbl_sum_Gymn$pop_vaxed / main_data_tbl_sum_Gymn$population)


# add age_for_model
main_data_tbl_sum_Gymn <- main_data_tbl_sum_Gymn %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_Gymn$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_Gymn$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_Gymn$BirthYM_Cat[order(main_data_tbl_sum_Gymn$birth_y_m_D)])
  )



############
############
############
############
# sex DISTRIBUTION AROUND THE CUTOFF
####
# DO THE SAME FOR male

main_data_tbl_sum_Male <- main_data_tbl %>%
  filter(sex == 'Male') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_Male$prop_vaxed <-
  (main_data_tbl_sum_Male$pop_vaxed / main_data_tbl_sum_Male$population)


# add age_for_model
main_data_tbl_sum_Male <- main_data_tbl_sum_Male %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_Male$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_Male$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_Male$BirthYM_Cat[order(main_data_tbl_sum_Male$birth_y_m_D)])
  )





############
############
############
############
# income DISTRIBUTION AROUND THE CUTOFF
# DO THE SAME FOR High

main_data_tbl_sum_HighInk <- main_data_tbl %>%
  filter(Ind_slr_cat == 'High') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_HighInk$prop_vaxed <-
  (main_data_tbl_sum_HighInk$pop_vaxed / main_data_tbl_sum_HighInk$population)


# add age_for_model
main_data_tbl_sum_HighInk <- main_data_tbl_sum_HighInk %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_HighInk$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_HighInk$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_HighInk$BirthYM_Cat[order(main_data_tbl_sum_HighInk$birth_y_m_D)])
  )






############
############
############
############
#####
# trust DISTRIBUTION AROUND THE CUTOFF
####
# DO THE SAME FOR high-trust

main_data_tbl_sum_HighTrust <- main_data_tbl %>%  # length_stay_cat
  filter(trust_catgr == 'High') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_HighTrust$prop_vaxed <-
  (main_data_tbl_sum_HighTrust$pop_vaxed / main_data_tbl_sum_HighTrust$population)


# add age_for_model
main_data_tbl_sum_HighTrust <- main_data_tbl_sum_HighTrust %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_HighTrust$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_HighTrust$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_HighTrust$BirthYM_Cat[order(main_data_tbl_sum_HighTrust$birth_y_m_D)])
  )


############
############
############
############
####
# DO THE SAME FOR low-trust

main_data_tbl_sum_LowTrust <- main_data_tbl %>%
  filter(trust_catgr == 'Low') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_LowTrust$prop_vaxed <-
  (main_data_tbl_sum_LowTrust$pop_vaxed / main_data_tbl_sum_LowTrust$population)


# add age_for_model
main_data_tbl_sum_LowTrust <- main_data_tbl_sum_LowTrust %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_LowTrust$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_LowTrust$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_LowTrust$BirthYM_Cat[order(main_data_tbl_sum_LowTrust$birth_y_m_D)])
  )



############
############
############
############
#####
# trust DISTRIBUTION AROUND THE CUTOFF
####
# DO THE SAME FOR long-duration

main_data_tbl_sum_LongStay <- main_data_tbl %>%
  filter(length_stay_cat == 'Long') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_LongStay$prop_vaxed <-
  (main_data_tbl_sum_LongStay$pop_vaxed / main_data_tbl_sum_LongStay$population)


# add age_for_model
main_data_tbl_sum_LongStay <- main_data_tbl_sum_LongStay %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_LongStay$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_LongStay$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_LongStay$BirthYM_Cat[order(main_data_tbl_sum_LongStay$birth_y_m_D)])
  )


############
############
############
############
####
# DO THE SAME FOR short-duration

main_data_tbl_sum_ShortStay <- main_data_tbl %>%
  filter(length_stay_cat == 'Short') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_ShortStay$prop_vaxed <-
  (main_data_tbl_sum_ShortStay$pop_vaxed / main_data_tbl_sum_ShortStay$population)


# add age_for_model
main_data_tbl_sum_ShortStay <- main_data_tbl_sum_ShortStay %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_ShortStay$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_ShortStay$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_ShortStay$BirthYM_Cat[order(main_data_tbl_sum_ShortStay$birth_y_m_D)])
  )



############
############
############
############
#####
# risk group DISTRIBUTION AROUND THE CUTOFF
####
# DO THE SAME FOR high risk group

main_data_tbl_sum_Risk <- main_data_tbl %>%
  filter(Risk_group == 'Yes') %>%
  group_by(Letter_received,
           Birth_year,
           Birth_month,
           BirthYM_Cat,
           birth_y_m_D) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup() %>%
  left_join(pop_per_month_cat, by = 'BirthYM_Cat')

main_data_tbl_sum_Risk$prop_vaxed <-
  (main_data_tbl_sum_Risk$pop_vaxed / main_data_tbl_sum_Risk$population)


# add age_for_model
main_data_tbl_sum_Risk <- main_data_tbl_sum_Risk %>%
  mutate(age_for_model = (2021 - Birth_year) + (6 - Birth_month) / 12)


# recreate the ordered factor for birth year cat
main_data_tbl_sum_Risk$BirthYM_Cat <-
  factor(
    main_data_tbl_sum_Risk$BirthYM_Cat,
    ordered = T,
    levels = unique(main_data_tbl_sum_Risk$BirthYM_Cat[order(main_data_tbl_sum_Risk$birth_y_m_D)])
  )


############
############
############
############
##### # # # # BASELINE VACCINATIONS # # # # ##### ----------------------------------------------
# Baseline vaccinations among people born in 1972



############
############
############
############
# find the total population for that year
pop_per_1972 <- pop %>%
  filter(Birth_year == '1972') %>%
  group_by(Birth_year) %>%
  summarise(population = n()) %>%
  ungroup() %>%
  arrange(Birth_year)



############
############
############
############
# for the following calculation you need the education, sex, foreign born salary and sex to do the manipulations
pop_for_1972 <- pop %>%
  filter(Birth_year == '1972') %>%
  left_join(
    main_data_tbl %>% dplyr::select(
      P1105_LopNr_PersonNr,
      sex,
      Education,
      Ind_slr_cat,
      High_Low_Cntr,
      Educ_sex,
      Foreign_vs_Sweden,
      trust_catgr,
      length_stay_cat,
      Risk_group
    )
  )

pop_per_education_1972 <- pop_for_1972 %>%
  filter(Birth_year == '1972') %>%
  group_by(Birth_year, Education) %>%
  summarise(population = n()) %>%
  ungroup()

pop_per_trust_1972 <- pop_for_1972 %>%
  filter(Birth_year == '1972') %>%
  group_by(Birth_year, trust_catgr) %>%  #
  summarise(population = n()) %>%
  ungroup()

pop_per_stay_1972 <- pop_for_1972 %>%
  filter(Birth_year == '1972') %>%
  group_by(Birth_year, length_stay_cat) %>%  #
  summarise(population = n()) %>%
  ungroup()

pop_per_Risk_1972 <- pop_for_1972 %>%
  filter(Birth_year == '1972') %>%
  group_by(Birth_year, Risk_group) %>%  #
  summarise(population = n()) %>%
  ungroup()


pop_per_salary_1972 <- pop_for_1972 %>%
  filter(Birth_year == '1972') %>%
  group_by(Birth_year, Ind_slr_cat) %>%
  summarise(population = n()) %>%
  ungroup()

pop_per_sex_1972 <- pop_for_1972 %>%
  filter(Birth_year == '1972') %>%
  group_by(Birth_year, sex) %>%
  summarise(population = n()) %>%
  ungroup()



############
############
############
############
# MAIN
# 1. find people vaccinated in that birth year
# 2. find the proportion vaccinated

pop_vaxed_main_1972 <- main_data_tbl %>%
  filter(Birth_year == '1972') %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Birth_year) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_vaxed_main_1972$Birth_year <-
  as.character(pop_vaxed_main_1972$Birth_year)

pop_vaxed_main_1972 <- pop_vaxed_main_1972 %>%
  left_join(pop_per_1972, by = c('Birth_year'))


pop_vaxed_main_1972$prop_vaxed <-
  (pop_vaxed_main_1972$pop_vaxed / pop_vaxed_main_1972$population)




# do the same for each subcategory

############
############
############
############
# EDUCATION
pop_vaxed_main_1972_educ <- main_data_tbl %>%
  filter(Birth_year == '1972') %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Birth_year, Education) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_vaxed_main_1972_educ$Birth_year <-
  as.character(pop_vaxed_main_1972_educ$Birth_year)

pop_vaxed_main_1972_educ <- pop_vaxed_main_1972_educ %>%
  left_join(pop_per_education_1972, by = c('Education', 'Birth_year'))


pop_vaxed_main_1972_educ$prop_vaxed <-
  (pop_vaxed_main_1972_educ$pop_vaxed / pop_vaxed_main_1972_educ$population)

############
############
############
############
# TRUST

pop_vaxed_main_1972_trust <- main_data_tbl %>%
  filter(Birth_year == '1972') %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Birth_year, trust_catgr) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_vaxed_main_1972_trust$Birth_year <-
  as.character(pop_vaxed_main_1972_trust$Birth_year)

pop_vaxed_main_1972_trust <- pop_vaxed_main_1972_trust %>%
  left_join(pop_per_trust_1972, by = c('trust_catgr', 'Birth_year'))


pop_vaxed_main_1972_trust$prop_vaxed <-
  (pop_vaxed_main_1972_trust$pop_vaxed / pop_vaxed_main_1972_trust$population)


############
############
############
############
# LENGTH STAY

pop_vaxed_main_1972_stay <- main_data_tbl %>%
  filter(Birth_year == '1972') %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Birth_year, length_stay_cat) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_vaxed_main_1972_stay$Birth_year <-
  as.character(pop_vaxed_main_1972_stay$Birth_year)

pop_vaxed_main_1972_stay <- pop_vaxed_main_1972_stay %>%
  left_join(pop_per_stay_1972, by = c('length_stay_cat', 'Birth_year'))


pop_vaxed_main_1972_stay$prop_vaxed <-
  (pop_vaxed_main_1972_stay$pop_vaxed / pop_vaxed_main_1972_stay$population)

############
############
############
############
# MEDICAL RISK

pop_vaxed_main_1972_risk <- main_data_tbl %>%
  filter(Birth_year == '1972') %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Birth_year, Risk_group) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_vaxed_main_1972_risk$Birth_year <-
  as.character(pop_vaxed_main_1972_risk$Birth_year)

pop_vaxed_main_1972_risk <- pop_vaxed_main_1972_risk %>%
  left_join(pop_per_Risk_1972, by = c('Risk_group', 'Birth_year'))


pop_vaxed_main_1972_risk$prop_vaxed <-
  (pop_vaxed_main_1972_risk$pop_vaxed / pop_vaxed_main_1972_risk$population)

############
############
############
############
# INCOME

pop_vaxed_main_1972_income <- main_data_tbl %>%
  filter(Birth_year == '1972') %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Birth_year, Ind_slr_cat) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_vaxed_main_1972_income$Birth_year <-
  as.character(pop_vaxed_main_1972_income$Birth_year)

pop_vaxed_main_1972_income <- pop_vaxed_main_1972_income %>%
  left_join(pop_per_salary_1972, by = c('Ind_slr_cat', 'Birth_year'))


pop_vaxed_main_1972_income$prop_vaxed <-
  (pop_vaxed_main_1972_income$pop_vaxed / pop_vaxed_main_1972_income$population)

############
############
############
############
# SEX

pop_vaxed_main_1972_sex <- main_data_tbl %>%
  filter(Birth_year == '1972') %>%
  filter(Vax_within_90 == 'Yes') %>%
  group_by(Birth_year, sex) %>%
  summarize(pop_vaxed = n()) %>%
  ungroup()

pop_vaxed_main_1972_sex$Birth_year <-
  as.character(pop_vaxed_main_1972_sex$Birth_year)

pop_vaxed_main_1972_sex <- pop_vaxed_main_1972_sex %>%
  left_join(pop_per_sex_1972, by = c('sex', 'Birth_year'))


pop_vaxed_main_1972_sex$prop_vaxed <-
  (pop_vaxed_main_1972_sex$pop_vaxed / pop_vaxed_main_1972_sex$population)



############
############
############
############
pop_vaxed_main_1972$model <- 'main'

pop_vaxed_main_1972 <-
  pop_vaxed_main_1972 %>%  dplyr::select(Birth_year, model, pop_vaxed, population, prop_vaxed)

pop_vaxed_main_1972_educ <- pop_vaxed_main_1972_educ %>%
  dplyr::rename(model = Education)

pop_vaxed_main_1972_stay <- pop_vaxed_main_1972_stay %>%
  dplyr::rename(model = length_stay_cat) %>%
  mutate(model = paste0(model, '-', 'Stay'))

pop_vaxed_main_1972_risk <- pop_vaxed_main_1972_risk %>%
  dplyr::rename(model = Risk_group) %>%
  mutate(model = paste0(model, '-', 'Risk'))

pop_vaxed_main_1972_income <- pop_vaxed_main_1972_income %>%
  dplyr::rename(model = Ind_slr_cat) %>%
  mutate(model = paste0(model, '-', 'Income'))

pop_vaxed_main_1972_sex <- pop_vaxed_main_1972_sex %>%
  dplyr::rename(model = sex)


pop_vaxed_main_1972_trust <- pop_vaxed_main_1972_trust %>%
  dplyr::rename(model = trust_catgr) %>%
  mutate(model = paste0(model, '-', 'Trust'))


############
############
############
############
# bring together in one table

all_baseline_vaccinations <- rbind(
  pop_vaxed_main_1972,
  pop_vaxed_main_1972_trust,
  pop_vaxed_main_1972_educ,
  pop_vaxed_main_1972_stay,
  pop_vaxed_main_1972_risk,
  pop_vaxed_main_1972_income,
  pop_vaxed_main_1972_sex
)


# round the percentages
all_baseline_vaccinations$prop_vaxed_100 <-
  all_baseline_vaccinations$prop_vaxed * 100

all_baseline_vaccinations$prop_vaxed_100 <-
  round(all_baseline_vaccinations$prop_vaxed_100, digits = 2)







############
############
############
############
##### # # # # FILE GENERATION FOR ANALYSES # # # # ##### ----------------------------------------------



# FAST TRACK
# write.csv(x= main_data_uppsala_tbl_restricted_39_59,
# file= '//directory/main_data_tbl_Uppsala_20240528.csv')
#
# write.csv(x= main_data_gavle_tbl_restricted_39_59,
# file= '//directory/main_data_tbl_Gavle_20240528.csv')
#
# write.csv(x= main_data_stockholm_tbl_restricted_39_59,
# file= '//directory/main_data_tbl_Stockholm_20240528.csv')
#
# write.csv(x= Spillover_data,
# file= '//directory/Spillover_data_Uppsala_20240528.csv')



############
############
############
############
##### # # # # TABLE 1 PREPARATION - UPPSALA # # # # ##### ----------------------------------------------

library(table1)
############
############
############
############
### UPPSALA
main_data_Table1_uppsala <-
  main_data_uppsala_tbl_restricted_39_59 %>%
  dplyr::select(
    First_dose,
    Birth_year_group,
    Vax_within_90,
    Vax_within_0,
    sex,
    age_for_model,
    Foreign_vs_Sweden,
    DispInk04,
    Ind_slr_cat,
    Risk_group,
    length_stay_cat,
    trust_catgr,
    Education,
    Letter_received
  )


############
# create a new variable that shows the median salary per low and high category and per birth cohort
main_data_Table1_uppsala <- main_data_Table1_uppsala %>%
  mutate(
    Salary_Birth = case_when(
      Birth_year_group ==  '1962 - 1971' &
        Ind_slr_cat == 'Low' ~ '50_59_Low',
      Birth_year_group ==  '1962 - 1971' ~ '50_59_High',
      Birth_year_group ==  '1972 - 1981' &
        Ind_slr_cat == 'Low' ~ '40_49_Low',
      Birth_year_group ==  '1972 - 1981' ~ '40_49_High',
      TRUE ~ NA
    )
  )


############
# find the earlist date of vaccination of individuals who received the letter and did not receive the letter and the latest
main_data_Table1_uppsala_vacc_dates <-
  main_data_Table1_uppsala %>%
  group_by(Letter_received) %>%
  summarize(
    min_date = min(First_dose, na.rm = T),
    max_date = max(First_dose, na.rm = T)
  ) %>%
  ungroup()


############
main_data_Table1_uppsala_Salary_Birth <-
  main_data_Table1_uppsala %>%
  group_by(Salary_Birth, Letter_received) %>%
  summarize(md_Ind_slr_cat = median(DispInk04)) %>%
  ungroup()

############
main_data_Table1_uppsala_Salary_Birth_IQR <-
  main_data_Table1_uppsala %>%
  group_by(Salary_Birth, Letter_received) %>%
  summarize(
    median_income = round(median(DispInk04)),
    lower_Q_income = round(quantile(DispInk04, 0.25)),
    upper_income = round(quantile(DispInk04, 0.75)),
    interquartile_income = round(IQR(DispInk04))
  ) %>%
  ungroup()

############
main_data_Table1_uppsala_Salary_Birth_IQR2 <-
  main_data_Table1_uppsala %>%
  group_by(Salary_Birth, Letter_received) %>%
  summarize(
    median_income = (median(DispInk04)),
    lower_Q_income = (quantile(DispInk04, 0.25)),
    upper_income = (quantile(DispInk04, 0.75)),
    interquartile_income = (IQR(DispInk04))
  ) %>%
  ungroup()


############
main_data_Table1_uppsala_Salary_Birth_Total <-
  main_data_Table1_uppsala %>%
  group_by(Ind_slr_cat) %>%
  summarize(md_Ind_slr_cat = median(DispInk04)) %>%
  ungroup()


############
label(main_data_Table1_uppsala$Birth_year_group) <- 'Birth year'
label(main_data_Table1_uppsala$Education) <- 'Education'
label(main_data_Table1_uppsala$sex) <- 'sex'
label(main_data_Table1_uppsala$Foreign_vs_Sweden) <-
  'Birth country**'
label(main_data_Table1_uppsala$age_for_model) <- 'Age *'
label(main_data_Table1_uppsala$Vax_within_90) <-
  'Vaccinated within 90d***'
label(main_data_Table1_uppsala$Vax_within_0) <-
  'Vaccinated ahead of time'
label(main_data_Table1_uppsala$Risk_group) <- 'Risk group status'
label(main_data_Table1_uppsala$length_stay_cat) <- 'Length of stay'
label(main_data_Table1_uppsala$trust_catgr) <-
  'Birth country trust level'

############
# remove unnecessary columsn
main_data_Table1_uppsala_main <- main_data_Table1_uppsala %>%
  dplyr::select(
    sex,
    age_for_model,
    Birth_year_group ,
    Foreign_vs_Sweden,
    Education,
    Risk_group,
    length_stay_cat,
    trust_catgr,
    Vax_within_0,
    Vax_within_90,
    Letter_received
  )


############
############
############
############
### calculate the p values
pvalue <- function(x, name, ...) {
  # Construct vectors of data y, and groups (strata) g
  # remove overall column
  x <- x[names(x) != 'overall']
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of
    # independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than
  # sign.
  # The initial empty string places the output on the line below
  # the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}




############
############
############
############
##### # # # # TABLE 1 - UPPSALA # # # # ##### ----------------------------------------------


############
############
############
############
table1(
  ~ sex +
    age_for_model +
    
    trust_catgr +
    
    length_stay_cat +
    Education +
    Risk_group +
    
    Vax_within_0 +
    Vax_within_90  | Letter_received,
  overall =  c(right = 'Total'),
  # F,
  caption = 'Uppsala',
  
  extra.col = list(`P-value` = pvalue),
  footnote = c(
    '* Age calculated as (2021-Age_year)+(6-Age_month)/12',
    '** Based on the World Bank data (Middle_Low includes Upper-Middle, Middle and Low Income countries',
    '*** Within XX days from the opening date corresponding to the birth year'
  ),
  data = main_data_Table1_uppsala_main
)


############
############
############
############
#calculate the mean age confidence intervals
avg_mean_age <-
  tapply(
    main_data_Table1_uppsala_main$age_for_model,
    main_data_Table1_uppsala_main$Letter_received,
    mean
  )

############
############
############
############
# calculate the confidence intervals
ci_per_group <-
  tapply(main_data_Table1_uppsala_main$age_for_model, main_data_Table1_uppsala_main$Letter_received, function(x)
    (t.test(x)$conf.int))


############
############
############
############
# Calculate average and standard deviation per group
avg_per_group <-
  tapply(
    main_data_Table1_uppsala_main$age_for_model,
    main_data_Table1_uppsala_main$Letter_received,
    mean
  )
sd_per_group <-
  tapply(
    main_data_Table1_uppsala_main$age_for_model,
    main_data_Table1_uppsala_main$Letter_received,
    sd
  )
n_per_group <-
  tapply(
    main_data_Table1_uppsala_main$age_for_model,
    main_data_Table1_uppsala_main$Letter_received,
    length
  )

# Calculate t-value for desired confidence level (e.g., 95%)
confidence_level <- 0.95
df <- n_per_group - 1
t_value <- qt((1 + confidence_level) / 2, df)

# Calculate margin of error and confidence intervals
margin_of_error <- sd_per_group / sqrt(n_per_group) * t_value
ci_lower <- round(avg_per_group - margin_of_error, 1)
ci_upper <- round(avg_per_group + margin_of_error, 1)

# Display results
ci_per_group <- cbind(ci_lower, ci_upper)
print(ci_per_group)


# Vaccination dates for table 1

main_data_Table1_uppsala_vacc_dates %>%
  kable %>%
  kable_styling('striped', full_width = F) %>%
  scroll_box(width = '350px' , height = '250px')



# Income for table 1

# Table 1: Participants (n) per Birthyear'
main_data_Table1_uppsala_Salary_Birth %>%
  kable %>%
  kable_styling('striped', full_width = F) %>%
  scroll_box(width = '250px' , height = '250px')


# make a contigency table
contingency_talbe  <-
  table(
    main_data_Table1_uppsala_Salary_Birth$Letter_received,
    main_data_Table1_uppsala_Salary_Birth$md_Ind_slr_cat
  )

chi_squared_income <- chisq.test(contingency_talbe)
p_value_income <- chi_squared_income$p.value




# income IQR for Table 1

# 'Table 1: Participants (n) per Birthyear'
main_data_Table1_uppsala_Salary_Birth_IQR %>%
  
  kable %>%
  kable_styling('striped', full_width = F) %>%
  scroll_box(width = '450px' , height = '250px')



# income total for table 1

# Table 1: Participants (n) per Birthyear'
main_data_Table1_uppsala_Salary_Birth_Total %>%
  kable %>%
  kable_styling('striped', full_width = F) %>%
  scroll_box(width = '250px' , height = '250px')
