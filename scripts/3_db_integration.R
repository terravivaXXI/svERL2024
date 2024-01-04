## Now that we have all data in place we can integrate a database with all
# we need and calculate new fields that are important for analysis.

library(tidyverse)
source("scripts/mappingSetup.R")
vum <- read_csv("data/processed/UrbanMunsMex2018.csv") %>% pull(CVEGEO)

# Table with all data ------------------------------------------------------

# Load all the tables we have in datP1

wd <- getwd()
setwd("./paper1/datP1")


#First of all, as always, the geograpic anchor, the MGN
mexmun <- read_sf("MGN_geo/00mun.shp") %>% select(-NOMGEO)

# Socieconomic variables
mun_sev <- read_csv("SocEconVar.csv") %>% 
  select(- c(NOMGEO, POB_TOT, i20_TVIVHAB)) 

# Hansen forest cover
fc <- read_csv("ForestCover2000andGain2012byMun.csv")
# Just to keep track here, pondTreeCover is a direct 
# estimate of all the tree cover above 5m that you can find in the 
# municipality, independently of whether it is in a forest or not. 
# genTreeCover  only sums tree cover when the pixels have more than 10% 
# cover, that according to FAO definition, would be the least you can 
# requiere to call that "forest cover"
fcl <- read_csv("ForestCoverLossByMun00to21.csv") # This table provides for each year after 2000 an estimate of tree cover loss

# Land cover from dynamic world allows us to calculate how much land is available
# by municipality for SV interventions
lusv <- read_csv("SV_viable_area_2018.csv")

# Incorporating a measure of economic size of the municipality based on 
# economic census 2018
mtc <- read_csv("MunicipalityTotalIncome2023-02-23.csv")

# Indice de rezago social
irs <- read_csv("IndiceRezagoSocial2022-08-30.csv") %>% 
  select(CVEGEO, contains(c("irs20_", "irs15"))) %>% 
  mutate(irs20_degree = factor(`irs20_Grado de rezago social`,
                           levels = (c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")),
                           labels = c("Very low", "Low", "Medium", "High", "Very high")),
         irs15_degree = factor(`irs15_Grado de rezago social`,
                          levels = (c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")),
                          labels = c("Very low", "Low", "Medium", "High", "Very high")),
         irs_20_index =  `irs20_Índice de rezago social`,
         irs_15_index =  `irs15_Índice de rezago social`,
         .after = CVEGEO
         ) %>% 
  select(1:5)

# Indexes for restoration and conservation have the same names so I will rename 
# them in a concise way
# As it can be seen in the original calculation performed in sfm/scripts/temp I
# left several NAs in the file that come from operating a sum of no-priority pixels
# across the whole municipality. Those municipalities for the sake of this exercise
# should have 0 priority and therefore I am replacing NA by 0
kci <- read_csv("KeyConservationIndex_Mun_Norm2022-05-04.csv") %>% 
  select(CVEGEO, 
         kpi_abs = overall_priority_index, 
         kpi_prop = prop_priority_index) %>% 
  replace_na(list(kpi_abs = 0, kpi_prop = 0))
rpi <- read_csv("RestorationPriorityIndex_Mun_Norm2022-05-04.csv") %>% 
  select(CVEGEO, 
         rpi_abs = overall_priority_index,
         rpi_prop = prop_priority_index) %>% 
  replace_na(list(rpi_abs = 0, rpi_prop = 0))

# Sembrando Vida tables are quite large, but I am uploading them so that calculations
# are transparent for any reviewer of the work
svn <- read_csv("SV_normalized_duplicates_individual_historic_payment_report2023-03-23.csv")
svph <- read_csv("SV_table_payment_history_pctg2023-03-23.csv") %>% 
  rename_with(.cols = !contains("CVE_"), .fn = ~str_c("sv_", .)) %>% 
  as_tibble(.name_repair = "universal") %>% 
  rename(CVEGEO = CVE_MUN) %>% 
  select(- CVE_ENT)

presi <- read_csv(file = "resultadosPresi2018_Mun2022-05-03.csv") %>% 
  mutate(CVEGEO = str_c(CVE_ENT, CVE_MUN)) %>% 
  select(CVEGEO, mean_sh_morena)

dist_to_mega <- read_csv(file = "Distance_Mun_to_Megaproyects4T-20221114.csv") %>% 
  select(CVEGEO, dist_to_4Tmega) %>% 
  mutate(dist_to_4Tmega = dist_to_4Tmega / 1000) # Converting to KM

z <- mexmun %>% 
  left_join(fc, by = "CVEGEO") %>% 
  left_join(fcl, by = "CVEGEO") %>% 
  left_join(kci, by = "CVEGEO") %>% 
  left_join(rpi, by = "CVEGEO") %>% 
  left_join(irs, by = "CVEGEO") %>% 
  left_join(mun_sev, by = "CVEGEO") %>% 
  left_join(svph , by = c("CVEGEO")) %>% 
  relocate(NOM_ENT, NOM_MUN, contains("i20_"), .after = CVE_MUN)
  
# An important fact to keep in mind here is that all losses and tree cover 
# are in hectares

# Calculated fields -------------------------------------------------------
z <- z %>% 
  arrange(desc(sv_final_num_beneficiarios)) %>% 
  mutate(i20_ind_lang_perc = i20_P5_HLI/i20_P_5YMAS, # What percentage of inhabitants over 5 years speak indigenous language
         i20_pop_dens_habKM2 = i20_POBTOT / area21, # Population density 
         area21ha = area21 * (10) * 10, # Municipality area in hectares. As there are 10 hectares by side in each km2
         is_urban = CVEGEO %in% vum,
         i20_rural_pop = as.numeric(!is_urban) * i20_POBTOT, # transforms the logic vector is_urban in one with 1s for all rural muns and 0 for urbans and then multiplies it by total population
         i20_rural_households = i20_TOTHOG * (i20_rural_pop/i20_POBTOT) %>% replace_na(0), # calculates the percentage of rural population and scales the number of households
         sv_pfam_rec = if_else(i20_rural_households > 0, sv_final_num_beneficiarios / i20_rural_households, sv_final_num_beneficiarios / i20_TOTHOG), # Percentage of all families in the municipality recruited by SV
         sv_pfam_rec_0 = replace_na(sv_pfam_rec, 0),
         sv_area_trans_abs = sv_final_num_beneficiarios * 2.5, # Hypotetical number of hectares transformed by SV in the municipality,
         sv_area_trans_perc = sv_area_trans_abs / area21ha, # Proportion of the total area of the municipality under restoration by SV
         sv_area_trans_abs_0 = replace_na(sv_area_trans_abs, 0),
         gfw_area_forest_perc = gfw_pondTreeCover / area21ha, # What proportion of the municipality is covered by 5m+ canopy,
         GM_2020f = factor(GM_2020,
                               levels = (c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")),
                               labels = c("Very low", "Low", "Medium", "High", "Very high")),
         sv_final_num_beneficiarios_0 = replace_na(sv_final_num_beneficiarios, 0),# replacing NA for 0 in some key fields allows us to make countrywide calculations without restricting our SV universe to the municipalities that were actually recruited
         sv_participates = as.integer(sv_final_num_beneficiarios_0 > 0)

  ) %>% 
  rowwise() %>% 
  mutate(
    gfw_avg_loss_01to21 = mean(c_across(14:34), na.rm = TRUE), # Average loss in the period comprising 2001 to 2021
    gfw_avg_loss_01to18 = mean(c_across(14:31), na.rm = TRUE), # Avg loss for period 01-18,
    gfw_avg_loss_19to21 = mean(c_across(32:34), na.rm = TRUE), # Avg loss for period 19-21,
    gfw_avg_loss_16to18 = mean(c_across(29:31), na.rm = TRUE), # Avg loss for period 16-18,
    gfw_abs_loss_01to21 = sum(c_across(contains("gfw_loss_ha_"))), # DO NOT USE Cumulative loss of forest cover from 2001 to 2021
    gfw_abs_loss_01to12 = sum(c_across(14:25), na.rm = TRUE), # # DO NOT USE Cumulative loss of forest cover from 2001 to 2012
    gfw_net_loss_01to12 = gfw_abs_loss_01to12 - gfw_netGain, # DO NOT USE in the period 01 to 12, how much of the forest loss did not grow back?
    gfw_prop_loss_perm_01to12 = gfw_net_loss_01to12 / gfw_abs_loss_01to12 # Out of all the forest lost, what proportion did not regrow? 
  ) %>% 
  ungroup() %>% 
  mutate( # DO NOT USE, These normalized losses do not work well, because accumulating the losses of 20 years ends up reporting the loss for the same pixel several times
    gfw_norm_loss_01to21 = pmax((gfw_abs_loss_01to21 - gfw_netGain) / gfw_pondTreeCover, 0), # Proportion of the total municipality cover lost from 2001 to 2021
    gfw_norm_loss_01to18 = pmax((gfw_abs_loss_01to21 - gfw_netGain - gfw_loss_ha_2021 - gfw_loss_ha_2020 - gfw_loss_ha_2019) / gfw_pondTreeCover, 0) # Proportion of mun canopy cover lost in 2001 - 2018
  ) %>% 
  mutate(kpi_abs_tertile = ntile(kpi_abs, 3),
         kpi_abs_tertile = case_when(
           kpi_abs_tertile == 1 ~ "Low", 
           kpi_abs_tertile == 2 ~ "Medium",
           kpi_abs_tertile == 3 ~ "High"
         ) %>% 
           factor(
             levels = c("Low", "Medium", "High"),
             labels = c("Low", "Medium", "High")
         ),
         rpi_abs_tertile = ntile(rpi_abs, 3),
         rpi_abs_tertile = case_when(
           rpi_abs_tertile == 1 ~ "Low", 
           rpi_abs_tertile == 2 ~ "Medium",
           rpi_abs_tertile == 3 ~ "High"
         ) %>% 
           factor(
             levels = c("Low", "Medium", "High"),
             labels = c("Low", "Medium", "High")
           ),
         kpi_prop_tertile = ntile(kpi_prop, 3),
         kpi_prop_tertile = case_when(
           kpi_prop_tertile == 1 ~ "Low", 
           kpi_prop_tertile == 2 ~ "Medium",
           kpi_prop_tertile == 3 ~ "High"
         )%>% 
           factor(
             levels = c("Low", "Medium", "High"),
             labels = c("Low", "Medium", "High")
           ),
         rpi_prop_tertile = ntile(rpi_prop, 3),
         rpi_prop_tertile = case_when(
           rpi_prop_tertile == 1 ~ "Low", 
           rpi_prop_tertile == 2 ~ "Medium",
           rpi_prop_tertile == 3 ~ "High"
         )%>% 
           factor(
             levels = c("Low", "Medium", "High"),
             labels = c("Low", "Medium", "High")
           ),
         gfw_sv_0118_period_delta = (gfw_avg_loss_19to21 - gfw_avg_loss_01to18)/100, # a standard delta between averages, positive means increase in the later period
         gfw_sv_1618_period_delta = (gfw_avg_loss_19to21 - gfw_avg_loss_16to18)/100, # a standard delta between averages, positive means increase in the later period
         sv_first_payment_year = round(year(sv_first_payment),0),
         across(.cols = contains("gfw_loss_ha"), ~ .x / gfw_pondTreeCover, .names = "{.col}_norm")
  ) %>% 
  rowwise() %>% 
  mutate(
    gfw_avg_norm_loss_01to21 = mean(c_across(125:145), na.rm = TRUE), # Average normalized loss obtained by averaging the normalized yearly loss, works way better than gfw_norm_loss_01to21
    gfw_avg_norm_loss_01to18 = mean(c_across(125:142), na.rm = TRUE),
    gfw_avg_norm_loss_01to18 = pmin(gfw_avg_norm_loss_01to18, 0.2) # a correction to account for those municipalities that show dissonance with Hansen et al calculations
  )

z <- z %>% 
  left_join(presi, by = "CVEGEO") %>% 
  left_join(dist_to_mega, by = "CVEGEO") %>% 
  left_join(lusv, by = "CVEGEO") %>% 
  left_join(mtc, by = "CVEGEO")

write_csv(st_drop_geometry(z), "finalDB.csv")
    
setwd(wd)
