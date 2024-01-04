## Before running this script you have to run 5.5_figsPaper and also
# load the table tpr

#source(file = "paper1/scrP1/5.5_figsPaper20230426.R")
#tpr <- read_csv(file = "data/processed/SV_table_payment_history2023-07-10.csv")

# Distribution of pfam_rec
z %>% select(sv_pfam_rec) %>% vtable::sumtable()

# Distribution of active SV / area susceptible for implementation of SV
z %>% mutate(sv_viable_area_trans_perc = sv_area_trans_abs/sv_viable_ha * 100) %>% 
  select(sv_viable_area_trans_perc) %>% st_drop_geometry() %>% arrange(desc(sv_viable_area_trans_perc))

z %>% mutate(sv_viable_area_trans_perc = sv_area_trans_abs/sv_viable_ha * 100) %>% 
  select(sv_viable_area_trans_perc) %>% filter(sv_viable_area_trans_perc < 1000) %>% # Removing the 'outliers'
  vtable::sumtable()

# get the numbers for f2alt
z %>% group_by(irs20_degree) %>% 
  st_drop_geometry() %>% 
  summarise(
    median(sv_pfam_rec, na.rm = TRUE)
  )

# get the numbers for f2altB
s4 %>% group_by(irs20_degree) %>% 
  st_drop_geometry() %>% 
  summarise(
    median(share_mun_income_from_SV, na.rm = TRUE)
  )

## variable correlation in final database
correl_lm_vars <- cor(z %>% st_drop_geometry() %>% select(area21, gfw_pondTreeCover, gfw_avg_loss_01to18), use = "complete.obs") 
corrplot::corrplot.mixed(correl_lm_vars,
                         diag = "u",
                         tl.pos = "lt",
                         upper = "pie")

# Num mun in z
z

# Num mun where sv was deployed
nrow(tpr)

# Pctg of muns with at least one historic payment
nrow(tpr) / nrow(z)

# States w/ at least one payment
tpr %>% pull(CVE_ENT) %>% unique() %>% length()

# Num of currently active beneficiaires
(abene <- sum(z$sv_final_num_beneficiarios, na.rm = TRUE))

# Num of hectares in the program
sum(z$sv_final_num_beneficiarios, na.rm = TRUE) * 2.5

# Num mun with sv in z
z %>% filter(sv_participates > 0)

# Pctg of muns with active bene in z
z %>% filter(sv_participates > 0) %>% nrow() / nrow(z)

# Num of states with active beneficiaries
z %>% filter(sv_participates > 0) %>% pull(CVE_ENT) %>% unique() %>% length()

# registered payrolls
pyrls <- str_subset(names(z), "sv_20") 
pyrls %>% length()

# Num of months between first and last payroll
pyrlsf <- pyrls[1] %>% str_remove_all("\\.") %>% str_remove("sv_")
pyrlsl <- pyrls %>% last() %>% str_remove_all("\\.") %>% str_remove("sv_")
interval(ymd(pyrlsf), ymd(pyrlsl)) %/% months(1)

# Beneficiaires in each active mun
z %>% 
  filter(sv_participates > 0) %>% 
  arrange(desc(sv_final_num_beneficiarios)) %>% 
  select(NOM_ENT, NOM_MUN, sv_final_num_beneficiarios) %>% 
  st_drop_geometry() %>% View()

# Beneficiaries by State
z %>% group_by(NOM_ENT) %>% 
  st_drop_geometry() %>% 
  summarise(
    num_bene = sum(sv_final_num_beneficiarios_0)
  ) %>% 
  arrange(desc(num_bene)) %T>%
  slice_head(n = 3) %$% 
  sum(num_bene)

# Num of inactive municipalities
tpr %>% pull(final_num_beneficiarios) %>% is.na() %>% sum()

# Num of payments to inactive municipalities
tpr %>% 
  ungroup() %>% 
  filter(is.na(final_num_beneficiarios)) %>% 
  replace(is.na(.), 0) %>% 
  transmute(sum = rowSums(across(contains("20")))) %>% 
  summarise(total_payments_inactive_muns = sum(sum)) 

# Num of municipalities without active sv
nrow(z) - nrow(z %>% 
       filter(sv_participates > 0) %>% 
       arrange(sv_final_num_beneficiarios) %>% 
       select(NOM_ENT, NOM_MUN, sv_final_num_beneficiarios))

# Pctg of municipalities with sv
nrow(z %>% 
                 filter(sv_participates > 0) %>% 
                 arrange(sv_final_num_beneficiarios) %>% 
                 select(NOM_ENT, NOM_MUN, sv_final_num_beneficiarios)) / nrow(z)

# number of payments in payrolls
svn %>% 
  summarise(num_payments = sum(apoyo_total) / 5000)

# amount of money paid to date in USD
(value_all_transfers <- svn %>% 
  summarise(USD_volume_deposits = sum(apoyo_total)/17))

# A map with state borders
tm_shape(z) +
  tm_fill(col = "sv_area_trans_perc",
          breaks = seq(from = 0, to = 0.4, by= 0.05),
          title = "Sembrando Vida density\nby percentage of\nmunicipal area transformed") +
  tm_shape(state_borders) + 
  tm_borders()

# pctg of bene in higher tertile

o3n[7:9] %>% sum()
o3$tb_breakdown[7:9] %>% sum()

# Sum of all percentages
o3n %>% sum()

# Visualize populations
z %>% select(NOM_MUN, NOM_ENT, i20_POBTOT, i20_TOTHOG, i20_rural_households)

# Total households in Mexico/Study Area depending on what 'z' you are using
(hholds <- z$i20_TOTHOG %>% sum(na.rm = TRUE))
# Pctg of households with sv
abene/hholds

# Total rural households in Mexico
rhholds <- z$i20_rural_households %>% sum(na.rm = TRUE)
# Pctg of rural households with sv
abene/rhholds

# Avg transfer in rough numbers
value_all_transfers/abene

# For poverty levels in the selected states
pov <- read_excel("data/raw/pop/pobreza/Cambios_pobreza_pobreza_extrema_2008_2018/Evolución de pobreza y pobreza extrema nacional y entidades 2008-2018.xlsx",
skip = 8, range = cell_cols(c("H", "Z"))) %>% select(1, 19) %>% slice(3:34) %>% 
  rename(Poverty = ...1, ePoverty = ...19)

# In order to execute this you need the dataset finaldb or 4.1_SVDataAnalysis.. loaded
pov %>% 
  mutate(
    CVE_ENT = row_number() %>% str_pad(width = 2, side = "left", pad = "0")
  ) %>% 
  left_join(sv_active_states %>% select(CVE_ENT), .) %>% 
  summarise(
    mean_Poverty = mean(Poverty),
    mean_ePoverty = mean(ePoverty)
  )

z %>% st_drop_geometry() %>% ungroup() %>% 
  summarise(
  total_pop = sum(i20_POBTOT, na.rm = TRUE),
  total_loss_ha_2020 = sum(gfw_loss_ha_2020, na.rm = TRUE),
  avg_loss_01_21 = sum(gfw_avg_loss_01to21, na.rm = TRUE),
  avg_loss_01_21 = sum(gfw_avg_loss_01to21, na.rm = TRUE)
  
)

z2 <- read_csv("paper1/datP1/finalDB.csv")

z2 %>% st_drop_geometry() %>% ungroup() %>%
summarise(
total_pop = sum(i20_POBTOT, na.rm = TRUE),
total_loss_ha_2020 = sum(gfw_loss_ha_2020, na.rm = TRUE),
avg_loss_01_21 = sum(gfw_avg_loss_01to21, na.rm = TRUE),
avg_loss_01_21 = sum(gfw_avg_loss_01to21, na.rm = TRUE)
)

# Percentage of forest cover loss that happens within the Study Area
227/242

# "deforestation" rate according to FAO FRA 2020 
(127770/65692080) * 100# ha lost vs total forest ha
# 'forest cover loss' according to GFW
# go to dashboard of GFW on Mexico and average the loss rates
# between 2015 and 2020
mean(c(.37, 0.52, .56, .5, .62, .56))

