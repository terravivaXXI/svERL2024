## Paper 1 figures, first draft

# Setup -------------------------------------------------------------------

source(file = "scripts/mappingSetup.R")
source(file = "scripts/functions/quickMaps.R")

library(ggpubr)
library(cowplot)
data("World")
library(patchwork)
library(ggpattern)
library(lattice)
library(grid)
library(multcompView)
library(ggspatial)

pal <- "RdYlBu"
rlcol <- "purple"
gr <- "#38c73f"
path <- "paper1/ploP1/p1figs/"

## DATASETS 

# This dataset was produced by sfm/paper1/scrP1 scripts 1 to 3   
latest_normalized_report <- "paper1/datP1/SV_normalized_duplicates_individual_historic_payment_report2023-03-23.csv"

x1 <- read_csv("paper1/datP1/finalDB.csv")

# Loads MGN2021 modified applying rmapshaper::ms_filter_islands to remove islands (See sfm/scripts/sv/WAC.. for more info)   
mun21 <- read_sf("data/raw/spatial/mgs/INEGI_2021-12_MarcoGeoestadistico/mg2021_integrado/conjunto_de_datos/00munNoIslands.shp")

# Loads state borders from the same MGN2021
state_borders <- read_sf("data/raw/spatial/mgs/INEGI_2021-12_MarcoGeoestadistico/mg2021_integrado/conjunto_de_datos/00entNoIslands.shp")

# Join spatial with numeric data and format
z <- left_join(mun21, x1, by = c("CVEGEO")) %>% 
  mutate(
    irs20_degree = factor(irs20_degree,
                      levels = (c("Very low", "Low", "Medium", "High", "Very high")),
                      labels = c("Very low", "Low", "Medium", "High", "Very high")),
    ent_centroid = st_centroid(geometry)
  )

# Removes from z all municipalities in states that have less than 19 beneficiaries
sv_active_states <- z %>% group_by(CVE_ENT) %>% 
  st_drop_geometry() %>% 
  summarise(state_total = sum(sv_final_num_beneficiarios_0)) %>% 
  filter(state_total > 18) 
# reports 21 states with SV and here I am removing Zacatecas because it will just
# generate more confusion to keep a state with 18 beneficiaries

# Filter the database to only active states
z <- z %>% filter(CVE_ENT %in% pull(sv_active_states, CVE_ENT))

# We also need inactive states to remark our universe
sv_inactive_states <- setdiff(1:32, pull(sv_active_states, CVE_ENT) %>% as.numeric()) %>% str_pad(width = 2, side = "left", pad = "0")
noz <- state_borders %>% filter(CVEGEO %in% sv_inactive_states)


# Alterations to dataset FOR VISUALIZATION PURPOSES ONLY    
tweaked_z <- z %>% mutate(
  # beware of using this dataset outside of map visualizations
  sv_final_num_beneficiarios = replace(sv_final_num_beneficiarios, sv_final_num_beneficiarios > 6000, 6000)
) 

## FUNCTIONS
## afplot adds format to any box or bar plot

afplot <- function(p){
  p + theme_light() +
    theme(legend.position = "none",
          legend.key.size = unit(18, "pt")) +
    scale_fill_brewer(palette = pal, direction = -1) + 
    theme(legend.position = "none",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18)
    )
}
    
  # Plots -------------------------------------------------------------------


# Fig 1 -------------------------------------------------------------------

# In order to do this calculation correctly, we have to take the full list
# of beneficiaries and join it by CVEGEO to attach all the municipality info
# that allows us to classify the payments by poverty degreee and year of recruitment

exch_rate_usd_mxn <- 18.36

svn <- read_csv(file = latest_normalized_report) %>% # Load latest table, careful with this, edit on setup
  rename(CVEGEO = CVE_MUN) %>% 
  left_join(z %>% st_drop_geometry() %>% select(CVEGEO, irs20_degree, i20_rural_households), by = "CVEGEO") %>% #join relevant fields from municipality based data
  mutate(year_of_recruitment = round(year(primer_pago), 0) %>% as.integer(),  # for each beneficiary, consider the first payment as start date, take the year
         irs20_degree = as.character(irs20_degree) # convert poverty degree to character in order to perform join
         ) %>%
  drop_na() 

# get table of number of rural households per poverty degree
s2rh <- svn %>% 
  distinct(CVEGEO, .keep_all = TRUE) %>% 
  group_by(irs20_degree) %>% 
  summarise(
    total_rural_hh = sum(i20_rural_households) 
  )


s2 <- svn %>% 
  group_by(year_of_recruitment) %>% 
  mutate(total_bene_year = n()) %>% 
  group_by(irs20_degree) %>% 
  mutate(total_bene_degree = n()) %>% 
  group_by(year_of_recruitment, irs20_degree) %>% 
  summarise(
    total_bene_year,
    total_bene_yearNdegree = n(),
    total_money_yearNdegree = sum(norm_apoyo_total) / (exch_rate_usd_mxn * 1000000), # This $ figure is in millions of dollars
    pctg_of_year_rec_in_degree = round(total_bene_yearNdegree*100/total_bene_year, 1),
    pctg_of_all_degree_in_year = round(total_bene_yearNdegree*100/total_bene_degree, 1),
    label = str_c(pctg_of_year_rec_in_degree, "%", "\n", pctg_of_all_degree_in_year, "%")
  ) %>% 
  ungroup() %>% 
  left_join(s2rh, by = "irs20_degree") %>% 
  mutate(
    irs20_degree = factor(irs20_degree,
                          levels = (c("Very low", "Low", "Medium", "High", "Very high")),
                          labels = c("Very low", "Low", "Medium", "High", "Very high"))
  ) %>% 
  unique() %>% 
  drop_na()

f1a <- ggplot(data = s2, mapping = aes(x = year_of_recruitment,
                                        y = total_bene_yearNdegree, 
                                        fill = irs20_degree)) +
  geom_col(position = "dodge") +
  scale_color_manual(values = c(rep("black", 6))) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    y = "# beneficiaries",
    x = "Year",
    fill = "Municipal degree of\npoverty (social lag)"
  ) 

f1a <- afplot(f1a) + theme_classic() + 
  theme(legend.position = c(0.77, 0.8),
        rect = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))
f1a


f1b <- ggplot(data = s2, mapping = aes(x = year_of_recruitment,
                                        y = total_bene_yearNdegree/total_rural_hh, 
                                        fill = irs20_degree)) +
  geom_col(position = "dodge") +
  scale_color_manual(values = c(rep("black", 6))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  labs(
    y = "% rural households",
    x = "Year",
    fill = "Degree of poverty\n(social lag index)"
  ) 
f1b <- afplot(f1b) + theme_classic() + 
  theme(legend.position = "none",
        rect = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))
f1b

s3 <- s2 %>% group_by(irs20_degree) %>% 
  summarise(
    total_money_degree = sum(total_money_yearNdegree)
  )

f1c <- ggplot(data = s3, mapping = aes(x = irs20_degree,
                                       y = total_money_degree, 
                                       fill = irs20_degree)) +
  geom_col(position = position_dodge(), width = .9) +
  scale_color_manual(values = c(rep("black", 6))) +
  scale_y_continuous(labels = scales::dollar, expand = expansion(mult = c(0, 0.05))) +
  labs(
    y = "Million USD",
    x = "Municipal degree of\npoverty (social lag)",
    fill = "Municipal degree of\npoverty (social lag)"
  ) 

f1c <- afplot(f1c) + theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        # aspect.ratio = 3/1,
        rect = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))
f1c

f1 <- (f1a) + (f1b) + ((f1c) + plot_spacer() + plot_spacer()) + plot_layout(nrow = 1)
f1 <- f1 + plot_annotation(tag_levels = 'A')

# Deprecated 
# f1 <- gridExtra::grid.arrange(f1a, f1b, ncol = 2)
# f1 <- as_ggplot(f1) +                                # transform to a ggplot
#   draw_plot_label(label = c("A", "B"), size = 15,
#                   x = c(0, 0.5), y = c(1, 1)) # Add labels

ggsave(f1, 
       filename = file.path(path, "f1.png"), 
       bg = "transparent",
       height = 5,
       width = 8.5,
       units = "in")

# Journal requires EPS so save in such format and refer to https://stackoverflow.com/questions/5142842/export-a-graph-to-eps-file-with-r
ggsave(f1, 
       filename = file.path(path, "eps/", "f1.eps"), 
       bg = "transparent",
       device = "eps")

# Fig 2 -------------------------------------------------------------------

f2a_anova <- aov(sv_pfam_rec ~ irs20_degree, data = z %>% filter(!is.na(irs20_degree)))
f2a_tukey <- TukeyHSD(f2a_anova)
f2a_letters <- multcompLetters4(f2a_anova, f2a_tukey)
f2a_labels <- z %>% 
  st_drop_geometry() %>% 
  filter(!is.na(irs20_degree)) %>% 
  select(sv_pfam_rec, irs20_degree) %>% 
  group_by(irs20_degree) %>% 
  summarise(
    median = median(sv_pfam_rec, na.rm = TRUE),
    q = quantile(sv_pfam_rec, probs = 0.75, na.rm = TRUE)
  ) %>% 
  arrange(desc(median)) %>% 
  bind_cols(labels = f2a_letters$irs20_degree$Letters)
  

f2a <- ggplot(data = z %>% filter(!is.na(irs20_degree)),
              mapping = aes(x = irs20_degree, y = sv_pfam_rec, fill = irs20_degree )) +
  geom_boxplot(width = 0.5) +
  labs(x = "Municipal degree of poverty",
       y = "% of households recuited") +
  scale_y_continuous(labels = scales::label_percent()) +
  stat_compare_means(method = "anova", label.y = .5) + # Add global annova p-value
  geom_text(
    data = f2a_labels, 
    aes(x = irs20_degree, y = q, label = labels),
    size = 5, 
    vjust = -1,
    hjust = -1
    ) # followed tutorial https://statdoe.com/one-way-anova-and-box-plot-in-r/
  
f2a <- afplot(f2a)
f2a

s3 <- svn %>% 
  group_by(CVEGEO, irs20_degree) %>% 
  summarise(
    norm_apoyo_total = norm_apoyo_total/exch_rate_usd_mxn,
    avg_payment = mean(norm_apoyo_total),
    median_payment = median(norm_apoyo_total)
  ) %>% 
  mutate(
    irs20_degree = factor(irs20_degree,
                          levels = (c("Very low", "Low", "Medium", "High", "Very high")),
                          labels = c("Very low", "Low", "Medium", "High", "Very high"))
  )

f2b_anova <- aov(avg_payment ~ irs20_degree, data = s3 %>% filter(!is.na(irs20_degree)))
f2b_tukey <- TukeyHSD(f2b_anova)
f2b_letters <- multcompLetters4(f2b_anova, f2b_tukey)
f2b_labels <- s3 %>% 
  st_drop_geometry() %>% 
  filter(!is.na(irs20_degree)) %>% 
  group_by(irs20_degree) %>% 
  summarise(
    median = median(avg_payment, na.rm = TRUE),
    q = quantile(avg_payment, probs = 0.75, na.rm = TRUE)
  ) %>% 
  arrange(desc(median)) %>% 
  bind_cols(labels = f2b_letters$irs20_degree$Letters)

f2b <- ggplot(data = s3, mapping = aes(x = irs20_degree, y = avg_payment, fill = irs20_degree)) +
  geom_boxplot(width = 0.5) +
  labs(x = "Municipal degree of poverty",
       y = "Average of cummulative amount transferred\nper beneficiary USD\nby December 2022") +
  scale_y_continuous(labels = scales::label_dollar()) +
  stat_compare_means(method = "anova", label.y = 10000) +        # Add global annova p-value
  geom_text(
    data = f2b_labels, 
    aes(x = irs20_degree, y = q, label = labels),
    size = 5, 
    vjust = -1,
    hjust = -1
  )

f2b <- afplot(f2b)
f2b

s4 <- svn %>% 
  group_by(CVEGEO) %>% 
  summarise(
    avg_months_in_program_per_bene = mean(meses_duracion_participacion_fechas_pago, na.rm = TRUE), # The average duration of program in that municipality
    total_SV_money_mun_USD = sum(norm_apoyo_total) / (exch_rate_usd_mxn * 1000000), # This $ figure is in millions of dollars
    total_SV_money_mun_USD_annualized = total_SV_money_mun_USD / (avg_months_in_program_per_bene / 12), # millions of dollars per year invested by SV in municipality
  ) %>% 
  ungroup() %>% 
  left_join(z %>% st_drop_geometry() %>% select(CVEGEO, total_income_MEX, irs20_degree), by = "CVEGEO") %>% 
  mutate(
    total_income_mun_USD = total_income_MEX / exch_rate_usd_mxn, # Note that total_income_MEX is already in millions
    share_mun_income_from_SV = total_SV_money_mun_USD_annualized / total_income_mun_USD 
  ) %>% 
  drop_na() %>% 
  filter(avg_months_in_program_per_bene > 0)

f2c_anova <- aov(share_mun_income_from_SV ~ irs20_degree, data = s4)
f2c_tukey <- TukeyHSD(f2c_anova)
f2c_letters <- multcompLetters4(f2c_anova, f2c_tukey)
f2c_labels <- s4 %>% 
  group_by(irs20_degree) %>% 
  summarise(
    median = median(share_mun_income_from_SV, na.rm = TRUE),
    q = quantile(share_mun_income_from_SV, probs = 0.75, na.rm = TRUE)
  ) %>% 
  arrange(desc(median)) %>% 
  bind_cols(labels = f2c_letters$irs20_degree$Letters)

f2c <- ggplot(data = s4, mapping = aes(x = irs20_degree, y = share_mun_income_from_SV, fill = irs20_degree)) +
  geom_boxplot(width = 0.5) +
  labs(x = "Municipal degree of poverty",
       y = "Value of SV payments in a year\nrelative to municipality income in 2018") +
  scale_y_continuous(labels = scales::label_percent()) +
  stat_compare_means(method = "anova", label.y = 4, label.x = "Medium") +
  geom_text(
    data = f2c_labels, 
    aes(x = irs20_degree, y = q, label = labels),
    size = 5, 
    vjust = -1,
    hjust = -1
  ) +
  coord_cartesian(ylim = c(0, 6))

f2c <- afplot(f2c)
f2c

f2 <- gridExtra::grid.arrange(f2a, f2b, ncol = 2)
f2 <- as_ggplot(f2) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1)) # Add labels
ggsave(f2, 
       filename = file.path(path, "f2.png"), 
       bg = "transparent",
       height = 7,
       width = 10,
       units = "in")

f2alt <- gridExtra::grid.arrange(f2a, f2c, ncol = 2)
f2alt <- as_ggplot(f2alt) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1)) # Add labels
ggsave(f2alt, 
       filename = file.path(path, "f2alt.png"), 
       bg = "transparent",
       height = 7,
       width = 10,
       units = "in")

# Fig 3 -------------------------------------------------------------------

excluded_tag <- "Outside study area"

produce_maps_f3 <- function(leg_pos_vec = c(0.7,0.5), font_size_vec = c(1, .7), compass_h = .3, letter_position = c(0.6, 0.725)){
f3a <<- tm_shape(state_borders) +
  tm_borders(
    lwd = 1,
    alpha = 0
  ) +
  tm_shape(z %>% mutate(sv_final_num_beneficiarios = pmin(sv_final_num_beneficiarios, 6000))) +
  tm_fill(col = "irs20_degree", 
              style = "jenks", 
              palette= "-RdYlBu",
              colorNA = NULL,
              title = "Social Lag Index\nby municipality\n in 2020"
  ) +
  tm_layout(
    title = "C",
    title.position = letter_position,
    title.size = 2,
    legend.title.size = font_size_vec[1],
    legend.text.size = font_size_vec[2],
    legend.width = .7,
    legend.outside = FALSE,
    legend.position = leg_pos_vec,
    frame = FALSE
  ) + 
  tm_shape(noz) +
  tm_fill(col = "gray90") +
  tm_add_legend(
    type = "fill",
    labels = excluded_tag,
    col = "gray90",
    border.col = "gray90") +
  tm_shape(state_borders) +
  tm_borders(
    lwd = 1
  ) 

f3b <<- tm_shape(state_borders) +
  tm_borders(
    alpha = 0,
    lwd = 1
  ) +
  tm_shape(z %>% mutate(sv_final_num_beneficiarios = pmin(sv_final_num_beneficiarios, 6000))) +
  tm_fill(col = "sv_final_num_beneficiarios", 
          style = "fixed", breaks = c(1,250,700, 1400, 3000, round(max(z$sv_final_num_beneficiarios_0))), 
          palette= "Purples",
          colorNA = NULL,
          title = "Number of active\nSV beneficiaries\nby December 2022"
  ) +
  tm_layout(
    title = "A",
    title.position = letter_position,
    title.size = 2,
    legend.title.size = font_size_vec[1],
    legend.text.size = font_size_vec[2],
    legend.width = .7,
    legend.outside = FALSE,
    legend.position = leg_pos_vec,
    frame = FALSE
  ) + 
  tm_shape(noz) +
  tm_fill(col = "gray90") +
  tm_add_legend(
    type = "fill",
    labels = excluded_tag,
    col = "gray90",
    border.col = "gray90") +
  tm_shape(state_borders) +
  tm_borders(
    lwd = 1
  )  +
  tm_compass(position = c(.15, compass_h + 0.05)) +
  tm_scale_bar(position = c(.1, compass_h))


f3c <<- tm_shape(state_borders) +
  tm_borders(
    lwd = 1,
    alpha = 0
  ) +
  tm_shape(z %>% mutate(sv_pfam_rec = sv_pfam_rec * 100)) +
  tm_fill(col = "sv_pfam_rec", 
          style = "fixed", breaks = c(0.001,1,5,15, 30, 50, 83), 
          palette= "Greens",
          colorNA = NULL,
          title = "% of families enrolled\nin SV by December 2022",
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))
  ) +
  tm_layout(
    title = "B",
    title.position = letter_position,
    title.size = 2,
    legend.title.size = font_size_vec[1],
    legend.text.size = font_size_vec[2],
    legend.width = .7,
    legend.outside = FALSE,
    legend.position = leg_pos_vec,
    frame = FALSE
  ) + 
  tm_shape(noz) +
  tm_fill(col = "gray90") +
  tm_add_legend(
    type = "fill",
    labels = excluded_tag,
    col = "gray90",
    border.col = "gray90") +
  tm_shape(state_borders) +
  tm_borders(
    lwd = 1
  ) 

f3d <<- tm_shape(state_borders) +
  tm_borders(
    lwd = 1,
    alpha = 0
  ) +
  tm_shape(z %>% mutate(sv_viable_area_trans_perc = sv_area_trans_abs/sv_viable_ha * 100)) +
  tm_fill(col = "sv_viable_area_trans_perc", 
          style = "fixed", breaks = c(2, 5, 10, 30, 50, 100, 26432),  
          palette= "Oranges",
          colorNA = NULL,
          title = "% of municipal area\nsuceptible of being restored\nin 2018 undergoing\nSV management\nby December 2022",
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f"), "%"))
  ) +
  tm_layout(
    title = "D",
    title.position = letter_position,
    title.size = 2,
    legend.title.size = font_size_vec[1],
    legend.text.size = font_size_vec[2],
    legend.width = .7,
    legend.outside = FALSE,
    legend.position = leg_pos_vec,
    frame = FALSE
  ) + 
  tm_shape(noz) +
  tm_fill(col = "gray90") +
  tm_add_legend(
    type = "fill",
    labels = excluded_tag,
    col = "gray90",
    border.col = "gray90") +
  tm_shape(state_borders) +
  tm_borders(
    lwd = 1
  ) 
}

#HERE IS BETTER TO USE PATCHWORK PACKAGE

produce_maps_f3(font_size_vec = c(0.8, 0.7))

f3 <- tmap_arrange(f3b, f3c, f3a, nrow = 1)

tmap_save(tm = f3, 
          filename = file.path(path, "f3_long.png"),
          width = 12,
          height = 6,
          units = "in"
          )

produce_maps_f3(font_size_vec = c(0.8, 0.7), compass_h = 0.1)

f3_alt <- tmap_arrange(f3b, f3c, f3d, f3a, ncol = 2)

# Not saving it anymore because we are using f3
# tmap_save(tm = f3_alt, 
#           filename = file.path(path, "f3_square.png"),
#           width = 9,
#           height = 9,
#           units = "in"
# )

# na <- World %>% filter(continent == "North America" & name != "Greenland")
# mex <- World %>% filter(iso_a3 == "MEX")
# nam <- na %>% tm_shape + tm_borders() + tm_shape(mex) + tm_polygons(col = "darkgray")

# na_grob <- tmap_grob(nam)
# map_grob <- 
# tmap_grob(f3)
# 
# f3 <- ggdraw() +
#   draw_plot(map_grob) +
#   draw_plot(na_grob,
#             width = 0.1,
#             height = 0.1, 
#             x = 0.05,
#             y = 0.05
#   ) 

# print(f3, vp = grid::viewport(.1, 0.1, width = 0.1, height = 0.2))


# Fig 5 -------------------------------------------------------------------

# We need specific variables for this exercise, normally distributed (as much as possible)
tcd <- z %>% 
  select(irs_20_index, irs20_degree, rpi_prop, gfw_avg_loss_01to18, sv_final_num_beneficiarios_0, sv_area_trans_abs_0, sv_area_trans_perc, i20_rural_households, area21ha) %>%
  mutate(
    forest_loss_year_50ha = gfw_avg_loss_01to18 / 50,
    poverty_index = (irs_20_index - min(irs_20_index, na.rm = TRUE)) * (5 / (max(irs_20_index, na.rm = TRUE)- min(irs_20_index, na.rm = TRUE))), # poverty index distribution is zero centered, but it takes values in the negative numbers, so we have to substract the min to make the lowest value a zero
    poverty_d_index = as.character(irs20_degree),
    forest_loss_index = (log(forest_loss_year_50ha) + 10)/3,
    restoration_index = sqrt(rpi_prop) * (5 / sqrt(max(rpi_prop, na.rm = TRUE))), # rescale to [0,5] and apply square root transformation to widen the distribution
    households = i20_rural_households,
    sv_area_trans_perc = replace_na(sv_area_trans_perc, 0)
  ) %>%
  select((contains("index") & !contains("20")) | contains("sv") | contains("area") | households) %>% 
  drop_na()

# Have a look at the distributions
# for(i in tcd %>% select(!contains("_d_")) %>% st_drop_geometry() %>% names()){
#   print(
#   ggplot(tcd) + geom_histogram(aes(x = get(i)))
#   )
# }

## Trying to produce two types of bivariate choropleth

# One of the options is to work with three primary
# colors by generating a complicated palette using color-blending software

# In order to generate the palette, we have to take our three primary colors
# and do the mixing:
  # Red: almost white, light red, dark red
  # Green: white, etc..


# The other option is to use a value by alpha, in which we give higher opacity
# to those municipalities that are very poor, as SV intended to do

o2 <- tcd %>% 
  select(contains("sv"), forest_loss_index, restoration_index, poverty_index, poverty_d_index, households, area21ha) %>% 
  mutate(qf = ntile(forest_loss_index, 3),
         qr = ntile(restoration_index, 3),
         qp = ntile(poverty_index, 3),
         qpi = ntile(poverty_index, 2),
         code_color = str_c(qf, qr)
         ) 

# Tried but did not work
# c("#F7DE79","#D2F572","#2ADB5F",
#   "#e6a2d0","#9ec5d3","#30EAFA",
#   "#FF7B2E","#B81FE6","#2A39AD")

# c("#F0ED8C","#c2f0ce","#18F373",
#   "#eac5dd","#657E87","#0E8A41",
#   "#E628B8","#8B65AD","#000000")

# Some carpentry
# Now we make a table with the replacement scheme
bvColors <- c("#FAF4B3","#c2f0ce","#8ae1ae",
              "#eac5dd","#9ec5d3","#7ec5b1",
              "#e6a2d0","#bb9fce","#7a8eae")

bvColors1 <- c("#FAF4B3","#c2f0ce","#C8F031",
              "#eac5dd","#52D1D7","#18F373",
              "#E628B8","#CB51ED","#2D5AAD")
                       
bvColors2 <- c("#e8e8e8","#e4acac","#c85a5a",
               "#b0d5df","#ad9ea5","#985356",
               "#64acbe","#627f8c","#574249")
 
codes <- c(11, 21, 31, 12, 22, 32, 13, 23, 33 ) %>% as.character()

#bvColors[[1]] <- "transparent"

bvColors <- c(bvColors[c(1,4,7)], bvColors[c(2,5,8)], bvColors[c(3,6,9)])
bvColors2 <- c(bvColors2[c(1,4,7)], bvColors2[c(2,5,8)], bvColors2[c(3,6,9)])


ct <- bind_cols(code_color = codes) #, palette1 = bvColors, palette2 = bvColors2)
# at <- bind_cols(qp = 1:2, alpha_code = c(0.35, 1))

o2 <- o2 %>% left_join(ct, by = "code_color") %>% 
#  left_join(at, by = "qp") %>% 
  mutate(qp = case_when(qp == 1 ~ "a", qp == 2 ~ "b", qp == 3 ~ "c"))

o3 <- o2 %>% 
  group_by(qf, qr, qp)  %>% 
  st_drop_geometry() %>% 
  summarise(tb_breakdown = sum(sv_final_num_beneficiarios_0)) %>% 
  ungroup() %>% 
  arrange(qp)

o3n <- round((o3$tb_breakdown/sum(o3$tb_breakdown))*100, digits = 1)
o3n1 <- o3n[1:9]
o3n2 <- o3n[10:18]

o4 <- o2 %>% 
  group_by(qf, qr)  %>% 
  st_drop_geometry() %>% 
  summarise(tb_breakdown = sum(sv_final_num_beneficiarios_0)) %>% 
  ungroup()

o4n <- round((o4$tb_breakdown/sum(o4$tb_breakdown))*100, digits = 1)

## Make a bar chart for the legend displaying number of beneficiaries by
# color and pattern

# Using pattern as the alpha for poverty we map a choropleth based on 
# forest cover loss and restoration priority, and then overlay a pattern

png(filename = file.path(path, "f5_value_by_alpha_circle_pattern_V4.png"), 
    width = 12, 
    height = 10, 
    units = "in", 
    res = 300)

# This is a great version!
# f5b_alt <- ggplot() + 
#   geom_sf(data = state_borders, mapping = aes(alpha = 0), color = "grey90", fill = NA) +
#   geom_sf_pattern(data = o2 %>% slice_tail(n = 100), 
#                   mapping = aes(
#                     fill = code_color,
#                     pattern = qp
#                   ),
#                   pattern_spacing = .01,
#                   pattern_fill = "grey80",
#                   pattern_colour = "grey80",
#                   colour = NA
#   ) + 
#   scale_pattern_manual(values = c(a = "crosshatch", b="circle", c="none")) +
#   scale_fill_manual(values = bvColors) + 
#   scale_pattern_colour_manual(values = bvColors) +
#   theme_void() +
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "white"), 
#         panel.grid.major = element_line(colour = "transparent"))


f5b <- ggplot() + 
  geom_sf(data = state_borders, color = "grey20", fill = "grey95") + 
  geom_sf_pattern(data = o2, 
                  mapping = aes(
                    fill = code_color,
                    pattern_density = qp
                  ),
                  pattern = "pch",
                  pattern_spacing = .01,
                  colour = NA
  ) + 
  scale_fill_manual(values = bvColors) + 
  scale_pattern_density_manual(values = c(0.5, 0.15, 0)) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "transparent")) +
  annotation_scale(
    location = "bl",
    bar_cols = c("grey20", "white")) +
  annotation_north_arrow(
    location = "bl",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_col = "grey20"))

f5b

myPanel <- function(x, y, z, ..., subscripts=subscripts) {
  panel.levelplot(x=x, y=y, z=z, ..., subscripts=subscripts)        
  panel.text(x = x[subscripts], 
             y = y[subscripts],
             cex = 1,
             labels = o4n)
}

# myPanel2 <- function(x, y, z, ..., subscripts=subscripts) {
#   panel.levelplot(x=x, y=y, z=z, ..., subscripts=subscripts)        
#   panel.text(x = x[subscripts], 
#              y = y[subscripts], 
#              cex = 0.8,
#              labels = o4n)
# }

# library(grid)
# library(lattice)
vp <- grid::viewport(x=.85, y=.7, width=.25, height=.25)
pushViewport(vp)
print(lattice::levelplot(
  matrix(1:9, nrow=3), 
  panel = myPanel,
  axes=TRUE, 
  col.regions=bvColors,
  xlab=list("", cex=0.8), 
  ylab=list("",cex=0.8), 
  cuts=8, 
  colorkey=FALSE,
  scales=list(draw=0)),
  newpage=FALSE)
popViewport()

# vp2 <- grid::viewport(x=.50, y=.75, width=.16, height=.16)
# pushViewport(vp2)
# print(lattice::levelplot(
#   matrix(1:9, nrow=3), 
#   axes=TRUE, 
#   col.regions=bvColors,
#   alpha.regions = .25,
#   xlab=list("",cex=0.6), 
#   ylab=list("Low poverty",cex=0.6), 
#   cuts=8, 
#   colorkey=FALSE,
#   scales=list(draw=0)),
#   newpage=FALSE)
# popViewport()

# vp3 <- grid::viewport(x=.65, y=.7, width=.25, height=.25)
# pushViewport(vp3)
# print(lattice::levelplot(
#   matrix(1:9, nrow=3),
#   panel = myPanel2,
#   axes=TRUE,
#   col.regions=bvColors,
#   alpha.regions = .65,
#   xlab=list("",cex=0.6),
#   ylab=list("",cex=0.6),
#   cuts=8,
#   colorkey=FALSE,
#   scales=list(draw=0)),
#   newpage=FALSE)
# popViewport()

dev.off()


# Fig 6 -------------------------------------------------------------------

o5 <- o2 %>% st_drop_geometry() %>% 
  select(qf, qr, qp, qpi, contains("sv"), households, code_color, area21ha) %>% 
  mutate(
    sv_area_trans_abs_0 = sv_area_trans_abs_0/1000,
    # This is not a smart way of obtaining a mean
    # households_index = case_when(
    #   households_index == 0 ~ mh,
    #   TRUE ~ households_index
    # ),
    # pctg_bene = tbene_index/households_index,
    qf = case_when(qf == 1 ~ "a", qf == 2 ~ "b", qf == 3 ~ "c"),
    qr = case_when(qr == 1 ~ "a", qr == 2 ~ "b", qr == 3 ~ "c"),
    qf = factor(qf, levels = c("c", "b", "a")),
    area21ha = area21ha/1e5
  ) %>% 
  group_by(qf, qr, qp) %>% 
  mutate(avg_hlds_group = mean(households),
         # households = case_when(households == 0 ~ avg_hlds_group, 
         #                        TRUE ~ households),
         households = households / 1e4,
         sv_final_num_beneficiarios_0 = sv_final_num_beneficiarios_0/1e3
         )

degrees <- c( a = "low",
              b = "medium",
              c = "high")

plot_f6 <- function(df, column, labelx, labely){
  ggplot(df) + # %>% slice_sample(n = 100) makes it faster
    facet_grid(rows = vars(qf), 
               cols = vars(qr), 
               scales = "fixed",
               labeller = labeller(
                 qf = degrees,
                 qr = degrees
               )
    ) +
    geom_rect(data = df, aes(fill = code_color),
              xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf, alpha = 0.05) +
    coord_flip() +
    geom_col_pattern(
      mapping = aes(x = qp, 
                    y = .data[[column]], 
                    pattern_density = qp
      ),
      pattern_colour = "black",
      fill = "white",
      pattern_fill = NA,
      pattern_size = 1.1,  # adjust stroke/linewidth but does not look nice, requires pattern_fill = NA
      pattern = "circle",
      pattern_spacing = 0.12,
      colour = NA
    ) +
    scale_fill_manual(values = bvColors) + 
    scale_pattern_density_manual(values = c(0.5, 0.1, 0)) + 
    scale_y_continuous(labels = scales::label_number_auto()) +
    scale_x_discrete(labels = c('richer', 'middle', 'poorer')) +
    theme_minimal() +
    labs(x = labely,
         y = labelx) + # Due to coord_flip we are flipped here
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(colour = "transparent"))
}

# Summary of total area by color-pattern category
f6a <- plot_f6(df = o5, column = "area21ha", labelx = "Hectares (x 100,000)", labely = "")

# Summary of total area transformed by sv
f6b <- plot_f6(df = o5, column = "sv_final_num_beneficiarios_0", labelx = "SV beneficiaries (x 1000)\nor\n Hectares restored through SV (x 2,500)", labely = "")

# Total RURAL population: clarify that some of the municipalities that were not rural have the average of their category
f6c <- plot_f6(df = o5, column = "households", labelx = "Rural households (x 10,000)", labely = "")

# Number of beneficiaries
f6d <- plot_f6(df = o5, column = "sv_area_trans_abs_0", labelx = "Thousands of SV beneficiaries", labely = "")

f6 <- (f6a) / (f6b) / (f6c)
f6 <- f6 + plot_annotation(tag_levels = 'A')

ggsave(f6, 
       filename = file.path(path, "f6_area_pop_beneAndAreaTransfV2.png"), 
       bg = "transparent",
       height = 10,
       width = 7,
       units = "in")

# Good try, but this package cowplot is old and inconsistent
# f5 <- gridExtra::grid.arrange(f5a, f5b, ncol = 2)
# f5 <- as_ggplot(f5) +                                # transform to a ggplot
#   draw_plot_label(label = c("A", "B"), size = 15,
#                   x = c(0, 0.5), y = c(1, 1)) + 
#   draw_plot_label(label = c("Restoration potential"),
#                   size = 9,
#                   x = 0, y = 0.35,
#                   angle = 90) +
#   draw_plot_label(label = c("Forest cover loss rate"),
#                   size = 9,
#                   x = 0.75, y = 1,
#                   angle = 90)

# Fig 4 -------------------------------------------------------------------

# This code is all imported from 6.5.1 so I will just source the file and then
# format the graphs

# This script reloads z so it is better to run it last of all, still, based
# on finalDB.csv so should not be a problem

source(file = "scripts/temp/sourceRmdFile.R", )
par(ask = FALSE)
source_rmd(file_path = "paper1/scrP1/6.5.1_SV_model_summary.Rmd")

# Out of the document we get two tables 
counts <- counts %>% mutate(pos = ifelse(c > 0, "p", "n"))
zero <- zero %>% mutate(pos = ifelse(c > 0, "p", "n"))
zero[c(4,6),5] <- "ns"


f4a <- ggplot(data = counts %>% filter(c < 4), aes(n, c)) + 
  geom_linerange(aes(ymin = l, ymax = u, color = pos), size = 1) +
  geom_pointrange(aes(ymin = l, ymax = u, color = pos), size = 1, fatten = 2) +
  scale_x_discrete(
    limits = c("count_poverty_index", "count_conservation_index", "count_forest_loss_year_50ha", "count_sqrt(area_forest_50sqkm)", "count_restoration_index", NA) %>% rev(),
    breaks = c("count_poverty_index", "count_conservation_index", "count_forest_loss_year_50ha", "count_sqrt(area_forest_50sqkm)", "count_restoration_index", NA),
    labels = c("poverty [0,5]", "conservation\npriority [0,5]", "absolute forest\ncover loss [0, 156]", "absolute\nforest cover [0, 260]", "restoration\npriority [0,5]", "")) +
  scale_color_manual(values = c(n = "#d8b365", p = "#5ab4ac"), na.value = "blank") +
  scale_y_continuous(limits = c(-0.5, 0.7)) +
  coord_flip() + 
  geom_hline(yintercept = 0, color = "grey30", linetype = 2) +
  labs(title = "Household participation rates",
       x = NULL,
       y = "ZINB counts\ncoefficient value") 
f4a <- afplot(f4a) + 
  theme(plot.title = element_text(size = 15))
f4a

f4b <- ggplot(data = zero %>% filter(c < 0.8), aes(n, c)) + 
  geom_linerange(aes(ymin = l, ymax = u, color = pos), size = 1) +
  geom_pointrange(aes(ymin = l, ymax = u, color = pos), size = 1, fatten = 2) +
  scale_x_discrete(
    limits = c("zero_poverty_index", "zero_conservation_index", "zero_forest_loss_year_50ha",  "zero_sqrt(area_forest_50sqkm)",   "zero_restoration_index", "zero_forest_cover_index") %>% rev(),
    breaks = c("zero_poverty_index", "zero_conservation_index", "zero_forest_loss_year_50ha",  "zero_sqrt(area_forest_50sqkm)",  "zero_restoration_index", "zero_forest_cover_index"),
    labels = c("poverty [0,5]", "conservation\npriority [0,5]",  "absolute forest\ncover loss [0, 156] (n.s)", "absolute\nforest cover [0, 260]",  "restoration\npriority [0,5] (n.s)", "proportion of\nmunicipality\nforested [0,5]")) +
  scale_y_reverse() +
  scale_color_manual(values = c(p = "#d8b365", n = "#5ab4ac", ns = "#c7eae5")) +
  coord_flip() + 
  geom_hline(yintercept = 0, color = "grey30", linetype = 2) +
  labs(title = "Municipality participation odds",
       x = NULL,
       y = "ZINB zero-inflation\ncoefficient value") 
f4b <- afplot(f4b) + theme(plot.title = element_text(size = 15))
f4b

f4 <- gridExtra::grid.arrange(f4b, f4a, ncol = 2)
f4 <- as_ggplot(f4) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1)) # Add labels
ggsave(f4, 
       filename = file.path(path, "f4.png"), 
       bg = "transparent",
       height = 7,
       width = 10,
       units = "in")


# Fig 6 appendix -------------------------------------------------------------------

## Remember to recalculate z without filtering to study universe

forest_a_nonorm <- ggplot(data = z %>% filter(gfw_avg_loss_01to18 > 0)) +
  geom_point(mapping = aes(y = sv_final_num_beneficiarios_0, x = gfw_avg_loss_01to18, alpha = gfw_pondTreeCover, size = gfw_pondTreeCover) , color = "#278b2c") +
  guides(alpha = guide_legend(title = "Woody cover in 2001"), size = guide_legend(title = "Woody cover in 2001")) +
  # geom_point(data = z %>% arrange(desc(gfw_avg_loss_01to18)) %>% slice_head(n = 10), mapping = aes(y = sv_final_num_beneficiarios_0, x = gfw_avg_loss_01to18, size = gfw_pondTreeCover), colour = "black") +
  geom_smooth(mapping = aes(y = sv_final_num_beneficiarios_0, x = gfw_avg_loss_01to18), method = "lm", color = rlcol, linetype = "dashed" ) +
  labs(x = "Woody cover loss per year (hectares) \n in 2001-2018 by municipality, Hansen et al. (2013)",
       y = "Number of families recruited by SV",
       colour = "Degree of marginalization") +
  # ggrepel::geom_text_repel(data = z %>% arrange(desc(gfw_avg_loss_01to18)) %>% slice_head(n = 10), mapping = aes(y = sv_final_num_beneficiarios_0, x = gfw_avg_loss_01to18, label = CVEGEO, size = 10)) +
  scale_x_continuous(labels = scales::label_number()) +
  scale_y_continuous(labels = scales::label_number()) +
  ggpubr::stat_regline_equation(label.y = 12000, label.x = 5000, aes(y = sv_final_num_beneficiarios, x = gfw_avg_loss_01to18, label = ..eq.label..)) +
  ggpubr::stat_regline_equation(label.y = 11000 , label.x = 5000, aes(y = sv_final_num_beneficiarios, x = gfw_avg_loss_01to18, label = ..rr.label..)) +
  ggpubr::stat_cor(label.y = 10000 , label.x = 5000, method = "pearson", aes(y = sv_final_num_beneficiarios, x = gfw_avg_loss_01to18)) + 
  theme_light() +
  theme(legend.key.size = unit(18, "pt")) +
  scale_fill_brewer(palette = pal, direction = -1) + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)
  ) 

ggsave(filename = "paper1/ploP1/p1figs/f6_appendix.png",
       plot = forest_a_nonorm,
       width = 10,
       height = 7,
       units = "in")

# This did not work -------------------------------------------------------------------

# Prepare a dataset with the variables of interest (poverty, restoration priority and deforestation) 
# plus geometry, had to transform all of them into an index in [0,5] for them to be comparable
# and transformed restoration_index with a sqrt and forest_loss_index with a log(e)

## HERE I SHOULD BE IMPORTING DATASET FROM 6.5.1 AND WILL SORT THAT OUT LATER

# 
# # Apply Tricolore to assign each municipality an RGB point
# tc <- tricolore::Tricolore(tcd, p1 = 'poverty_index', 
#                            p2 = 'forest_loss_index', 
#                            p3 = 'restoration_index',
#                            center = NA, breaks = 4, hue = 0.5, chroma = 0.6, 
#                            lightness = 0.9, contrast = 0.5, spread = 1.1, 
#                            legend = TRUE, show_data = TRUE, show_center = FALSE, 
#                            label_as = "pct", crop = FALSE) # call DemoTricolore to adjust
# 
# tcd <- tcd %>% add_column(rgb = tc$rgb, .after = 3)
# 
# library(ggtern)
# plot_tc <- ggplot() +
#   geom_sf(data = noz, aes(geometry = geometry)) +
#   geom_sf(data = tcd, aes(fill = rgb, geometry = geometry), size = 0.1, color = NA) + 
#   scale_fill_identity() +
#   annotation_custom(
#     ggplotGrob(tc$key),
#     xmax = 93e5, xmin = 97e5, ymin = 40e5
#   )
#   
#   
# plot_tc
