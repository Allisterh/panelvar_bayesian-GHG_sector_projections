##### 
# File for piecing together data from various sources for estimation in panel VAR
# data structure due to coding of PVAR

library(tidyverse)

load("./01_data/processed/ghg_data.Rda")
load("./01_data/processed/macro_data.Rda")
load("./01_data/processed/additional_vars.Rda")

# re-shape additional variables before piecing them together
age_structure <- pop_structure %>% 
  reshape2::melt(id = "year") %>% 
  mutate(iso3c = str_split(variable, "\\.", simplify = TRUE)[,1], 
         var = str_split(variable, "\\.", simplify = TRUE)[,2]) %>% 
  select(-variable) %>% 
  pivot_wider(names_from = var, values_from = value)

educ_structure <- edu_structure %>% 
  reshape2::melt(id = "year") %>% 
  mutate(iso3c = str_split(variable, "\\.", simplify = TRUE)[,1], 
         var = str_split(variable, "\\.", simplify = TRUE)[,2]) %>% 
  select(-variable) %>% pivot_wider(names_from = var, values_from = value)

urbanization <- SSP_pop_urb %>% 
  reshape2::melt(id = "year") %>%
  mutate(iso3c = str_split(variable, "\\.", simplify = TRUE)[,1], 
         var = str_split(variable, "\\.", simplify = TRUE)[,2]) %>% 
  select(-variable) %>% pivot_wider(names_from = var, values_from = value)
urbanization$year <- as.numeric(as.character(urbanization$year))


#####
# Creating dataframes for estimation with PVAR

###
# Transform Minx et al (2021) emissions data to COeq

df_CO2_ESSD <- essd_ghg %>% 
  filter(year <= 2018) %>%
  mutate(CO2_eq = value * gwp100_ar5) %>% 
  group_by(iso3c, year, sector_title) %>% 
  summarise(CO2 = sum(CO2_eq, na.rm = TRUE))


###
# Total emissions based on Minx et al (2021)

df_CO2_ESSD_total <- df_CO2_ESSD %>% 
  group_by(iso3c, year) %>% 
  summarise(CO2 = sum(CO2, na.rm = TRUE) / 10^3)

# Serbia & Montenegro are reported as one entity, split up using WRI values
shares_missings <- cait_ghg %>% 
  select(iso3c, year, total) %>%
  pivot_wider(names_from = iso3c, values_from = total) %>% 
  mutate(total = MNE + SRB, 
         SRB.share = SRB / total, 
         MNE.share = MNE / total) %>% 
  select(year, SRB.share, MNE.share) %>% 
  left_join(df_CO2_ESSD_total %>% filter(iso3c == "SCG")) %>% 
  mutate(SRB = SRB.share * CO2, 
         MNE = MNE.share * CO2) %>%  
  select(year, SRB, MNE) %>%
  pivot_longer(cols = c("SRB", "MNE")) %>% 
  rename(iso3c = name, CO2 = value)
df_CO2_ESSD_total <- rbind(df_CO2_ESSD_total %>% 
                             filter(!iso3c %in% c("SCG")), 
                           shares_missings)

df_CO2_ESSD_total <- df_CO2_ESSD_total %>% 
  left_join(macro_data, by = c("iso3c", "year")) %>% 
  filter(year > 1979, !iso3c %in% c("PRK", "FSM")) %>% 
  rename(GDPPPP = GDP.PPP, GDPpcPPP = GDP.PC.PPP, POP = pop) %>% 
  mutate(GDPPPP = GDPPPP / 10^9, POP = POP / 10^6, 
         CO2GDPPPP = CO2 / (GDPPPP * 10^2)) %>% 
  left_join(age_structure[,c("year", "iso3c", "old", "old_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(educ_structure[,c("year", "iso3c", "sec_educ", "sec_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(urbanization, by = c("iso3c", "year")) %>% 
  drop_na(CO2, GDPPPP, GDPpcPPP, POP) %>% 
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(year, iso_var, value) %>% arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value) %>% 
  arrange(year)


###
# Transport emissions based on Minx et al (2021)

df_CO2_ESSD_transport <- df_CO2_ESSD %>% 
  filter(sector_title == "Transport") %>% 
  select(-sector_title) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  mutate(CO2 = CO2 / 10^3)

# Serbia & Montenegro are reported as one entity, split up using WRI values
shares_missings <- cait_ghg %>% 
  select(iso3c, year, transport) %>%
  pivot_wider(names_from = iso3c, values_from = transport) %>% 
  mutate(transport = rowSums(cbind(SRB, MNE), na.rm = TRUE), 
         SRB.share = SRB / transport, 
         MNE.share = MNE / transport, 
         MNE.share = replace(MNE.share, is.na(MNE.share), 
                             mean(MNE.share, na.rm = TRUE)), 
         SRB.share = 1 - MNE.share) %>% 
  select(year, SRB.share, MNE.share) %>% 
  left_join(df_CO2_ESSD_transport %>% filter(iso3c == "SCG")) %>% 
  mutate(SRB = SRB.share * CO2, 
         MNE = MNE.share * CO2) %>%  
  select(year, SRB, MNE) %>%
  pivot_longer(cols = c("SRB", "MNE")) %>% 
  rename(iso3c = name, CO2 = value)
df_CO2_ESSD_transport <- rbind(df_CO2_ESSD_transport %>% 
                             filter(!iso3c %in% c("SCG")), 
                           shares_missings)

df_CO2_ESSD_transport <- df_CO2_ESSD_transport %>% 
  left_join(macro_data, by = c("iso3c", "year")) %>% 
  filter(year > 1979, !iso3c %in% c("PRK", "FSM")) %>% 
  rename(GDPPPP = GDP.PPP, GDPpcPPP = GDP.PC.PPP, POP = pop) %>% 
  mutate(GDPPPP = GDPPPP / 10^9, POP = POP / 10^6,
         CO2GDPPPP = CO2 / (GDPPPP * 10^2)) %>% 
  left_join(age_structure[,c("year", "iso3c", "old", "old_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(educ_structure[,c("year", "iso3c", "sec_educ", "sec_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(urbanization, by = c("iso3c", "year")) %>% 
  drop_na(CO2, GDPPPP, GDPpcPPP, POP) %>%
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(year, iso_var, value) %>% arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value) %>% 
  arrange(year)


###
# Industry emissions based on Minx et al (2021)

df_CO2_ESSD_industry <- df_CO2_ESSD %>% 
  filter(sector_title == "Industry") %>% 
  select(-sector_title) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  mutate(CO2 = CO2 / 10^3)

# Serbia & Montenegro are reported as one entity, split up using WRI values
shares_missings <- cait_ghg %>% 
  select(iso3c, year, industry) %>%
  pivot_wider(names_from = iso3c, values_from = industry) %>% 
  mutate(industry = MNE + SRB, 
         SRB.share = SRB / industry, 
         MNE.share = MNE / industry) %>% 
  select(year, SRB.share, MNE.share) %>% 
  left_join(df_CO2_ESSD_industry %>% filter(iso3c == "SCG")) %>% 
  mutate(SRB = SRB.share * CO2, 
         MNE = MNE.share * CO2) %>%  
  select(year, SRB, MNE) %>%
  pivot_longer(cols = c("SRB", "MNE")) %>% 
  rename(iso3c = name, CO2 = value)
df_CO2_ESSD_industry <- rbind(df_CO2_ESSD_industry %>% 
                             filter(!iso3c %in% c("SCG")), 
                           shares_missings)

df_CO2_ESSD_industry <- df_CO2_ESSD_industry %>% 
  left_join(macro_data, by = c("iso3c", "year")) %>% 
  filter(year > 1979, !iso3c %in% c("PRK", "FSM")) %>% 
  rename(GDPPPP = GDP.PPP, GDPpcPPP = GDP.PC.PPP, POP = pop) %>% 
  mutate(GDPPPP = GDPPPP / 10^9, POP = POP / 10^6,
         CO2GDPPPP = CO2 / (GDPPPP * 10^2)) %>% 
  left_join(age_structure[,c("year", "iso3c", "old", "old_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(educ_structure[,c("year", "iso3c", "sec_educ", "sec_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(urbanization, by = c("iso3c", "year")) %>% 
  drop_na(CO2, GDPPPP, GDPpcPPP, POP) %>%
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(year, iso_var, value) %>% 
  arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value) %>% 
  arrange(year)


###
# Buildings emissions based on Minx et al (2021)

df_CO2_ESSD_buildings <- df_CO2_ESSD %>% 
  filter(sector_title == "Buildings") %>% 
  select(-sector_title) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  mutate(CO2 = CO2 / 10^3)

# Serbia & Montenegro are reported as one entity, split up using WRI values
shares_missings <- cait_ghg %>% 
  select(iso3c, year, buildings) %>%
  pivot_wider(names_from = iso3c, values_from = buildings) %>% 
  mutate(buildings = MNE + SRB, 
         SRB.share = SRB / buildings, 
         MNE.share = MNE / buildings) %>% 
  select(year, SRB.share, MNE.share) %>% 
  left_join(df_CO2_ESSD_buildings %>% filter(iso3c == "SCG")) %>% 
  mutate(SRB = SRB.share * CO2, 
         MNE = MNE.share * CO2) %>%  
  select(year, SRB, MNE) %>%
  pivot_longer(cols = c("SRB", "MNE")) %>% 
  rename(iso3c = name, CO2 = value)
df_CO2_ESSD_buildings <- rbind(df_CO2_ESSD_buildings %>% 
                                filter(!iso3c %in% c("SCG")), 
                               shares_missings)

df_CO2_ESSD_buildings <- df_CO2_ESSD_buildings %>% 
  left_join(macro_data, by = c("iso3c", "year")) %>% 
  filter(year > 1979, !iso3c %in% c("PRK", "FSM")) %>% 
  rename(GDPPPP = GDP.PPP, GDPpcPPP = GDP.PC.PPP, POP = pop) %>% 
  mutate(GDPPPP = GDPPPP / 10^9, POP = POP / 10^6, 
         CO2GDPPPP = CO2 / (GDPPPP * 10^2)) %>% 
  left_join(age_structure[,c("year", "iso3c", "old", "old_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(educ_structure[,c("year", "iso3c", "sec_educ", "sec_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(urbanization, by = c("iso3c", "year")) %>% 
  drop_na(CO2, GDPPPP, GDPpcPPP, POP) %>%
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(year, iso_var, value) %>% 
  arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value) %>% 
  arrange(year)


###
# Energy emissions based on Minx et al (2021)

df_CO2_ESSD_energy <- df_CO2_ESSD %>% 
  filter(sector_title == "Energy systems") %>% 
  select(-sector_title) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  mutate(CO2 = CO2 / 10^3)

# Serbia & Montenegro are reported as one entity, split up using WRI values
shares_missings <- cait_ghg %>% 
  select(iso3c, year, energy) %>%
  pivot_wider(names_from = iso3c, values_from = energy) %>% 
  mutate(energy = MNE + SRB, 
         SRB.share = SRB / energy, 
         MNE.share = MNE / energy) %>% 
  select(year, SRB.share, MNE.share) %>% 
  left_join(df_CO2_ESSD_energy %>% filter(iso3c == "SCG")) %>% 
  mutate(SRB = SRB.share * CO2, 
         MNE = MNE.share * CO2) %>%  
  select(year, SRB, MNE) %>%
  pivot_longer(cols = c("SRB", "MNE")) %>% 
  rename(iso3c = name, CO2 = value)
df_CO2_ESSD_energy <- rbind(df_CO2_ESSD_energy %>% 
                              filter(!iso3c %in% c("SCG")), 
                            shares_missings)

df_CO2_ESSD_energy <- df_CO2_ESSD_energy %>% 
  left_join(macro_data, by = c("iso3c", "year")) %>% 
  filter(year > 1979, !iso3c %in% c("PRK", "FSM")) %>% 
  rename(GDPPPP = GDP.PPP, GDPpcPPP = GDP.PC.PPP, POP = pop) %>% 
  mutate(GDPPPP = GDPPPP / 10^9, POP = POP / 10^6, 
         CO2GDPPPP = CO2 / (GDPPPP * 10^2)) %>% 
  left_join(age_structure[,c("year", "iso3c", "old", "old_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(educ_structure[,c("year", "iso3c", "sec_educ", "sec_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(urbanization, by = c("iso3c", "year")) %>% 
  drop_na(CO2, GDPPPP, GDPpcPPP, POP) %>%
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(year, iso_var, value) %>% 
  arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value) %>% 
  arrange(year)


###
# Agriculture emissions based on Minx et al (2021)

df_CO2_ESSD_agriculture <- df_CO2_ESSD %>% 
  filter(sector_title == "AFOLU") %>% 
  select(-sector_title) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  mutate(CO2 = CO2 / 10^3)

# Serbia & Montenegro are reported as one entity, split up using WRI values
shares_missings <- cait_ghg %>% 
  select(iso3c, year, agriculture) %>%
  pivot_wider(names_from = iso3c, values_from = agriculture) %>% 
  mutate(agriculture = MNE + SRB, 
         SRB.share = SRB / agriculture, 
         MNE.share = MNE / agriculture) %>% 
  select(year, SRB.share, MNE.share) %>% 
  left_join(df_CO2_ESSD_agriculture %>% filter(iso3c == "SCG")) %>% 
  mutate(SRB = SRB.share * CO2, 
         MNE = MNE.share * CO2) %>%  
  select(year, SRB, MNE) %>%
  pivot_longer(cols = c("SRB", "MNE")) %>% 
  rename(iso3c = name, CO2 = value)
df_CO2_ESSD_agriculture <- rbind(df_CO2_ESSD_agriculture %>% 
                                   filter(!iso3c %in% c("SCG")), 
                                 shares_missings)

df_CO2_ESSD_agriculture <- df_CO2_ESSD_agriculture %>% 
  left_join(macro_data, by = c("iso3c", "year")) %>% 
  filter(year > 1979, !iso3c %in% c("PRK", "FSM")) %>% 
  rename(GDPPPP = GDP.PPP, GDPpcPPP = GDP.PC.PPP, POP = pop) %>% 
  mutate(GDPPPP = GDPPPP / 10^9, POP = POP / 10^6, 
         CO2GDPPPP = CO2 / (GDPPPP * 10^2)) %>% 
  left_join(age_structure[,c("year", "iso3c", "old", "old_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(educ_structure[,c("year", "iso3c", "sec_educ", "sec_share")], 
            by = c("iso3c", "year")) %>% 
  left_join(urbanization, by = c("iso3c", "year")) %>% 
  drop_na(CO2, GDPPPP, GDPpcPPP, POP) %>%
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(year, iso_var, value) %>% 
  arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value) %>% 
  arrange(year)


#####
# saving
save(df_CO2_ESSD_transport, df_CO2_ESSD_buildings, df_CO2_ESSD_industry, 
     df_CO2_ESSD_energy, df_CO2_ESSD_agriculture, df_CO2_ESSD_total,
     file = "./01_data/processed/GHG_est_data.Rda")



#####
# Projections of covariates besides emission intensities are SSP-consistent

macro_data_proj <- macro_data %>% 
  rename(POP = pop, GDPPPP = GDP.PPP, GDPpcPPP = GDP.PC.PPP) %>% 
  mutate(POP = POP / 10^6, GDPPPP = GDPPPP / 10^9) %>% 
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(-iso3c, -variable) %>% 
  pivot_wider(names_from = iso_var, values_from = value)

UN_pop$year <- as.numeric(as.character(UN_pop$year))

projections_data <- SSP_pop_urb %>% filter(year > 1989) %>%
  left_join(edu_structure %>% 
              select(year, contains("sec_")), 
            by = "year") %>% 
  left_join(pop_structure %>% 
              select(year, contains("old")), 
            by = "year") %>% 
  left_join(macro_data_proj, by = "year")

save(projections_data, 
     file = "./01_data/processed/projections_data.Rda")

