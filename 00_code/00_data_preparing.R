#####
# File for data preparation needed for various parts in the WEC
library(tidyverse)
library(wcde)
library(countrycode)
library(dplyr)

#####
# Load macro data
# Contains data of GDP and population series and population

# Historical GDP figures are taken from World Bank WDI database, with some
# interpolations/adjustments where necessary. Projections of GDP are a mixture
# of short-term forecasts from the IMF's World Economic Outlook and reconciled
# long-term projections consistent with SSP2 projections taken from Crespo 
# Cuaresma (2017) [https://doi.org/10.1016/j.gloenvcha.2015.02.012]

# Population figures are taken from World Bank, United Nations Population Division, 
# with SSP2 projections taken from the Wittgenstein Centre for Demography and Global 
# Human Capital (based on KC & Lutz, 2017) [https://doi.org/10.1016/j.gloenvcha.2014.06.004]
load("./01_data/macro_data.Rda")
names(macro_data)[1] <- "iso3c"
save(macro_data, file = "./01_data/processed/macro_data.Rda")

#####
# GHG data

# Sectoral emissions from Minx et al (2021) [https://doi.org/10.5194/essd-13-5213-2021]

essd_ghg <- readxl::read_excel("./01_data/essd_ghg_data.xlsx", sheet = "data")
essd_ghg <- essd_ghg[, -grep("region|country", names(essd_ghg))]
names(essd_ghg)[1] <- "iso3c"
essd_ghg <- essd_ghg %>% # remove international aviation and shipping
  filter(!iso3c %in% c("AIR", "SEA"))


# Emissions from WRI [https://www.climatewatchdata.org/], for breaking up Serbia and Montenegro

cait_ghg <- read.csv("./01_data/CW_HistoricalEmissions_CAIT.csv")
names(cait_ghg)[1:4] <- c("iso3c", "source", "sector", "gas")
names(cait_ghg)[5:ncol(cait_ghg)] <- 1990:2018
cait_ghg$source <- NULL

cait_ghg <- cait_ghg %>% 
  filter(gas == "All GHG", iso3c %in% c("MNE", "SRB")) %>% 
  select(-gas) %>% 
  reshape2::melt(id = c("iso3c", "sector")) %>% 
  mutate(variable = as.numeric(as.character(variable)))%>% 
  pivot_wider(names_from = sector, values_from = value) %>% 
  mutate(transport = `Transportation `, 
         industry = rowSums(cbind(`Industrial Processes `, 
                                  `Manufacturing/Construction `), na.rm = TRUE),
         energy = rowSums(cbind(`Electricity/Heat `, 
                                `Fugitive Emissions `, 
                                `Other Fuel Combustion `), na.rm = TRUE), 
         buildings = rowSums(cbind(Building,
                                   `Waste `), na.rm = TRUE),
         agriculture = `Agriculture `, 
         total = rowSums(cbind(transport, energy, industry, 
                               buildings, agriculture), na.rm = TRUE)) %>% 
  rename(year = variable) %>% 
  select(iso3c, year, transport, industry, energy, buildings, agriculture, total) %>% 
  arrange(iso3c, year)
cait_ghg[cait_ghg==0] <- NA


####
# Save GHG data

save(essd_ghg, cait_ghg, file = "./01_data/processed/ghg_data.Rda")


#####
# Data for age structure and education structure taken from Wittgenstein Centre 
# for Demography and Global Human Capital via API

# Age structure
pop_structure <- get_wcde("bpop", scenario = 2)
unique(pop_structure$age)
pop_structure <- pop_structure %>% 
  filter(sex == "Both", year > 1965, year < 2055) %>% 
  select(country_code, year, age, bpop)

# recode brackets to distinguish between old (40 and above) and young (below 40)
pop_structure <- pop_structure %>% filter(age %in% c("All", "0--19", "20--39", "40--64", "65+"))
pop_structure <- pop_structure %>%
  mutate(age = replace(age, age == "0--19", "young"),
         age = replace(age, age == "20--39", "young"),
         age = replace(age, age == "40--64", "old"),
         age = replace(age, age == "65+", "old")) %>% 
  rename(ccode = country_code) %>% group_by(ccode, year, age) %>% 
  summarise(bpop = sum(bpop, na.rm = TRUE)) %>% ungroup()

# recode country codes
pop_structure$ccode <- countrycode(pop_structure$ccode, 
                                   "iso3n", 
                                   "iso3c", 
                                   custom_match = c("530"="ANT", "736"="SDN", "830"="CHA"))
names(pop_structure)[1] <- "iso3c"

# interpolate for years in between 5 year intervals
grid_pop <- expand.grid(unique(na.omit(pop_structure$iso3c)), 1970:2050) %>% 
  rename(iso3c = Var1, year = Var2)
pop_structure <- pop_structure %>% 
  filter(!is.na(iso3c)) %>% 
  pivot_wider(names_from = age, values_from = bpop) %>% 
  arrange(iso3c, year) %>% 
  right_join(grid_pop) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  mutate(All = zoo::na.approx(All, na.rm = FALSE), 
         old = zoo::na.approx(old, na.rm = FALSE), 
         young = zoo::na.approx(young, na.rm = FALSE), 
         old_share = old / All, 
         young_share = young / All)

# bring into format for PVAR
pop_structure <- pop_structure %>% 
  reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(iso_var, year, value) %>% 
  arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value) %>% 
  select(-contains("All"))


# Educational structure
edu_structure <- get_wcde("bprop", scenario = 2)
edu_structure <- edu_structure %>% 
  filter(sex == "Both", age == "25+", year > 1965, year < 2055) %>% 
  select(country_code, year, education, bprop)

# structure of education levels
# exclude "Total", "Under 15", "Short Post Secondary", "Bachelor", "Master and higher"
# no education: "No Education", "Incomplete Primary"
# primary education: "Primary", "Lower Secondary"
# secondary education: "Upper Secondary"
# tertiary education: "Post Secondary"

excl <- c("Total", "Under 15", "Short Post Secondary", "Bachelor", "Master and higher")
edu_structure <- edu_structure %>% filter(!education %in% excl) %>% 
  mutate(education = replace(education, education == "No Education", "no_educ"),
         education = replace(education, education == "Incomplete Primary", "no_educ"),
         education = replace(education, education == "Primary", "pri_educ"),
         education = replace(education, education == "Lower Secondary", "pri_educ"),
         education = replace(education, education == "Upper Secondary", "sec_educ"),
         education = replace(education, education == "Post Secondary", "ter_educ")) %>% 
  rename(ccode = country_code) %>% group_by(ccode, year, education) %>% 
  summarise(bprop = sum(bprop, na.rm = TRUE))

# recode country codes
edu_structure$ccode <- countrycode(edu_structure$ccode, 
                                   "iso3n", 
                                   "iso3c", 
                                   custom_match = c("530"="ANT", "736"="SDN", "830"="CHA"))
names(edu_structure)[1] <- "iso3c"

# interpolate for years in between 5 year intervals
grid_edu <- expand.grid(unique(edu_structure$iso3c), 1970:2050) %>% 
  rename(iso3c = Var1, year = Var2)
edu_structure <- edu_structure %>% 
  filter(!is.na(iso3c)) %>% 
  pivot_wider(names_from = education, values_from = bprop) %>% 
  right_join(grid_edu) %>% 
  arrange(iso3c, year) %>% 
  filter(!is.na(iso3c)) %>%  
  group_by(iso3c) %>% 
  mutate(no_educ = zoo::na.approx(no_educ, na.rm = FALSE),
         pri_educ = zoo::na.approx(pri_educ, na.rm = FALSE),
         sec_educ = zoo::na.approx(sec_educ, na.rm = FALSE),
         ter_educ = zoo::na.approx(ter_educ, na.rm = FALSE), 
         All = rowSums(cbind(no_educ, pri_educ,
                             sec_educ, ter_educ), na.rm = TRUE), 
         no_share = no_educ / All, 
         pri_share = pri_educ / All, 
         sec_share = sec_educ / All, 
         ter_share = ter_educ/ All) %>% 
  select(-All)

# bring into format for PVAR
edu_structure <- edu_structure %>% reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(iso_var, year, value) %>% arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value)


##### 
# Urbanization data from United Nations 2018 Revision of World Urbanization Prospects
# taken where data from SSP projections is missing

# total population
UN_pop_tot <- readxl::read_xls("./01_data/WUP2018-F18-Total_Population_Annual.xls", 
                               skip = 16)
UN_pop_tot <- UN_pop_tot %>% 
  select(-Index, -`Region, subregion, country or area`, -Note) %>% 
  rename(iso3c = `Country\ncode`)

# recode country codes and bring in shape
UN_pop_tot$iso3c <- countrycode(UN_pop_tot$iso3c, "iso3n", "iso3c", 
              custom_match = c("530"="ANT", "736"="SDN", "830"="CHA", "412"="XKX"))
UN_pop_tot <- UN_pop_tot %>% filter(!is.na(iso3c)) %>% 
  reshape2::melt(id = "iso3c")
names(UN_pop_tot) <- c("iso3c", "year", "pop_tot")

# urban population
UN_pop_urb <- readxl::read_xls("./01_data/WUP2018-F19-Urban_Population_Annual.xls", 
                               skip = 16)
UN_pop_urb <- UN_pop_urb %>% 
  select(-Index, -`Region, subregion, country or area`, -Note) %>% 
  rename(iso3c = `Country\ncode`)

# recode country codes and bring in shape
UN_pop_urb$iso3c <- countrycode(UN_pop_urb$iso3c, "iso3n", "iso3c", 
                                custom_match = c("530"="ANT", "736"="SDN", "830"="CHA", "412"="XKX"))
UN_pop_urb <- UN_pop_urb %>% filter(!is.na(iso3c)) %>% 
  reshape2::melt(id = "iso3c")
names(UN_pop_urb) <- c("iso3c", "year", "pop_urb")

# get urban share
UN_pop <- left_join(UN_pop_tot, UN_pop_urb, by = c("iso3c", "year"))
UN_pop$pop_urb_share <- UN_pop$pop_urb / UN_pop$pop_tot
UN_pop$pop_tot <- NULL

# bring in shape for PVAR estimation
UN_pop <- UN_pop %>% reshape2::melt(id = c("iso3c", "year")) %>% 
  mutate(iso_var = paste0(iso3c, ".", variable)) %>% 
  select(iso_var, year, value) %>% arrange(iso_var, year) %>% 
  pivot_wider(names_from = iso_var, values_from = value)


##### 
# Updated, SSP-consistent urbanization data taken from Chen et al 2022
# [https://doi.org/10.1038/s41597-022-01209-5]

# data once for "regular" countries and once where urbanization is 100%
SSP_pop_urb <- readxl::read_xls("./01_data/urbanization_chen_2022/WB/SSP2_WB.xls")
SSP_pop_urb_100 <- readxl::read_xlsx("./01_data/urbanization_chen_2022/WB/urban_100_WB.xlsx")

# piece together and bring in shape
SSP_pop_urb <- SSP_pop_urb %>% 
  rbind(SSP_pop_urb_100) %>% 
  rename(iso3c = `country or area`) %>% 
  pivot_longer(cols = `1960`:`2100`) %>% 
  rename(year = name, pop_urb_share = value) %>% 
  mutate(year = as.numeric(year), pop_urb_share = pop_urb_share / 100) %>% 
  filter(year > 1979, year < 2051) %>% 
  left_join(macro_data %>% select(iso3c, year, pop), by = c("iso3c", "year")) %>% 
  mutate(pop_urb = (pop / 10^3) * pop_urb_share) %>% select(-pop) %>% 
  pivot_longer(cols = pop_urb_share:pop_urb) %>% 
  mutate(iso_var = paste0(iso3c, ".", name)) %>% 
  select(-iso3c, -name) %>% 
  pivot_wider(names_from = iso_var, values_from = value)

# get countries missing in SSP projections and take UN projections instead
cN_SSP <- unique(substr(names(SSP_pop_urb), 1, 3))
cN_UN <- unique(substr(names(UN_pop), 1, 3))
cN_SSP <- cN_UN[which(!cN_UN %in% cN_SSP)]

cN_UN <- grepl(paste0(c("year", cN_SSP), collapse = "|"), names(UN_pop))
SSP_UN_add <- UN_pop[, which(cN_UN)]
SSP_pop_urb <- SSP_pop_urb %>% 
  left_join(SSP_UN_add %>% 
              mutate(year = as.numeric(as.character(year))), by = "year")


# Save additional data to be used in various parts
save(edu_structure, pop_structure, 
     SSP_pop_urb, UN_pop, 
     file = "./01_data/processed/additional_vars.Rda")
