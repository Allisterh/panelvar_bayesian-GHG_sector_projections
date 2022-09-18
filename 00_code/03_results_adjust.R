#####
# Computing sectoral emissions on regional and global level

library(dplyr)
library(ggplot2)
library(tidyverse)
library(rgdal)
library(rnaturalearth)
library(sf)

adjust <- TRUE

#### Scramble data together on emissions intensities and total emissions

model <- "NG_PVAR"
small <- c(FALSE)
trans <- c("lvl")
det <- c(1)
plag <- c(1)
em_data <- "ESSD_intensity"
split <- c("peak")
year_end <- 2018

sector_vec <- c("transport", "agriculture", "industry", "buildings", "energy")


if(split != "no") {
  split_up <- paste0(split, c("_yes", "_no"))
} else {
  split_up <- "no"
}

folder_string <- paste0("./02_output/09_results/", model, "/", em_data, "/")
cN_full <- c()
for(spl in split_up) {
  
  if(spl == "peak_yes") det_temp <- 1 else if (spl == "peak_no") det_temp <- 0
  
  spec_string_no <- paste0("/split_", spl, "/small", small, "/", trans,
                           "_plag", plag, "_det", det_temp)
  for(j in "agriculture") {
    test_load <- try(load(paste0(folder_string, j, spec_string_no, 
                                 "/fcast_tot_em_draws.Rda")), silent = TRUE)
    if(is(test_load, "try-error")) next
    temp <- names(fcast_cond_tot_em)
    cN_full <- c(cN_full, temp)
  }
}
cN_full <- unique(cN_full)
cN_full <- cN_full[order(cN_full)]

list_em_draws <- list()
list_int_draws <- list()
for(spl in split_up) {
  
  if(spl == "peak_yes") det_temp <- 1 else det_temp <- 0
  spec_string <- paste0("/split_", spl, "/small", small, "/", 
                        trans, "_plag", plag, "_det", det_temp)

  for(j in sector_vec) {
    test_load <- try(load(paste0(folder_string, j, spec_string, "/fcast_tot_em_draws.Rda")), 
                     silent = TRUE)
    if(is(test_load, "try-error")) next
    if(is.null(list_em_draws[[j]])) {
      list_em_draws[[j]] <- fcast_cond_tot_em
    } else {
      list_em_draws[[j]] <- c(list_em_draws[[j]], fcast_cond_tot_em)
    }
    
    test_load <- try(load(paste0(folder_string, j, spec_string, "/fcast_cond_int_draws.Rda")), 
                     silent = TRUE)
    if(is(test_load, "try-error")) next
    if(is.null(list_int_draws[[j]])) {
      list_int_draws[[j]] <- fcast_cond_em_int
    } else {
      list_int_draws[[j]] <- c(list_int_draws[[j]], fcast_cond_em_int)
    }
  }
}

nrow_full <- nrow(list_em_draws$agriculture$USA)
for(j in sector_vec) {
  for(cc in names(list_em_draws[[j]])) {
    if(nrow(list_em_draws[[j]][[cc]]) < nrow_full) {
      list_em_draws[[j]][[cc]] <- rbind(matrix(0, 
                                               nrow = nrow_full - nrow(list_em_draws[[j]][[cc]]),
                                               ncol = ncol(list_em_draws[[j]][[cc]])), 
                                        list_em_draws[[j]][[cc]])
    }
  }
  for(cc in names(list_int_draws[[j]])) {
    if(nrow(list_int_draws[[j]][[cc]]) < nrow_full) {
      list_int_draws[[j]][[cc]] <- rbind(matrix(0, 
                                               nrow = nrow_full - nrow(list_int_draws[[j]][[cc]]),
                                               ncol = ncol(list_int_draws[[j]][[cc]])), 
                                         list_int_draws[[j]][[cc]])
    }
  }
}


##### 
# adjust here

# creating list with 5-nearest neighbors (geographically)
country_shp <- ne_countries(scale = 50)
country_shp <- country_shp[which(country_shp$adm0_a3_is %in% cN_full), ]
country_shp[which(country_shp$adm0_a3_is %in% c("PRI", "PYF", "NCL")), "type"] <- "sov"
country_shp <- country_shp[which(!country_shp$type %in% c("Indeterminate", 
                                                          "Dependency")), ]
country_shp <- country_shp[which(!country_shp$admin %in% c("Northern Cyprus", 
                                                           "Baykonur Cosmodrome", 
                                                           "Kosovo")), ]
country_shp <- country_shp[order(country_shp$adm0_a3_is), ]
knn_list <- spdep::knearneigh(coordinates(country_shp), k = 5)$nn


if(adjust) {
  # List of countries per sector that need to be adjusted ex-posted due to 
  # unrealistic emission intensity behavior and resulting emissions
  list_cN_replace <- list("buildings" = c("ABW", "BEN", "BHS", "BIH", "BLZ", 
                                          "CAF", "CMR", "GUY", "KAZ", "KGZ", 
                                          "OMN", "SWZ", "VCT"),
                          "energy" = c("AFG", "BDI", "BEN", "BWA", "CAF", "CIV", 
                                       "CRI", "FJI", "GIN", "GUY", "HTI", "IRQ", 
                                       "LAO", "LCA", "LKA", "MDV", "MLI", "MNE", 
                                       "MOZ", "NER", "PRI", "SUR", "SWZ", "TLS", 
                                       "URY", "VUT"),
                          "transport" = c("AFG", "AGO", "ARM", "BDI",  "BEN", "CIV",
                                          "COM", "CPV", "GIN", "GUY", "LAO",
                                          "LBR", "MDV", "MKD", "MLI", "MMR",
                                          "MOZ", "NER", "NPL", "SEN", "TGO",
                                          "TJK", "TZA", "UGA", "VCT", "VUT",
                                          "YEM"),
                          "agriculture" = c("BDI", "GIN", "GUY"), 
                          "industry" = c("BHR", "GAB", "GUY", "MAC", "MDV", "NPL", 
                                         "OMN", "QAT", "SEN", "SWZ", 
                                         "RUS", "MLT"))
  total_cN_replace <- unique(unlist(list_cN_replace)) 
  for(j in sector_vec) {
    cN_replace <- list_cN_replace[[j]]
    if(!is.null(cN_replace)) {
      for(cc in cN_replace) {
        
        temp_em_draws <- list_em_draws[[j]][[cc]]
        temp_int_draws <- list_int_draws[[j]][[cc]]
        
        if(is.null(temp_em_draws)) next
        
        temp_neighs <- cN_full[knn_list[which(cN_full == cc), ]]
        if(j == "buildings") excl <- c(cN_replace, "TJK") else excl <- cN_replace
        temp_neighs <- temp_neighs[which(!temp_neighs %in% excl)]
        temp_draws_neigh <- list_int_draws[[j]][temp_neighs]
        temp_draws_neigh <- temp_draws_neigh[which(!is.na(names(temp_draws_neigh)))]
        pos_ip <- which(rownames(temp_int_draws) == (year_end + 1))
        
        temp_gdp <- temp_em_draws / temp_int_draws
        temp_gdp <- temp_gdp[, which(!is.na(colSums(temp_gdp[pos_ip:nrow(temp_gdp), ])))[1]]
        
        temp_growth_neighs <- lapply(temp_draws_neigh, 
                                     function(x) apply(log(x), 2, diff))
        temp_growth_neighs <- Reduce("+", temp_growth_neighs)
        temp_growth_neighs <- temp_growth_neighs / length(temp_neighs)
        
        
        for(jj in 1:ncol(temp_int_draws)) {
          pos_growth_rate <- which(rownames(temp_growth_neighs) == (year_end + 1))
          for(ii in pos_ip:nrow(temp_int_draws)) {
            temp_int_draws[ii,jj] <- temp_int_draws[ii-1,jj] * 
              (1 + temp_growth_neighs[pos_growth_rate,jj])
            pos_growth_rate <- pos_growth_rate + 1
          }
        }
        list_int_draws[[j]][[cc]] <- temp_int_draws
        list_em_draws[[j]][[cc]] <- apply(temp_int_draws, 2, function(x) x * temp_gdp)
        
        print(paste0(cc, " - ", j))
      }
    }
  }
}

# get total emissions intensities per country
list_int_draws[["total"]] <- list()
for(cc in cN_full) {
  temp_list <- list()
  for(j in sector_vec) {
    temp_draw_list <- list_int_draws[[j]][[cc]]
    if(!is.null(temp_draw_list)) {
      temp_list[[j]] <- temp_draw_list
    }
  }
  list_int_draws[["total"]][[cc]] <- Reduce("+", temp_list)
}

# compute summary statistics
list_int_quants <- lapply(list_int_draws, 
                          function(x) lapply(x, 
                                             function(y) apply(y, 1, quantile, 
                                                               p = c(0.05, 0.16, 0.5, 0.84, 0.95), 
                                                               na.rm = TRUE)))

# get total emissions per country
list_em_draws[["total"]] <- list()
for(cc in cN_full) {
  temp_list <- list()
  for(j in sector_vec) {
    temp_draw_list <- list_em_draws[[j]][[cc]]
    if(!is.null(temp_draw_list)) {
      temp_list[[j]] <- temp_draw_list
    }
  }
  list_em_draws[["total"]][[cc]] <- Reduce("+", temp_list)
}

# compute summary statistics
list_em_quants <- lapply(list_em_draws, 
                         function(x) lapply(x, 
                                            function(y) apply(y, 1, quantile, 
                                                              p = c(0.05, 0.16, 0.5, 0.84, 0.95), 
                                                              na.rm = TRUE)))


# get emissions per sector over all countries
list_em_sectors <- list()
for(j in sector_vec) {
  cN_temp <- cN_full[which(cN_full %in% names(list_em_draws[[j]]))]
  list_em_draws[[j]] <- list_em_draws[[j]][cN_temp]
  list_em_sectors[[j]] <-  list_em_draws[[j]]
  for(cc in names(list_em_sectors[[j]])) {
    temp <- list_em_sectors[[j]][[cc]]
    pos_year_end <- which(rownames(temp) == year_end)
    pos_NAs <- which(rowSums(temp[1:pos_year_end, ], na.rm = TRUE) == 0)
    if(length(pos_NAs) > 0) {
      temp[pos_NAs, ] <- 0
    }
    list_em_sectors[[j]][[cc]] <- temp
  }
  
  list_em_sectors[[j]] <- Reduce("+", list_em_sectors[[j]])
}
list_em_sectors[["total"]] <- Reduce("+", list_em_sectors)

# compute summary statistics
list_em_sectors_quants <- lapply(list_em_sectors, 
                                 function(x)  apply(x, 1, quantile, 
                                                    p = c(0.05, 0.16, 0.5, 0.84, 0.95), 
                                                    na.rm = TRUE))
colnames(list_em_sectors_quants$agriculture) <- 1980:2050



#####
# Preparations for regional plots

# Income status from WB
regions_income <- WDI::WDI_data$country %>% 
  as.data.frame() %>% 
  select(iso3c, country, region, income) %>% 
  filter(iso3c %in% cN_full)


# Getting IAM regions
region_mapping <- xlsx::read.xlsx("./01_data/cmip6_iam_model_region_mapping.xlsx", 
                                  sheetIndex = 1)
names(region_mapping)[c(1,3)] <- c("iso3c", "region_ar5")

regions_income <- left_join(regions_income, region_mapping[ , c(1,3)])
regions_income <- regions_income %>% 
  mutate(region_ar5 = replace(region_ar5, region_ar5 == "R5ASIA", "Asia"),
         region_ar5 = replace(region_ar5, region_ar5 == "R5LAM", "Latin America and Caribbean"),
         region_ar5 = replace(region_ar5, region_ar5 == "R5MAF", "Middle East and Africa"),
         region_ar5 = replace(region_ar5, region_ar5 == "R5OECD", "OECD"),
         region_ar5 = replace(region_ar5, region_ar5 == "R5REF", "REF"))

# Oil-exporting countries according to IMF
cN_oil <- c("DZA", "AGO", "AZE", "BHR", "BRN", "TCD", "COG", "ECU", "GNQ", 
            "GAB", "IRN", "IRQ", "KAZ", "KWT", "LBY", "NGA", "OMN", "QAT", 
            "RUS", "SAU", "SSD", "TLS", "TTO", "TKM", "ARE", "VEN", "YEM")
regions_income <- regions_income %>% 
  mutate(oil = "oil_import", 
         oil = replace(oil, iso3c %in% cN_oil, "oil_export"))


# Creating dataframes holding various regional results


# AR5 regions
list_em_regions <- list()
for(rr in unique(regions_income$region_ar5)) {
  list_em_regions[[rr]] <- list()
  for(jj in names(list_em_draws)) {
    pos_cN <- regions_income[which(regions_income$region_ar5 == rr), "iso3c"]
    if(jj == "buildings" & rr == "REF") pos_cN <- pos_cN[-which(pos_cN == "TJK")]
    
    list_em_regions[[rr]][[jj]] <- list_em_draws[[jj]][pos_cN]
    for(cc in names(list_em_regions[[rr]][[jj]])) {
      temp <- list_em_regions[[rr]][[jj]][[cc]]
      pos_year_end <- which(rownames(temp) == year_end)
      pos_NAs <- which(rowSums(temp[1:pos_year_end, ], na.rm = TRUE) == 0)
      if(length(pos_NAs) > 0) {
        temp[pos_NAs, ] <- 0
      }
      list_em_regions[[rr]][[jj]][[cc]] <- temp
    }
    list_em_regions[[rr]][[jj]] <- Reduce("+", list_em_regions[[rr]][[jj]])
  }
}

list_em_regions_quants <- lapply(list_em_regions, 
                                 function(x) lapply(x, function(y) 
                                   apply(y, 1, quantile, 
                                         p = c(0.05, 0.16, 0.5, 0.84, 0.95), 
                                         na.rm = TRUE)))


# Income
list_em_income <- list()
for(rr in unique(regions_income$income)) {
  list_em_income[[rr]] <- list()
  for(jj in names(list_em_draws)) {
    pos_cN <- regions_income[which(regions_income$income == rr), "iso3c"]
    if(jj == "buildings" & rr == "Low income") pos_cN <- pos_cN[-which(pos_cN == "TJK")]
    
    list_em_income[[rr]][[jj]] <- list_em_draws[[jj]][pos_cN]
    for(cc in names(list_em_income[[rr]][[jj]])) {
      temp <- list_em_income[[rr]][[jj]][[cc]]
      pos_year_end <- which(rownames(temp) == year_end)
      pos_NAs <- which(rowSums(temp[1:pos_year_end, ], na.rm = TRUE) == 0)
      if(length(pos_NAs) > 0) {
        temp[pos_NAs, ] <- 0
      }
      list_em_income[[rr]][[jj]][[cc]] <- temp
    }
    
    list_em_income[[rr]][[jj]] <- Reduce("+", list_em_income[[rr]][[jj]])
  }
}

list_em_income_quants <- lapply(list_em_income, 
                               function(x) lapply(x, function(y) 
                                 apply(y, 1, quantile, 
                                       p = c(0.05, 0.16, 0.5, 0.84, 0.95), 
                                       na.rm = TRUE)))

# Oil & income
list_em_oil_income <- list()
for(rr in unique(regions_income$oil)) {
  list_em_oil_income[[rr]] <- list()
  for(jj in names(list_em_draws)) {
    pos_cN <- regions_income[which(regions_income$oil == rr & 
                                   regions_income$income == "High income"), "iso3c"]
    if(jj == "buildings" & rr == "oil_import") {
      pos_cN_now <- which(pos_cN == "TJK")
      if(length(pos_cN_now) > 0) {
        pos_cN <- pos_cN[-pos_cN_now]
      }
    }
    
    list_em_oil_income[[rr]][[jj]] <- list_em_draws[[jj]][pos_cN]
    for(cc in names(list_em_oil_income[[rr]][[jj]])) {
      temp <- list_em_oil_income[[rr]][[jj]][[cc]]
      pos_year_end <- which(rownames(temp) == year_end)
      pos_NAs <- which(rowSums(temp[1:pos_year_end, ], na.rm = TRUE) == 0)
      if(length(pos_NAs) > 0) {
        temp[pos_NAs, ] <- 0
      }
      list_em_oil_income[[rr]][[jj]][[cc]] <- temp
    }
    
    list_em_oil_income[[rr]][[jj]] <- Reduce("+", list_em_oil_income[[rr]][[jj]])
  }
}

list_em_oil_income_quants <- lapply(list_em_oil_income, 
                                    function(x) lapply(x, function(y) 
                                      apply(y, 1, quantile, 
                                            p = c(0.05, 0.16, 0.5, 0.84, 0.95), 
                                            na.rm = TRUE)))


#####
# saving only summary statistics, total draws need much storage space
save(list_int_quants, # list_int_draws,
     list_em_quants, # list_em_draws, 
     list_em_sectors_quants, # list_em_sectors,
     list_em_regions_quants, # list_em_regions, 
     list_em_income_quants, # list_em_income,
     list_em_oil_income_quants, # list_em_oil_income,
     file = paste0("./02_output/09_results/", 
                   model, "/", em_data, "_small", small, 
                   "_adjusted", adjust, ".Rda"))
