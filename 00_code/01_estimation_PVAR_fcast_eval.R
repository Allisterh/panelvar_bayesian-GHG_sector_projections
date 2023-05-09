#####
# Estimation file for PVAR with Normal-Gamma prior
rm(list=ls())

run <- as.integer(Sys.getenv("SGE_TASK_ID"))

if(is.na(run)) {
  user <- "LV"
} else {
  # These are settings for the cluster used to estimate the various PVAR models, 
  # one would have to apapt the settings here
  user <- "cluster"
  setwd("./world_emissions/")
  .libPaths("~/RLIB")
}

# Set number of draws to burn and save
if(user == "cluster") {
  nsave=50000
  nburn=100000
} else {
  # adjust for smaller number here, when not using a cluster or cloud instance
  nsave=500
  nburn=500
}


# load auxiliary R files
source("./00_code/model_functions/00_auxiliary_functions.R")

# shall all draws be saved or only summary statistics?
# Default is FALSE for forecast evaluation 
saving <- FALSE

# Settings to choose from
em_data <- c("ESSD_intensity")
sector <- c("transport", "energy", "buildings", "agriculture", "industry")
plag <- c(1)
trans <- c("lvl")
cons <- c(TRUE)
trend <- c(TRUE, FALSE)
small <- c("TRUE", "+urb", "+educ+old", "FALSE")
split_sample <- c("peak_yes", "peak_no")

fcast_in <- TRUE
fcast_out <- FALSE
fhorz <- 4

opts <- expand.grid(em_data, sector, plag, trans, cons, trend, split_sample,
                    fcast_in, fcast_out, small)

pos <- which(opts$Var7 == "peak_yes" & (!opts$Var6))
if(length(pos) > 0) {
  opts <- opts[-pos, ]
}

pos <- which(opts$Var7 == "peak_no" & (opts$Var6))
if(length(pos) > 0) {
  opts <- opts[-pos, ]
}

if(user != "cluster") {
  # selection for options below, if one runs the model on a standard PC, the run
  # parameter has to be adapted to select the right model, i.e. it would have be
  # change to be an element of 1:40 (10 runs per model specification)
  run <- 1
}

opts <- opts[run,]

# Re-assigning settings for a specific run of the model
em_data <- opts[,1]
sector <- opts[, 2]
plag <- opts[, 3]
trans <- opts[, 4]
cons <- opts[, 5]
trend <- opts[, 6]
split_sample <- opts[, 7]
fcast_in <- opts[, 8]
fcast_out <- !fcast_in
small <- opts[, 10]

if(trend) {
  tr_number <- 1
} else {
  tr_number <- 0
}


#####
# Load data
load("./01_data/processed/GHG_est_data.Rda")
if(sector == "industry") {
  Yraw <- df_CO2_ESSD_industry
} else if (sector == "energy") {
  Yraw <- df_CO2_ESSD_energy
} else if (sector == "transport") {
  Yraw <- df_CO2_ESSD_transport
} else if (sector == "buildings") {
  Yraw <- df_CO2_ESSD_buildings
} else if (sector == "agriculture") {
  Yraw <- df_CO2_ESSD_agriculture
} else if (sector == "total") {
  Yraw <- df_CO2_ESSD_total
}
Yraw[Yraw == 0] <- NA

#####
# Transform for checking peaks in intensities
library(dplyr)
library(reshape2)
library(stringr)
Yraw.temp <- Yraw %>% reshape2::melt(id = "year") %>% 
  dplyr::mutate(iso3c = stringr::str_split(variable, "\\.", simplify = TRUE)[,1], 
                var = stringr::str_split(variable, "\\.", simplify = TRUE)[,2]) %>% 
  dplyr::select(-variable) %>% 
  tidyr::pivot_wider(names_from = var, values_from = value) %>% 
  dplyr::select(iso3c, year, everything())
if(fcast_in) {
  last.year <- max(Yraw.temp$year)
  Yraw.temp <- Yraw.temp %>%
    dplyr::filter(year <= (last.year - fhorz))
  
}
detach("package:dplyr", unload=TRUE)

# few countries get assigned to no peak group due to trend reversals in emission
# intensities causing instabilities
if(split_sample != "no") {
  if(sector == "agriculture") {
    excl.countries <-  c("BDI", "CHN", "LBN", "LKA", "MMR", "PAN", "SGP", "SOM",
                         "SWZ", "ZMB")
  } else if (sector == "transport") {
    excl.countries <-   c("AFG", "BDI", "ETH", "JAM", "MMR", "NGA", "SGP", "SOM", 
                          "SWZ", "ZMB")
  } else if (sector == "energy") {
    excl.countries <-  c("AFG", "BDI", "IRQ", "LAO", "MOZ",  "PNG", "PRI", "SGP", 
                         "SWZ", "ZMB", "MMR", "CPV")
  } else if (sector == "industry") {
    excl.countries <-  c("BDI", "SGP", "SWZ", "ZMB", "MMR", "ZWE")
  } else if (sector == "buildings") {
    excl.countries <-  c("GEO", "ISR", "CHN", "KGZ", "SGP", "SWZ", "ZMB", "ZAF",
                         "ZWE", "MMR")
  } else {excl.countries <- c()}
}

# use data after 1995 for peak selection for post-Soviet countries and some
# other additional countries
list.soviet <- c("RUS", "UKR", "BLR", "MDA", "UZB", "KAZ", "KGZ", "TJK", "TKM", 
                 "AZE", "ARM", "GEO", "LTU", "LVA", "EST")
list.yugo.alb <- c("BIH", "HRV", "SRB", "MKD", "MNE", "SVK", "SVN", "ALB")
list.add <- c("ABW", "BRN", "COG", "CUB", "DJI", "ERI", "GNQ", "GUY", "IRQ", 
              "LBR", "MAC", "MLT", "MMR", "MRT", "NAM", "NGA", "PSE", "TLS", 
              "YEM", "ZAF", "SOM", "ZWE")

# check for peaks and assign groups accordingly
if(split_sample == "peak_yes") {
  temp <- find_maxes(Yraw.temp, variable = "intensity", 
                     list.shorten = c(list.soviet, list.yugo.alb, list.add), 
                     excl.countries = excl.countries)
  sample.inc <- temp[[1]]
  sample.excl <- temp[[2]]
} else if(split_sample == "peak_no") {
  temp <- find_maxes(Yraw.temp, variable = "intensity", 
                     list.shorten = c(list.soviet, list.yugo.alb, list.add), 
                     excl.countries = excl.countries)
  sample.inc <- temp[[2]]
  sample.excl <- temp[[1]]
} 


#####
# Select necessary data depending on specification
time.frame <- Yraw$year
Yraw$year <- NULL
if(small == "TRUE") {
  Yraw <- Yraw[, grep("(\\.CO2GDPPPP$|\\.GDPpcPPP|\\.POP)", names(Yraw))]
} else if(small == "+urb") {
  Yraw <- Yraw[, grep("(\\.CO2GDPPPP$|\\.GDPpcPPP|\\.POP|\\.pop_urb_share$)", names(Yraw))]
} else if(small == "+educ+old") {
  Yraw <- Yraw[, grep("(\\.CO2GDPPPP$|\\.GDPpcPPP|\\.POP|\\.old_share$|\\.sec_share$)", names(Yraw))]
} else if(small == "FALSE") {
  Yraw <- Yraw[, grep("(\\.CO2GDPPPP$|\\.GDPpcPPP|\\.POP|\\.old_share$|\\.pop_urb_share$|\\.sec_share$)", names(Yraw))]
}


#####
# Make copy of untransformed data, then log the data
Y_untrans <- Yraw
Yraw <- log(Yraw)


#####
# Transform data in matrix to list (for unbalanced panels), doing some input checks
Yraw <- matrix_to_list(Yraw, time.frame = time.frame)


#####
# Select data from Yraw depending on whether intensity peak was already reached
# cutting length to peak year, taking only the post-peak period for estimation
if(split_sample != "no") {
  incl.cN <- sample.inc$iso3c
  incl.cN <- incl.cN[which(incl.cN %in% names(Yraw))]
  Yraw <- Yraw[incl.cN]
  for(cc in names(Yraw)) {
    pos <- sample.inc[["year"]][which(sample.inc$iso3c == cc)]
    pos <- which(rownames(Yraw[[cc]]) == pos)
    if(length(pos) > 0) {
      Yraw[[cc]] <- Yraw[[cc]][pos:nrow(Yraw[[cc]]), ]
    }
  }
}
cN_avail <- names(Yraw)


#####
# Create untransformed Y list for mapping back emissions later on
Y_untrans <- matrix_to_list(Y_untrans, time.frame = time.frame)
Y_untrans <- Y_untrans[cN_avail]


#####
# Get projections to condition forecasts on
# For forecast evaluation, take historical values
# For projections, take SSP-consistent values of covariates
if(fcast_in) {
  rep_end <- 0
  # data to condition on can be taken from Yraw directly for forecast evaluation
  cond_data <- lapply(Yraw, function(x) x[(nrow(x)-fhorz+1):nrow(x), 2:ncol(x)])
  Y_comp <- lapply(Yraw, function(x) x[(nrow(x)-fhorz+1):nrow(x), ])
  Yraw <- lapply(Yraw, function(x) x[1:(nrow(x)-fhorz), ])
  Y_comp_untrans <- Y_untrans
  Y_untrans <- lapply(Y_untrans, function(x) x[1:(nrow(x)-fhorz), ])
} else if(fcast_out) {
  rep_end <- 5
  # projected data to condition on for projections up to year 2050
  load("./01_data/processed/projections_data.Rda")
  proj.time <- projections_data$year
  projections_data$year <- NULL
  proj.data <- projections_data
  proj.data <- proj.data[, which(substr(names(proj.data), 1, 3) %in% c(cN_avail))]
  
  if(small == "TRUE") {
    proj.data <- proj.data[, grep("(\\.GDPpcPPP|\\.POP)", names(proj.data))]
  } else if(small == "+urb") {
    proj.data <- proj.data[, grep("(\\.GDPpcPPP|\\.POP|\\.pop_urb_share$)", names(proj.data))]
  } else if(small == "+educ+old") {
    Yraw <- Yraw[, grep("(\\.GDPpcPPP|\\.POP|\\.old_share$|\\.sec_share$)", names(Yraw))]
  } else if(small == "FALSE") {
    proj.data <- proj.data[, grep("(\\.GDPpcPPP|\\.POP|\\.old_share$|\\.pop_urb_share$|\\.sec_share$)", names(proj.data))]
  }
  
  proj.data <- log(proj.data)
  pos <- which(proj.time == time.frame[length(time.frame)])
  proj.data <- proj.data[(pos+1):nrow(proj.data), ]
  proj.data <- matrix_to_list(proj.data, check.length = FALSE)
  vars_order <- colnames(Yraw[[1]])[-1]
  cond_data <- lapply(proj.data, function(x) x[, vars_order])
  for(i in 1:length(cond_data)) {
    temp.g <- cond_data[[i]][nrow(cond_data[[i]]),] - cond_data[[i]][nrow(cond_data[[i]])-1,]
    temp <- matrix(NA, rep_end, ncol(cond_data[[i]]))
    for(j in 1:length(temp.g)) {
      temp[,j] <- cond_data[[i]][nrow(cond_data[[i]]), j] + temp.g[j] * seq_len(rep_end)
    }
    cond_data[[i]] <- rbind(cond_data[[i]], temp)
  }
  cond_data <- cond_data[cN_avail]
  fhorz <- nrow(cond_data[[1]])
}


#####
# Create directories
dir.create("./02_output/09_results/NG_PVAR_fcast_eval", showWarnings = FALSE)
folder <- paste0("./02_output/09_results/NG_PVAR_fcast_eval/", em_data)
dir.create(folder, showWarnings = FALSE)
folder <- paste0(folder, "/", sector)
dir.create(folder, showWarnings = FALSE)
folder <- paste0(folder, "/split_", split_sample)
dir.create(folder, showWarnings = FALSE)
folder <- paste0(folder, "/small", small)
dir.create(folder, showWarnings = FALSE)
folder <- paste0(folder, "/", trans, "_plag", plag, "_det", tr_number, "/")
dir.create(folder, showWarnings = FALSE)


#####
# Estimate model
source("./00_code/model_functions/01_model_PVAR_NG.R")


#####
# Produce forecasts ex-post based on estimated mocdel
source("./00_code/model_functions/02_PVAR_cond_fcasts.R")

# Compute summary statistics of unconditional and conditional forecasts
pred_mean <- apply(pred_store, c(2,3,4), mean, na.rm = TRUE)
pred_sd <- apply(pred_store, c(2,3,4), sd, na.rm = TRUE)
fcast_cond_mean <- apply(fcast_cond, c(2,3,4), median, na.rm = TRUE)
fcast_cond_sd <- apply(fcast_cond, c(2,3,4), sd, na.rm = TRUE)
fcast_cond_quant <- apply(fcast_cond, c(2,3,4), 
                          quantile, p = c(0.025, 0.05, 0.95, 0.975), 
                          na.rm = TRUE)

#####
# Forecast evaluation (LPS, RMSE)
if(fcast_in) {
  rmse_dens_store <- array(NA, c(thindraws, M, N))
  lps_store <- array(NA, c(M, fhorz, N))
  
  pred_store_eval <- aperm(fcast_cond, c(1,3,2,4))
  pred_mean_eval <- aperm(fcast_cond_mean, c(2,1,3))
  pred_sd_eval <- aperm(fcast_cond_sd, c(2,1,3))
  
  for(cc in 1:N) {
    
    hold.out <- Y_comp[[cc]]
    
    ### RMSE
    for(irep in 1:thindraws) {
      for(mm in 1:M) {
        rmse_dens_store[irep,mm,cc] <- sqrt(mean((pred_store_eval[irep,mm,,cc] - hold.out[,mm])^2))
      }
    }

    ### LPS score
    for(mm in 1:M){
      lps_store[mm,,cc] <- dnorm(hold.out[,mm],
                                        mean = pred_mean_eval[mm, ,cc],
                                        sd = pred_sd_eval[mm, ,cc],
                                        log=TRUE)
    }
    
  }
  rmse_dens_mean <- apply(rmse_dens_store, c(2,3), mean, na.rm = TRUE)
  
  save(rmse_dens_store, rmse_dens_mean, lps_store,
       file = paste0(folder, "saves_fcast_eval.Rda"))
}


##### 
# Map emission intensity forecasts back to overall emissions
fcast_cond_mapped <- list()
for(cc in 1:N) {
  Y_untrans_temp <- Y_untrans[[cc]]
  fcast_cond_mapped_temp <- array(NA, dim = c(thindraws, nrow(Y_untrans_temp) + fhorz, M))
  compl_horz_start <- as.numeric(rownames(Y_untrans_temp)[1])
  compl_horz_end <- as.numeric(rownames(Y_untrans_temp)[nrow(Y_untrans_temp)]) + fhorz
  dimnames(fcast_cond_mapped_temp)[2] <- list(compl_horz_start:compl_horz_end)
  dimnames(fcast_cond_mapped_temp)[3] <- list(varNames)
  for(irep in 1:thindraws) {
    fcast_temp <- fcast_cond[irep,,,cc]
    fcast_cond_mapped_temp[irep,1:nrow(Y_untrans_temp), ] <- Y_untrans_temp
    fcast_cond_mapped_temp[irep, (nrow(Y_untrans_temp)+1):(nrow(Y_untrans_temp)+fhorz),] <- exp(fcast_temp)
  }
  fcast_cond_mapped_temp[is.infinite(fcast_cond_mapped_temp)] <- NA
  fcast_cond_mapped[[cc]] <- fcast_cond_mapped_temp
}

fcast_cond_mapped_mean <- lapply(fcast_cond_mapped, 
                                 function(x) apply(x, c(2, 3), median, na.rm = TRUE))
fcast_cond_mapped_quant <- lapply(fcast_cond_mapped, 
                                  function(x) apply(x, c(2, 3), quantile, 
                                                    p = c(0.025, 0.05, 0.95, 0.975), 
                                                    na.rm = TRUE))
names(fcast_cond_mapped_mean) <- names(fcast_cond_mapped_quant) <- cN

fcast_cond_tot_em <- list()
fcast_cond_em_int <- list()
for(cc in 1:N) {
  temp <- fcast_cond_mapped[[cc]][, , c("CO2GDPPPP", "GDPpcPPP", "POP")]
  fcast_cond_tot_em_temp <- matrix(NA, nrow = dim(temp)[2], ncol = thindraws)
  rownames(fcast_cond_tot_em_temp) <- dimnames(fcast_cond_mapped[[cc]])[[2]]
  for(irep in 1:thindraws){
    fcast_cond_tot_em_temp[,irep] <- c(temp[irep,,1] * temp[irep,,2] * temp[irep,,3])
  }
  fcast_cond_tot_em_temp[is.infinite(fcast_cond_tot_em_temp)] <- NA
  fcast_cond_tot_em[[cc]] <- fcast_cond_tot_em_temp
  fcast_cond_em_int[[cc]] <- t(temp[,,1])
  rownames(fcast_cond_em_int[[cc]]) <- dimnames(fcast_cond_mapped[[cc]])[[2]]
}
names(fcast_cond_tot_em) <- cN
names(fcast_cond_em_int) <- cN

fcast_cond_tot_em_mean <- lapply(fcast_cond_tot_em, function(x) 
  apply(x, 1, median, na.rm = TRUE))
fcast_cond_tot_em_quant <- lapply(fcast_cond_tot_em, 
                                  function(x) apply(x, 1, quantile, 
                                                    p = c(0.025, 0.05, 0.95, 0.975), 
                                                    na.rm = TRUE))
names(fcast_cond_tot_em_mean) <- names(fcast_cond_tot_em_quant) <- cN


#####
# Saving results here

# saving summary statistics always
if(fcast_in) {
  # include observed emissions for comparison
  em_tot_comp <- list()
  for(cc in 1:N) {
    temp <- Y_comp_untrans[[cc]][, c("CO2GDPPPP", "GDPpcPPP", "POP")]
    em_tot_comp[[cc]] <- temp[,1] * temp[,2] * temp[,3]
  }
  names(em_tot_comp) <- cN
  
  save(fcast_cond_mapped_mean, fcast_cond_mapped_quant, 
       fcast_cond_tot_em_mean, fcast_cond_tot_em_quant, em_tot_comp,
       file = paste0(folder, "fcast_cond_in_mapped_quantities.Rda"))
} else if(fcast_out) {
  save(fcast_cond_mapped_mean, fcast_cond_mapped_quant, 
       fcast_cond_tot_em_mean, fcast_cond_tot_em_quant, 
       file = paste0(folder, "fcast_cond_out_mapped_quantities.Rda"))
}

# saving individual draws only if needed, need much storage space
if(saving) {
  save(fcast_cond_tot_em,
       file = paste0(folder, "fcast_tot_em_draws.Rda"))
  save(fcast_cond_em_int,
       file = paste0(folder, "fcast_cond_int_draws.Rda"))
}

