#####
# Forecast comparisons across models using RMSEs and LPDs

library(dplyr)
library(tidyverse)
require(xtable)


#### Scramble data together on emissions intensities and total emissions

model <- "NG_PVAR_fcast_eval"
small <- c("TRUE", "+urb", "+educ+old", "FALSE")
trans <- c("lvl")
det <- c(1)
plag <- c(1)
em_data <- "ESSD_intensity"
split <- c("peak")
year_end <- 2018

opts <- expand.grid(model, small, trans, det, plag, em_data, split)


sector_vec <- c("transport", "agriculture", "industry", "buildings", "energy")

list_rmse_lps <- list()
list_comp_em <- list()
list_fcast_quant <- list()
for(i in 1:nrow(opts)) {
  
  model <- opts[i, 1]
  small <- opts[i, 2]
  trans <- opts[i, 3]
  det <- opts[i, 4]
  plag <- opts[i, 5]
  em_data <- opts[i, 6]
  split <- opts[i, 7]

  split_up <- paste0(split, c("_yes", "_no"))

  folder_string <- paste0("./02_output/09_results/", model, "/", em_data, "/")
  cN_full <- c()
  for(spl in split_up) {
    
    if(spl == "peak_yes") det_temp <- 1 else if (spl == "peak_no") det_temp <- 0
    spec_string <- paste0("/split_", spl, "/small", small, "/", trans, 
                          "_plag", plag, "_det", det_temp)
    
    for(j in "agriculture") {
      test_load <- try(load(paste0(folder_string, j, spec_string, 
                                   "/fcast_cond_in_mapped_quantities.Rda")), silent = TRUE)
      if(is(test_load, "try-error")) next
      temp <- names(fcast_cond_mapped_mean)
      cN_full <- c(cN_full, temp)
    }
  }
  cN_full <- unique(cN_full)
  cN_full <- cN_full[order(cN_full)]
  
  spec_string_no <-  paste0("small", small, "_", trans, 
                            "_plag", plag, "_detfit")

  list_rmse_lps[[spec_string_no]] <- list()
  list_rmse_lps[[spec_string_no]][["rmse_dens"]] <- list()
  list_rmse_lps[[spec_string_no]][["lps"]] <- list()
  
  list_fcast_quant[[spec_string_no]] <- list()
  list_comp_em[[spec_string_no]] <- list()
  
  for(spl in split_up) {

    if(spl == "peak_yes") det_temp <- 1 else det_temp <- 0
    spec_string <- paste0("/split_", spl, "/small", small, "/", trans, 
                          "_plag", plag, "_det", det_temp)
    
    for(j in sector_vec) {
      test_load <- try(load(paste0(folder_string, j, spec_string, 
                                   "/fcast_cond_in_mapped_quantities.Rda")), 
                       silent = TRUE)
      if(is(test_load, "try-error")) next
      
      test_load <- try(load(paste0(folder_string, j, spec_string, 
                                   "/saves_fcast_eval.Rda")), 
                       silent = TRUE)
      if(is(test_load, "try-error")) next
 
      colnames(rmse_dens_mean) <- names(fcast_cond_mapped_mean)
      dimnames(rmse_dens_store)[3] <- dimnames(lps_store)[3] <- list(names(fcast_cond_mapped_mean))
      rownames(rmse_dens_mean) <- colnames(fcast_cond_mapped_mean[[1]])
      dimnames(rmse_dens_store)[2] <- dimnames(lps_store)[1] <- 
        list(colnames(fcast_cond_mapped_mean[[1]]))
      
      if(is.null(list_rmse_lps[[spec_string_no]][["rmse_dens"]][[j]])) {
        list_rmse_lps[[spec_string_no]][["rmse_dens"]][[j]] <- rmse_dens_mean
        list_rmse_lps[[spec_string_no]][["lps"]][[j]] <- lps_store
      } else {
        list_rmse_lps[[spec_string_no]][["rmse_dens"]][[j]] <- 
          cbind(list_rmse_lps[[spec_string_no]][["rmse_dens"]][[j]], rmse_dens_mean)
        list_rmse_lps[[spec_string_no]][["lps"]][[j]] <- 
          abind::abind(list_rmse_lps[[spec_string_no]][["lps"]][[j]], lps_store)
        
        if(j == "buildings") cN_temp <- cN_full[-which(cN_full == "TJK")] else cN_temp <- cN_full
        list_rmse_lps[[spec_string_no]][["rmse_dens"]][[j]] <-
          list_rmse_lps[[spec_string_no]][["rmse_dens"]][[j]][, cN_temp]
        list_rmse_lps[[spec_string_no]][["lps"]][[j]] <- 
          list_rmse_lps[[spec_string_no]][["lps"]][[j]][,, cN_temp]
      }
      
      
      if(is.null(list_fcast_quant[[spec_string_no]][[j]])) {
        list_fcast_quant[[spec_string_no]][[j]] <- fcast_cond_tot_em_quant
        list_comp_em[[spec_string_no]][[j]] <- em_tot_comp
      } else {
        list_fcast_quant[[spec_string_no]][[j]] <- 
          c(fcast_cond_tot_em_quant, list_fcast_quant[[spec_string_no]][[j]])
        list_comp_em[[spec_string_no]][[j]] <- 
          c(em_tot_comp, list_comp_em[[spec_string_no]][[j]])
        
        if(j == "buildings") cN_temp <- cN_full[-which(cN_full == "TJK")] else cN_temp <- cN_full
        list_fcast_quant[[spec_string_no]][[j]] <- list_fcast_quant[[spec_string_no]][[j]][cN_temp]
        list_comp_em[[spec_string_no]][[j]] <- list_comp_em[[spec_string_no]][[j]][cN_temp]
      }
    }
  }
}

# RMSE
rmse_dens_table <- as.data.frame(matrix(NA, 
                                        length(sector_vec) + 1, 
                                        length(unique(opts$Var2))))
names(rmse_dens_table) <- opts$Var2
rownames(rmse_dens_table) <- c("total", sector_vec)

for(mm in opts$Var2) {
  spec_string <- paste0("small", mm, "_lvl_plag1_detfit")
  for(ss in sector_vec) {
    rmse_dens_table[ss, mm] <- sum(list_rmse_lps[[spec_string]][["rmse_dens"]][[ss]][1,])
  }
  rmse_dens_table["total", mm] <- sum(rmse_dens_table[2:6, mm])
}

# LPS
lps_table <- as.data.frame(matrix(NA, 
                                  length(sector_vec) + 1, 
                                  length(unique(opts$Var2))))
names(lps_table) <- opts$Var2
rownames(lps_table) <- c("total", sector_vec)

for(mm in opts$Var2) {
  spec_string <- paste0("small", mm, "_lvl_plag1_detfit")
  list_rmse_lps[[spec_string]][["lps"]] <- 
    lapply(list_rmse_lps[[spec_string]][["lps"]] ,
           function(x) apply(x, c(1,3), sum))
  for(ss in sector_vec) {
    lps_table[ss, mm] <- sum(list_rmse_lps[[spec_string]][["lps"]][[ss]][1,])
  }
  lps_table["total", mm] <- sum(lps_table[2:6, mm])
}

# Hit rates
hit_rates_table <- as.data.frame(matrix(NA, 
                                        1, 
                                        length(unique(opts$Var2))))
names(hit_rates_table) <- opts$Var2
rownames(hit_rates_table) <- c("total")

for(mm in opts$Var2) {
  spec_string <- paste0("small", mm, "_lvl_plag1_detfit")
  list_em <- list()
  list_range <- list()
  tot_em <- c()
  tot_quants <- c()
  tot_ind <- c()
  for(j in sector_vec) {
    list_comp_em[[spec_string]][[j]] <-
      lapply(list_comp_em[[spec_string]][[j]],
             function(x) x[length(x)])
    list_fcast_quant[[spec_string]][[j]] <-
      lapply(list_fcast_quant[[spec_string]][[j]],
             function(x) x[, ncol(x)])

    temp_range <- bind_rows(list_fcast_quant[[spec_string]][[j]])
    temp_true <- unlist(list_comp_em[[spec_string]][[j]])
    list_em[[j]] <- temp_true
    list_range[[j]] <- temp_range
  }
  

  for(cc in unique(substr(names(list_em[[1]]), 1, 3))) {
    temp_true <- c()
    temp_range<- c()
    for(j in sector_vec) {
      pos <- grep(cc, names(list_em[[j]]))
      if(length(pos) > 0){
        temp_true <- c(temp_true, list_em[[j]][pos])
        temp_range <- rbind(temp_range, list_range[[j]][pos,])
      }
    }
    temp_true <- sum(temp_true)
    temp_range <- colSums(temp_range)
    
    if(temp_true > temp_range[1] & temp_true < temp_range[4]) {
      tot_ind <- c(tot_ind, 1)
    } else {
      tot_ind <- c(tot_ind, 0)
    }
    tot_em <- c(tot_em, temp_true)
    tot_quants <- c(tot_quants, temp_range)
  }
  hit_rates_table["total", mm] <- sum(tot_ind) / length(tot_ind)
}

# Putting tables in shape for paper
lps_table_paper <- lps_table[c(1, 3, 5, 6, 4, 2) ,]
rmse_table_paper <- rmse_dens_table[c(1, 3, 5, 6, 4, 2) ,]
hit_rates_table_paper <- hit_rates_table[1, ]

names(lps_table_paper) <- names(rmse_table_paper) <- names(hit_rates_table_paper) <- 
  c("(1)", "(2)", "(3)", "(4)")

rownames(lps_table_paper) <- rownames(rmse_table_paper) <- 
  c("Total", "Agriculture", "Buildings", "Energy", "Industry", "Transport")

rownames(hit_rates_table_paper) <- "Total"

lps_table_paper <- lps_table_paper %>% 
  mutate(Sector = rownames(lps_table_paper), .before = `(1)`)

rmse_table_paper <- rmse_table_paper %>% 
  mutate(Sector = rownames(lps_table_paper), .before = `(1)`)

hit_rates_table_paper <- hit_rates_table_paper %>% 
  mutate(Sector = "Total", .before = `(1)`)

xt_all_tables <- xtable(rbind(rmse_table_paper, lps_table_paper, hit_rates_table_paper), 
                        label = "tab:eval", 
                        caption = "Table shows the root mean squared errors (RMSE), cumulative log predictive scores (LPS) and proportions of countries whose 95\\% prediction interval included the realized emission intensity at the end of the validation period of 4 years (Hit Rate). Column (1) refers to a model comprising only of the Kaya components--sectoral emission intensity, GDP per capita, and population. The model in column (2) adds urbanization dynamics, while (3) includes the age and educational structure of countries. Column (4) holds results for the full specification, i.e. all Kaya components and demographic variables. RMSE and cumulative LPS are computed as sums over all countries modelled; results to sector ``Total'' refer to the sums over all sectors. Hit rates are given for total emissions. Bold letters indicate the best-performing model, i.e. the one with the lowest RMSE and the highest cumulative LPS or hit rate.", 
                        align = "r|l|cccc|", 
                        digits = 3)

table.addtorow          <- list()
table.addtorow$pos      <- list()
table.addtorow$pos[[1]] <- c(0)
table.addtorow$pos[[2]] <- c(0)
table.addtorow$pos[[3]] <- c(6)
table.addtorow$pos[[4]] <- c(6)
table.addtorow$pos[[5]] <- c(6)
table.addtorow$pos[[6]] <- c(12)
table.addtorow$pos[[7]] <- c(12)
table.addtorow$pos[[8]] <- c(12)
table.addtorow$command  <- c("\\hline \n",
                             "\\multicolumn{5}{c}{{RMSE}}\\\\ \n", 
                             "\\hline \n",
                             "\\multicolumn{5}{c}{{LPS}}\\\\ \n",
                             "\\hline \n",
                             "\\hline \n",
                             "\\multicolumn{5}{c}{{Hit Rate}}\\\\ \n", 
                             "\\hline \n")

print.xtable(xt_all_tables, include.rownames = FALSE, 
             add.to.row = table.addtorow)
