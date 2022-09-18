#####
# plotting various stuff for paper

library(dplyr)
library(ggplot2)
library(tidyverse)
require(xtable)

## load data processed in previous step

model <- "NG_PVAR"
small <- c(FALSE)
trans <- c("lvl")
em_data <- "ESSD_intensity"
year_end <- 2018
adjust <- TRUE


output_folder <- paste0("./02_output/01_plots/", model, "_", em_data, 
                        "_small", small, "_adjusted", adjust)
dir.create(output_folder, showWarnings = FALSE)


load(file = paste0("./02_output/09_results/", 
                   model, "/", em_data, "_small", small,  
                   "_adjusted", adjust, ".Rda"))


df_plot_int <- c()
for(cc in cN_full) {
  for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
    temp_quant <- list_int_quants[[j]][[cc]]
    if(is.null(temp_quant)) next
    pos_y <- which(colnames(temp_quant) == year_end)
    pos_x <- nrow(temp_quant) %/% 2 + 1
    temp_quant[-pos_x, 1:pos_y] <- NA
    temp <- cbind("year" = names(temp_quant), t(temp_quant))
    temp <- as.data.frame(as.matrix(temp))
    temp <- cbind("iso3c" = cc, year = colnames(temp_quant), "sector" = j, temp)
    df_plot_int <- rbind(df_plot_int, temp)
  }
}

df_plot_int$year <- as.numeric(df_plot_int$year)
df_plot_int$sector <- factor(df_plot_int$sector, levels = c("total", "transport", 
                                                            "agriculture", "industry", 
                                                            "buildings", "energy"))
levels(df_plot_int$sector) <- c("Total Emissions Intensity", "Transport Emissions Intensity",
                                "Agriculture Emissions Intensity", "Industry Emissions Intensity", 
                                "Buildings Emissions Intensity", "Energy Emissions Intensity")

df_plot_int[which(df_plot_int$year <= year_end), c("5%","16%","84%","95%")] <- NA



df_plot_em <- c()
for(cc in cN_full) {
  for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
    temp_quant <- list_em_quants[[j]][[cc]]
    if(is.null(temp_quant)) next
    pos_y <- which(colnames(temp_quant) == year_end)
    pos_x <- nrow(temp_quant) %/% 2 + 1
    temp_quant[-pos_x, 1:pos_y] <- NA
    temp <- cbind("year" = names(temp_quant), t(temp_quant))
    temp <- as.data.frame(as.matrix(temp))
    temp <- cbind("iso3c" = cc, year = colnames(temp_quant), "sector" = j, temp)
    df_plot_em <- rbind(df_plot_em, temp)
  }
}

df_plot_em$year <- as.numeric(df_plot_em$year)
df_plot_em$sector <- factor(df_plot_em$sector, levels = c("total", "transport", 
                                                          "agriculture", "industry", 
                                                          "buildings", "energy"))
levels(df_plot_em$sector) <- c("Total Emissions", "Transport Emissions",
                               "Agriculture Emissions", "Industry Emissions", 
                               "Buildings Emissions", "Energy Emissions")

plot_totals <- c()
for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
  temp_quant <- list_em_sectors_quants[[j]]
  if(is.null(temp_quant)) next
  pos_y <- which(colnames(temp_quant) == year_end)
  pos_x <- nrow(temp_quant) %/% 2 + 1
  temp_quant[-pos_x, 1:pos_y] <- NA
  temp <- cbind("year" = names(temp_quant), t(temp_quant))
  temp <- as.data.frame(as.matrix(temp))
  temp <- cbind(year = colnames(temp_quant), "sector" = j, temp)
  plot_totals <- rbind(plot_totals, temp)
}
plot_totals$year <- as.numeric(plot_totals$year)

plot_totals$sector <- factor(plot_totals$sector, levels = c("total", "transport", 
                                                            "agriculture", "industry", 
                                                            "buildings", "energy"))
levels(plot_totals$sector) <- c("Total Emissions","Transport Emissions",
                                "Agriculture Emissions", "Industry Emissions", 
                                "Buildings Emissions", "Energy Emissions")



# #####
# # Country results
# 
# pdf(file = paste0(output_folder, "/emissions_all_cN.pdf"),
#     onefile = TRUE, width = 10, height = 6)
# ggplot(data = plot_totals) +
#   geom_ribbon(aes(x = year, ymin = (`50%` / 10^7), ymax = (`84%`/10^7)),
#               fill = "#FF0000", alpha = 0.4) +
#   geom_ribbon(aes(x = year, ymax = (`50%` / 10^7), ymin = (`16%`/10^7)),
#               fill = "#FF0000", alpha = 0.4) +
#   geom_ribbon(aes(x = year, ymin = (`84%` / 10^7), ymax = (`95%`/10^7)),
#               fill = "#FF0000", alpha = 0.2) +
#   geom_ribbon(aes(x = year, ymax = (`16%` / 10^7), ymin = (`5%`/10^7)),
#               fill = "#FF0000", alpha = 0.2) +
#   geom_line(aes(x = year, y = (`50%` / 10^7))) +
#   facet_wrap(.~sector, scales = "free_y") +
#   theme_bw() +
#   labs(x = "", y = "Sectoral emissions (Gigatons CO2eq)") +
#   ggtitle(paste0("Global Sectoral Emissions")) +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         plot.title = element_text(size = 20),
#         legend.key.width = unit(3, "cm"),
#         plot.caption = element_text(hjust = 0, size = 14),
#         legend.text = element_text(size = 14),
#         axis.title = element_text(size = 14),
#         axis.text = element_text(size = 12),
#         strip.text = element_text(size = 12))
# for(i in unique(df_plot_em$iso3c)) {
#   p <- ggplot(data = df_plot_em %>% filter(iso3c == i)) +
#     geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)),
#                 fill = "#FF0000", alpha = 0.4) +
#     geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)),
#                 fill = "#FF0000", alpha = 0.4) +
#     geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
#                 fill = "#FF0000", alpha = 0.2) +
#     geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
#                 fill = "#FF0000", alpha = 0.2) +
#     geom_line(aes(x = year, y = (`50%` / 10^4))) +
#     facet_wrap(.~sector, scales = "free_y") +
#     theme_bw() +
#     scale_y_continuous(trans="log10") +
#     labs(x = "", y = "Sectoral emissions (Megatons CO2eq)") +
#     ggtitle(paste0(i, " Sectoral Emissions")) +
#     theme(legend.position = "bottom",
#           legend.title = element_blank(),
#           plot.title = element_text(size = 20),
#           legend.key.width = unit(3, "cm"),
#           plot.caption = element_text(hjust = 0, size = 14),
#           legend.text = element_text(size = 14),
#           axis.title = element_text(size = 14),
#           axis.text = element_text(size = 12),
#           strip.text = element_text(size = 12))
#   print(p)
# }
# dev.off()
# 
# 
# pdf(file = paste0(output_folder, "/intensities_all_cN.pdf"),
#     onefile = TRUE, width = 10, height = 6)
# for(i in unique(df_plot_int$iso3c)) {
#   p <- ggplot(data = df_plot_int %>% filter(iso3c == i)) +
#     geom_ribbon(aes(x = year, ymin = (`50%`), ymax = (`84%`)),
#                 fill = "#FF0000", alpha = 0.4) +
#     geom_ribbon(aes(x = year, ymax = (`50%`), ymin = (`16%`)),
#                 fill = "#FF0000", alpha = 0.4) +
#     geom_ribbon(aes(x = year, ymin = (`84%`), ymax = (`95%`)),
#                 fill = "#FF0000", alpha = 0.2) +
#     geom_ribbon(aes(x = year, ymax = (`16%`), ymin = (`5%`)),
#                 fill = "#FF0000", alpha = 0.2) +
#     geom_line(aes(x = year, y = (`50%`))) +
#     facet_wrap(.~sector, scales = "free_y") +
#     theme_bw() +
#     scale_y_continuous(trans="log10") +
#     labs(x = "", y = "Sectoral emissions intensities (tons / 10k GDP)") +
#     ggtitle(paste0(i, " Sectoral Emissions Intensities")) +
#     theme(legend.position = "bottom",
#           legend.title = element_blank(),
#           plot.title = element_text(size = 20),
#           legend.key.width = unit(3, "cm"),
#           plot.caption = element_text(hjust = 0, size = 14),
#           legend.text = element_text(size = 14),
#           axis.title = element_text(size = 14),
#           axis.text = element_text(size = 12),
#           strip.text = element_text(size = 12))
#   print(p)
# }
# dev.off()


#####
# Preparations for regional plots

# Income status from WB
regions_income <- WDI::WDI_data$country %>% 
  as.data.frame() %>% 
  select(iso3c, country, region, income) %>% 
  filter(iso3c %in% unique(df_plot_int$iso3c))


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


#####
# Creating plots for various regional results


# Global plot
p_fcast_global <- ggplot(data = plot_totals) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^7), ymax = (`84%`/10^7)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^7), ymin = (`16%`/10^7)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^7), ymax = (`95%`/10^7)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^7), ymin = (`5%`/10^7)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^7))) + 
  facet_wrap(.~sector, scales = "free_y") + 
  theme_bw() + 
  labs(x = "", y = "Sectoral emissions (Gigatons CO2eq)") + 
  ggtitle(paste0("Global Sectoral Emissions")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))



# Plot with AR5 regions
df_plot_em_regions <- c()
for(rr in unique(regions_income$region_ar5)) {
  for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
    temp_quant <- list_em_regions_quants[[rr]][[j]]
    if(is.null(temp_quant)) next
    pos_y <- which(colnames(temp_quant) == year_end)
    pos_x <- nrow(temp_quant) %/% 2 + 1
    temp_quant[-pos_x, 1:pos_y] <- NA
    temp <- cbind("year" = names(temp_quant), t(temp_quant))
    temp <- as.data.frame(as.matrix(temp))
    temp <- cbind("region" = rr, year = colnames(temp_quant), "sector" = j, temp)
    df_plot_em_regions <- rbind(df_plot_em_regions, temp)
  }
}

df_plot_em_regions$year <- as.numeric(df_plot_em_regions$year)
df_plot_em_regions$sector <- factor(df_plot_em_regions$sector, levels = c("total", "transport", 
                                                                          "agriculture", "industry", 
                                                                          "buildings", "energy"))
levels(df_plot_em_regions$sector) <- c("Total Emissions", "Transport Emissions",
                                       "Agriculture Emissions", "Industry Emissions", 
                                       "Buildings Emissions", "Energy Emissions")

plot_regions_all <- plot_totals %>% 
  mutate(region = " World", .before = year) %>% 
  rbind(df_plot_em_regions)

p_fcast_regions <- ggplot(data = plot_regions_all %>% 
                           filter(sector == "Total Emissions", year > 1979)) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^7), ymax = (`84%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^7), ymin = (`16%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^7), ymax = (`95%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^7), ymin = (`5%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  scale_fill_manual(values = c("#FF0000", "#FF0000"), guide = "none") +
  geom_line(aes(x = year, y = (`50%` / 10^7)), 
            color = "#000000", size = 1) + 
  facet_wrap(.~region, scales = "free") +
  theme_bw() + 
  # scale_y_continuous(trans="log10") +
  labs(x = "", y = "Total GHG emissions (Gigatons CO2eq)") + 
  ggtitle(paste0("Emissions forecasts by AR5 regions")) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 14), 
        legend.title.align = 0.5,
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Plot with income distinction
df_plot_em_income <- c()
for(rr in unique(regions_income$income)) {
  for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
    temp_quant <- list_em_income_quants[[rr]][[j]]
    if(is.null(temp_quant)) next
    pos_y <- which(colnames(temp_quant) == year_end)
    pos_x <- nrow(temp_quant) %/% 2 + 1
    temp_quant[-pos_x, 1:pos_y] <- NA
    temp <- cbind("year" = names(temp_quant), t(temp_quant))
    temp <- as.data.frame(as.matrix(temp))
    temp <- cbind("income" = rr, year = colnames(temp_quant), "sector" = j, temp)
    df_plot_em_income <- rbind(df_plot_em_income, temp)
  }
}

df_plot_em_income$year <- as.numeric(df_plot_em_income$year)
df_plot_em_income$sector <- factor(df_plot_em_income$sector, levels = c("total", "transport", 
                                                                          "agriculture", "industry", 
                                                                          "buildings", "energy"))
levels(df_plot_em_income$sector) <- c("Total Emissions", "Transport Emissions",
                                       "Agriculture Emissions", "Industry Emissions", 
                                       "Buildings Emissions", "Energy Emissions")


df_plot_em_income$income <- factor(df_plot_em_income$income, 
                                   levels = c("High income", 
                                              "Upper middle income",
                                              "Lower middle income", 
                                              "Low income"))



p_fcast_income <- ggplot(data = df_plot_em_income %>% 
                           filter(sector == "Total Emissions", year > 1979)) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^7), ymax = (`84%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^7), ymin = (`16%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^7), ymax = (`95%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^7), ymin = (`5%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  scale_fill_manual(values = c("#FF0000", "#FF0000"), guide = "none") +
  geom_line(aes(x = year, y = (`50%` / 10^7)), 
            color = "#000000", size = 1) + 
  facet_wrap(.~income, scales = "free") +
  theme_bw() + 
  labs(x = "", y = "Total GHG emissions (Gigatons CO2eq)") + 
  ggtitle(paste0("Emissions forecasts by income classification")) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 14), 
        legend.title.align = 0.5,
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Plot with income and oil distinction
df_plot_em_oil_income <- c()
for(rr in unique(regions_income$oil)) {
  for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
    temp_quant <- list_em_oil_income_quants[[rr]][[j]]
    if(is.null(temp_quant)) next
    pos_y <- which(colnames(temp_quant) == year_end)
    pos_x <- nrow(temp_quant) %/% 2 + 1
    temp_quant[-pos_x, 1:pos_y] <- NA
    temp <- cbind("year" = names(temp_quant), t(temp_quant))
    temp <- as.data.frame(as.matrix(temp))
    temp <- cbind("oil" = rr, year = 1980:2050, "sector" = j, temp)
    df_plot_em_oil_income <- rbind(df_plot_em_oil_income, temp)
  }
}

df_plot_em_oil_income$year <- as.numeric(df_plot_em_oil_income$year)
df_plot_em_oil_income$sector <- factor(df_plot_em_oil_income$sector, levels = c("total", "transport", 
                                                                        "agriculture", "industry", 
                                                                        "buildings", "energy"))
levels(df_plot_em_oil_income$sector) <- c("Total Emissions", "Transport Emissions",
                                      "Agriculture Emissions", "Industry Emissions", 
                                      "Buildings Emissions", "Energy Emissions")

df_plot_em_oil_income$oil <- factor(df_plot_em_oil_income$oil, levels = c("oil_import", "oil_export"))
levels(df_plot_em_oil_income$oil) <- c("Oil importing countries", "Oil exporting countries")

p_comp_oil_income <- ggplot(data = df_plot_em_oil_income %>% 
                           filter(year > 1979)) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^7), ymax = (`84%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^7), ymin = (`16%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^7), ymax = (`95%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^7), ymin = (`5%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  scale_fill_manual(values = c("#FF0000", "#FF0000"), guide = "none") +
  geom_line(aes(x = year, y = (`50%` / 10^7)), 
            color = "#000000", size = 1) + 
  facet_wrap(.~oil+sector, scales = "free_y", ncol = 3) +
  theme_bw() + 
  #scale_y_continuous(trans="log10") +
  labs(x = "", y = "Total GHG emissions (Gigatons CO2eq)") + 
  ggtitle(paste0("Emissions forecasts for high income countries")) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 14), 
        legend.title.align = 0.5,
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Comparison of forecasts to IAM results

IAM_SSP2_GHG_data <- xlsx::read.xlsx("./01_data/SSP2_IAM_GHG_gases.xlsx", 
                                     sheetIndex = 1)

IAM_SSP2_LUC_data <- xlsx::read.xlsx("./01_data/SSP2_IAM_CO2_land_use.xlsx", 
                                     sheetIndex = 1)

IAM_SSP2_baseline_GHG <- IAM_SSP2_GHG_data %>% 
  filter(Scenario %in% c("SSP2-Baseline")) %>% 
  select(-Scenario, -Variable, -Unit, -X2005,
         -X2060, -X2070, -X2080, -X2090, -X2100, -Notes) %>% 
  rename(`2010` = X2010, `2020` = X2020, `2030` = X2030, 
         `2040` = X2040, `2050` = X2050) %>% 
  pivot_longer(cols = `2010`:`2050`) %>% 
  rename(model = Model, region = Region, year = name, GHG = value) %>% 
  mutate(year = as.numeric(as.character(year)))

IAM_SSP2_baseline_LUC <- IAM_SSP2_LUC_data %>% 
  filter(Scenario %in% c("SSP2-Baseline")) %>% 
  select(-Scenario, -Variable, -Unit, -X2005,
         -X2060, -X2070, -X2080, -X2090, -X2100, -Notes) %>% 
  rename(`2010` = X2010, `2020` = X2020, `2030` = X2030, 
         `2040` = X2040, `2050` = X2050) %>% 
  pivot_longer(cols = `2010`:`2050`) %>% 
  rename(model = Model, region = Region, year = name, LUC = value) %>% 
  mutate(year = as.numeric(as.character(year)))

IAM_grid <- expand.grid(unique(IAM_SSP2_baseline_GHG$model), 
                        unique(IAM_SSP2_baseline_GHG$region), 
                        2010:2050) %>% 
  rename(model = Var1, region = Var2, year = Var3)

scale_factor <- plot_totals[which(plot_totals$year == 2018 &
                                    plot_totals$sector == "Total Emissions"), "50%"]

IAM_SSP2_baseline_GHG <- IAM_SSP2_baseline_GHG %>% 
  left_join(IAM_SSP2_baseline_LUC) %>% 
  mutate(GHG = GHG - LUC) %>% 
  select(-LUC) %>% 
  right_join(IAM_grid) %>% 
  arrange(model, region, year) %>% 
  group_by(model, region) %>% 
  mutate(GHG = zoo::na.approx(GHG, na.rm = FALSE) * 10^4,
         region = replace(region, region == "R5.2ASIA", "Asia"),
         region = replace(region, region == "R5.2LAM", "Latin America and Caribbean"),
         region = replace(region, region == "R5.2MAF", "Middle East and Africa"),
         region = replace(region, region == "R5.2OECD", "OECD"),
         region = replace(region, region == "R5.2REF", "REF"))

scale_factor_regions <- df_plot_em_regions %>% 
  filter(year == 2018, sector == "Total Emissions") %>% 
  select(region, year, `50%`) %>% 
  rename(scale = `50%`)

df_plot_em_regions_global <- plot_totals %>% 
  mutate(region = " World", .before = year) %>% 
  rbind(df_plot_em_regions)

scale_factor_all <- data.frame("region" = " World", "year" = 2018, "scale" = scale_factor) %>% 
  rbind(scale_factor_regions)


p_IAM_fcast_comp_all <- ggplot(data = df_plot_em_regions_global %>% filter(sector == "Total Emissions", year > 1999)) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^7), ymax = (`84%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^7), ymin = (`16%`/10^7), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^7), ymax = (`95%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^7), ymin = (`5%`/10^7), fill = "90% interval"),
              alpha = 0.2) +
  scale_fill_manual(values = c("#FF0000", "#FF0000"), guide = "none") +
  geom_line(aes(x = year, y = (`50%` / 10^7)), 
            color = "#000000", size = 1) + 
  geom_line(data = IAM_SSP2_baseline_GHG %>% 
              mutate(region = replace(region, region == "World", " World")) %>% 
              left_join(scale_factor_all) %>% 
              ungroup() %>% 
              group_by(model, region) %>% 
              mutate(GHG = GHG / (GHG[year == 2018] / scale[year == 2018]))
            , 
            aes(x = year, y = GHG / 10^7, group = model, col = model), 
            size = 1) + 
  facet_wrap(.~region, scales = "free") +
  theme_bw() + 
  labs(x = "", y = "Total GHG emissions (Gigatons CO2eq)") + 
  ggtitle(paste0("Emissions forecasts compared to IAM scenarios")) +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 14), 
        legend.title.align = 0.5,
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 12), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12)) + 
  guides(color = guide_legend(title = c("IAM models"), nrow = 2, 
                              title.position = "top"), 
         reverse = TRUE)


#####
# Country-specific Plots 

# Plot with selected countries in main paper
df_plot_em_sub_main <- df_plot_em %>% 
  filter((iso3c == "CHN" & sector %in% c("Energy Emissions", "Industry Emissions")) | 
         (iso3c == "USA" & sector %in% c("Transport Emissions", "Agriculture Emissions")) | 
         (iso3c == "IND" & sector %in% c("Industry Emissions")) |
         (iso3c == "DEU" & sector %in% c("Transport Emissions")) | 
         (iso3c == "KOR" & sector %in% c("Energy Emissions")) |
         (iso3c == "BRA" & sector %in% c("Agriculture Emissions")) | 
         (iso3c == "ZAF" & sector %in% c("Energy Emissions"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         cN = replace(cN, cN == "China", " China"), 
         cN = replace(cN, cN == "United States", " United States"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions")) 

df_plot_int_sub_main <- df_plot_int %>% 
  filter((iso3c == "CHN" & sector %in% c("Energy Emissions Intensity", 
                                         "Industry Emissions Intensity")) |
         (iso3c == "USA" & sector %in% c("Transport Emissions Intensity", 
                                         "Agriculture Emissions Intensity")) | 
         (iso3c == "IND" & sector %in% c("Industry Emissions Intensity")) | 
         (iso3c == "DEU" & sector %in% c("Transport Emissions Intensity")) | 
         (iso3c == "KOR" & sector %in% c("Energy Emissions Intensity")) |
         (iso3c == "BRA" & sector %in% c("Agriculture Emissions Intensity")) | 
         (iso3c == "ZAF" & sector %in% c("Energy Emissions Intensity"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         cN = replace(cN, cN == "China", " China"), 
         cN = replace(cN, cN == "United States", " United States"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions Intensity")) 


p_select_1 <- ggplot(data = df_plot_int_sub_main) + 
  geom_ribbon(aes(x = year, ymin = (`50%`), ymax = (`84%`)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%`), ymin = (`16%`)), 
              fill = "#FF0000", alpha = 0.4) +
  geom_ribbon(aes(x = year, ymin = (`84%`), ymax = (`95%`)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%`), ymin = (`5%`)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%`))) + 
  facet_wrap(.~iso_sector, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Sectoral emissions intensities (tons / 10k GDP)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))

p_select_2 <- ggplot(data = df_plot_em_sub_main) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^4))) + 
  facet_wrap(.~iso_sector, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Sectoral emissions (Megatons CO2eq)") + 
  # ggtitle(paste0(i, " Sectoral Emissions")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))

# Plot for energy sector
df_plot_em_sub_energy <- df_plot_em %>% 
  filter((iso3c %in% c("IND", "IRN", "GHA", "USA", "DEU", "GBR", "AUS", "MEX", "ISR") & 
            sector == c("Energy Emissions"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         cN = replace(cN, cN == "Australia", " Australia"), 
         cN = replace(cN, cN == "Mexico", " Mexico"), 
         cN = replace(cN, cN == "Israel", " Israel"), 
         cN = replace(cN, cN == "United States", "  United States"), 
         cN = replace(cN, cN == "Germany", "  Germany"), 
         cN = replace(cN, cN == "United Kingdom", "  United Kingdom"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions")) 

p_select_energy <- ggplot(data = df_plot_em_sub_energy) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^4))) + 
  facet_wrap(.~cN, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Sectoral emissions (Megatons CO2eq)") + 
  # ggtitle(paste0("Energy Emissions for Selected Countries")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))

# Plot for transport sector
df_plot_em_sub_trans <- df_plot_em %>% 
  filter((iso3c == "DNK" & sector %in% c("Total Emissions", "Transport Emissions")) | 
           (iso3c == "BGR" & sector %in% c("Total Emissions", "Transport Emissions")) | 
           (iso3c == "CZE" & sector %in% c("Total Emissions", "Transport Emissions"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions"), 
         iso_sector = replace(iso_sector, iso_sector == "Bulgaria - Total", 
                              " Bulgaria - Total"), 
         iso_sector = replace(iso_sector, iso_sector == "Czechia - Total", 
                              " Czechia - Total"), 
         iso_sector = replace(iso_sector, iso_sector == "Denmark - Total", 
                              " Denmark - Total")) 

p_select_transport <- ggplot(data = df_plot_em_sub_trans) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^4))) + 
  facet_wrap(.~iso_sector, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Sectoral emissions (Megatons CO2eq)") + 
  # ggtitle(paste0(" Emissions for Selected Countries")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Plot with reducing countries
df_plot_em_sub_reducer <- df_plot_em %>% 
  filter((iso3c %in% c("CHE", "LUX", "CUB", "AUS", "POL", "TWN") & 
            sector %in% c("Total Emissions"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions")) 

p_select_reducer <- ggplot(data = df_plot_em_sub_reducer) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^4))) + 
  facet_wrap(.~iso_sector, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Total emissions (Megatons CO2eq)") + 
  # ggtitle(paste0(" Emissions for Selected Countries")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Plot with increasing countries
df_plot_em_sub_increaser <- df_plot_em %>% 
  filter((iso3c %in% c("PRT", "GRC", "HUN", "CYP", "JAM", "MKD") & 
            sector %in% c("Total Emissions"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions")) 

p_select_increaser <- ggplot(data = df_plot_em_sub_increaser) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^4))) + 
  facet_wrap(.~iso_sector, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Total emissions (Megatons CO2eq)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Plot for Russia
p_emissions_RUS <- ggplot(data = df_plot_em %>% filter(iso3c == "RUS")) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^4))) + 
  facet_wrap(.~sector, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Sectoral emissions (Megatons CO2eq)") + 
  ggtitle(paste0("Russia", " Sectoral Emissions")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Plot for Ukraine
p_emissions_UKR <- ggplot(data = df_plot_em %>% filter(iso3c == "UKR")) + 
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^4), ymax = (`84%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^4), ymin = (`16%`/10^4)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^4), ymax = (`95%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^4), ymin = (`5%`/10^4)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^4))) + 
  facet_wrap(.~sector, scales = "free_y") + 
  theme_bw() + 
  scale_y_continuous(trans="log10") +
  labs(x = "", y = "Sectoral emissions (Megatons CO2eq)") + 
  ggtitle(paste0("Ukraine", " Sectoral Emissions")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


# Outputting all plots to PDF

### Figures main text

# Figure 1
pdf(file = paste0(output_folder, "/Fig1-comp_fcast_global.pdf"), 
    onefile = TRUE, width = 10, height = 6)
p_fcast_global
dev.off()

# Figure 2
pdf(file = paste0(output_folder, "/Fig2-comp_fcast_regions.pdf"), 
    width = 10, height = 6)
p_fcast_regions
dev.off()

# Figure 3
pdf(file = paste0(output_folder, "/Fig3-comp_fcast_selected_intensity.pdf"), 
    onefile = TRUE, width = 10, height = 7)
p_select_1
dev.off()

# Figure 4
pdf(file = paste0(output_folder, "/Fig4-comp_fcast_selected_emissions.pdf"), 
    onefile = TRUE, width = 10, height = 7)
p_select_2
dev.off()

# Figure 5
pdf(file = paste0(output_folder, "/Fig5-comp_fcast_IAM.pdf"),
    width = 10, height = 7.5)
p_IAM_fcast_comp_all
dev.off()



### Figures supplementary material

# Figure A1
pdf(file = paste0(output_folder, "/FigA1-comp_fcast_income.pdf"),
    width = 10, height = 6)
p_fcast_income
dev.off()

# Figure A2
pdf(file = paste0(output_folder, "/FigA2-comp_fcast_oil_income.pdf"),
    width = 10, height = 9)
p_comp_oil_income
dev.off()


# Figure B1
pdf(file = paste0(output_folder, "/FigB1-comp_fcast_energy.pdf"),
    onefile = TRUE, width = 10, height = 7.5)
p_select_energy
dev.off()

# Figure B2
pdf(file = paste0(output_folder, "/FigB2-comp_fcast_transport.pdf"), 
    onefile = TRUE, width = 10, height = 6)
p_select_transport
dev.off()

# Figure B3
pdf(file = paste0(output_folder, "/FigB3-comp_fcast_RUS.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_emissions_RUS
dev.off()

# Figure B4
pdf(file = paste0(output_folder, "/FigB4-comp_fcast_UKR.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_emissions_UKR
dev.off()

# Figure B5
pdf(file = paste0(output_folder, "/FigB5-comp_fcast_reducer.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_select_reducer
dev.off()

# Figure B6
pdf(file = paste0(output_folder, "/FigB6-comp_fcast_increaser.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_select_increaser
dev.off()


#####
# Table for top-15 emitting sectors

data_hist <- df_plot_em %>% 
  filter(year == 2018, sector != "Total Emissions") %>% 
  slice_max(`50%`, n = 15) %>% 
  select(iso3c, sector, `50%`) %>% 
  rename(`2018` = `50%`) %>% 
  mutate(`2018` = round(`2018` / 10^7, 2), 
         sector = as.character(sector))
data_hist$sector <- unlist(lapply(str_split(data_hist$sector, " "), function(x) x[1]))
data_hist <- data_hist %>% 
  mutate(temp = NA) %>% 
  mutate(Rank = c(1:15), .before = iso3c) %>% 
  pivot_longer(`2018`:temp) %>% 
  mutate(iso3c = replace(iso3c, name == "temp", NA),
         Rank = replace(Rank, name == "temp", NA),
         sector = replace(sector, name == "temp", NA)) %>% 
  select(-name) %>% 
  rename(`2018` = value)
names(data_hist) <- c("Rank", "Country (ISO)", "Sector", "2018")

data_proj <- df_plot_em %>% 
  filter(year == 2050, sector != "Total Emissions") %>% 
  slice_max(`50%`, n = 15) %>% 
  select(iso3c, sector, `16%`, `50%`, `84%`) %>% 
  rename(`2050` = `50%`) %>% 
  mutate(`2050` = as.character(round(`2050` / 10^7, 2)), 
         `conf` = paste0("(", round(`16%` / 10^7, 2), "-", round(`84%` / 10^7, 2), ")"),
         sector = as.character(sector)) %>% 
  select(-`16%`, -`84%`)
data_proj$sector <- unlist(lapply(str_split(data_proj$sector, " "), function(x) x[1]))
data_proj <- data_proj %>% 
  pivot_longer(cols = `2050`:conf) %>% 
  mutate(iso3c = replace(iso3c, name == "conf", NA),
         sector = replace(sector, name == "conf", NA)) %>% 
  select(-name) %>% 
  rename(`2050` = value)
names(data_proj) <- c("Country (ISO) ", "Sector ", "2050")

table_em <- cbind(data_hist, data_proj)

xt_table_em <- xtable(table_em)
print.xtable(xt_table_em, include.rownames = FALSE)

