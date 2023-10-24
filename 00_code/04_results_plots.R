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

cN_full <- names(list_int_quants$agriculture)

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


#####
# Country results

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
  filter(iso3c %in% unique(df_plot_int$iso3c)) %>% 
  mutate(income = replace(income, iso3c == "VEN", "Upper middle income"))
  # Venezuela is rated as "Not classified" in newest version of WB ranking
  # Changed it manually to the last rating it had before to avoid it being placed separately


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

# Taking 10 IPCC AR6 regions for region split
regions_IPCC <- openxlsx::read.xlsx("./01_data/essd_ghg_data_gwp100.xlsx", 
                                    sheet = 5)
regions_IPCC <- regions_IPCC[, c("ISO", "region_ar6_10")]
names(regions_IPCC) <- c("iso3c", "region_ipcc")
regions_income <- left_join(regions_income, regions_IPCC) 
regions_income <- regions_income %>% 
  mutate(region_ipcc = replace(region_ipcc, 
                               region_ipcc == "South-East Asia and developing Pacific", 
                               "South-East Asia"))

# Create 9 regions out of the AR6 regions for better fit of plots 
regions_income <- regions_income %>% 
  mutate(region_ipcc_9 = region_ipcc, 
         region_ipcc_9 = replace(region_ipcc_9, region_ipcc_9 == "Europe", "Europe & Eurasia"),
         region_ipcc_9 = replace(region_ipcc_9, region_ipcc_9 == "Eurasia", "Europe & Eurasia"))

# add population figures for per capita plots
load("./01_data/processed/macro_data.Rda")
regions_macro <- macro_data %>% 
  filter(iso3c %in% df_plot_int$iso3c) %>% 
  select(iso3c, year, pop, GDP.PPP) %>% 
  left_join(regions_income, by = c("iso3c"))

macro_ipcc_regions <- regions_macro %>% 
  group_by(year, region_ipcc) %>% 
  summarise(pop = sum(pop, na.rm = T), 
            gdp = sum(GDP.PPP, na.rm = T))

macro_ipcc_regions_9 <- regions_macro %>% 
  group_by(year, region_ipcc_9) %>% 
  summarise(pop = sum(pop, na.rm = T), 
            gdp = sum(GDP.PPP, na.rm = T))


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
            color = "#000000") + 
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

# 10 IPCC regions
df_plot_em_ipcc <- c()
for(rr in unique(regions_income$region_ipcc)) {
  for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
    temp_quant <- list_em_ipcc_quants[[rr]][[j]]
    if(is.null(temp_quant)) next
    pos_y <- which(colnames(temp_quant) == year_end)
    pos_x <- nrow(temp_quant) %/% 2 + 1
    temp_quant[-pos_x, 1:pos_y] <- NA
    temp <- cbind("year" = names(temp_quant), t(temp_quant))
    temp <- as.data.frame(as.matrix(temp))
    temp <- cbind("region_ipcc" = rr, year = colnames(temp_quant), "sector" = j, temp)
    df_plot_em_ipcc <- rbind(df_plot_em_ipcc, temp)
  }
}

df_plot_em_ipcc$year <- as.numeric(df_plot_em_ipcc$year)
df_plot_em_ipcc$sector <- factor(df_plot_em_ipcc$sector, levels = c("total", "transport", 
                                                                    "agriculture", "industry", 
                                                                    "buildings", "energy"))
levels(df_plot_em_ipcc$sector) <- c("Total Emissions", "Transport Emissions",
                                    "Agriculture Emissions", "Industry Emissions", 
                                    "Buildings Emissions", "Energy Emissions")

# 9 IPCC regions, Europe + Eurasia grouped as one for expository reasons
df_plot_em_ipcc_9 <- c()
for(rr in unique(regions_income$region_ipcc_9)) {
  for(j in c("total", "transport", "agriculture", "industry", "buildings", "energy")) {
    temp_quant <- list_em_ipcc_9_quants[[rr]][[j]]
    if(is.null(temp_quant)) next
    pos_y <- which(colnames(temp_quant) == year_end)
    pos_x <- nrow(temp_quant) %/% 2 + 1
    temp_quant[-pos_x, 1:pos_y] <- NA
    temp <- cbind("year" = names(temp_quant), t(temp_quant))
    temp <- as.data.frame(as.matrix(temp))
    temp <- cbind("region_ipcc_9" = rr, year = colnames(temp_quant), "sector" = j, temp)
    df_plot_em_ipcc_9 <- rbind(df_plot_em_ipcc_9, temp)
  }
}

df_plot_em_ipcc_9$year <- as.numeric(df_plot_em_ipcc_9$year)
df_plot_em_ipcc_9$sector <- factor(df_plot_em_ipcc_9$sector, levels = c("total", "transport", 
                                                                    "agriculture", "industry", 
                                                                    "buildings", "energy"))
levels(df_plot_em_ipcc_9$sector) <- c("Total Emissions", "Transport Emissions",
                                    "Agriculture Emissions", "Industry Emissions", 
                                    "Buildings Emissions", "Energy Emissions")


p_fcast_ipcc_9 <- ggplot(data = df_plot_em_ipcc_9 %>% 
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
            color = "#000000") + 
  facet_wrap(.~region_ipcc_9, scales = "free") +
  theme_bw() + 
  # scale_y_continuous(trans="log10") +
  labs(x = "", y = "Aggregate GHG emissions (Gigatons CO2eq)") + 
  ggtitle(paste0("Aggregate emissions by IPCC regions")) +
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

# intensity plot
df_plot_em_ipcc_9_int <- df_plot_em_ipcc_9 %>% 
  left_join(macro_ipcc_regions_9, by = c("year", "region_ipcc_9")) %>% 
  mutate(`50%` = `50%`/ gdp * 10^6, 
         `5%` = `5%`/ gdp * 10^6,
         `16%` = `16%`/ gdp * 10^6,
         `84%` = `84%`/ gdp * 10^6, 
         `95%` = `95%`/ gdp * 10^6,)

p_fcast_ipcc_9_int <- ggplot(data = df_plot_em_ipcc_9_int %>% 
                             filter(sector == "Total Emissions", year > 1979)) + 
  geom_ribbon(aes(x = year, ymin = (`50%`), ymax = (`84%`), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%`), ymin = (`16%`), fill = "66% interval"), 
              alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%`), ymax = (`95%`), fill = "90% interval"),
              alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%`), ymin = (`5%`), fill = "90% interval"),
              alpha = 0.2) +
  scale_fill_manual(values = c("#FF0000", "#FF0000"), guide = "none") +
  geom_line(aes(x = year, y = (`50%`)), 
            color = "#000000") + 
  facet_wrap(.~region_ipcc_9, scales = "free") +
  theme_bw() + 
  labs(x = "", y = "Emissions intensity (ton CO2eq / 10k GDP)") + 
  ggtitle(paste0("Emissions intensities by IPCC regions")) +
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
            color = "#000000") + 
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
            color = "#000000") + 
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


p_IAM_fcast_comp_all <- ggplot(data = df_plot_em_regions_global %>% 
                                 filter(sector == "Total Emissions", year > 1999)) + 
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
            color = "#000000", size = 0.75) + 
  geom_line(data = IAM_SSP2_baseline_GHG %>% 
              mutate(region = replace(region, region == "World", " World")) %>% 
              left_join(scale_factor_all) %>% 
              ungroup() %>% 
              group_by(model, region) %>% 
              mutate(GHG = GHG / (GHG[year == 2018] / scale[year == 2018]))
            , 
            aes(x = year, y = GHG / 10^7, group = model, col = model), 
            size = 0.75) + 
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

# Expository plot, Figure 1 in paper
df_plot_em_sub_main <- df_plot_em %>% 
  filter((iso3c == "CHN" & sector %in% c("Energy Emissions")) | 
           (iso3c == "USA" & sector %in% c("Transport Emissions")) | 
           (iso3c == "IND" & sector %in% c("Industry Emissions"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions"), 
         l_y = "A") 

df_plot_int_sub_main <- df_plot_int %>% 
  filter((iso3c == "CHN" & sector %in% c("Energy Emissions Intensity")) |
           (iso3c == "USA" & sector %in% c("Transport Emissions Intensity")) | 
           (iso3c == "IND" & sector %in% c("Industry Emissions Intensity"))) %>% 
  mutate(cN = countrycode::countrycode(iso3c, 
                                       "iso3c", 
                                       "country.name"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions Intensity"), 
         l_y = "B") 

df_plot_sub_combined <- rbind(df_plot_em_sub_main, df_plot_int_sub_main)


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
  labs(x = "", y = "Emission intensities\n(tons / 10k GDP)") + 
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
  geom_ribbon(aes(x = year, ymin = (`50%` / 10^7), ymax = (`84%`/10^7)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%` / 10^7), ymin = (`16%`/10^7)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%` / 10^7), ymax = (`95%`/10^7)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%` / 10^7), ymin = (`5%`/10^7)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%` / 10^7))) + 
  facet_wrap(.~iso_sector, scales = "free_y") + 
  theme_bw() + 
  labs(x = "", y = "Sectoral emissions\n(Gigatons CO2eq)") + 
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
         # cN = replace(cN, cN == "Australia", " Australia"), 
         # cN = replace(cN, cN == "Mexico", " Mexico"), 
         # cN = replace(cN, cN == "Israel", " Israel"), 
         # cN = replace(cN, cN == "United States", "  United States"), 
         # cN = replace(cN, cN == "Germany", "  Germany"), 
         # cN = replace(cN, cN == "United Kingdom", "  United Kingdom"),
         iso_sector = paste0(cN, " - ", sector), 
         iso_sector = str_remove(iso_sector, " Emissions")) 

df_plot_em_sub_energy$cN <- factor(df_plot_em_sub_energy$cN, 
     levels = c("Germany", "United Kingdom", "United States", "Ghana", "India", "Iran", "Australia", "Israel", "Mexico"))

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
  # scale_y_continuous(trans="log10") +
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
  # scale_y_continuous(trans="log10") +
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
  # scale_y_continuous(trans="log10") +
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
  # scale_y_continuous(trans="log10") +
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
  # scale_y_continuous(trans="log10") +
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
  # scale_y_continuous(trans="log10") +
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


# Plots for emission intensity evolution
df_plot_int <- df_plot_int %>% 
  left_join(regions_income, by = c("iso3c"))

df_plot_int$income <- factor(df_plot_int$income, 
                             levels = c("High income", "Upper middle income", 
                                        "Lower middle income", "Low income"))

p_int_time <- ggplot(data = df_plot_int %>% filter(year > 1999), 
                     aes(x = year, y = `50%`, group = iso3c, color = income)) + 
  geom_line() +
  facet_wrap(.~sector, scales = "free_y") + 
  theme_bw() + 
  labs(x = "", y = "Emission Intensity (tons / 10k GDP)") +
  ggtitle(paste0("Evolution Emission Intensities Over Time")) +
  scale_colour_viridis_d() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12)) + 
  guides(color = guide_legend(nrow = 2))

df_plot_int_scatter <- df_plot_int %>% 
  transmute(iso3c, year, sector, med = `50%`, region_ipcc_9, region_ipcc, income) %>%
  filter(year %in% c(2018, 2050)) %>% 
  tidyr::pivot_wider(names_from = year, values_from = med) %>% 
  mutate(gr = (`2050` - `2018`) / `2018`)

facet_limits <- df_plot_int_scatter %>% 
  group_by(sector) %>% 
  summarise(min = min(`2018`, `2050`), max = max(`2018`, `2050`)) %>%
  gather(range, `2050`, -sector) %>%
  mutate(`2018` = `2050`, range = NULL)

p_conv_int <- ggplot(data = df_plot_int_scatter, aes(x = (`2018`), y = `2050`)) + 
  geom_point(fill = "grey40", color = "grey40", alpha = 0.5) +
  geom_smooth(se = F, method = "lm", col = "black", size = .75, fullrange = T) + 
  geom_abline(intercept = 0, slope = 1, size = .75, col = "red", lty = 2) + 
  facet_wrap(.~sector, scales = "free") + 
  geom_blank(data = facet_limits) + 
  theme_bw() + 
  labs(x = "Emission Intensity (tons / 10k GDP) in 2018", 
       y = "Emission Intensity (tons / 10k GDP) in 2050") +
  ggtitle(paste0("Comparison Emission Intensities over time")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))

# BAU country calculations
BAU_country <- openxlsx::read.xlsx("./01_data/BAU_country_data.xlsx")
country_cov <- unique(BAU_country$iso3c)
year_cov <- BAU_country %>% 
  group_by(iso3c) %>% 
  summarise(year_cov = min(year))
BAU_country <- BAU_country %>% 
  tidyr::pivot_wider(names_from = scenario, values_from = emissions) %>% 
  mutate(sector = replace(sector, sector == "total", "Total Emissions"), 
         sector = replace(sector, sector == "transport", "Transport Emissions"),
         sector = replace(sector, sector == "agriculture", "Agriculture Emissions"),
         sector = replace(sector, sector == "industry", "Industry Emissions"),
         sector = replace(sector, sector == "buildings", "Buildings Emissions"),
         sector = replace(sector, sector == "energy", "Energy Emissions"))

df_plot_BAU <- df_plot_em %>% 
  filter(iso3c %in% country_cov, year > 1999, year < 2036) %>% 
  left_join(BAU_country, by = c("iso3c", "year", "sector")) %>% 
  left_join(year_cov, by = c("iso3c")) %>% 
  mutate(year_cov = replace(year_cov, year_cov != year, NA)) %>% 
  group_by(iso3c, sector) %>% 
  filter(any(!is.na(BAU))) %>% 
  mutate(BAU = zoo::na.approx(BAU, na.rm = F),
         NDC = zoo::na.approx(NDC, na.rm = F),
         `50%`= `50%` / 10^4,
         `5%`= `5%` / 10^4,
         `16%`= `16%` / 10^4,
         `84%`= `84%` / 10^4,
         `95%`= `95%` / 10^4, 
         NDC = replace(NDC, iso3c == "MEX" & sector != "Total Emissions", NA),
         country = countrycode::countrycode(iso3c, "iso3c", "country.name"),
         BAU = BAU * (`50%`[!is.na(year_cov)] / BAU[!is.na(year_cov)]),
         NDC = NDC * (`50%`[!is.na(year_cov)] / NDC[!is.na(year_cov)]),
         BAU = replace(BAU, year < as.numeric(na.omit(unique(year_cov))), NA),
         NDC = replace(NDC, year < as.numeric(na.omit(unique(year_cov))), NA)) %>% 
  filter(year < 2031)

df_plot_BAU$sector <- factor(df_plot_BAU$sector, 
                             levels = c("Total Emissions", "Transport Emissions",
                                        "Agriculture Emissions", "Industry Emissions",
                                        "Buildings Emissions", "Energy Emissions"))

p_comp_BAU_total <- ggplot(data = df_plot_BAU %>% filter(sector == "Total Emissions", 
                                                         iso3c %in% c("IDN", "IRN", "TUR"))) +
  geom_ribbon(aes(x = year, ymin = (`50%`), ymax = (`84%`)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%`), ymin = (`16%`)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%`), ymax = (`95%`)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%`), ymin = (`5%`)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%`))) + 
  geom_line(aes(x = year, y = (BAU), color = "BAU emissions (country estimate)")) + 
  geom_line(aes(x = year, y = (NDC), color = "NDC emissions")) + 
  scale_color_manual(values = c("#133abd", "#1fb417")) +
  facet_wrap(.~country, scales = "free_y") + 
  theme_bw() + 
  labs(x = "", y = "Total emissions (Megatons CO2eq)") +
  # ggtitle(paste0("Comparison BAU Scenarios for Total GHG Emissions")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))

p_comp_BAU_MEX <- ggplot(data = df_plot_BAU %>% filter(iso3c == "MEX")) + 
  geom_ribbon(aes(x = year, ymin = (`50%`), ymax = (`84%`)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymax = (`50%`), ymin = (`16%`)), 
              fill = "#FF0000", alpha = 0.4) + 
  geom_ribbon(aes(x = year, ymin = (`84%`), ymax = (`95%`)),
              fill = "#FF0000", alpha = 0.2) +
  geom_ribbon(aes(x = year, ymax = (`16%`), ymin = (`5%`)),
              fill = "#FF0000", alpha = 0.2) +
  geom_line(aes(x = year, y = (`50%`))) + 
  geom_line(aes(x = year, y = (BAU), color = "BAU emissions (country estimate)")) + 
  geom_line(aes(x = year, y = (NDC), color = "NDC emissions")) + 
  scale_color_manual(values = c("#133abd", "#1fb417")) +
  facet_wrap(.~sector, scales = "free_y") + 
  theme_bw() + 
  # scale_y_continuous(trans="log10") +
  labs(x = "", y = "Sectoral emissions (Megatons CO2eq)") +
  ggtitle(paste0("Comparison BAU Sectoral Emissions for Mexico")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(size = 20), 
        legend.key.width = unit(3, "cm"), 
        plot.caption = element_text(hjust = 0, size = 14), 
        legend.text = element_text(size = 14), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))


### Maps for aggregate and per capita emissions in 2018 and 2050

# add population figures for per capita plots
load("./01_data/processed/macro_data.Rda")
regions_macro <- macro_data %>% 
  filter(iso3c %in% df_plot_em$iso3c) %>% 
  select(iso3c, year, pop, GDP.PPP) %>% 
  left_join(regions_income, by = c("iso3c"))

macro_ipcc_regions <- regions_macro %>% 
  group_by(year, region_ipcc) %>% 
  summarise(pop = sum(pop, na.rm = T), 
            gdp = sum(GDP.PPP, na.rm = T))

df_plot_em_ipcc_pc <- df_plot_em_ipcc %>% 
  left_join(macro_ipcc_regions, by = c("year", "region_ipcc")) %>% 
  mutate(`50%` = `50%` * 10^2 / pop, 
         `5%` = `5%` * 10^2 / pop,
         `16%` = `16%` * 10^2 / pop,
         `84%` = `84%` * 10^2 / pop, 
         `95%` = `95%` * 10^2 / pop)

# map preliminaries
library(ggnewscale)
library(scatterpie)
library(cowplot)

world <- map_data("world")
world$iso3c <- countrycode::countrycode(world$region, origin = "country.name", 
                                        destination = "iso3c")
world <- world %>% 
  left_join(regions_IPCC)
coords_centroids <- CoordinateCleaner::countryref %>% 
  filter(iso3 %in% c("AUS", "USA", "COL", "DEU", "TCD", "KWT", "KAZ", "IND", "BRN", "CHN")) %>% 
  group_by(iso3) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  transmute(iso3c = iso3, centroid.lon, centroid.lat) %>% 
  left_join(regions_IPCC) %>%
  select(-iso3c) %>% 
  mutate(region_ipcc = replace(region_ipcc, 
                               region_ipcc == "South-East Asia and developing Pacific", 
                               "South-East Asia"))

df_plot_em_map_ipcc_agg <- df_plot_em_ipcc %>% 
  filter(year %in% c(2018, 2050)) %>% 
  transmute(region_ipcc, year, sector, med = `50%` / 10^7) %>% 
  tidyr::pivot_wider(names_from = sector, values_from = med) %>% 
  left_join(coords_centroids)

df_plot_em_map_ipcc_pc <- df_plot_em_ipcc_pc %>% 
  filter(year %in% c(2018, 2050)) %>% 
  transmute(region_ipcc, year, sector, med = `50%`) %>% 
  tidyr::pivot_wider(names_from = sector, values_from = med) %>% 
  left_join(coords_centroids)

# aggregate emissions maps
p_map_agg_2018 <- ggplot(world %>% filter(region != "Antarctica"), aes(long, lat)) + 
  geom_map(map = world, aes(map_id = region, fill = region_ipcc), 
           alpha = 0.4, color = "black", size = 0.5) + 
  scale_fill_brewer("Region", palette = "Paired", na.translate = F) +
  theme_void() + 
  coord_quickmap() + 
  new_scale("fill") + 
  geom_scatterpie(data = df_plot_em_map_ipcc_agg %>% filter(year == 2018), 
                  aes(x = centroid.lon, y = centroid.lat, group = region_ipcc, 
                      r = `Total Emissions` * .82), # reduce size of pies a bit for visibility
                  cols = c("Energy Emissions", "Transport Emissions", 
                           "Industry Emissions", "Agriculture Emissions", 
                           "Buildings Emissions"), 
                  color = "white", alpha = 1, size = 0.001) + 
  scale_fill_viridis_d("Sector") + 
  theme(legend.position = "bottom", 
        legend.box = "vertical", 
        legend.title = element_text(size = 14))

p_map_agg_2050 <- ggplot(world %>% filter(region != "Antarctica"), aes(long, lat)) + 
  geom_map(map = world, aes(map_id = region, fill = region_ipcc), 
           alpha = 0.4, color = "black", size = 0.5) + 
  scale_fill_brewer("Region", palette = "Paired", na.translate = F) +
  theme_void() + 
  coord_quickmap() + 
  new_scale("fill") + 
  geom_scatterpie(data = df_plot_em_map_ipcc_agg %>% filter(year == 2050), 
                  aes(x = centroid.lon, y = centroid.lat, group = region_ipcc, 
                      r = `Total Emissions` * .82), # reduce size of pies a bit for visibility
                  cols = c("Energy Emissions", "Transport Emissions", 
                           "Industry Emissions", "Agriculture Emissions", 
                           "Buildings Emissions"), 
                  color = "white", alpha = 1, size = 0.001) + 
  geom_scatterpie_legend(df_plot_em_map_ipcc_agg %>% 
                           filter(year == 2050) %>% 
                           pull(`Total Emissions`) * .82, # make legend fit to pie size
                         x=-160, y=-55, n = 5, 
                         labeller = function(x) paste0(round(x / 0.82), " GT")) + 
  scale_fill_viridis_d("Sector") + 
  theme(legend.position = "bottom", 
        legend.box = "vertical", 
        legend.title = element_text(size = 14))

legend_agg <- get_legend(p_map_agg_2018)
map_agg_row <- plot_grid(p_map_agg_2018 + theme(legend.position = "none"), 
                         p_map_agg_2050 + theme(legend.position = "none"), 
                         labels = c("2018", "2050"),
                         label_x = 0.5,
                         label_size = 18,
                         ncol = 1)
map_agg_final <- plot_grid(map_agg_row, legend_agg, 
                           ncol = 1, rel_heights = c(1, .1))


# per capita emissions maps
p_map_pc_2018 <- ggplot(world %>% filter(region != "Antarctica"), aes(long, lat)) + 
  geom_map(map = world, aes(map_id = region, fill = region_ipcc), 
           alpha = 0.4, color = "black", size = 0.5) + 
  scale_fill_brewer("Region", palette = "Paired", na.translate = F) +
  theme_void() + 
  coord_quickmap() + 
  new_scale("fill") + 
  geom_scatterpie(data = df_plot_em_map_ipcc_pc %>% filter(year == 2018), 
                  aes(x = centroid.lon, y = centroid.lat, group = region_ipcc, 
                      r = `Total Emissions` * .6), # reduce size of pies a bit for visibility
                  cols = c("Energy Emissions", "Transport Emissions", 
                           "Industry Emissions", "Agriculture Emissions", 
                           "Buildings Emissions"), 
                  color = "white", alpha = 1, size = 0.001) + 
  scale_fill_viridis_d("Sector") + 
  theme(legend.position = "bottom", 
        legend.box = "vertical", 
        legend.title = element_text(size = 14))

p_map_pc_2050 <- ggplot(world %>% filter(region != "Antarctica"), aes(long, lat)) + 
  geom_map(map = world, aes(map_id = region, fill = region_ipcc), 
           alpha = 0.4, color = "black", size = 0.5) + 
  scale_fill_brewer("Region", palette = "Paired", na.translate = F) +
  theme_void() + 
  coord_quickmap() + 
  new_scale("fill") + 
  geom_scatterpie(data = df_plot_em_map_ipcc_pc %>% filter(year == 2050), 
                  aes(x = centroid.lon, y = centroid.lat, group = region_ipcc, 
                      r = `Total Emissions` * .6), # reduce size of pies a bit for visibility
                  cols = c("Energy Emissions", "Transport Emissions", 
                           "Industry Emissions", "Agriculture Emissions", 
                           "Buildings Emissions"), 
                  color = "white", alpha = 1, size = 0.001) + 
  geom_scatterpie_legend(df_plot_em_map_ipcc_pc %>% 
                           filter(year == 2018) %>% 
                           pull(`Total Emissions`) * .6, # make legend fit to pie size
                         x=-160, y=-55, n = 5, 
                         labeller = function(x) paste0(round(x / .6), " T")) + 
  scale_fill_viridis_d("Sector") + 
  theme(legend.position = "bottom", 
        legend.box = "vertical", 
        legend.title = element_text(size = 14))

legend_pc <- get_legend(p_map_pc_2018)
map_pc_row <- plot_grid(p_map_pc_2018 + theme(legend.position = "none"), 
                        p_map_pc_2050 + theme(legend.position = "none"), 
                        labels = c("2018", "2050"),
                        label_x = 0.5,
                        label_size = 18,
                        ncol = 1)
map_pc_final <- plot_grid(map_pc_row, legend_pc, 
                          ncol = 1, rel_heights = c(1, .1))


# Outputting all plots to PDF

### Figures main text

# Figure 1
pdf(file = paste0(output_folder, "/Fig1-selected_countries.pdf"),
    onefile = TRUE, width = 10, height = 6)
cowplot::plot_grid(p_select_1, p_select_2, ncol = 1, labels = "auto")
dev.off()

# Figure 2
pdf(file = paste0(output_folder, "/Fig2-comp_fcast_global.pdf"), 
    onefile = TRUE, width = 10, height = 6)
p_fcast_global
dev.off()

# Figure 3
pdf(file = paste0(output_folder, "/Fig3-comp_fcast_ipcc9_int.pdf"),
    onefile = TRUE, width = 10, height = 8)
p_fcast_ipcc_9_int
dev.off()

# Figure 4
pdf(file = paste0(output_folder, "/Fig4-comp_fcast_ipcc9_em.pdf"),
    onefile = TRUE, width = 10, height = 8)
p_fcast_ipcc_9
dev.off()

# Figure 5
pdf(file = paste0(output_folder, "/Fig5-map_agg.pdf"),
    onefile = TRUE, width = 10, height = 10)
map_agg_final
dev.off()

# Figure 6
pdf(file = paste0(output_folder, "/Fig6-map_pc.pdf"),
    onefile = TRUE, width = 10, height = 10)
map_pc_final
dev.off()

# Figure 7
pdf(file = paste0(output_folder, "/Fig7-comp_fcast_BAU_total.pdf"),
    onefile = TRUE, width = 10, height = 4)
p_comp_BAU_total
dev.off()


### Figures supplementary material

# Figure S1
pdf(file = paste0(output_folder, "/FigS1-comp_fcast_income.pdf"),
    width = 10, height = 6)
p_fcast_income
dev.off()

# Figure S2
pdf(file = paste0(output_folder, "/FigS2-comp_fcast_oil_income.pdf"),
    width = 10, height = 9)
p_comp_oil_income
dev.off()

# Figure S3
pdf(file = paste0(output_folder, "/FigS3-comp_fcast_energy.pdf"),
    onefile = TRUE, width = 10, height = 7.5)
p_select_energy
dev.off()

# Figure S4
pdf(file = paste0(output_folder, "/FigS4-comp_fcast_transport.pdf"), 
    onefile = TRUE, width = 10, height = 6)
p_select_transport
dev.off()

# Figure S5
pdf(file = paste0(output_folder, "/FigS5-comp_fcast_RUS.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_emissions_RUS
dev.off()

# Figure S6
pdf(file = paste0(output_folder, "/FigS6-comp_fcast_UKR.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_emissions_UKR
dev.off()

# Figure S7
pdf(file = paste0(output_folder, "/FigS7-comp_fcast_reducer.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_select_reducer
dev.off()

# Figure S8
pdf(file = paste0(output_folder, "/FigS8-comp_fcast_increaser.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_select_increaser
dev.off()

# Figure S9
pdf(file = paste0(output_folder, "/FigS9-comp_fcast_int_time.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_int_time
dev.off()

# Figure S10
pdf(file = paste0(output_folder, "/FigS10-comp_fcast_int_conv.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_conv_int
dev.off()

# Figure S11 
pdf(file = paste0(output_folder, "/FigS11-comp_fcast_BAU_MEX.pdf"),
    onefile = TRUE, width = 10, height = 6)
p_comp_BAU_MEX
dev.off()

# Figure S12
pdf(file = paste0(output_folder, "/FigS12-comp_fcast_IAM.pdf"),
    width = 10, height = 7.5)
p_IAM_fcast_comp_all
dev.off()


#####
# Table 1: top-15 emitting sectors in 2018 and 2050

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


#####
# Table S1: Shares of global emissions within sectors for world regions in 2018 and 2050

data_tab_risk_hist <- df_plot_em_ipcc %>% 
  filter(year == 2018) %>% 
  transmute(region_ipcc, 
            sector = str_split(sector, " ", simplify = T)[,1], 
            med = `50%`) %>% 
  group_by(sector) %>% 
  mutate(med = round(med / sum(med) * 100, 1)) %>% 
  pivot_wider(names_from = sector, values_from = med) %>% 
  arrange(desc(Total)) %>% 
  as.data.frame()
rownames(data_tab_risk_hist) <- data_tab_risk_hist$region_ipcc
data_tab_risk_hist$region_ipcc <- NULL

xt_table_em_risk_hist <- xtable(data_tab_risk_hist, digits = 1)
print.xtable(xt_table_em_risk_hist)


data_tab_risk_proj <- df_plot_em_ipcc %>% 
  filter(year == 2050) %>% 
  transmute(region_ipcc, 
            sector = str_split(sector, " ", simplify = T)[,1],
            lower = `16%`,
            med = `50%`, 
            upper = `84%`) %>% 
  group_by(sector) %>% 
  mutate(lower = sprintf("%.1f", lower / sum(lower) * 100), 
         med = sprintf("%.1f", med / sum(med) * 100), 
         upper = sprintf("%.1f", upper / sum(upper) * 100), 
         conf = paste0("(", lower, "-", upper, ")")) %>% 
  dplyr::select(-lower, -upper) %>% 
  pivot_longer(cols = med:conf) %>% 
  pivot_wider(names_from = sector, values_from = value)
order_proj <- df_plot_em_ipcc %>% 
  filter(year == 2050, sector == "Total Emissions") %>% 
  arrange(desc(`50%`)) %>% 
  pull(region_ipcc)
data_tab_risk_proj <- data_tab_risk_proj %>% 
  arrange(factor(region_ipcc, levels = order_proj)) %>% 
  mutate(region_ipcc = replace(region_ipcc, name == "conf", NA)) %>% 
  dplyr::select(-name)

xt_table_em_risk_proj <- xtable(data_tab_risk_proj)
print.xtable(xt_table_em_risk_proj, include.rownames = FALSE)


data_tab_risk_proj_med <- df_plot_em_ipcc %>% 
  filter(year == 2050) %>% 
  transmute(region_ipcc, 
            sector = str_split(sector, " ", simplify = T)[,1],
            med = `50%`) %>% 
  group_by(sector) %>% 
  mutate(med = med / sum(med) * 100) %>% 
  pivot_wider(names_from = sector, values_from = med) %>% 
  arrange(desc(Total)) %>% 
  as.data.frame()
rownames(data_tab_risk_proj_med) <- data_tab_risk_proj_med$region_ipcc
data_tab_risk_proj_med$region_ipcc <- NULL

xt_table_em_risk_proj_med <- xtable(data_tab_risk_proj_med, digits = 1)
print.xtable(xt_table_em_risk_proj_med)


data_tab_risk_proj_upper <- df_plot_em_ipcc %>% 
  filter(year == 2050) %>% 
  transmute(region_ipcc, 
            sector = str_split(sector, " ", simplify = T)[,1],
            med = `84%`) %>% 
  group_by(sector) %>% 
  mutate(med = med / sum(med) * 100) %>% 
  pivot_wider(names_from = sector, values_from = med) %>% 
  arrange(desc(Total)) %>% 
  as.data.frame()
rownames(data_tab_risk_proj_upper) <- data_tab_risk_proj_upper$region_ipcc
data_tab_risk_proj_upper$region_ipcc <- NULL

xt_table_em_risk_proj_upper <- xtable(data_tab_risk_proj_upper, digits = 1)
print.xtable(xt_table_em_risk_proj_upper)


data_tab_risk_proj_lower <- df_plot_em_ipcc %>% 
  filter(year == 2050) %>% 
  transmute(region_ipcc, 
            sector = str_split(sector, " ", simplify = T)[,1],
            med = `16%`) %>% 
  group_by(sector) %>% 
  mutate(med = med / sum(med) * 100) %>% 
  pivot_wider(names_from = sector, values_from = med) %>% 
  arrange(desc(Total)) %>% 
  as.data.frame()
rownames(data_tab_risk_proj_lower) <- data_tab_risk_proj_lower$region_ipcc
data_tab_risk_proj_lower$region_ipcc <- NULL

xt_table_em_risk_proj_lower <- xtable(data_tab_risk_proj_lower, digits = 1)
print.xtable(xt_table_em_risk_proj_lower)



#####
# Calculations for global carbon budget

load("./01_data/processed/ghg_data.Rda")

share_CO2_2018 <- essd_ghg %>% 
  filter(iso3c %in% cN_full, year == 2018) %>%
  mutate(gas = replace(gas, gas != "CO2", "nonCO2"), 
         value_gwp = gwp100_ar5 * value) %>% 
  group_by(year, gas) %>% 
  summarise(sum_gwp = sum(value_gwp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(share_gas = sum_gwp / sum(sum_gwp)) %>% 
  filter(gas == "CO2") %>% 
  pull(share_gas)
  

carbon_budget_total <- plot_totals %>% 
  filter(sector == "Total Emissions") %>% 
  select(year, `50%`) %>% 
  rename(median = `50%`) %>% 
  mutate(median = median * share_CO2_2018) %>% 
  filter(year > 2019) %>% 
  mutate(median = cumsum(median / 10^7))

carbon_budget_total %>% filter(median > 400) %>% filter(year == min(year))
carbon_budget_total %>% filter(median > 1150) %>% filter(year == min(year))


