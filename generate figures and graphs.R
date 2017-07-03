############################################################################################################
# Scripts for computing figures and generating graphs in Statistics Explained article
# http://ec.europa.eu/eurostat/statistics-explained/index.php?title=Households%27_income,_consumption_and_wealth,_and_how_they_interact_-_statistics_on_main_results
# Run on R 64-bits 3.3.1


library(dplyr)
library(haven)
library(Cairo)
library(eurostat)
library(reshape2)

col1 <- rgb(250, 165, 25, maxColorValue = 255)
col2 <- rgb(40, 110, 180, maxColorValue = 255)
col3 <- rgb(240, 99, 34, maxColorValue = 255)
col4 <- rgb(185, 195, 30, maxColorValue = 255)

#################################################################################################################################################
### FIGURE 1
#################################################################################################################################################

indicators_NA <- get_eurostat("nasa_10_ki", time_format = "num")
saving_rate_NA <- filter(indicators_NA,
                         na_item == "SRG_S14_S15")

saving_rate_NA_2015 <- filter(saving_rate_NA,
                              time == 2015)

indicators_silc <- get_eurostat("ilc_mdes09", time_format = "num")
indicators_diff <- filter(indicators_silc,
                          subjnmon %in% c("EM_D","EM_GD") & hhtyp == "TOTAL" & time == 2015 & incgrp == "TOTAL" & !geo %in% c("EU28","EA19"))

indicators_diff <- indicators_diff %>%
  group_by(geo) %>%
  summarise(prop_diff = sum(values))

figure1 <- merge(saving_rate_NA_2015, indicators_diff)
names(figure1)[6] <- "saving_rate"

plot(figure1$prop_diff, figure1$saving_rate, type = "p", pch = 18, col = col1, bty = "n", xaxt="n",yaxt="n", 
     xlim = c(0,70), ylim = c(-10,25), xlab = NA, ylab = NA)
grid(nx = NA, ny = NULL)
axis(1,pos=0)
axis(2,pos=0, tick = FALSE, las = 1)
text(figure1$prop_diff, figure1$saving_rate, labels = figure1$geo, cex = 0.7, pos = 3)


#################################################################################################################################################
### FIGURE 2
#################################################################################################################################################

indicators_pov <- get_eurostat("icw_pov_01", time_format ="num")
indicators_exp <- filter(indicators_pov,
                         lev_expn == "LOW" & ind_type == "TOTAL" & lev_depr == "TOTAL" & workint == "TOTAL")
indicators_inc <- filter(indicators_pov,
                         lev_expn == "TOTAL" & ind_type == "ARP" & lev_depr == "TOTAL" & workint == "TOTAL")
names(indicators_exp)[8] <- "prop_expn"
names(indicators_inc)[8] <- "prop_inc"
figure2 <- merge(indicators_exp[,c(6,8)], indicators_inc[,c(6,8)])
figure2 <- figure2[order(figure2$prop_inc),]

barplot(t(figure2[,2:3]), beside = TRUE, col = c(col1, col2), main = NA,
        border = NA, legend.text = c("Low levels of expenditure", "Monetary poverty"),
        names.arg = figure2$geo, cex.names = 0.5,
        args.legend = list(x = "topleft", bty = "n", border = NA, cex = 0.5))


#################################################################################################################################################
### FIGURE 3
#################################################################################################################################################

indicators_asset_pov <- get_eurostat("icw_pov_02", time_format = "num")
indicators_asset_pov <- filter(indicators_asset_pov,
                               ind_type == "ARP" & duration %in% c("M1", "M3","M6","M12"))
indicators_asset_pov <- mutate(indicators_asset_pov,
                               period = as.numeric(substr(duration,2,4)))
indicators_asset_pov <- indicators_asset_pov[order(indicators_asset_pov$geo, indicators_asset_pov$period),]
figure3 <- dcast(indicators_asset_pov, geo+time~period, value.var = "values")
figure3 <- figure3[order(figure3[,3]),]

barplot(t(figure3[,3:6]), beside = TRUE, col = c(col1, col2, col3, col4), main = NA,
        border = NA, legend.text = c("1 month", "3 months", "6 months", "12 months"),
        names.arg = figure3$geo, cex.names = 0.7,
        args.legend = list(x = "topleft", bty = "n", border = NA, cex = 0.5))


#################################################################################################################################################
### FIGURE 4
#################################################################################################################################################

gini_index <- get_eurostat("icw_sr_05", time_format = "num")
figure4 <- dcast(gini_index, geo+time~stk_flow, value.var = "values")
figure4 <- figure4[order(figure4[,5]),]

barplot(t(figure4[,3:6]), beside = TRUE, col = c(col1, col2, col3, col4), main = NA,
        border = NA, legend.text = c("Income", "Expenditures", "Savings", "Wealth"),
        names.arg = figure4$geo, cex.names = 0.5,
        args.legend = list(x = "topleft", bty = "n", border = NA, cex = 0.5))
