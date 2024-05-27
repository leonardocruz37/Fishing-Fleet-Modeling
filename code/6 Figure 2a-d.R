library(ggplot2)
library(dplyr)
library(raster)
library(ggpubr)
library(ggrepel)



##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

color_pallet = c("#1f77b4", "#ff7f0e", "#006d2c", "#ff9896",
                 "#74c476", "#8c564b",  "#e377c2", "#7f7f7f",
                 "#bcbd22", "black", "#9467bd", "#17becf", "#d62728")

gear_list <- c('drifting_longlines', 'trawlers', 'tuna_purse_seines',
               'set_longlines', 'squid_jigger', 'set_gillnets', 'pole_and_line',
               'fixed_gear', 'pots_and_traps', 'purse_seines', 'dredge_fishing',
               'trollers', 'seiners')

r1 <- raster(paste0(path,'/environmental variables/present/thetao_baseline_2000_2019_depthsurf_74ff_39fa_9adc_U1714141988978.nc'))
r1 <- raster::aggregate(r1, 1/res(r1)) * 0

df85_1 <- list()
for(do in gear_list){
  
  df <- read.csv(paste0(path,'/output/fishing gears/',do,'/BART_DIFF85 (',do,').csv'))
  r1 <- r1 * 0
  r1 <- rasterize(df[1:2], r1, df$layer, fun = sum, na.rm = FALSE)
  
  df85_1 <- rbind(df85_1 ,data.frame(mean = apply(as.matrix(r1), 1, function(x) mean(x, na.rm = TRUE)),
                                   sd = apply(as.matrix(r1), 1, function(x) sd(x, na.rm = TRUE)),
                                   y = unique(df$y),
                                   gear = do))
}

df26_1 <- list()
for(do in gear_list){
  
  df <- read.csv(paste0(path,'/output/fishing gears/',do,'/BART_DIFF26 (',do,').csv'))
  r1 <- r1 * 0
  r1 <- rasterize(df[1:2], r1, df$layer, fun = sum, na.rm = FALSE)
  
  df26_1 <- rbind(df26_1 ,data.frame(mean = apply(as.matrix(r1), 1, function(x) mean(x, na.rm = TRUE)),
                                   sd = apply(as.matrix(r1), 1, function(x) sd(x, na.rm = TRUE)),
                                   y = unique(df$y),
                                   gear = do))
}

g1 = ggplot(df85_1, aes(y = mean, x = y, color = gear)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 0.6) +
  labs(y = expression(paste('Mean ',Delta,'2100-2020')), x = 'Latitude', title = 'SSP4-6.0') +
  scale_x_continuous(breaks = c(-90,-60,-30,0,30,60,90)) +
  scale_y_continuous(breaks = c(-0.1,0,0.1,0.2,0.3), limits = c(-0.1,0.3)) +
  scale_color_manual(values = color_pallet) +
  guides(color = 'none') +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

g2 = ggplot(df26_1, aes(y = mean, x = y, color = gear)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 0.6) +
  labs(y = expression(paste('Mean ',Delta,'2100-2020')), x = 'Latitude', title = 'SSP1-1.9', color = 'Fishing gear') +
  scale_x_continuous(breaks = c(-90,-60,-30,0,30,60,90)) +
  scale_y_continuous(breaks = c(-0.1,0,0.1,0.2,0.3), limits = c(-0.1,0.3)) +
  scale_color_manual(values = color_pallet) +
  guides(color = 'none') +
  theme_classic() +
  theme(plot.title = element_text(size = 10))

df85 <- NULL
df26 <- NULL
for(do in gear_list){
  
  df85 <- rbind(df85,
                read.csv(paste0(path,'/output/fishing gears/',do,'/BART_DIFF85 (',do,').csv')) %>%
                  na.omit() %>%
                  summarise(change = sum(layer),
                            change_lower_95_ci = sum(lower_95_ci),
                            change_upper_95_ci = sum(upper_95_ci),
                            change_abs = sum(abs(layer))) %>%
                  mutate(gear = do,
                         area_p = nrow(read.csv(paste0(path,'/output/fishing gears/',do,'/BART_P_presence (',do,').csv')) %>%
                                         na.omit()),
                         area_f85 = nrow(read.csv(paste0(path,'/output/fishing gears/',do,'/BART_F85_presence (',do,').csv')) %>%
                                         na.omit())))
  
  df26 <- rbind(df26,
                read.csv(paste0(path,'/output/fishing gears/',do,'/BART_DIFF26 (',do,').csv')) %>%
                  na.omit() %>%
                  summarise(change = sum(layer),
                            change_lower_95_ci = sum(lower_95_ci),
                            change_upper_95_ci = sum(upper_95_ci),
                            change_abs = sum(abs(layer))) %>%
                  mutate(gear = do,
                         area_p = nrow(read.csv(paste0(path,'/output/fishing gears/',do,'/BART_P_presence (',do,').csv')) %>%
                                         na.omit()),
                         area_f26 = nrow(read.csv(paste0(path,'/output/fishing gears/',do,'/BART_F26_presence (',do,').csv')) %>%
                                         na.omit())))
  
}


df85$gear <- c('Drifting longlines', 'Trawlers', 'Tuna purse seines',
               'Set longlines', 'Squid jigger', 'Set gillnets', 'Pole and line',
               'Fixed gear', 'Pots and traps', 'Purse seines', 'Dredge fishing',
               'Trollers', 'Seiners')
df26$gear <- c('Drifting longlines', 'Trawlers', 'Tuna purse seines',
               'Set longlines', 'Squid jigger', 'Set gillnets', 'Pole and line',
               'Fixed gear', 'Pots and traps', 'Purse seines', 'Dredge fishing',
               'Trollers', 'Seiners')

#Ratio of change between scenarios gear types
sum(df85$change_abs)/sum(df26$change_abs)

# Normalizing the changes by the spatial occupation of the gears
df85$norm <- df85$change/df85$area_p
df26$norm <- df26$change/df26$area_p

df85$norm_low <- df85$change_lower_95_ci/df85$area_p
df26$norm_low <- df26$change_lower_95_ci/df26$area_p

df85$norm_up <- df85$change_upper_95_ci/df85$area_p
df26$norm_up <- df26$change_upper_95_ci/df26$area_p

g3 = ggplot(df85, aes(x=round(((area_f85/45231)-(area_p/45231))*100,2), y=norm, fill = gear)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_errorbar(aes(ymin = norm_low, ymax = norm_up), width = 0, color = 'grey') +
  geom_point(aes(size = round((area_p/45231)*100)), shape = 21) +
  scale_radius(range = c(2, 8)) + 
  scale_y_continuous(limits = c(-0.1, 0.3)) +
  labs(x = expression(paste(Delta,'Ocean fished (%)')), y = 'Weighted net change',
       title = 'SSP4-6.0', size = 'Current ocean fished (%)', fill = 'Fishing gear') +
  scale_fill_manual(values = color_pallet) +
  scale_color_manual(values = color_pallet) +
  geom_text_repel(aes(label = paste0(round(change),'%'), color = gear),
                   box.padding   = 0.25,
                   point.padding = 0.4,
                   segment.color = 'grey50', show.legend = FALSE, max.overlaps = 15) +
  theme_classic() +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  theme(plot.title = element_text(size = 10))

g4 = ggplot(df26, aes(x=round((((area_f26/45231)-(area_p/45231))*100),2), y=norm, fill = gear)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_errorbar(aes(ymin = norm_low, ymax = norm_up), width = 0, color = 'grey') +
  geom_point(aes(size = round((area_p/45234)*100)), shape = 21) +
  scale_radius(range = c(2, 8)) + 
  scale_x_continuous(limits = c(-0.5,3))+
  scale_y_continuous(limits = c(-0.02, 0.06)) +
  labs(x = expression(paste(Delta,'Ocean fished (%)')), y = 'Weighted net change',
       title = 'SSP1-1.9', size = 'Current ocean fished (%)', fill = 'Fishing gear') +
  scale_fill_manual(values = color_pallet) +
  scale_color_manual(values = color_pallet) +
  geom_text_repel(aes(label = paste0(round(change),'%'), color=gear),
                   box.padding   = 0.25,
                   point.padding = 0.4,
                   segment.color = 'grey50', inherit.aes = TRUE, show.legend = FALSE) +
  theme_classic() +
  guides(fill = guide_legend(override.aes = list(size = 3))) +
  theme(plot.title = element_text(size = 10))

png(paste0(path,'/figures/Figure 2A-D.png'),res = 800,units = 'in',height = 6.5, width = 11.7)
ggarrange(g4,g2,g3,g1, ncol=2,nrow=2,labels = 'AUTO', common.legend = T, legend = 'right')
dev.off()
