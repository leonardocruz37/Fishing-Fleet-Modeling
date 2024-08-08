library(ggplot2)
library(dplyr)
library(raster)
library(ggpubr)
library(ggrepel)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

source(paste0(path,'/code/function create_plot.R'))

####
########## FIGURE 2a-d #########
####

color_pallet <- c("#d62728", "#ff7f0e", "#abd9e9", 'black',
                  "#8c564b", "pink", "#F0E442", "#7f7f7f",
                  "#E7298A", "#17becf", "#9467bd", "#fee090", "#0072B2")

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
  
  df85_1 <- rbind(df85_1 ,data.frame(sum = apply(as.matrix(r1), 1, function(x) sum(x, na.rm = TRUE)),
                                     y = unique(df$y),
                                     gear = do))
}

df26_1 <- list()
for(do in gear_list){
  
  df <- read.csv(paste0(path,'/output/fishing gears/',do,'/BART_DIFF26 (',do,').csv'))
  r1 <- r1 * 0
  r1 <- rasterize(df[1:2], r1, df$layer, fun = sum, na.rm = FALSE)
  
  df26_1 <- rbind(df26_1 ,data.frame(sum = apply(as.matrix(r1), 1, function(x) sum(x, na.rm = TRUE)),
                                     y = unique(df$y),
                                     gear = do))
}

g1 = ggplot(df85_1, aes(y = sum, x = y, color = gear)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 0.4) +
  labs(y = expression(paste('Net change (%)')), x = 'Latitude', title = 'SSP4-6.0') +
  scale_x_continuous(breaks = c(-90,-60,-30,0,30,60,90),
                     labels = c("90°S", "60°S", "30°S", "0°", "30°N", "60°N", "90°N"),) +
  scale_y_continuous(breaks = c(-30,0,30,60,90),
                     limits = c(-30,90)) +
  scale_color_manual(values = color_pallet) +
  theme_classic()  +
  theme(plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 6, family = "Helvetica"),
        axis.title = element_text(size = 7, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25)) +
  guides(color = 'none')

g2 = ggplot(df26_1, aes(y = sum, x = y, color = gear)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 0.4) +
  labs(y = expression(paste('Net change (%)')), x = 'Latitude', title = 'SSP1-1.9', color = 'Fishing gear') +
  scale_x_continuous(breaks = c(-90,-60,-30,0,30,60,90),
                     labels = c("90°S", "60°S", "30°S", "0°", "30°N", "60°N", "90°N"),) +
  scale_y_continuous(breaks = c(-20,-10,0,10,20,30),
                     limits = c(-20,30)) +
  scale_color_manual(values = color_pallet) +
  theme_classic()  +
  theme(plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 6, family = "Helvetica"),
        axis.title = element_text(size = 7, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25)) +
  guides(color = 'none')

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

g3 = ggplot(df85, aes(x=round(((area_f85/45231)-(area_p/45231))*100,2), y=change, fill = gear)) +
  geom_hline(yintercept = 0, linetype = 3, linewidth=0.25) +
  geom_vline(xintercept = 0, linetype = 3, linewidth=0.25) +
  geom_errorbar(aes(ymin = change_lower_95_ci, ymax = change_upper_95_ci), linewidth = 0.4, width = 0, color = 'grey') +
  geom_point(aes(size = round((area_p/45231)*100)), shape = 21, stroke = 0.2) +
  scale_radius(range = c(1, 4)) + 
  scale_y_continuous(limits = c(-200, 3100),
                     breaks = c(-200, 1000, 2000, 3000)) +
  labs(x = expression(paste(Delta,'Ocean fished (%)')), y = 'Net change (%)',
       title = 'SSP4-6.0', size = 'Current ocean fished (%)', fill = 'Fishing gear') +
  scale_fill_manual(values = color_pallet) +
  theme_classic() +
  theme(plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 6, family = "Helvetica"),
        axis.title = element_text(size = 7, family = "Helvetica"),
        legend.text = element_text(size = 5, family = "Helvetica", hjust = 0),
        legend.title = element_text(size = 7, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25)) +
  guides(fill = guide_legend(order = 1, barwidth = unit(3,'mm'), barheight = unit(3,'mm'),override.aes = list(size = 1.6)),
         size = guide_legend(order = 2, barwidth = unit(3,'mm'), barheight = unit(3,'mm')))

g4 = ggplot(df26, aes(x=round((((area_f26/45231)-(area_p/45231))*100),2), y=change, fill = gear)) +
  geom_hline(yintercept = 0, linetype = 3, linewidth=0.25) +
  geom_vline(xintercept = 0, linetype = 3, linewidth=0.25) +
  geom_errorbar(aes(ymin = change_lower_95_ci, ymax = change_upper_95_ci), linewidth= 0.4, width = 0, color = 'grey') +
  geom_point(aes(size = round((area_p/45234)*100)), shape = 21, stroke = 0.2) +
  scale_radius(range = c(1, 4)) + 
  scale_x_continuous(limits = c(-0.5,3))+
  scale_y_continuous(limits = c(-200, 650),
                     breaks = c(-200, 200, 400, 600)) +
  labs(x = expression(paste(Delta,'Ocean fished (%)')), y = 'Net change (%)',
       title = 'SSP1-1.9', size = 'Current ocean fished (%)', fill = 'Fishing gear') +
  scale_fill_manual(values = color_pallet) +
  theme_classic() +
  theme(plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 6, family = "Helvetica"),
        axis.title = element_text(size = 7, family = "Helvetica"),
        legend.text = element_text(size = 5, family = "Helvetica", hjust = 0),
        legend.title = element_text(size = 7, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25)) +
  guides(fill = guide_legend(order = 1, barwidth = unit(3,'mm'), barheight = unit(3,'mm'),override.aes = list(size = 1.6)),
         size = guide_legend(order = 2, barwidth = unit(3,'mm'), barheight = unit(3,'mm')))


####
########## FIGURE 2e-j #########
####

BART_DIFF26_DL <- read.csv(paste0(path,'/output/fishing gears/drifting_longlines/BART_DIFF26 (drifting_longlines).csv'))
BART_DIFF85_DL <- read.csv(paste0(path,'/output/fishing gears/drifting_longlines/BART_DIFF85 (drifting_longlines).csv'))

BART_DIFF26_TL <- read.csv(paste0(path,'/output/fishing gears/trawlers/BART_DIFF26 (trawlers).csv'))
BART_DIFF85_TL <- read.csv(paste0(path,'/output/fishing gears/trawlers/BART_DIFF85 (trawlers).csv'))

BART_DIFF26_TPS <- read.csv(paste0(path,'/output/fishing gears/tuna_purse_seines/BART_DIFF26 (tuna_purse_seines).csv'))
BART_DIFF85_TPS <- read.csv(paste0(path,'/output/fishing gears/tuna_purse_seines/BART_DIFF85 (tuna_purse_seines).csv'))

# Summary net change
net_sum <- function(x){
  x %>%
    mutate(type = case_when(layer > 0 ~ 'increase',
                            layer < 0 ~ 'decrease')) %>%
    group_by(type) %>%
    summarise_all(sum, .keep_all=TRUE)
}

net_sum(BART_DIFF26_DL)
net_sum(BART_DIFF85_DL)

net_sum(BART_DIFF26_TL)
net_sum(BART_DIFF85_TL) 

net_sum(BART_DIFF26_TPS) 
net_sum(BART_DIFF85_TPS)

g5=create_plot(BART_DIFF26_DL, title = 'Drifting longlines SSP1-1.9', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g6=create_plot(BART_DIFF85_DL, title = 'Drifting longlines SSP4-6.0', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g7=create_plot(BART_DIFF26_TL, title = 'Trawlers SSP1-1.9', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g8=create_plot(BART_DIFF85_TL, title = 'Trawlers SSP4-6.0', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g9=create_plot(BART_DIFF26_TPS, title = 'Tuna purse seines SSP1-1.9', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g10=create_plot(BART_DIFF85_TPS, title = 'Tuna purse seines SSP4-6.0', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))


pdf(paste0(path,'/figures/Figure 2.pdf'), height = 215/25.4, width = 180/25.4)
ggarrange(ggarrange(g4,g2,g3,g1, ncol=2,nrow=2,labels = 'auto', common.legend = T, legend = 'right', align = 'hv',
                    font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
          
          ggarrange(ggarrange(g5 + guides(fill = 'none'), g6, nrow = 1, ncol = 2, labels = c('e','f'),
                              legend = 'right', common.legend = TRUE, align = 'hv',
                              font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
                    
                    ggarrange(g7 + guides(fill = 'none'), g8, nrow = 1, ncol = 2, labels = c('g','h'),
                              legend = 'right', common.legend = TRUE, align = 'hv',
                              font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
                    
                    ggarrange(g9 + guides(fill = 'none'), g10, nrow = 1, ncol = 2, labels = c('i','j'),
                              legend = 'right', common.legend = TRUE, align = 'hv',
                              font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
                    nrow = 3,
                    ncol = 1, align = 'hv'), align = 'hv', nrow = 2, ncol = 1, widths = c(0.7,1.3), heights = c(0.8,1.2))
dev.off()
