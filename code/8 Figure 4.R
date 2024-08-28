library(dplyr)
library(ggpubr)
library(ggplot2)
library(raster)
library(RColorBrewer)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

df <- read.csv(paste0(path, '/output/partial_dependence_gear_global.csv'))
partial_gear <- subset(df, class %in% c('tuna_purse_seines','trawlers', 'drifting_longlines'))

varimp_gear <- read.csv(paste0(path,'/output/variable_importance_gear_global.csv')) %>%
                       filter(!class %in% c('other_seines', 'other_purse_seines','Global')) %>%
                       mutate(class = 'Fishing gears') %>%
  mutate(names = case_when(names == 'Average.OceanTemperature.1' ~ 'Bottom temperature',
                           names == 'Average.OceanTemperature.2' ~ 'Surface temperature',
                           names == 'Average.Salinity' ~ 'Surface salinity',
                           names == 'Bathymetry.Mean' ~ 'Bathymetry',
                           names == 'Average.SeaWaterSpeed.1' ~ 'Bottom currents',
                           names == 'Average.SeaWaterSpeed.2' ~ 'Surface currents',
                           names == 'Average.SeaIceThickness' ~ 'Ice cover',
                           names == 'Average.Chlorophyll' ~ 'Chlorophyll-a'))

varimp_gear <- transform(varimp_gear, names = reorder(names, -ave(varimps, names, FUN = mean)))


#### SUMMARY
partial_gear %>%
  filter(var=='Average.OceanTemperature.2') %>%
  group_by(class) %>%
  filter(med == max(med))

varimp_gear %>%
  group_by(names) %>%
  summarise_if(is.numeric,mean)

varimp_gear %>%
  group_by(names) %>%
  summarise_if(is.numeric,sd)
####

g1 = ggplot(varimp_gear, aes(x = names, y = varimps)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.3) +
  geom_jitter(width = 0.2, size = 0.3, alpha = 0.6) +
  labs(x = '', y = 'Variable importance', fill = '') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 5, family = "Helvetica"),
        axis.title = element_text(size = 5, family = "Helvetica"),
        legend.text = element_text(size = 4, family = "Helvetica", hjust = 1),
        legend.title = element_text(size = 5, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25))

g2=ggplot(partial_gear %>% filter(var=='Average.OceanTemperature.2'),aes(x=x, y =med, color = class,fill=class)) +
  geom_ribbon(aes(ymin=q05,ymax=q95), alpha =0.2,color=NA) +
  geom_line(linewidth=0.5) +
  labs(x = 'Surface temperature (°C)', y = 'Response', fill='',color='') +
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0,1))+
  scale_color_manual(values = c('#ff7f0e','#9467bd','#1f77b4'),
                     labels = c('Drifting longlines', 'Trawlers','Tuna purse seines')) +
  scale_fill_manual(values = c('#ff7f0e','#9467bd','#1f77b4'),
                    labels = c('Drifting longlines', 'Trawlers','Tuna purse seines')) +
  theme_classic() +
  theme(plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 5, family = "Helvetica"),
        axis.title = element_text(size = 5, family = "Helvetica"),
        legend.text = element_text(size = 4, family = "Helvetica", hjust = 1),
        legend.title = element_text(size = 5, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25)) +
  guides(fill = guide_legend(barwidth = unit(3,'mm'), barheight = unit(3,'mm')),
         color = guide_legend(barwidth = unit(3,'mm'), barheight = unit(3,'mm')))


pdf(paste0(path,'/figures/Figure 4.pdf'), height = 95/25.4, width = 70/25.4)
ggarrange(g1,
          g2,
          ncol=1,nrow=2,labels = c('a','b'),legend = 'bottom',common.legend = TRUE,
          font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica"))
dev.off()
