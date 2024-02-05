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
  mutate(names = case_when(names == 'mean_temperature' ~ 'SST',
                           names == 'mean_salinity' ~ 'Salinity',
                           names == 'bathymetry' ~ 'Bathymetry',
                           names == 'mean_current_vel' ~ 'Currents velocity',
                           names == 'mean_ice_thickness' ~ 'Ice thickness'))

varimp_gear <- transform(varimp_gear, names = reorder(names, -ave(varimps, names, FUN = mean)))


g1 = ggplot(varimp_gear,
            aes(x = names, y = varimps)) +
  geom_bar(stat = 'summary', fun = 'mean', position = position_dodge(width = 0.8), width = 0.3, size = 0.7) +
  geom_errorbar(stat = 'summary', fun.data = 'mean_cl_boot', position = position_dodge(width = 0.8),
                width = 0.05, color = 'black', size = 0.5) +
  labs(x = '', y = 'Variable importance', fill = '') +
  theme_classic()

g2=ggplot(partial_gear %>% filter(var=='mean_temperature'),aes(x=x, y =med, color = class,fill=class)) +
  geom_ribbon(aes(ymin=q05,ymax=q95), alpha =0.2,color=NA) +
  geom_line(linewidth=0.8) +
  labs(x = 'SST (°C)', y = 'Response', fill='',color='') +
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0,1))+
  scale_color_manual(values = c('#ff7f0e','#9467bd','#d62728'),
                     labels = c('Drifting longlines', 'Trawlers','Tuna purse seines')) +
  scale_fill_manual(values = c('#ff7f0e','#9467bd','#d62728'),
                    labels = c('Drifting longlines', 'Trawlers','Tuna purse seines')) +
  theme_classic()

df <- read.csv(paste0(path,'/output/Global/BART_DIFF85 (Global).csv'))
r2 <- raster(paste0(path,'/environmental variables/present/mean_temperature.tif'))
r2 <- raster::aggregate(r2, 1/res(r2))
r1 <- raster(paste0(path,'/environmental variables/rcp85_2100/mean_temperature.tif'))
r1 <- raster::aggregate(r1, 1/res(r1))

df85 <- data.frame(mean = apply(as.matrix(r1), 1, function(x) mean(x, na.rm = TRUE)),
                   sd = apply(as.matrix(r1), 1, function(x) sd(x, na.rm = TRUE)),
                   y = unique(df$y),
                   proj = 'b_RCP8.5')

df26 <- data.frame(mean = apply(as.matrix(r2), 1, function(x) mean(x, na.rm = TRUE)),
                   sd = apply(as.matrix(r2), 1, function(x) sd(x, na.rm = TRUE)),
                   y = unique(df$y),
                   proj = 'a_Present')

df <- rbind(df26,df85)

g3 = ggplot(df, aes(y = mean, x = y, fill = proj, colour = proj)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd), alpha = .2, colour = NA) +
  labs(y = 'Mean SST (°C)', x = 'Latitude', fill = '', colour = '') +
  scale_x_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90),
                     labels = c('90º S', '60º S', '30º S',
                                '0º',
                                '30º N', '60º N', '90º N')) +
  scale_fill_manual(values = c('grey','#fec44f'), labels = c('Present', 'RCP8.5')) +
  scale_colour_manual(values = c('grey', '#fec44f'), labels = c('Present', 'RCP8.5')) +
  theme_classic()
  theme(axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.position = c(0.5,1),
        legend.justification = "center")+
  guides(
    fill = guide_legend(label.vjust = 0.5, ncol = 2),
    colour = guide_legend(label.vjust = 0.5, ncol = 2))

png(paste0(path,'/figures/Figure 4A-C.png'),res = 600,units = 'in',height = 6.2, width = 8)
ggarrange(ggarrange(g1,
                    g2,
                    ncol=1,nrow=2,labels = c('A','B'), align = 'hv',legend = 'bottom',common.legend = TRUE),
          
          ggarrange(g3 + coord_flip(),
                    align='hv',labels = 'C', legend = 'bottom', common.legend = TRUE),
          
          nrow = 1, ncol = 2, widths = c(2.1,0.9), align='hv', common.legend = TRUE)
dev.off()

