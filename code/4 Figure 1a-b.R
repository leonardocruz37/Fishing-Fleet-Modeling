library(ggpubr)
library(ggplot2)
library(maptools)
library(tidyverse)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "C:/Users/leona/Desktop/Project Fishing Fleet Modeling"

BART_DIFF26 <- read.csv(paste0(path,'/output/Global/BART_DIFF26 (Global).csv'))
BART_DIFF85 <- read.csv(paste0(path,'/output/Global/BART_DIFF85 (Global).csv'))

#Summary net changes
BART_DIFF26  %>%
  mutate(type = case_when(layer > 0 ~ 'increase',
                          layer < 0 ~ 'decrease')) %>%
  group_by(type) %>%
  summarise_all(sum, .keep_all=TRUE)

BART_DIFF85 %>%
  mutate(type = case_when(layer > 0 ~ 'increase',
                          layer < 0 ~ 'decrease')) %>%
  group_by(type) %>%
  summarise_all(sum, .keep_all=TRUE)

#Ratio of change between scenarios globally
sum(abs(na.omit(BART_DIFF85$layer)))/sum(abs(na.omit(BART_DIFF26$layer)))
#lower
sum(abs(na.omit(BART_DIFF85$lower_95_ci)))/sum(abs(na.omit(BART_DIFF26$lower_95_ci)))
#upper
sum(abs(na.omit(BART_DIFF85$upper_95_ci)))/sum(abs(na.omit(BART_DIFF26$upper_95_ci)))

data("wrld_simpl")
wrld_simpl <- fortify(wrld_simpl)

theme_cst <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=12,vjust = -3,hjust = 0.5)))

g1=ggplot(BART_DIFF26, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='grey30') +
  labs(title='RCP2.6', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g2=ggplot(BART_DIFF85, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='grey30') +
  labs(title = 'RCP8.5', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst 

png(paste0(path,'/figures/Figure 1A-B.png'), bg = 'white', height = 8, width = 8, units = 'in', res = 600)
ggarrange(g1,g2,
          nrow=2,
          labels = 'AUTO',
          align = 'hv')
dev.off()
