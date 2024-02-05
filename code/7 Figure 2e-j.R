library(ggpubr)
library(ggplot2)
library(maptools)
library(dplyr)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

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

data("wrld_simpl")
wrld_simpl <- fortify(wrld_simpl)

theme_cst <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=12,vjust = -5)))

g1=ggplot(BART_DIFF26_DL,
          aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title='Drifting longlines RCP2.6', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g2=ggplot(BART_DIFF85_DL, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Drifting longlines RCP8.5', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g3=ggplot(BART_DIFF26_TL, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Trawlers RCP2.6', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g4=ggplot(BART_DIFF85_TL, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Trawlers RCP8.5', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g5=ggplot(BART_DIFF26_TPS, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Tuna purse seines RCP2.6', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g6=ggplot(BART_DIFF85_TPS, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Tuna purse seines RCP8.5', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-1,1)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst


png(paste0(path,'/figures/Figure 2E-J.png'), bg = 'white', height = 10.2, width = 11.3, units = 'in', res = 600)
ggarrange(
  ggarrange(g1 + guides(fill = 'none'), g2, nrow = 1, ncol = 2, labels = c('E','F'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g3 + guides(fill = 'none'), g4, nrow = 1, ncol = 2, labels = c('G','H'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g5 + guides(fill = 'none'), g6, nrow = 1, ncol = 2, labels = c('I','J'), legend = 'right', common.legend = TRUE, align = 'hv'),
  nrow = 3,
  ncol = 1
)
dev.off()
