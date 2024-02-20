library(ggpubr)
library(ggplot2)
library(maptools)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

BART_DIFF26_1 <- read.csv(paste0(path,'/output/countries/CHN/BART_DIFF26 (CHN).csv'))
BART_DIFF85_1 <- read.csv(paste0(path,'/output/countries/CHN/BART_DIFF85 (CHN).csv'))

BART_DIFF26_2 <- read.csv(paste0(path,'/output/countries/ISL/BART_DIFF26 (ISL).csv'))
BART_DIFF85_2 <- read.csv(paste0(path,'/output/countries/ISL/BART_DIFF85 (ISL).csv'))

BART_DIFF26_3 <- read.csv(paste0(path,'/output/countries/BRA/BART_DIFF26 (BRA).csv'))
BART_DIFF85_3 <- read.csv(paste0(path,'/output/countries/BRA/BART_DIFF85 (BRA).csv'))

data("wrld_simpl")
wrld_simpl <- fortify(wrld_simpl)

theme_cst <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=12, vjust = -5)))

g1=ggplot(BART_DIFF26_1, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title='China RCP2.6', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                      low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                      mid='white',
                      na.value='white',limits=c(-0.5,0.5)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g2=ggplot(BART_DIFF85_1, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'China RCP8.5', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                      low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                      mid='white',
                      na.value='white',limits=c(-0.5,0.5)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst


g3=ggplot(BART_DIFF26_2, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Iceland RCP2.6', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                      low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                      mid='white',
                      na.value='white',limits=c(-0.5,0.5)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g4=ggplot(BART_DIFF85_2, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Iceland RCP8.5', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-0.5,0.5)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g5=ggplot(BART_DIFF26_3, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Brazil RCP2.6', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-0.5,0.5)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g6=ggplot(BART_DIFF85_3, aes(x=x,y=y,fill=layer)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Brazil RCP8.5', fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white',limits=c(-0.5,0.5)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

png(paste0(path,'/figures/Figure 3C-H.png'), bg = 'white', height = 10.2, width = 11.3, units = 'in', res = 600)
ggarrange(
  ggarrange(g3 + guides(fill = 'none'), g4, nrow = 1, ncol = 2, labels = c('C','D'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g5 + guides(fill = 'none'), g6, nrow = 1, ncol = 2, labels = c('E','F'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g1 + guides(fill = 'none'), g2, nrow = 1, ncol = 2, labels = c('G','H'), legend = 'right', common.legend = TRUE, align = 'hv'),
  nrow = 3,
  ncol = 1
)
dev.off()
