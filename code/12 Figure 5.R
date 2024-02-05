library(ggplot2)
library(maptools)
library(ggpubr)
library(dplyr)
library(tidyr)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

theme_cst <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size=12,vjust = -5)))

path <- "C:/Users/leona/Desktop/Project Fishing Fleet Modeling"

data('wrld_simpl')
wrld_simpl=fortify(wrld_simpl)

#RCP85
df85 <- read.csv(paste0(path,'/output/richness_beta_RCP85.csv'))

part_85 <- df85[,c(3,6:8)] %>%
  na.omit() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(p_turn = beta.sim/beta.sor,
         p_nest = beta.sne/beta.sor)

df85[is.na(df85)] <- 0
g1=ggplot(df85, aes(x=x,y=y,fill=rich_difference)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Number of countries RCP8.5',fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white', limits = c(-10, 10)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g2=ggplot(df85 %>%
            mutate(beta.sor.discrete = case_when(beta.sor == 0 ~ '0.0',
                                                 beta.sor > 0 & beta.sor < 0.2 ~ '0.0-0.2',
                                                 beta.sor >= 0.2 & beta.sor < 0.4 ~ '0.2-0.4',
                                                 beta.sor >= 0.4 & beta.sor < 0.6 ~ '0.4-0.6',
                                                 beta.sor >= 0.6 & beta.sor < 0.8 ~ '0.6-0.8',
                                                 beta.sor >= 0.8 & beta.sor <= 1 ~ '0.8-1')),
          aes(x=x,y=y,fill=beta.sor.discrete)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Country composition RCP8.5',fill = 'Dissimilarity') +
  scale_fill_manual(values = c('0.0'='white',
                               '0.0-0.2'='#d9f0d3',
                               '0.2-0.4'='#89A3B9',
                               #'0.2-0.4'='#DBE3C2',
                               '0.4-0.6'='#FFD498',
                               '0.6-0.8'='#F67F51',
                               '0.8-1'='#a50f15')) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  guides(fill = guide_legend(override.aes = list(colour = "black", size = 2))) +
  theme_cst 

#RCP26
df26 <- read.csv(paste0(path,'/output/richness_beta_RCP26.csv'))

part_26 <- df26[,c(3,6:8)]  %>%
  na.omit() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(p_turn = beta.sim/beta.sor,
         p_nest = beta.sne/beta.sor)

df26[is.na(df26)] <- 0
g5=ggplot(df26, aes(x=x,y=y,fill=rich_difference)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Number of countries RCP2.6',fill = expression(paste(Delta,'2100-2014'))) +
  scale_fill_gradient2(high = c('#cdd2e4','#a6a2cc','#7a74ad','#513294','#241349'),
                       low=c('#783b16','#ae5d25','#e0842a','#fbb268','#fedbae'),
                       mid='white',
                       na.value='white', limits = c(-10,10)) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  theme_cst

g6=ggplot(df26 %>%
            mutate(beta.sor.discrete = case_when(beta.sor == 0 ~ '0.0',
                                                 beta.sor > 0 & beta.sor < 0.2 ~ '0.0-0.2',
                                                 beta.sor >= 0.2 & beta.sor < 0.4 ~ '0.2-0.4',
                                                 beta.sor >= 0.4 & beta.sor < 0.6 ~ '0.4-0.6',
                                                 beta.sor >= 0.6 & beta.sor < 0.8 ~ '0.6-0.8',
                                                 beta.sor >= 0.8 & beta.sor <= 1 ~ '0.8-1')),
          aes(x=x,y=y,fill=beta.sor.discrete)) +
  geom_raster() +
  geom_polygon(data=wrld_simpl,aes(x=long,y=lat,group=group),fill='black',color='gray30') +
  labs(title = 'Country composition RCP2.6',fill = 'Dissimilarity') +
  scale_fill_manual(values = c('0.0'='white',
                               '0.0-0.2'='#d9f0d3',
                               '0.2-0.4'='#89A3B9',
                               #'0.2-0.4'='#DBE3C2',
                               '0.4-0.6'='#FFD498',
                               '0.6-0.8'='#F67F51',
                               '0.8-1'='#a50f15')) +
  scale_y_continuous(labels=c('0º'),breaks=c(0)) +
  scale_x_continuous(labels = c('180º','120º W','60º W','0º','60º E','120º E','180º'),
                     breaks = c(-180,-120,-60,0,60,120,180)) +
  guides(fill = guide_legend(override.aes = list(colour = "black", size = 2))) +
  theme_cst 

png(paste0(path,'/figures/Figure 5.png'), bg = 'white', height = 6, width = 10, units = 'in', res = 600)
ggarrange(
  ggarrange(g5 + guides(fill = 'none'), g1, nrow = 1, ncol = 2, labels = c('A','B'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g6 + guides(fill = 'none'), g2, nrow = 1, ncol = 2, labels = c('C','D'), legend = 'right', common.legend = TRUE, align = 'hv'),
  nrow = 2,
  ncol = 1
)
dev.off()