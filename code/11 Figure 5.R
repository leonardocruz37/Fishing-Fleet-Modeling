library(dplyr)
library(tidyr)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

source(paste0(path,'/code/function create_plot.R'))

#SSP4
df85 <- read.csv(paste0(path,'/output/richness_beta_SSP4.csv'))

part_85 <- df85[,c(3,6:8)] %>%
  na.omit() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(p_turn = beta.sim/beta.sor,
         p_nest = beta.sne/beta.sor)

df85[is.na(df85)] <- 0
g1=create_plot(df85 %>% rename(layer='rich_difference'), title = 'Number of countries SSP4-6.0',limits = c(-10, 10))

g2=create_plot_beta(df85, title = 'Country composition SSP4-6.0')

#SSP1
df26 <- read.csv(paste0(path,'/output/richness_beta_SSP1.csv'))

part_26 <- df26[,c(3,6:8)]  %>%
  na.omit() %>%
  summarise_if(is.numeric, sum) %>%
  mutate(p_turn = beta.sim/beta.sor,
         p_nest = beta.sne/beta.sor)

df26[is.na(df26)] <- 0
g5=create_plot(df26 %>% rename(layer='rich_difference'), title = 'Number of countries SSP1-1.9', limits=c(-10,10))

g6=create_plot_beta(df26, title = 'Country composition SSP1-1.9')

png(paste0(path,'/figures/Figure 5.png'), bg = 'white', height = 6, width = 10, units = 'in', res = 600)
ggarrange(
  ggarrange(g5 + guides(fill = 'none'), g1, nrow = 1, ncol = 2, labels = c('A','B'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g6 + guides(fill = 'none'), g2, nrow = 1, ncol = 2, labels = c('C','D'), legend = 'right', common.legend = TRUE, align = 'hv'),
  nrow = 2,
  ncol = 1
)
dev.off()