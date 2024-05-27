##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

source(paste0(path,'/code/function create_plot.R'))

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

g1=create_plot(BART_DIFF26_DL, title = 'Drifting longlines SSP1-1.9', limits = c(-1,1))

g2=create_plot(BART_DIFF85_DL, title = 'Drifting longlines SSP4-6.0', limits = c(-1,1))

g3=create_plot(BART_DIFF26_TL, title = 'Trawlers SSP1-1.9', limits = c(-1,1))

g4=create_plot(BART_DIFF85_TL, title = 'Trawlers SSP4-6.0', limits = c(-1,1))

g5=create_plot(BART_DIFF26_TPS, title = 'Tuna purse seines SSP1-1.9', limits = c(-1,1))

g6=create_plot(BART_DIFF85_TPS, title = 'Tuna purse seines SSP4-6.0', limits = c(-1,1))


png(paste0(path,'/figures/Figure 2E-J.png'), bg = 'white', height = 10.2, width = 10, units = 'in', res = 600)
ggarrange(
  ggarrange(g1 + guides(fill = 'none'), g2, nrow = 1, ncol = 2, labels = c('E','F'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g3 + guides(fill = 'none'), g4, nrow = 1, ncol = 2, labels = c('G','H'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g5 + guides(fill = 'none'), g6, nrow = 1, ncol = 2, labels = c('I','J'), legend = 'right', common.legend = TRUE, align = 'hv'),
  nrow = 3,
  ncol = 1
)
dev.off()
