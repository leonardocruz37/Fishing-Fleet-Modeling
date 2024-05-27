##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

source(paste0(path,'/code/function create_plot.R'))

BART_DIFF26_1 <- read.csv(paste0(path,'/output/countries/CHN/BART_DIFF26 (CHN).csv'))
BART_DIFF85_1 <- read.csv(paste0(path,'/output/countries/CHN/BART_DIFF85 (CHN).csv'))

BART_DIFF26_2 <- read.csv(paste0(path,'/output/countries/ISL/BART_DIFF26 (ISL).csv'))
BART_DIFF85_2 <- read.csv(paste0(path,'/output/countries/ISL/BART_DIFF85 (ISL).csv'))

BART_DIFF26_3 <- read.csv(paste0(path,'/output/countries/BRA/BART_DIFF26 (BRA).csv'))
BART_DIFF85_3 <- read.csv(paste0(path,'/output/countries/BRA/BART_DIFF85 (BRA).csv'))


g1=create_plot(BART_DIFF26_1, title='China SSP1-1.9', limits = c(-0.25,0.25))

g2=create_plot(BART_DIFF85_1, title='China SSP4-6.0', limits = c(-0.25,0.25))

g3=create_plot(BART_DIFF26_2, title='Iceland SSP1-1.9', limits = c(-0.25,0.25))

g4=create_plot(BART_DIFF85_2, title='Iceland SSP4-6.0', limits = c(-0.25,0.25))

g5=create_plot(BART_DIFF26_3, title='Brazil SSP1-1.9', limits = c(-0.25,0.25))

g6=create_plot(BART_DIFF85_3, title='Brazil SSP4-6.0', limits = c(-0.25,0.25))

png(paste0(path,'/figures/Figure 3C-H.png'), bg = 'white', height = 10.2, width = 10, units = 'in', res = 600)
ggarrange(
  ggarrange(g3 + guides(fill = 'none'), g4, nrow = 1, ncol = 2, labels = c('C','D'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g5 + guides(fill = 'none'), g6, nrow = 1, ncol = 2, labels = c('E','F'), legend = 'right', common.legend = TRUE, align = 'hv'),
  ggarrange(g1 + guides(fill = 'none'), g2, nrow = 1, ncol = 2, labels = c('G','H'), legend = 'right', common.legend = TRUE, align = 'hv'),
  nrow = 3,
  ncol = 1
)
dev.off()
