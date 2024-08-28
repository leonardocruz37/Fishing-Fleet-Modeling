library(dplyr)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

source(paste0(path,'/code/function create_plot.R'))

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

g1=create_plot(BART_DIFF26, title='Industrial fishing fleet SSP1-1.9', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(16,'mm')))

g2=create_plot(BART_DIFF85, title = 'Industrial fishing fleet SSP4-6.0', limits = c(-1,1)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(16,'mm')))


pdf(paste0(path,'/figures/Figure 1.pdf'), height = 100/25.4, width = 88/25.4)
ggarrange(g1,g2,
          nrow=2,
          labels = 'auto',
          align = 'hv',
          font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica"))
dev.off()
