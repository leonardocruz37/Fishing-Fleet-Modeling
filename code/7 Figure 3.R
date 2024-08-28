library(ggrepel)
library(dplyr)


##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

source(paste0(path,'/code/function create_plot.R'))


####
########## FIGURE 3a-b #########
####


color_pallet <- c("#1f77b4", "#ff7f0e", "#9467bd", '#F0E442',
                  "#d62728", "#17becf", '#8c564b')

country_list <- unlist(read.table(paste0(path,'/data/countries.txt'), header = FALSE))

# World bank classification
stat <- read.csv(paste0(path,'/data/region.csv')) %>%
  filter(code %in% country_list) %>%
  na.omit()

# The only Central Asia country was Russia in our data
stat$region[stat$region == 'Europe & Central Asia'] <- 'Europe & Russia'

df85 <- NULL
df26 <- NULL
for(do in country_list){
  
  df85 <- rbind(df85,
                read.csv(paste0(path,'/output/countries/',do,'/BART_DIFF85 (',do,').csv')) %>%
                  na.omit() %>%
                  summarise(change = sum(layer),
                            change_lower_95_ci = sum(lower_95_ci),
                            change_upper_95_ci = sum(upper_95_ci),
                            change_abs = sum(abs(layer))) %>%
                  mutate(code = do,
                         area = nrow(read.csv(paste0(path,'/output/countries/',do,'/BART_P_presence (',do,').csv')) %>%
                                       na.omit())))
  
  df26 <- rbind(df26,
                read.csv(paste0(path,'/output/countries/',do,'/BART_DIFF26 (',do,').csv')) %>%
                  na.omit() %>%
                  summarise(change = sum(layer),
                            change_lower_95_ci = sum(lower_95_ci),
                            change_upper_95_ci = sum(upper_95_ci),
                            change_abs = sum(abs(layer))) %>%
                  mutate(code = do,
                         area = nrow(read.csv(paste0(path,'/output/countries/',do,'/BART_P_presence (',do,').csv') %>%
                                                na.omit()))))
  
}

#Ratio of change between scenarios countries
sum(df85$change_abs)/sum(df26$change_abs)

# Converting to percentages (45231 is the total number of ocean cells)
df85$area_perc <- round((df85$area/45231)*100,2)
df26$area_perc <- round((df26$area/45231)*100,2)

g1 = ggplot(df85 %>%
              left_join(stat[,-1], by = 'code') %>%
              na.omit(), aes(x=area_perc,y=change, fill = region)) +
  geom_hline(yintercept = 0, linetype = 3, linewidth=0.25) +
  geom_errorbar(aes(ymin = change_lower_95_ci, ymax = change_upper_95_ci), linewidth= 0.2, width = 0, color = 'grey') +
  geom_point(size = 1, shape = 21, stroke = 0.2) +
  scale_x_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
  labs(x = 'Current ocean fished (%)', y = 'Net change (%)', title = 'SSP4-6.0', fill = '') +
  scale_fill_manual(values = color_pallet) +
  scale_color_manual(values = color_pallet) +
  geom_text_repel(aes(label = sub('ECU','', code), #omit label for more space
                      color = region),
                   size = 2,
                   point.padding = 0.1,
                   segment.color = 'grey50',
                   box.padding = 0.1,
                   max.overlaps = 9,
                   show.legend = FALSE) +
  guides(fill = guide_legend(order = 1, barwidth = unit(3,'mm'), barheight = unit(3,'mm'),override.aes = list(size = 3))) +
  theme_classic()  +
  theme(plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 6, family = "Helvetica"),
        axis.title = element_text(size = 7, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        legend.text = element_text(size = 5, family = "Helvetica", hjust = 0),)

g2 = ggplot(df26 %>%
              left_join(stat[,-1], by = 'code') %>%
              na.omit(), aes(x=area_perc,y=change, fill = region)) +
  geom_hline(yintercept = 0, linetype = 3, linewidth=0.25) +
  geom_errorbar(aes(ymin = change_lower_95_ci, ymax = change_upper_95_ci), linewidth= 0.2, width = 0, color = 'grey') +
  geom_point(size = 1, stroke = 0.2, shape = 21) +
  scale_x_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
  labs(x = 'Current ocean fished (%)', y = 'Net change (%)', title = 'SSP1-1.9', fill = '') +
  scale_fill_manual(values = color_pallet) +
  scale_color_manual(values = color_pallet) +
  geom_text_repel(aes(label = sub('ECU','', code), #omit label for more space
                      color = region),
                  size = 2,
                  point.padding = 0.1,
                  box.padding = 0.1,
                  segment.color = 'grey50',
                  max.overlaps = 5,
                  show.legend = FALSE) +
  guides(fill = guide_legend(order = 1, barwidth = unit(3,'mm'), barheight = unit(3,'mm'),override.aes = list(size = 3))) +
  theme_classic()  +
  theme(plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
        axis.text = element_text(size = 6, family = "Helvetica"),
        axis.title = element_text(size = 7, family = "Helvetica"),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        legend.text = element_text(size = 5, family = "Helvetica", hjust = 0),)


####
########## FIGURE 3c-h #########
####

BART_DIFF26_1 <- read.csv(paste0(path,'/output/countries/CHN/BART_DIFF26 (CHN).csv'))
BART_DIFF85_1 <- read.csv(paste0(path,'/output/countries/CHN/BART_DIFF85 (CHN).csv'))

BART_DIFF26_2 <- read.csv(paste0(path,'/output/countries/ISL/BART_DIFF26 (ISL).csv'))
BART_DIFF85_2 <- read.csv(paste0(path,'/output/countries/ISL/BART_DIFF85 (ISL).csv'))

BART_DIFF26_3 <- read.csv(paste0(path,'/output/countries/BRA/BART_DIFF26 (BRA).csv'))
BART_DIFF85_3 <- read.csv(paste0(path,'/output/countries/BRA/BART_DIFF85 (BRA).csv'))


g3=create_plot(BART_DIFF26_1, title='China SSP1-1.9', limits = c(-0.25,0.25)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g4=create_plot(BART_DIFF85_1, title='China SSP4-6.0', limits = c(-0.25,0.25)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g5=create_plot(BART_DIFF26_2, title='Iceland SSP1-1.9', limits = c(-0.25,0.25)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g6=create_plot(BART_DIFF85_2, title='Iceland SSP4-6.0', limits = c(-0.25,0.25)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g7=create_plot(BART_DIFF26_3, title='Brazil SSP1-1.9', limits = c(-0.25,0.25)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

g8=create_plot(BART_DIFF85_3, title='Brazil SSP4-6.0', limits = c(-0.25,0.25)) +
  guides(fill = guide_colourbar(barwidth = unit(3,'mm'), barheight = unit(18,'mm')))

pdf(paste0(path,'/figures/Figure 3.pdf'), height = 210/25.4, width = 180/25.4)
ggarrange(ggarrange(g2,g1, ncol=2,nrow=1,labels = c('a','b'), common.legend = T, legend = 'bottom', align = 'hv',
                    font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
          
          ggarrange(
            ggarrange(g5 + guides(fill = 'none'), g6, nrow = 1, ncol = 2, labels = c('c','d'), legend = 'right', common.legend = TRUE, align = 'hv',
                      font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
            
            ggarrange(g7 + guides(fill = 'none'), g8, nrow = 1, ncol = 2, labels = c('e','f'), legend = 'right', common.legend = TRUE, align = 'hv',
                      font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
            
            ggarrange(g3 + guides(fill = 'none'), g4, nrow = 1, ncol = 2, labels = c('g','h'), legend = 'right', common.legend = TRUE, align = 'hv',
                      font.label = list(size = 7, color = "black", face = "bold", family = "Helvetica")),
            nrow = 3,
            ncol = 1), ncol = 1, nrow = 2, heights = c(1.16,1.84), align = 'hv')
dev.off()
