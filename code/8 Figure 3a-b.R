library(ggplot2)
library(ggrepel)
library(ggpubr)
library(dplyr)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

color_pallet = c("#1f77b4", "#ff7f0e", "#2ca02c", "#ff9896",
                          "#9467bd", "#8c564b",  "#e377c2", "#7f7f7f",
                          "#bcbd22", "#17becf", "#9edae5", "#98df8a", "#d62728")

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

# Normalizing the changes by the spatial occupation of the countries
df85$norm <- df85$change/df85$area
df85$norm_low <- df85$change_lower_95_ci/df85$area
df85$norm_up <- df85$change_upper_95_ci/df85$area

df26$norm <- df26$change/df26$area
df26$norm_low <- df26$change_lower_95_ci/df26$area
df26$norm_up <- df26$change_upper_95_ci/df26$area

# Converting to percentages (45231 is the total number of ocean cells)
df85$area_perc <- round((df85$area/45231)*100,2)
df26$area_perc <- round((df26$area/45231)*100,2)

#Change to 1 so it does not display 0 change after rounding
df85[df85$code == 'BGR',]$change <- 1

g1 = ggplot(df85 %>%
              left_join(stat[,-1], by = 'code') %>%
              na.omit(), aes(x=area_perc,y=norm, fill = region)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(size = 2, shape = 21) +
  scale_y_continuous(limits = c(-0.07,0.06)) +
  scale_x_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
  labs(x = 'Current ocean fished (%)', y = 'Weighted net change', title = 'SSP4-6.0', fill = '') +
  scale_fill_manual(values = color_pallet) +
  geom_label_repel(aes(label = paste0(code,' (',round(change),'%)')),
                  box.padding   = 0.25,
                  size = 2.7,
                  point.padding = 0.4,
                  segment.color = 'grey50',
                  max.overlaps = 10,
                  show.legend = FALSE) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme_classic()


#Change to 1 so it does not display 0 change after rounding
df26[df26$code == 'BGR',]$change <- 1

g2 = ggplot(df26 %>%
              left_join(stat[,-1], by = 'code') %>%
              filter(!code %in% c('MLT','BGR','QAT')) %>%
              na.omit(), aes(x=area_perc,y=norm, fill = region)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(size = 2, shape = 21) +
  scale_y_continuous(limits = c(-0.07,0.06)) +
  scale_x_continuous(limits = c(0,40), breaks = seq(0,40,10)) +
  labs(x = 'Current ocean fished (%)', y = 'Weighted net change', title = 'SSP1-1.9', fill = '') +
  scale_fill_manual(values = color_pallet) +
  geom_label_repel(aes(label = paste0(code,' (',round(change),'%)')),
                   box.padding   = 0.25,
                   size = 2.7,
                   point.padding = 0.4,
                   segment.color = 'grey50',
                   max.overlaps = 10,
                   show.legend = FALSE) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme_classic()


png(paste0(path,'/figures/Figure 3A-B.png'),res = 450,units = 'in',height = 4.5, width = 12)
ggarrange(g2,g1, ncol=2,nrow=1,labels = c('A','B'), common.legend = T, legend = 'bottom')
dev.off()
