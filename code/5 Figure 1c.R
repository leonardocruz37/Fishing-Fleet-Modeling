library(ggplot2)
library(ggpubr)
library(raster)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "C:/Users/leona/Desktop/Project Fishing Fleet Modeling"

df <- read.csv(paste0(path,'/output/Global/BART_DIFF85 (Global).csv'))
r1 <- raster(paste0(path,'/environmental variables/present/mean_temperature.tif'))
r1 <- raster::aggregate(r1, 1/res(r1)) * 0
r1 <- rasterize(df[1:2], r1, df$layer, fun = sum, na.rm = FALSE)

df85 <- data.frame(mean = apply(as.matrix(r1), 1, function(x) mean(x, na.rm = TRUE)),
                   sd = apply(as.matrix(r1), 1, function(x) sd(x, na.rm = TRUE)),
                   y = unique(df$y),
                   proj = 'a_RCP8.5')

df <- read.csv(paste0(path,'/output/Global/BART_DIFF26 (Global).csv'))
r1 <- r1 * 0
r1 <- rasterize(df[1:2], r1, df$layer, fun = sum, na.rm = FALSE)

df26 <- data.frame(mean = apply(as.matrix(r1), 1, function(x) mean(x, na.rm = TRUE)),
                   sd = apply(as.matrix(r1), 1, function(x) sd(x, na.rm = TRUE)),
                   y = unique(df$y),
                   proj = 'b_RCP2.6')

df <- rbind(df26,df85)

g1=ggplot(df, aes(y = mean, x = y, fill = proj, colour = proj)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean +sd), alpha = .2, colour = NA) +
                labs(y = expression(paste('Mean ', Delta, '2100-2014')), x = 'Latitude', fill = '', colour = '') +
                scale_x_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90),
                                   labels = c('90º S', '60º S', '30º S',
                                              '0º',
                                              '90º N', '60º N', '30º N')) +
                scale_y_continuous(limits = c(-0.25, 0.75), breaks = seq(-0.25, 0.75, 0.25)) +
                scale_fill_manual(values = c('#fec44f', '#5ab4ac'), labels = c('RCP8.5', 'RCP2.6')) +
                scale_colour_manual(values = c('#fec44f', '#5ab4ac'), labels = c('RCP8.5', 'RCP2.6')) +
                theme_classic() +
                theme(axis.title.x = element_text(size = 8),
                      axis.text.x = element_text(size = 8),
                      axis.title.y = element_text(size = 8),
                      axis.text.y = element_text(size = 8),
                      legend.text = element_text(size = 8),
                      legend.position = c(0.5,1),
                      legend.justification = "center")+
                guides(
                  fill = guide_legend(label.hjust = 0.5, ncol = 2),
                  colour = guide_legend(label.hjust = 0.5, ncol = 2)
                )

png(paste0(path,'/figures/Figure 1C.png'), height = 2.3, width = 6, units = 'in', res = 600)
ggarrange(g1, labels = 'C')
dev.off()
