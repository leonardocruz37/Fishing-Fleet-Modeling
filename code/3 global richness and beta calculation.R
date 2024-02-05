library(dplyr)
library(betapart)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

path <- "~/Project Fishing Fleet Modeling"

country_list <- unlist(read.table(paste0(path,'/data/countries.txt'), header = FALSE))

#Getting all the coordinates of the world map
df_p <- read.csv(paste0(path,'/output/countries/ARG/BART_P (ARG).csv'))[,1:2]
df_85 <- read.csv(paste0(path,'/output/countries/ARG/BART_P (ARG).csv'))[,1:2]
df_26 <- read.csv(paste0(path,'/output/countries/ARG/BART_P (ARG).csv'))[,1:2]

# Creating the presence-absence 'species community matrix', where each row (cell) is a community
# and the columns species (countries).
for(do in country_list){
  
  df_p <- df_p %>%
    left_join(read.csv(paste0(path,'/output/countries/',do,'/BART_P_presence (',do,').csv')) %>%
                select(x, y, layer) %>%
                mutate(layer = 1) %>%
                setNames(.,c('x','y', do)), by = c('x','y'))
  
  df_85 <- df_85 %>%
    left_join(read.csv(paste0(path,'/output/countries/',do,'/BART_F85_presence (',do,').csv')) %>%
                select(x, y, layer) %>%
                mutate(layer = 1) %>%
                setNames(.,c('x','y', do)), by = c('x','y'))
  
  df_26 <- df_26 %>%
    left_join(read.csv(paste0(path,'/output/countries/',do,'/BART_F26_presence (',do,').csv')) %>%
                select(x, y, layer) %>%
                mutate(layer = 1) %>%
                setNames(.,c('x','y', do)), by = c('x','y'))
  
}

df_p[is.na(df_p)] <- 0
df_85[is.na(df_85)] <- 0
df_26[is.na(df_26)] <- 0

# RCP85

# Richness
df85 <- cbind(df_p[,1:2], rowSums(df_85[,3:84]) - rowSums(df_p[,3:84]),rowSums(df_85[,3:84]), rowSums(df_p[,3:84])) %>%
  setNames(., c('x','y','rich_difference','rich85','rich_p'))

# Beta diversity
beta.temp_85 <- list()
for(i in 1:64800){
  beta.temp_85 <- rbind(beta.temp_85, beta.temp(df_p[i,3:84],df_85[i,3:84], index.family = 'sorensen'))
}

df85 <- cbind(df85, beta.temp_85)
write.csv(df85, paste0(path, '/output/richness_beta_RCP85.csv'), row.names = FALSE)


# RCP26

# Richness
df26 <- cbind(df_p[,1:2], rowSums(df_26[,3:84]) - rowSums(df_p[,3:84]),rowSums(df_26[,3:84]), rowSums(df_p[,3:84])) %>%
  setNames(., c('x','y','rich_difference','rich26','rich_p'))

# Beta diversity
beta.temp_26 <- list()
for(i in 1:64800){
  beta.temp_26 <- rbind(beta.temp_26, beta.temp(df_p[i,3:84],df_26[i,3:84], index.family = 'sorensen'))
}

df26 <- cbind(df26, beta.temp_26)
write.csv(df26, paste0(path,'/output/richness_beta_RCP26.csv'), row.names = FALSE)