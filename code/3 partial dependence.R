library(embarcadero)
library(fuzzySim)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

set.seed(2024)

path <- "~/Project Fishing Fleet Modeling"

# Raster data
setwd(paste0(path, "/environmental variables/present/"))
var_stack <- stack(list.files(".", full.names = TRUE, pattern = '.nc'))
var_stack <- raster::aggregate(var_stack, 1/res(var_stack))
setwd(path)

# MMSI data
dat.all <- read.csv(paste0(path,'/data/mmsi_2013-2020_processed.csv')) %>%
  left_join(read.csv(paste0(path,'/data/metadata.csv'))[,c(1,4,8)], by = 'mmsi') %>%
  filter(vessel_class_gfw != 'fishing') %>%
  na.omit()


### Due to being highly computationally demanding we performed
### the partial dependence analysis on a subsample of the occurrence data used
### for the modeling and predictions

subsample_size = 5000 # 5000 presences + 5000 absences = 10000

vessel_classes <- sort(unique(dat.all$vessel_class_gfw))
list <- c('Global', vessel_classes)

country_list <- c('BRA','CHN','ISL')


##### FISHING GEARS PARTIAL DEPENDENCE

varimp_df <- list()
partial_df <- list()
for(do in list){
  
  if(do == 'Global'){
    
    dat.presence <- dat.all %>%
      select(cell_ll_lon,cell_ll_lat) %>%
      setNames(., c('x','y')) %>%
      distinct()
    
    dat <- fuzzySim::gridRecords(var_stack, dat.presence)
    dat <- dat[complete.cases(dat),]
  }
  
  if(do %in% vessel_classes){
    
    dat.presence <- dat.all %>%
      filter(vessel_class_gfw == do) %>%
      select(cell_ll_lon,cell_ll_lat) %>%
      setNames(., c('x','y')) %>%
      distinct()
    
    dat <- fuzzySim::gridRecords(var_stack, dat.presence)
    dat <- dat[complete.cases(dat),]
  }
  
  #Subsampling the data
  dat <- dat %>%
    group_by(presence) %>%
    slice_sample(n=subsample_size) %>%
    ungroup() %>%
    as.data.frame()
  
  # Modeling
  names(dat)
  spc_col <- 1 # presence (1) - absence (0) columns
  var_cols <- 5:12 # predictor variable columns
  names(dat)[spc_col]
  names(dat)[var_cols]
  myspecies <- names(dat)[spc_col]
  myspecies
  
  mod_BART <- bart(x.train = dat[ , var_cols], y.train = dat[ , myspecies], keeptrees = TRUE)
  
  #Partial dependence
  for(i in names(dat)[var_cols]){
    partial_BART <- partial(mod_BART, x.vars = i, smooth = 5, trace = FALSE)
    partial_df <- rbind(partial_df, partial_BART[[1]][["data"]] %>% mutate(class = do, var = i))
  }
  
  # Delete model to free memory space
  rm(mod_BART)
  
}

write.csv(partial_df, paste0(path,'/output/partial_dependence_gear_global.csv'), row.names = FALSE)



##### COUNTRIES PARTIAL DEPENDENCE

varimp_df <- list()
partial_df <- list()
for(do in country_list){
  
  dat.presence <- dat.all %>%
    filter(flag_gfw == do) %>%
    select(cell_ll_lon,cell_ll_lat) %>%
    setNames(., c('x','y')) %>%
    distinct()
  
  # Modeling 1 (the historical fishing range)
  dat <- fuzzySim::gridRecords(var_stack, dat.presence)
  dat <- dat[complete.cases(dat),]
  
  #Subsampling
  dat <- dat %>%
    group_by(presence) %>%
    slice_sample(n=subsample_size) %>%
    ungroup() %>%
    as.data.frame()
  
  names(dat)
  spc_col <- 1 # presence (1) - absence (0) columns
  var_cols <- 5:12 # predictor variable columns
  names(dat)[spc_col]
  names(dat)[var_cols]
  myspecies <- names(dat)[spc_col]
  myspecies
  
  mod_BART <- bart(x.train = dat[ , var_cols], y.train = dat[ , myspecies], keeptrees = TRUE)
  
  fishing_range <- predict2.bart(mod_BART, var_stack)
  names(fishing_range) <- 'fishing_range'
  
  # Delete model to free memory space
  rm(mod_BART)
  
  # Modeling 2 (actual final model - fishing range + environmental variables)
  dat <- cbind(dat, data.frame(raster::extract(fishing_range,dat[2:3])))
  dat <- dat[complete.cases(dat),]
  
  names(dat)
  spc_col <- 1 # presence (1) - absence (0) columns
  var_cols <- 5:13 # predictor variable columns + historical fishing range
  names(dat)[spc_col]
  names(dat)[var_cols]
  myspecies <- names(dat)[spc_col]
  myspecies
  
  mod_BART <- bart(x.train = dat[ , var_cols], y.train = dat[ , myspecies], keeptrees = TRUE)
  
  # Partial dependence
  for(i in names(dat)[var_cols]){
    partial_BART <- partial(mod_BART, x.vars = i, smooth = 5, trace = FALSE)
    partial_df <- rbind(partial_df, partial_BART[[1]][["data"]] %>% mutate(class = do, var = i))
  }
  
  # Delete model to free memory space
  rm(mod_BART)
  
}

write.csv(partial_df, paste0(path,'/output/partial_dependence_countries.csv'), row.names = FALSE)