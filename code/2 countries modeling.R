library(embarcadero)
library(fuzzySim)

##  CLEAR EVERYTHING
rm(list = ls(all.names = TRUE))

set.seed(2023)

path <- "C:/Users/leona/Desktop/Project Fishing Fleet Modeling"
           
# Raster data
setwd(paste0(path, "/environmental variables/present/"))
var_stack <- stack(list.files(".", full.names = TRUE, pattern = '.tif'))
var_stack <- raster::aggregate(var_stack, 1/res(var_stack))

setwd(paste0(path, "/environmental variables/rcp85_2100/"))
var_stack_f85 <- stack(list.files(".", full.names = TRUE, pattern = '.tif'))
var_stack_f85 <- raster::aggregate(var_stack_f85, 1/res(var_stack_f85))

setwd(paste0(path, "/environmental variables/rcp26_2100/"))
var_stack_f26 <- stack(list.files(".", full.names = TRUE, pattern = '.tif'))
var_stack_f26 <- raster::aggregate(var_stack_f26, 1/res(var_stack_f26))
setwd(path)

# MMSI data
dat.all <- read.csv(paste0(path,'/data/mmsi_2013-2020_processed.csv')) %>%
  left_join(read.csv(paste0(path,'/data/metadata.csv'))[,c(1,4,8)], by = 'mmsi') %>%
  filter(vessel_class_gfw != 'fishing') %>%
  na.omit()

#The list of 82 countries with fishing hours >= median
country_list <- unlist(read.table(paste0(path,'/data/countries.txt'), header = FALSE))

### Partial dependence (it takes A LOT of time to run, especially in large datasets)
### if do_subsample is true it will ONLY do and save the partial dependence
do_partial = FALSE
do_subsample = FALSE
subsample_size = 5000
country_list <- c('BRA','CHN','ISL')

varimp_df <- list()
partial_df <- list()
for(do in country_list){
  
  ifelse(!dir.exists(file.path(paste0(path,'/output/countries/'), do)), dir.create(file.path(paste0(path,'/output/countries/'), do)), FALSE)
  do_path <- paste0(path,'/output/countries/', do)
  
  dat.presence <- dat.all %>%
    filter(flag_gfw == do) %>%
    select(cell_ll_lon,cell_ll_lat) %>%
    setNames(., c('x','y')) %>%
    distinct()
  
  # Modeling 1 (the historical fishing range)
  dat <- fuzzySim::gridRecords(var_stack, dat.presence)
  dat <- dat[complete.cases(dat),]
  
  if(all(do_partial,do_subsample)){
    
    dat <- dat %>%
      group_by(presence) %>%
      slice_sample(n=subsample_size) %>%
      ungroup() %>%
      as.data.frame()
  }
  
  names(dat)
  spc_col <- 1 # presence (1) - absence (0) columns
  var_cols <- 5:9 # predictor variable columns
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
  var_cols <- 5:10 # predictor variable columns + historical fishing range
  names(dat)[spc_col]
  names(dat)[var_cols]
  myspecies <- names(dat)[spc_col]
  myspecies
  
  mod_BART <- bart(x.train = dat[ , var_cols], y.train = dat[ , myspecies], keeptrees = TRUE)
  
  if(!do_subsample){
    
    #Save summary
    sink(paste0(do_path, '/Table 1. BART summary (', do,').txt'), type = 'output')
    print(summary(mod_BART))
    sink()
    
    # Variable importance
    varimp_plot <- varimp(mod_BART, plots = TRUE)
    varimp_plot$class <- do
    varimp_df <- rbind(varimp_df, varimp_plot)
    
    # Raster predictions
    BART_P <- as.data.frame(predict2.bart(mod_BART, stack(var_stack, fishing_range), quantiles=c(0.025, 0.975)),xy=TRUE)
    BART_F85 <- as.data.frame(predict2.bart(mod_BART, stack(var_stack_f85, fishing_range), quantiles=c(0.025, 0.975)),xy=TRUE)
    BART_F26 <- as.data.frame(predict2.bart(mod_BART, stack(var_stack_f26, fishing_range), quantiles=c(0.025, 0.975)),xy=TRUE)
    
    # 'layer' is the posterior mean
    colnames(BART_P) <- c('x','y','layer','lower_95_ci','upper_95_ci')
    colnames(BART_F85) <- c('x','y','layer','lower_95_ci','upper_95_ci')
    colnames(BART_F26) <- c('x','y','layer','lower_95_ci','upper_95_ci')
    
    # Difference
    BART_DIFF85 <- data.frame(x=BART_P[,1],
                              y=BART_P[,2],
                              layer = BART_F85[,3] - BART_P[,3],
                              lower_95_ci = BART_F85[,4] - BART_P[,4],
                              upper_95_ci = BART_F85[,5] - BART_P[,5])
    
    BART_DIFF26 <- data.frame(x=BART_P[,1],
                              y=BART_P[,2],
                              layer=BART_F26[,3] - BART_P[,3],
                              lower_95_ci = BART_F26[,4] - BART_P[,4],
                              upper_95_ci = BART_F26[,5] - BART_P[,5])
    
    # Save predictions
    write.csv(BART_P, paste0(do_path,'/BART_P (',do,').csv'),row.names=FALSE)
    write.csv(BART_F85, paste0(do_path,'/BART_F85 (',do,').csv'),row.names=FALSE)
    write.csv(BART_F26, paste0(do_path,'/BART_F26 (',do,').csv'),row.names=FALSE)
    
    write.csv(BART_DIFF85, paste0(do_path,'/BART_DIFF85 (',do,').csv'),row.names=FALSE)
    write.csv(BART_DIFF26, paste0(do_path,'/BART_DIFF26 (',do,').csv'),row.names=FALSE)
    
    # Presence points based on model cutoff
    threshold <- summary(mod_BART)[[3]][["plot_env"]][["thresh"]]
    write.csv(BART_P %>% filter(layer >= threshold), paste0(do_path,'/BART_P_presence (',do,').csv'),row.names=FALSE)
    write.csv(BART_F85 %>% filter(layer >= threshold), paste0(do_path,'/BART_F85_presence (',do,').csv'),row.names=FALSE)
    write.csv(BART_F26 %>% filter(layer >= threshold), paste0(do_path,'/BART_F26_presence (',do,').csv'),row.names=FALSE)
  }
  
  # Partial dependence
  if(do_partial){
    for(i in names(dat)[var_cols]){
      partial_BART <- partial(mod_BART, x.vars = i, smooth = 5, trace = FALSE)
      partial_df <- rbind(partial_df, partial_BART[[1]][["data"]] %>% mutate(class = do, var = i))
    }
  }
  
  # Delete model to free memory space
  rm(mod_BART)
  
}

if(!do_subsample){
  write.csv(varimp_df, paste0(path,'/output/variable_importance_countries.csv'), row.names = FALSE)
}
if(do_partial){
  write.csv(partial_df, paste0(path,'/output/partial_dependence_countries.csv'), row.names = FALSE)
}