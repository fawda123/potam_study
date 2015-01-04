load('data/mn_potam.RData')

get_files <- list.files(path = 'C:/Users/mbeck/Desktop/MNstoret/Minnesota/', 
  pattern = '^.*_res_.*\\.txt', recursive = T, full.names = T)

strt <- Sys.time()
mn_dows <- mn_potam$lake
out_dat <- NULL
for(fl in get_files){
  
  # counter
  cat(basename(fl), '\n')
  print(Sys.time() - strt)
  
  # get file
  tmp <- read.table(get_files[1], sep = '\t', header = T)
  
  # filter by lakes
  tmp <- tmp[grepl('LAKE:', tmp$Station.Name), ]
  
  # filter by phosphorus, color, secchi, and alkalinity
  # secchi: 77-78
  # color: 79-84
  # alkalinity: 409-431
  # phosphorus: 650-678
  get_parms <- as.numeric(as.character(tmp$Param)) %in% c(77:84, 409:431, 650:678)
  tmp <- tmp[get_parms, ]
  
  # convert station to numeric, 7 or 8 digit
  tmp$Station <- gsub('-', '', as.character(tmp$Station))
  tmp$Station <- as.numeric(as.character(tmp$Station))
  tmp$Station[nchar(tmp$Station) == 5] <- 100 * tmp$Station[nchar(tmp$Station) == 5]
  
  tmp$result <- as.numeric(as.character(tmp$Result.Value))
  
  # retain columns of interest
  keep_cols <- c('Station', 'Param', 'Start.Date', 'result')
  tmp <- tmp[, keep_cols]
  names(tmp) <- c('lake', 'param', 'date', 'value')
  
  # subset by veg dows
  tmp <- tmp[tmp$lake %in% mn_dows, ]
  
  out_dat <- rbind(out_dat, tmp)
  
}



