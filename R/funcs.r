######
# functions for macrophyte analyses

######
# formatting of values in S expressions
form_fun <- function(x, rnd_val = 2, dig_val = 2, nsm_val = 2) {
  to_form <- as.numeric(as.character(x))
  format(round(to_form, rnd_val), digits = dig_val, nsmall = nsm_val)
  }

######
# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#######
# potamegetons sp freqs (or counts)
# survey is comma delimited txt file spp p/a, first column is depth
# counts is logical to return counts of points with species and additional column for total survey points
# total survey points can be less than the sum of all species present because more than one species can be at a point
# default is counts = F to return sp frequency (num. pts/tot pts)
pot_freq <- function(survey, counts = FALSE) {

  pot_sp <- c('PB', 'POFR', 'POR', 'PR', 'POAL', 'PA', 'POB', 'PC', 'PD', 'PE', 'POF', 'PF', 'PG', 'PI', 'PN', 'PON', 'POO', 'PO', 'PP', 'POP', 'PPUL', 'POPU', 'POS', 'PS', 'PV', 'POV', 'PZ', 'POSB', 'PFL', 'POSN', 'PONF')
  
  pot_comm <- c('Potamogeton Berchtoldi', 'Potamogeton Friesii', 'Potamogeton Richardsoni', 'Potamogeton Robbinsii', 'Potamogeton alpinus', 'Potamogeton amplifolius', 'Potamogeton bicupulatus', 'Potamogeton crispus', 'Potamogeton diversifolius', 'Potamogeton epihydrus', 'Potamogeton filiformis', 'Potamogeton foliosus', 'Potamogeton gramineus', 'Potamogeton illinoensis', 'Potamogeton natans', 'Potamogeton nodosus', 'Potamogeton oakesianus', 'Potamogeton obtusifolius', 'Potamogeton pectinatus', 'Potamogeton praelongus', 'Potamogeton pulcher', 'Potamogeton pusillus', 'Potamogeton spirillus', 'Potamogeton strictifolius', 'Potamogeton vaginatus', 'Potamogeton vaseyi', 'Potamogeton zosteriformis', 'Broad-leaf Pondweed Group', 'Floating-leaf Water Smartweed Group', 'Narrow-leaf Pondweed Group', 'Narrow-leaf Pondweed w/ Floating Leaves Group')
  
  nonrooted <- c('BGA', 'D', 'EMT', 'FA', 'GA', 'PLA', 'SPAG', 'SPM', 'SPNG', 'WM')

  lake_data <- survey
  lake_data[is.na(lake_data)] <- 0
  
  match_test <- match(names(lake_data), nonrooted, nomatch = 0)
  rooted <- {
    sp_names <- subset(cbind(names(lake_data), match_test), match_test == 0)[, 1]
    subset(lake_data, select = sp_names)
    }
  Sum <- ifelse(as.matrix(rowSums(rooted) - rooted[, 1]) > 0, 1, 0)
  all_pts <- cbind(rooted, Sum)[order(cbind(rooted, Sum)$AQPNT_Depth), ]
  
  ADPG <- max((subset(all_pts, Sum == 1)$AQPNT_Depth))
  Cumsum <- cumsum(subset(all_pts, (AQPNT_Depth <= ADPG))$Sum)
  Freq <- round(Cumsum/max(Cumsum), 2)
  
  data_prep <- cbind(subset(all_pts, (AQPNT_Depth <= ADPG)), Cumsum, Freq)

  sp_freq <- colSums(rooted[, 1:ncol(rooted)])[2:ncol(rooted)]

  # divide by survey pts to get freq if F
  if(!counts)
    sp_freq <- round(sp_freq/nrow(data_prep), 4)
  
  pot_val <- sp_freq[names(sp_freq) %in% pot_sp]
  
  freq <- data.table(pot_val, keys = names(pot_val))
  spp <- data.table(pot_sp, keys = pot_sp)
    
  out <- merge(spp, freq, by = 'keys', all = T)
  out <- data.frame(out)
  out[is.na(out$pot_val), 'pot_val'] <- 0
  out$keys <- NULL
  
  # add extra row for total survey points
  out <- rbind(out, c('tot', nrow(data_prep)))
  
  # outputs as numeric
  out$pot_val <- as.numeric(out$pot_val)
  
  return(out)
  
}

######
#' Get climate data
#' 
#' Get climate data by sampling raster grids available from http://www.worldclim.org
#' 
#' @param path local path for downloaded data
#' @param ext_dat SpatialPointsDataframe of points used for sampling
#' @param mos numeric vector of months to sample
#' 
#' @return a matrix of sampled values with months as columns and locations as rows
clim_fun <- function(path, ext_dat, mos = 1:12){
  
  out <- NULL
  
  for(mo in mos){
    cat(mo, '\t')
    r <- raster(paste0(path, mo))
    r <- raster::extract(r, ext_dat)
    out <- cbind(out, r)
  }
  
  out <- as.matrix(out)
  return(out)
  
}

# forward variable selection of rda using blanchet
bla_sel <- function(resp, exp, forout = FALSE){
  
  library(packfor)
  library(vegan)
  
  mod <- rda(Y = resp, X = exp)
  mod_R2a <- RsquareAdj(mod)$adj.r.squared
  modfwd <- forward.sel(resp, as.matrix(exp), nperm = 999)
  
  return(modfwd)

}

######
# same function as above but variable selection for individual mods (loc, sli, spa) uses blanchet variable selection
pot_var_bla <- function(dat_in, resp_nm, mod_out = FALSE){
  
  library(vegan)
  library(packfor)
  library(dplyr)
  
  # names of variables to select
  loc_nm <- c('alk','color', 'tp', 'secchi', 'area', 'depth')
  cli_nm <- c('tmean', 'tmax', 'tmin', 'prec', 'alt')
  spa_nm <- '^V'
  
  # subset dat_in by explanatory variable sets
  loc <- dplyr::select(dat_in, matches(paste(loc_nm, collapse = '|')))
  cli <- dplyr::select(dat_in, matches(paste(cli_nm, collapse = '|')))
  spa <- dplyr::select(dat_in, matches(spa_nm, ignore.case = F))
  
  # subset dat_in by response variable(s)
  res <- dplyr::select(dat_in, matches(resp_nm, ignore.case = F))

  # otherwise, glm
  loc_sel <- bla_sel(res, loc)
  cli_sel <- bla_sel(res, cli)
  spa_sel <- bla_sel(res, spa)
  
  # multivariate mod
  if(ncol(res) > 1){
  
    # return variable selection by category
    if(mod_out) return(list(loc = loc_sel, cli = cli_sel, spa = spa_sel)) 
      
    loc <- loc[, sort(loc_sel$order), drop = F]
    cli <- cli[, sort(cli_sel$order), drop = F]
    spa <- spa[, sort(spa_sel$order), drop = F]
    
    mod <- varpart(Y = res, X = loc, cli, spa, transfo = 'hel')

  # univariate mod
  } else {
     
    loc <- loc[, sort(loc_sel$order), drop = F]
    cli <- cli[, sort(cli_sel$order), drop = F]
    spa <- spa[, sort(spa_sel$order), drop = F]
    
    if(mod_out) {
      
      # strip resp_nm for mods
      resp_nm <- gsub('\\^|\\$', '', resp_nm)
      
      # local model
      loc_sel <- paste0(resp_nm, ' ~ ', paste(names(loc), collapse = ' + '))
      modloc <- glm(as.formula(loc_sel), family = poisson(link = 'log'), 
        data = dat_in) 
      
      # climate model
      cli_sel <- paste0(resp_nm, ' ~ ', paste(names(cli), collapse = ' + '))
      modcli <- glm(as.formula(cli_sel), family = poisson(link = 'log'), 
        data = dat_in) 
      
      # space model
      spa_sel <- paste0(resp_nm, ' ~ ', paste(names(spa), collapse = ' + '))
      modspa <- glm(as.formula(spa_sel), family = poisson(link = 'log'), 
        data = dat_in) 
      
      # adj rsquared from rda of the mods, no easy way to do this with glms
      exps <- list(
        loc = rda(res, loc),
        cli = rda(res, cli),
        spa = rda(res, spa)
      )
      exps <- lapply(exps, function(x) RsquareAdj(x)$adj.r.squared)
      exps <- unlist(exps)
      
      return(list(loc = modloc, cli = modcli, spa = modspa, exps = exps))
          
    } 
    
    mod <- varpart(Y = res, X = loc, cli, spa)   
    
  }
  
  # get fractions of variance, 
  # pure x1, pure x2, pure x3, shared x1/x2, shared x2/x3, shared x1/x3, shared x1/x2/x3, residual
  vars <- mod$part$indfract[, 'Adj.R.square']
  
  # add total as extra, 1 - residual
  vars <- c(vars, 1 - vars[length(vars)])
  
  names(vars) <- c('loc', 'cli', 'spa', 'loc + cli', 'cli + spa', 'loc + spa', 'loc + cli + spa', 'res', 'tot')
  return(vars) 
   
}

######
# summarize model results for RDA and GLM
#
# spp_varmod list of actual models
pot_summ <- function(spp_varmod){
  
  sig_cats <- c('***', '**', '*', 'ns')
  sig_vals <- c(-Inf, 0.0001, 0.001, 0.05, Inf)
  
  # assemb comp is treated different
  cc_varmod <- spp_varmod[['cc_mod']]
  spp_varmod <- spp_varmod[!names(spp_varmod) %in% c('cc_mod')]
  names(spp_varmod)[!names(spp_varmod) %in% 'rich_mod'] <- pot_nms(names(spp_varmod)[!names(spp_varmod) %in% 'rich_mod'])
  
  # summarized models by index and chr
  spp_summ <- function(spp_varmod, ind, chr){
    
    summs <- lapply(spp_varmod, function(x){
      
      mod <- x[[ind]]
          
      if(length(mod$coefficients) == 1) return(NULL)
       
      tocat <- summary(mod)$coefficients[-1, , drop = FALSE]     
      
      dirs <- rep('+', nrow(tocat))
      dirs[sign(tocat[, 1]) == -1] <- '-'
      vars <- row.names(tocat)
      sigs <- cut(tocat[, 4], breaks = sig_vals, labels = sig_cats)
      
      out <- try({data.frame(mod = chr, vars, sigs, dirs)})
      
      return(out)
      
    })
    
    if(chr == 'spa')
      summs <- lapply(summs, function(x) {
        if(is.null(x)) return(NULL)
        else data.frame(mod = chr, vars = 'n', sigs = as.character(nrow(x)), dirs = '')
      })
    
    do.call('rbind', summs)
    
  }
  
  # create model summaries for each exp var
  loc_mods <- spp_summ(spp_varmod, 1, 'loc')
  cli_mods <- spp_summ(spp_varmod, 2, 'cli')
  spa_mods <- spp_summ(spp_varmod, 3, 'spa')
  
  # combine each group mod
  all_mods <- rbind(loc_mods, cli_mods, spa_mods) %>% 
    mutate(
      spp = row.names(.),
      spp = gsub('[0-9]*$', '', spp), 
      spp = gsub('\\.$', '', spp),
      spp = gsub('Assemb\\. comp', 'Assemb\\.\\. comp\\.', spp), 
      spp = gsub('rich_mod', 'Richness', spp)
      ) 
  
  # take care of assemb comp mods
  cc_mods <- lapply(cc_varmod, function(x) {
    
    vars <- x[, 'variables']
    sigs <- cut(x[, 'pval'], breaks = sig_vals, labels = sig_cats)
  
    if(any(grepl('^V[0-9]*$', vars, ignore.case = F))){ 
      sigs <- as.character(length(sigs))
      vars <- 'n'
    }
  
    data.frame(vars = vars, sigs = sigs, dirs = '', spp = 'Assemb. comp.')
    
  }) %>% 
  do.call('rbind', .) %>% 
  data.frame(mod = row.names(.), ., row.names = 1:nrow(.)) %>% 
  mutate(mod = gsub('\\.[0-9]*$', '', mod))
    
  # combine rda and glm mods, make wide format
  all_mods <- rbind(all_mods, cc_mods) %>% 
    unite(mod_vars, mod, vars, sep = ' ') %>% 
    unite(ests, sigs, dirs, sep = '') %>% 
    spread(mod_vars, ests, fill = '') %>% 
    mutate(spp = gsub('^rich_mod$', 'Richness', spp))
  
  # add exp var for each category
  cc_exp <- lapply(cc_varmod, function(x) max(x[, 'AdjR2Cum']) * 100) %>% 
    as.data.frame(., row.names = c('Assemb. comp.'))
  sp_exp <- lapply(spp_varmod, function(spp){
    100 * spp$exps
    }) %>% 
    do.call('rbind', .) %>% 
    as.data.frame
  all_exp <- rbind(cc_exp, sp_exp) %>% 
    data.frame %>% 
    mutate(spp = row.names(.)) %>% 
    data.frame(., row.names = c(1:nrow(.))) %>% 
    mutate(
      spp = gsub('cc_exp', 'Assemb\\. comp\\.', spp),
      spp = gsub('rich_mod', 'Richness', spp)
      )
  
  # join by spp, remove generic species
  out <- left_join(all_mods, all_exp, by = 'spp') %>% 
    filter(!spp %in% c('Narrow-leaf Pondweed Group', 'Floating-leaf Water Smartweed Group')) 
  
  # sort rows by assemb comp, rich, then spp
  spp <- grep('^P\\.', out$spp, value = T)
  out$spp <- factor(out$spp, levels = c('Assemb. comp.', 'Richness', spp))
  out <- out[order(out$spp), ]
  out$spp <- as.character(out$spp)
  
  # create column labels and sublabels
  cats <- gsub('[[:space:]][a-z]*$', '', names(out))
  names(cats) <- names(out)
  vars <- gsub('loc|cli|spa|[[:space:]]', '', names(out))
  vars[nchar(vars) == 0] <- '%'
  names(vars) <- names(out)
  
  # sort columns by column labels and sublabels
  col_ord <- order(cats[-1], names(out)[-1])

  out <- rbind(cats, vars, out)[, c(1, 1 + col_ord)]
    
  return(out)
  
}

######
# switch potamogeton names between full species and DNR codes
# 
# chr_in is vector of codes or species
# to_spp is logical indicating codes to species (TRUE) or species to codes (FALSE)
# abb_spp is logical indicating if Potamogeon is changed to P.
#
pot_nms <- function(chr_in, to_spp = TRUE, abb_spp = TRUE){
  
  pot_sp <- c('PB', 'POFR', 'POR', 'PR', 'POAL', 'PA', 'POB', 'PC', 'PD', 'PE', 'POF', 'PF', 'PG', 'PI', 'PN', 'PON', 'POO', 'PO', 'PP', 'POP', 'PPUL', 'POPU', 'POS', 'PS', 'PV', 'POV', 'PZ', 'POSB', 'PFL', 'POSN', 'PONF')
  
  pot_comm <- c('Potamogeton berchtoldi', 'Potamogeton friesii', 'Potamogeton richardsoni', 'Potamogeton robbinsii', 'Potamogeton alpinus', 'Potamogeton amplifolius', 'Potamogeton bicupulatus', 'Potamogeton crispus', 'Potamogeton diversifolius', 'Potamogeton epihydrus', 'Potamogeton filiformis', 'Potamogeton foliosus', 'Potamogeton gramineus', 'Potamogeton illinoensis', 'Potamogeton natans', 'Potamogeton nodosus', 'Potamogeton oakesianus', 'Potamogeton obtusifolius', 'Potamogeton pectinatus', 'Potamogeton praelongus', 'Potamogeton pulcher', 'Potamogeton pusillus', 'Potamogeton spirillus', 'Potamogeton strictifolius', 'Potamogeton vaginatus', 'Potamogeton vaseyi', 'Potamogeton zosteriformis', 'Broad-leaf Pondweed Group', 'Floating-leaf Water Smartweed Group', 'Narrow-leaf Pondweed Group', 'Narrow-leaf Pondweed w/ Floating Leaves Group')
  
  # codes to species
  if(to_spp){
    
    out <- pot_comm[match(chr_in, pot_sp)]
    
    if(abb_spp) out <- gsub('Potamogeton', 'P\\.', out, ignore.case = FALSE)
    
  # species to codes    
  } else {
    
    chr_in <- gsub('^P\\.', 'Potamogeton', chr_in)
    out <- pot_sp[match(chr_in, pot_comm)]
    
  }
  
  return(out)
  
}

