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

######
# MN macrophyte IBI v1.5.4, modified for inclusion of EMFL metric
# this is similar to MNmacroIBIstrats.r but data are not stratified by ecoregion groups
# see commented code at bottom for how 'coeffs' was obtained
# addition of EMFL, similar to SUBM but evaluates emergent and floating-leaf species
# no boxcox transformation of SUBM (for simplicity of interpretation)
macrophyte.IBI<-function(survey,location=T)

{
  
  all.sp<-c('A','AA','AC','AG','AI','AL','ALG','ALS','AM','AS','AT','B','BA','BB','BC','BGA','BRS','BS','BUB','BW','C','CA','CAC','CAH','CAL','CALA','CAN','CAP','CAS','CAW','CB','CC','CD','CE','CEE','CH','CHC','CM','COMP','COS','CP','CRES','CS','CV','CY','CYP','D','DA','DV','E','EA','EC','EF','ELAS','ELM','ELNE','ELOD','ELS','ELSP','ELT','EM','EN','EP','EQS','ERI','ES','EUM','EUP','EV','F','FA','FF','G','GAL','GB','GG','GL','H','HA','HD','HE','HJ','HV','HYS','HYV','I','IE','IM','IMA','IMS','IP','IRV','IS','IV','J','JB','JE','JEWL','JF','JN','LA','LAB','LC','LD','LG','LM','LO','LS','LT','LU','LYS','LYT','M','MA','MB','MEA','MF','MG','MH','MR','MS','MT','MV','MVO','MYH','MYS','MYT','N','NAG','NF','NG','NL','NM','NO','NS','NUL','NUR','NUS','NV','NX','NX','NYL','NYS','P','PA','PC','PD','PE','PF','PFL','PG','PHA','PHAU','PI','PL','PLA','PN','PO','POA','POAL','POB','POC','POF','POFR','PON','PONF','POO','POP','POPA','POPU','POR','POS','POSB','POSN','POV','PP','PPUL','PR','PS','PV','PZ','R','RAF','RF','RG','RIF','RL','RM','RO','RS','RT','RU','RUO','S','SA','SAC','SAL','SAS','SB','SC','SCA','SCC','SCE','SCF','SCH','SCP','SCS','SCT','SCTS','SCU','SE','SEME','SF','SFLO','SG','SIS','SL','SM','SO','SP','SPA','SPAG','SPAM','SPC','SPG','SPGL','SPI','SPM','SPNG','SPP','SPPE','SPT','SR','SS','SUA','SV','SX','TA','TG','TL','TS','TSNL','UC','UG','UI','UM','UP','UR','UV','V','VA','VEA','VES','VM','VS','WC','WM','WS','ZIP','ZP')
  
  nonrooted<-c('BGA','D','EMT','FA','GA','PLA','SPAG','SPM','SPNG','WM')
  
  emergfloatleaf.sp<-c('AL','ALG','ALS','AT','BRS','BUB','CAN','CAW','CP','CRES','CS','CY','CYP','EA','EF','ELNE','ELS','ELSP','EP','EQS','FF','IS','J','JB','JE','JF','JN','NL','NO','NUL','NUR','NUS','NV','NX','NYL','NYS','P','PFL','POA','S','SA','SAC','SAL','SAS','SB','SC','SCA','SCC','SCF','SCH','SCP','SCS','SCT','SE','SEME','SFLO','SG','SM','SP','SPA','SPAM','SPC','SPG','SPGL','SPM','SR','SV','TA','TG','TL','TS','TSNL','ZIP')
  
  submersed.sp<-c('BW','C','CAH','CAS','CD','CEE','CF','CH','CV','EC','ELAS','ELM','ELOD','ELT','EN','ES','FS','H','HD','HV','I','IE','IM','IMA','LD','LU','M','MA','MB','MF','MH','MS','MV','MYH','MYS','MYT','N','NAG','NF','NG','NM','NS','PA','PAN','PB','PC','PD','PE','PF','PG','PI','PN','PO','POAL','POB','POC','POF','POFR','POH','PON','PONF','POO','POP','POPU','POR','POS','POSB','POSN','POV','PP','PPUL','PR','PS','PTO','PV','PZ','R','RAF','RF','RG','RL','RO','RS','RSU','RT','SS','SUA','UC','UG','UI','UM','UP','UR','UV','VA','X','ZP')
  
  sensitive.sp<-c('AG','CP','CH','CAH','CEE','CHC','DA','ELM','ELT','ES','ERI','GB','HV','HA','HE','I','IE','IM','IMA','LG','LU','LD','MB','MT','MG','MA','MF','MYT','MV','NAG','NG','NL', 'NYL','POC','POAL','POB', 'PD','PE','POF','POFR','POO','PO','PR','POS','PS','POV','SB','SAC','SH','SS','SCT','SPAM','SPA','SPF','SN','PV','SUA','TM','UC','UG','UI','UM','UP','UR','VM','VO','X','ZAQ','ZIP')
  
  tolerant.sp<-c('AC','AS','BUB','CAT','CB','CD','CE','EM','HJ','IP','IMS','JEWL','LS','MYH','MYS','PC','PHA','PHAU','RM','TA','TG','TL','TS','TSNL')
  
  exotic.sp<-c('AC','BUB','CE','IP','LS','MYH','MYS','PC','PHA','TA','TG','TSNL')
  
  data.prep<-{
    all.pts<-{
      if(location==T){
        if(strsplit(survey,'.',fixed=T)[[1]][2] == '.csv') lake.data<-read.csv(survey,header=T)
        else lake.data<-read.table(survey,header=T,sep=',')
        }
      if(location==F) lake.data<-survey
      lake.data[is.na(lake.data)]<-0
      match.test<-match(names(lake.data),nonrooted,nomatch=0)
      rooted<-{
        sp.names<-subset(cbind(names(lake.data),match.test),match.test==0)[,1]
        subset(lake.data,select=sp.names)
        }
      Sum<-ifelse(as.matrix(rowSums(rooted)-rooted[,1])>0,1,0)
      cbind(rooted,Sum)[order(cbind(rooted,Sum)$AQPNT_Depth),]
      }
    ADPG<-max((subset(all.pts,Sum==1)$AQPNT_Depth))
    Cumsum<-cumsum(subset(all.pts,(AQPNT_Depth<ADPG|AQPNT_Depth==ADPG))$Sum)
    Freq<-round(Cumsum/max(Cumsum),2)
    cbind(subset(all.pts,(AQPNT_Depth<ADPG|AQPNT_Depth==ADPG)),Cumsum,Freq)
    }
  
  sp.freq<-round(colSums(rooted[,1:ncol(rooted)])[2:ncol(rooted)]/nrow(data.prep),4)
  
  if(sum(is.na(match(names(sp.freq),all.sp)))>0){
    unk_names<-c('UNK','UNKS','UNK1','UNK2','UNK3','UNK4','UNK5','UNK6')
    unknown<<-names(sp.freq)[which(is.na(match(names(sp.freq),all.sp))==TRUE)]
    if(sum(unknown %in% unk_names) == length(unknown))
      warning(paste('unknown species:', paste(unknown, collapse=' ')))
    else
      stop(paste('unknown species:', paste(unknown, collapse=' ')))
    }
  if(sum(as.numeric(colSums(data.prep[,2:ncol(data.prep)]))==0)>0){
    unsampled<<-names(sp.freq)[which(as.numeric(colSums(data.prep[,2:ncol(data.prep)]))==0)]
    stop("unsampled species, type 'unsampled' for identification")
    }
  if(ncol(lake.data)-sum(names(lake.data)=='PC')==1){
    stop('curly-leaf pondweed surveys not used for IBI')
    }
  
  pos.scale<-function(x,quan,min){
    if(x>quan|x==quan){10}
  	else{10*(x-min)/(quan-min)}
    }
  neg.scale<-function(x,quan,max){
  	if(x<quan|x==quan){10}
  	else{10-(10*(x-quan)/(max-quan))}
    }
  
  coeffs<-data.frame(
    quan=c(19,1,12.45,0.4553829,0.5000664,0.3058327,0,34),
    maxmin=c(1.75,0,0,0,1,0,0.9426771,1),
    row.names=c('MAXD','LITT','OVER','EMFL','SUBM','SENS','TOLR','TAXA')
    )
  
  MAXD<-{
    MAXD.raw<-max(subset(data.prep, Freq<0.95|Freq==0.95)$AQPNT_Depth)
    round(pos.scale(MAXD.raw,coeffs['MAXD','quan'],coeffs['MAXD','maxmin']),2)
    }
  LITT<-{
    LITT.raw<-{
      littoral.zone<-subset(data.prep, Freq<0.95|Freq==0.95)
      length(subset(littoral.zone,Sum==1)$Sum)/length(littoral.zone$AQPNT_Depth)
      }
    round(pos.scale(LITT.raw,coeffs['LITT','quan'],coeffs['LITT','maxmin']),2)
    }
  OVER<-{
    OVER.raw<-sum(ifelse(sp.freq>0.1|sp.freq==0.1,1,0))
    round(pos.scale(OVER.raw,coeffs['OVER','quan'],coeffs['OVER','maxmin']),2)
    }
  EMFL<-{
    EMFL.raw<-sum(sp.freq[(names(sp.freq) %in% emergfloatleaf.sp)]/sum(sp.freq))
    round(pos.scale(EMFL.raw,coeffs['EMFL','quan'],coeffs['EMFL','maxmin']),2)
    }
  SUBM<-{
    SUBM.raw<-sum(sp.freq[(names(sp.freq) %in% submersed.sp)]/sum(sp.freq))
    round(neg.scale(SUBM.raw,coeffs['SUBM','quan'],coeffs['SUBM','maxmin']),2)
    }
  SENS<-{
    SENS.raw<-sum(sp.freq[(names(sp.freq) %in% sensitive.sp)]/sum(sp.freq))
    round(pos.scale(SENS.raw,coeffs['SENS','quan'],coeffs['SENS','maxmin']),2)
    }
  TOLR<-{
    TOLR.raw<-sum(sp.freq[(names(sp.freq) %in% tolerant.sp)]/sum(sp.freq))
    round(neg.scale(TOLR.raw,coeffs['TOLR','quan'],coeffs['TOLR','maxmin']),2)
    }
  TAXA<-{
    TAXA.raw<-(ncol(rooted)-1)-length(names(rooted)[names(rooted) %in% exotic.sp])
    round(pos.scale(TAXA.raw,coeffs['TAXA','quan'],coeffs['TAXA','maxmin']),2)
    }
  IBIscore<-round(10*(MAXD+LITT+OVER+EMFL+SUBM+SENS+TOLR+TAXA)/8,2)
  
  out<-list(
    IBIscore=IBIscore,
    MAXD=MAXD,
    LITT=LITT,
    OVER=OVER,
    EMFL=EMFL,
    SUBM=SUBM,
    SENS=SENS,
    TOLR=TOLR,
    TAXA=TAXA,
    MAXD.raw=MAXD.raw,
    LITT.raw=LITT.raw,
    OVER.raw=OVER.raw,
    EMFL.raw=EMFL.raw,
    SUBM.raw=SUBM.raw,
    SENS.raw=SENS.raw,
    TOLR.raw=TOLR.raw,
    TAXA.raw=TAXA.raw
  )
  
  out

}

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
  
  # add extra row for total effort if T
  if(counts) out <- rbind(out, c('tot', nrow(data_prep)))
  
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

######
# run partial redundancy analysis or partial least squares, then variance partitioning
#
# dat_in: all_potam data frame
# resp_in: chr string of variables to model (one to many), uses regexpr matching to select from dat_in
# 
# output is numeric vector pure x1, pure x2, pure x3, shared x1/x2, shared x2/x3, shared x1/x3, shared x1/x2/x3, residual, where x1 is loc, x2 is cli, x3 is spa
pot_var <- function(dat_in, resp_nm, mod_out = FALSE){
  
  library(vegan)
  library(packfor)
  library(dplyr)
  
  # names of variables to select
  loc_nm <- c('alk','color', 'tp', 'secchi', 'area', 'depth')
  cli_nm <- c('tmean', 'tmax', 'tmin', 'prec', 'alt')
  spa_nm <- '^V'
  
  # subset dat_in by explanatory variable sets
  loc <- select(dat_in, matches(paste(loc_nm, collapse = '|')))
  cli <- select(dat_in, matches(paste(cli_nm, collapse = '|')))
  spa <- select(dat_in, matches(spa_nm, ignore.case = F))
  
  # subset dat_in by response variable(s)
  res <- select(dat_in, matches(resp_nm, ignore.case = F))
  
  # use RDA if res more than one variable
  # otherwise, glm
  if(ncol(res) > 1){
    
    loc_sel <- forward.sel(res, loc)
    loc <- loc[, names(loc) %in% loc_sel$variables]
    cli_sel <- forward.sel(res, cli)
    cli <- cli[, names(cli) %in% cli_sel$variables]
    spa_sel <- forward.sel(res, spa)
    spa <- spa[, names(spa) %in% spa_sel$variables]
    
    mod <- varpart(Y = res, X = loc, cli, spa, transfo = 'hel')
     
  } else {

    # strip resp_nm for mods
    resp_nm <- gsub('\\^|\\$', '', resp_nm)
    
    # local mod
    loc_sel <- paste0(resp_nm, ' ~ ', paste(loc_nm, collapse = ' + '))
    loc_sel <- glm(as.formula(loc_sel), family = poisson(link = 'log'), 
      data = dat_in, offset = log(tot)) %>% 
      MASS::stepAIC(., direction = 'both', criterion = 'AIC', trace = F)
    modloc <- formula(loc_sel)[c(1, 3)]
    
    # climate mod
    cli_sel <- paste0(resp_nm, ' ~ ', paste(cli_nm, collapse = ' + '))
    cli_sel <- glm(as.formula(cli_sel), family = poisson(link = 'log'), 
      data = dat_in, offset = log(tot)) %>% 
      MASS::stepAIC(., direction = 'both', criterion = 'AIC', trace = F)
    modcli <- formula(cli_sel)[c(1, 3)]
    
    # spatial mod, stepwise, then get formula
    form <- grep(spa_nm, names(dat_in), value = T) %>% 
      paste(., collapse = ' + ') %>% 
      paste0(resp_nm, ' ~ ', .) %>% 
      as.formula
    spa_sel <- glm(form, data = dat_in, family = poisson(link = 'log'), offset = log(tot)) %>% 
      MASS::stepAIC(., direction = 'both', criterion = 'AIC', trace = F)
    modspa <- formula(spa_sel)[c(1, 3)]
    
    # variance partitioning
    mod <- varpart(res[, 1], modloc, modcli, modspa, data = dat_in)
        
  }

  # return individual models if true
  if(mod_out) return(list(loc = loc_sel, cli = cli_sel, spa = spa_sel))
  
  # get fractions of variance, 
  # pure x1, pure x2, pure x3, shared x1/x2, shared x2/x3, shared x1/x3, shared x1/x2/x3, residual
  vars <- mod$part$indfract[, 'Adj.R.square']
  names(vars) <- c('loc', 'cli', 'spa', 'loc + cli', 'cli + spa', 'loc + spa', 'loc + cli + spa', 'res')
  return(vars) 
   
}

# forward variable selection of rda using blanchet
bla_sel <- function(resp, exp){
  
  library(packfor)

  mod <- rda(resp, exp)
  mod_R2a <- RsquareAdj(mod)$adj.r.squared
  modfwd <- forward.sel(resp, as.matrix(exp), adjR2thresh = mod_R2a, nperm = 999)
  
  # Write the significant variables to a new object
  exp_sign <- sort(modfwd$order)
  vars_ho <- exp[, c(exp_sign), drop = F]
  
  colnames(vars_ho)
  
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
  loc <- select(dat_in, matches(paste(loc_nm, collapse = '|')))
  cli <- select(dat_in, matches(paste(cli_nm, collapse = '|')))
  spa <- select(dat_in, matches(spa_nm, ignore.case = F))
  
  # subset dat_in by response variable(s)
  res <- select(dat_in, matches(resp_nm, ignore.case = F))

  # otherwise, glm
  loc_sel <- bla_sel(res, loc)
  loc <- loc[, loc_sel, drop = FALSE]
  cli_sel <- bla_sel(res, cli)
  cli <- cli[, cli_sel, drop = FALSE]
  spa_sel <- bla_sel(res, spa)
  spa <- spa[, spa_sel, drop = FALSE]

  if(ncol(res) > 1){
    mod <- varpart(Y = res, X = loc, cli, spa, transfo = 'hel')
  } else {
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
  
  # combine eqach group mod
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
    out <- lapply(spp , function(mod) 100 * Dsquared(mod, adjust = TRUE))
    unlist(out)
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

######
# deviance squared for GLM
# https://modtools.wordpress.com/2013/08/14/dsquared/
Dsquared <- function(model = NULL, 
                     obs = NULL, 
                     pred = NULL, 
                     family = NULL, # needed only when 'model' not provided
                     adjust = FALSE, 
                     npar = NULL) { # needed only when 'model' not provided
  # version 1.4 (31 Aug 2015)
 
  model.provided <- ifelse(is.null(model), FALSE, TRUE)
 
  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
 
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'.")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (is.null(family)) stop ("With 'obs' and 'pred' arguments (rather than a model object), you must also specify one of two model family options: 'binomial' or 'poisson' (in quotes).")
    else if (!is.character(family)) stop ("Argument 'family' must be provided as character (i.e. in quotes: 'binomial' or 'poisson').")
    else if (length(family) != 1 | !(family %in% c("binomial", "poisson"))) stop ("'family' must be either 'binomial' or 'poisson' (in quotes).")
 
    if (family == "binomial") {
      if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("'binomial' family implies that 'obs' data should be binary (with values 0 or 1) and 'pred' data should be bounded between 0 and 1.")
      link <- log(pred / (1 - pred))  # logit
    }  # end if binomial
 
    else if (family == "poisson") {
      if (any(obs %%1 != 0)) stop ("'poisson' family implies that 'obs' data should consist of whole numbers.")
      link <- log(pred)
    }  # end if poisson
 
    model <- glm(obs ~ link, family = family)
  }  # end if model not provided
 
  D2 <- (model$null.deviance - model$deviance) / model$null.deviance
 
  if (adjust) {
    if (model.provided) {
      n <- length(model$y)
      #p <- length(model$coefficients)
      p <- attributes(logLik(model))$df
    } else {
      if (is.null(npar)) stop ("Adjusted D-squared from 'obs' and 'pred' values (rather than a model object) requires specifying the number of parameters in the underlying model ('npar').")
      n <- length(na.omit(obs))
      p <- npar
    }  # end if model.provided else
 
    D2 <- 1 - ((n - 1) / (n - p)) * (1 - D2)
  }  # end if adjust
 
  return (D2)
}