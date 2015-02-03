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
# potamegetons sp freqs
# survey is comma delimited txt file spp p/a, first column is depth
pot_freq <- function(survey) {

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
  sp_freq <- round(sp_freq/nrow(data_prep), 4)
  
  pot_freq <- sp_freq[names(sp_freq) %in% pot_sp]
  
  freq <- data.table(pot_freq, keys = names(pot_freq))
  spp <- data.table(pot_sp, keys = pot_sp)
    
  out <- merge(spp, freq, by = 'keys', all = T)
  out <- data.frame(out)
  out[is.na(out$pot_freq), 'pot_freq'] <- 0
  out$keys <- NULL
  
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

