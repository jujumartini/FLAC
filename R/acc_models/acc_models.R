montoye <- function(vm) {
  
  # DOI: 10.1080/02640414.2020.1794244
  # PMID: 32677510
  
  # Vector magnitude is calculated and not from AG file as AG vector_magnitude
  # has less significant digits. Shouldn't matter but making it consistent.
  # vm <- 
  #   df$vm
  
  n <-
    length(vm)
  mins <-
    ceiling(n / 60)
  min <- 
    rep(1:mins, 
        each = 60)[1:n]
  vm.per.min <- 
    rep(tapply(vm,
               INDEX = min,
               FUN = sum), 
        times = table(min))
  PA.lev <- 
    rep("light", 
        times = length(min))
  PA.lev[vm.per.min < 2860] <- 
    "sed"
  PA.lev[vm.per.min > 3941] <- 
    "mvpa"
  
  return(rep(PA.lev))
  
}

rowlands <- function(acc_data_raw,
                     VMcorrG_mod_15s = 489,
                     samp_freq = 100, 
                     epoch = 15,
                     expand_1sec = TRUE) {
  
  # DOI: 10.1249/MSS.0000000000000224
  # PMID: 24263980
  
  acc_data_raw$VMcorrG <- 
    abs(
      sqrt(acc_data_raw$AxisX ^ 2 + 
             acc_data_raw$AxisY ^ 2 + 
             acc_data_raw$AxisZ ^ 2) - 1
    )
  n <- 
    dim(acc_data_raw)[1]
  mins <- 
    ceiling(n / (samp_freq * epoch))
  acc_data_raw$min <- 
    rep(1:mins, 
        each = epoch * samp_freq)[1:n]
  
  acc_data_raw.sum <-
    data.frame(
      mean.x = 
        tapply(acc_data_raw$AxisX,
               INDEX = acc_data_raw$min,
               FUN   = mean,
               na.rm = TRUE),
      mean.y = 
        tapply(acc_data_raw$AxisY,
               INDEX = acc_data_raw$min,
               FUN   = mean,
               na.rm = TRUE),
      mean.z = 
        tapply(acc_data_raw$AxisZ,
               INDEX = acc_data_raw$min,
               FUN   = mean,
               na.rm = TRUE),
      sum.VMcorrG = 
        tapply(acc_data_raw$VMcorrG,
               INDEX = acc_data_raw$min,
               FUN   = sum,
               na.rm = TRUE)
    )
  acc_data_raw.sum$v.ang <- 
    ifelse(
      test = acc_data_raw.sum$mean.y > 1,
      yes  = asin(1) * 180 / pi,
      no   = ifelse(test = acc_data_raw.sum$mean.y < -1, 
                    yes  = asin(-1) * 180 / pi, 
                    no   = asin(pmin(pmax(acc_data_raw.sum$mean.y, -1), 1)) * 180 / pi)
    )
  acc_data_raw.sum$SedSphere <- 
    ifelse(
      test = acc_data_raw.sum$sum.VMcorrG > VMcorrG_mod_15s,
      yes  = 2,
      no   = ifelse(test = acc_data_raw.sum$v.ang < -15,
                    yes  = 1,
                    no   = 0)
    )
  acc_data_raw.sum$SedSphere <- 
    factor(
      acc_data_raw.sum$SedSphere,
      levels = c(0, 1, 2),
      labels = c("sed", "light", "mvpa")
    )
  
  if (expand_1sec == TRUE) {
    
    SedSphere <-
      factor(
        rep(acc_data_raw.sum$SedSphere,
            each = epoch),
        levels = c("sed", "light", "mvpa"),
        labels = c("sed", "light", "mvpa")
      )[1:floor(n / samp_freq)]
    
    return(SedSphere)
    
  } else {
    
    return(acc_data_raw.sum)
    
  }
  
}



freedson <- function(ag_data_vaxis_hip_1sec) {
  
  # DOI: 10.1097/00005768-199805000-00021
  # PMID: 9588623
  
  n <- 
    length(ag_data_vaxis_hip_1sec)
  
  mins <- 
    ceiling(n / 60)
  min <- 
    rep(1:mins,
        each = 60)[1:n]
  cpm <- 
    rep(tapply(ag_data_vaxis_hip_1sec,
               INDEX = min,
               FUN   = sum),
        each = 60)[1:n]
  
  ee.lev <- 
    rep("sed",
        times = n)
  ee.lev[cpm > 100] <- 
    "light"
  ee.lev[cpm > 1951] <- 
    "mvpa"
  
  return(ee.lev)
  
}


hildebrand <- function(ag_data_raw_wrist,
                       freq = 100) {
  
  # DOI: 10.1249/MSS.0000000000000289
  # PMID: 24887173
  
  win.width <- 
    60
  hild_mvpa_cutpoint <- 
    100.6
  n <- 
    dim(ag_data_raw_wrist)[1]
  mins <- 
    ceiling(n / (freq * win.width)) 
  
  ag_data_raw_wrist$min <- 
    rep(1:mins,
        each = win.width * freq)[1:n]
  
  ag_data_raw_wrist_Hild <- 
    data.frame(
      mean.VMcorrG = 
        tapply(ag_data_raw_wrist$VMcorrG,
               INDEX = ag_data_raw_wrist$min,
               FUN   = mean,
               na.rm = TRUE)
    )
  
  # Express ENMO in units of mg
  ag_data_raw_wrist_Hild <- 
    ag_data_raw_wrist_Hild %>% 
    mutate(mean.VMcorrG = mean.VMcorrG * 1000)
  
  # 0 = Sedentary or light, 1 = moderate, 2 = vigorious
  ag_data_raw_wrist_Hild$hild2 <- 
    ifelse(
      test = ag_data_raw_wrist_Hild$mean.VMcorrG >= hild_mvpa_cutpoint,
      yes  = 1,
      no   = 0
    )
  
  METs <- 
    (.0320 * ag_data_raw_wrist_Hild$mean.VMcorrG + 7.28) / 3.5
  
  MET.lev <- 
    rep("sed",
        times = length(ag_data_raw_wrist_Hild$mean.VMcorrG))
  MET.lev[METs > 1.5] <- 
    "light" 	
  MET.lev[METs >= 3] <- 
    "mvpa" 	
  
  return(rep(MET.lev,
             each = 60)[1:floor(n  / 100)])
  
}    

umass.wrist <- function(ag_data_raw_wrist,
                        freq = 100) {
  
  # DOI: 10.1152/japplphysiol.00026.2015
  # PMID: 26112238
  
  win.width <- 
    15
  n <- 
    dim(ag_data_raw_wrist)[1]
  
  mins <- 
    ceiling(n / (freq * win.width))  # this is really the number of 15-sec windows in the file 
  
  ag_data_raw_wrist$min <- 
    rep(1:mins,
        each = win.width * 100)[1:n]
  ag_data_raw_wrist$v.ang <- 
    90 * (asin(ag_data_raw_wrist$AxisX / ag_data_raw_wrist$VM) / (pi / 2))
  ag_data_raw_wrist_Staud <- 
    data.frame(
      mean.vm = 
        tapply(ag_data_raw_wrist$VM,
               INDEX = ag_data_raw_wrist$min,
               FUN   = mean,
               na.rm = TRUE),
      sd.vm = 
        tapply(ag_data_raw_wrist$VM,
               INDEX = ag_data_raw_wrist$min,
               FUN   = sd,
               na.rm = TRUE),
      mean.ang = 
        tapply(ag_data_raw_wrist$v.ang,
               INDEX = ag_data_raw_wrist$min,
               FUN   = mean,
               na.rm = TRUE),
      sd.ang = 
        tapply(ag_data_raw_wrist$v.ang,
               INDEX = ag_data_raw_wrist$min,
               FUN   = sd,
               na.rm = TRUE),
      p625 = 
        tapply(ag_data_raw_wrist$VM,
               INDEX = ag_data_raw_wrist$min,
               FUN   = pow.625),
      dfreq = 
        tapply(ag_data_raw_wrist$VM,
               INDEX = ag_data_raw_wrist$min,
               FUN   = dom.freq),
      ratio.df = 
        tapply(ag_data_raw_wrist$VM,
               INDEX = ag_data_raw_wrist$min,
               FUN   = frac.pow.dom.freq)
    )
  
  # apply the models (estimates are for each 15 second epoch)
  
  # MET estimates by random forest
  ag_data_raw_wrist_Staud$METs.rf <- 
    predict(rf.met.model,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$METs.rf[ag_data_raw_wrist_Staud$sd.vm < .01] <- 
    1
  
  # MET estimates by linear regression
  ag_data_raw_wrist_Staud$METs.lm <- 
    predict(lm.met.model,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$METs.lm[ag_data_raw_wrist_Staud$sd.vm < .01] <- 
    1
  
  # MET level estimates (rf and tree)
  ag_data_raw_wrist_Staud$MET.lev.rf <- 
    predict(rf.met.level.model,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$MET.lev.tr <- 
    predict(tr.met.level.model,
            newdata = ag_data_raw_wrist_Staud,
            type = "class")
  
  # sedentary or not estimates (rf and tree)
  ag_data_raw_wrist_Staud$sed.rf <- 
    predict(rf.sed.model,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$sed.tr <- 
    predict(tr.sed.model,
            newdata = ag_data_raw_wrist_Staud,
            type = "class")
  
  # locomotion or not estimates (rf and tree)
  ag_data_raw_wrist_Staud$loc.rf <- 
    predict(rf.loc.model,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$loc.tr <- 
    predict(tr.loc.model,
            newdata = ag_data_raw_wrist_Staud,
            type = "class")
  
  # append model estimates back to main data frame
  METs.rf <- 
    rep(ag_data_raw_wrist_Staud$METs.rf,
        each = win.width)
  METs.lm <- 
    rep(ag_data_raw_wrist_Staud$METs.lm,
        each = win.width)
  MET.lev.tr <- 
    rep(ag_data_raw_wrist_Staud$MET.lev.tr,
        each = win.width)
  MET.lev.rf <- 
    rep(ag_data_raw_wrist_Staud$MET.lev.rf,
        each = win.width)
  sed.rf <- 
    rep(ag_data_raw_wrist_Staud$sed.rf,
        each = win.width)
  sed.tr <- 
    rep(ag_data_raw_wrist_Staud$sed.tr,
        each = win.width)
  loc.rf <- 
    rep(ag_data_raw_wrist_Staud$loc.rf,
        each = win.width)
  loc.tr <- 
    rep(ag_data_raw_wrist_Staud$loc.tr,
        each = win.width)
  
  junk <- 
    rep("sed",
        times = length(ag_data_raw_wrist_Staud$METs.rf))
  junk[ag_data_raw_wrist_Staud$METs.rf > 1.5] <- 
    "light"
  junk[ag_data_raw_wrist_Staud$METs.rf >= 3] <- 
    "mvpa"
  
  return(rep(junk,
             each = 15)[1:floor(n / 100)])
}

