montoye_2020 <- function(vm) {
  
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
    rep(1, 
        times = length(min))
  PA.lev[vm.per.min < 2860] <- 
    0
  PA.lev[vm.per.min > 3941] <- 
    2
  
  PA.lev <- 
    factor(PA.lev,
           levels = c(0, 1, 2),
           labels = c("sedentary", "light", "mvpa"))
  
  return(PA.lev)
  
}

rowlands_2014 <- function(raw_x,
                          raw_y,
                          raw_z,
                          vmcorrg_mod_15s = 489,
                          samp_freq = 100, 
                          epoch = 15,
                          expand_1sec = TRUE) {
  
  # DOI: 10.1249/MSS.0000000000000224
  # PMID: 24263980
  
  vmcorrg <- 
    abs(
      sqrt(raw_x ^ 2 + 
             raw_y ^ 2 + 
             raw_z ^ 2) - 1
    )
  n <- 
    length(raw_x)
  minutes <- 
    ceiling(n / (samp_freq * epoch))
  min <- 
    rep(1:minutes, 
        each = epoch * samp_freq)[1:n]
  
  acc_data_raw.sum <-
    data.frame(
      mean.x = 
        tapply(raw_x,
               INDEX = min,
               FUN   = mean,
               na.rm = TRUE),
      mean.y = 
        tapply(raw_y,
               INDEX = min,
               FUN   = mean,
               na.rm = TRUE),
      mean.z = 
        tapply(raw_z,
               INDEX = min,
               FUN   = mean,
               na.rm = TRUE),
      sum.vmcorrg = 
        tapply(vmcorrg,
               INDEX = min,
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
  sedsphere <- 
    ifelse(
      test = acc_data_raw.sum$sum.vmcorrg > vmcorrg_mod_15s,
      yes  = 2,
      no   = ifelse(test = acc_data_raw.sum$v.ang < -15,
                    yes  = 1,
                    no   = 0)
    )
  
  if (expand_1sec == TRUE) {
    
    sedsphere <-
      rep(sedsphere,
          each = epoch)[1:floor(n / samp_freq)]
    
  }
  
  sedsphere <- 
    factor(sedsphere,
           levels = c(0, 1, 2),
           labels = c("sedentary", "light", "mvpa"))
  
  return(sedsphere)
  
}



freedson_1998 <- function(ag_data_vaxis_hip_1sec) {
  
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
    rep(0,
        times = n)
  ee.lev[cpm > 100] <- 
    1
  ee.lev[cpm > 1951] <- 
    2
  ee.lev <- 
    factor(ee.lev,
           levels = c(0, 1, 2),
           labels = c("sedentary", "light", "mvpa"))
  
  return(ee.lev)
  
}


hildebrand_2014 <- function(raw_x,
                            raw_y,
                            raw_z,
                            freq = 100,
                            win.width = 60) {
  
  # DOI: 10.1249/MSS.0000000000000289
  # PMID: 24887173
  
  hild_mvpa_cutpoint <- 
    100.6
  vmcorrg <- 
    sqrt(raw_x ^ 2 + raw_y ^ 2 + raw_z ^ 2) - 1
  vmcorrg <- 
    fifelse(test = vmcorrg < 0,
            yes  = 0,
            no   = vmcorrg,
            na   = NA)
  n <- 
    length(raw_x)
  mins <- 
    ceiling(n / (freq * win.width)) 
  min <- 
    rep(1:mins,
        each = win.width * freq)[1:n]
  ag_data_raw_wrist_Hild <- 
    data.frame(
      mean.vmcorrg = 
        tapply(
          vmcorrg,
          INDEX = min,
          FUN   = mean,
          na.rm = TRUE
        )
    )
  
  # Express ENMO in units of mg
  ag_data_raw_wrist_Hild <- 
    ag_data_raw_wrist_Hild %>% 
    mutate(mean.vmcorrg = mean.vmcorrg * 1000)
  
  # 0 = Sedentary or light, 1 = moderate, 2 = vigorious
  ag_data_raw_wrist_Hild$hild2 <- 
    ifelse(
      test = ag_data_raw_wrist_Hild$mean.vmcorrg >= hild_mvpa_cutpoint,
      yes  = 1,
      no   = 0
    )
  
  METs <- 
    (.0320 * ag_data_raw_wrist_Hild$mean.vmcorrg + 7.28) / 3.5
  
  MET.lev <- 
    rep(0,
        times = length(ag_data_raw_wrist_Hild$mean.vmcorrg))
  MET.lev[METs > 1.5] <- 
    1 	
  MET.lev[METs >= 3] <- 
    2 
  MET.lev <- 
    rep(MET.lev,
        each = win.width)[1:floor(n  / freq)]
  MET.lev <- 
    factor(MET.lev,
           levels = c(0, 1, 2),
           labels = c("sedentary", "light", "mvpa"))
  
  return(MET.lev)
  
}    

lyden_2014_soj3x <- function(counts,
                             counts.2,
                             counts.3,
                             vect.mag,
                             short = 30) {
  
  # DOI: 10.1249/MSS.0b013e3182a42a2d
  # PMID: 23860415
  
  y <- 
    counts
  counts.2 <- 
    counts.2
  counts.3 <- 
    counts.3
  inds <- 
    1:length(y)
  mmm <- 
    length(y)
  one <- 
    y[-mmm]
  two <- 
    y[-1]
  
  # Find transitions.
  trans <- 
    ((one - two) > 15) & (two <= 10) # This is how I find initial transitions.
  trans <- 
    c(0, trans)
  trans.inds <- 
    (1:mmm)[trans == 1]

  # How long between transitions
  durations <- 
    trans.inds[-1] - trans.inds[-length(trans.inds)]

  # Put first duration in and make last trans go till end of file
  dd <- 
    length(durations)
  tt <- 
    length(trans.inds)
  durations[dd + 1] <- 
    mmm - trans.inds[tt]
  dd <- 
    length(durations)
  durations.junk <- 
    trans.inds[1]
  durations <- 
    c(durations.junk, durations)
  dd <- 
    length(durations)

  durations.compare <- 
    durations
  length(durations.compare)

  # get number of sojourns

  sojourns <- 
    rep(1:length(durations),
        times = 1)
  sojourns.long <- 
    rep(sojourns,
        times = durations)
  mean.cnts.soj <- 
    as.vector(
      tapply(y,
             INDEX = sojourns.long,
             FUN = mean)
    )

  # Combine too short sojourns.

  # 	Combine too short sojourns with neighboring sojourn.
  # 	This loop repeats until there are no more too short sojourns.

  counter <- 
    1

  repeat { # Loop 1
    
    too.short <- 
      (1:dd)[durations < short]
    ts <- 
      length(too.short)

    if (length(too.short) == 0) {
      
      break()
      
    }

    if (length(too.short) > 0) {

      # This loop deals with instances where the first "too.short" sojourn is 
      # first sojourn of file (i.e. it only has a second neighbor to combine
      # it with).
      counter.1 <- 1

      repeat { # Loop 2

        if (too.short[counter.1] == counter.1) {
          
          sojourns[1:counter.1] <- 
            sojourns[counter.1 + 1]
          counter.1 <- 
            counter.1 + 1
          
        }

        if (too.short[counter.1] != counter.1) {
          
          break()
          
        }
        
      } # End Loop 2

      s <- 
        length(sojourns)

      # This loop deals with if last "too.short" sojourn is last sojourn of 
      # file (i.e. it only has a first neighbor to combine it with).

      counter.2 <- 
        s
      counter.ts <- 
        ts

      repeat { # Loop 3
        
        if (too.short[counter.ts] == counter.2) {
          
          sojourns[counter.2:s] <- 
            sojourns[counter.2 - 1]

          counter.2 <- 
            counter.2 - 1
          counter.ts <- 
            counter.ts - 1
          
        }

        if (too.short[counter.ts] != counter.2) {
          
          break()
          
        }
        
      } # End Loop 3

      s <- 
        length(sojourns)

      # Now deal with all other too short sojourns.
      junk.too.short <- 
        too.short

      if (counter.ts < ts - 1) {
        
        junk.too.short <- 
          too.short[-(counter.ts + 1:ts)]
        
      }
      
      if (counter.1 > 1) {
        
        junk.too.short <- 
          junk.too.short[-(1:counter.1 - 1)]
        
      }

      j.t.s <- 
        length(junk.too.short)

      first.neighbors <- 
        junk.too.short - 1
      second.neighbors <- 
        junk.too.short + 1

      # Right now I combine too short sojourns with its neighbor that was 
      # shorter in duration (e.g. first neighbor = 60 seconds long and second
      # neighbor = 300 seconds long, it gets combined with first neighbor)
      revised.sojourns <- 
        sojourns
      durations[junk.too.short]
      durations.first.neighbors <- 
        durations[first.neighbors]
      durations.second.neighbors <- 
        durations[second.neighbors]

      # Put in dummy duration for too.short sojourns at beginning 
      # and end of file.
      durations.first.neighbors[is.na(durations.first.neighbors)] <- 
        100000
      durations.second.neighbors[is.na(durations.second.neighbors)] <- 
        100000

      n.neighbors <- 
        length(durations.first.neighbors)
      n.neighbors.2 <- 
        length(durations.second.neighbors)

      inds.first <- # BUG ----
        (1:n.neighbors)[durations.first.neighbors <= durations.second.neighbors]
      inds.second <- 
        (1:n.neighbors)[durations.first.neighbors > durations.second.neighbors]

      too.short.inds.first <- 
        junk.too.short[inds.first]
      too.short.inds.second <- 
        junk.too.short[inds.second]

      revised.sojourns[too.short.inds.first] <- 
        first.neighbors[inds.first]
      revised.sojourns[too.short.inds.second] <- 
        second.neighbors[inds.second]

      # Deal with instances where need to combine more than 2 sojourns
      # (i.e. short sojourn became first neighbor, and then sojourn before
      # first neighbor also becomes that sojourn via second neighbor grouping).
      # Want all 3 of these sojourns to be combined.
      rs <- 
        length(revised.sojourns)
      one.order <- 
        revised.sojourns[-rs]
      two.order <- 
        revised.sojourns[-1]
      o <- 
        length(one.order)
      inds.order <- 
        (1:o)[one.order > two.order]
      
      if (length(inds.order > 0)) {
        
        revised.sojourns[inds.order + 1] <- 
          revised.sojourns[inds.order]
        
      }

      # Get new durations now that sojourns are combined.
      rs <- 
        length(revised.sojourns)
      revised.durations <- 
        as.vector(
          tapply(durations,
                 INDEX = revised.sojourns,
                 FUN   = sum)
        )
      rd <- 
        length(revised.durations)

      # Get new sojourns now that durations are combined.
      revised.sojourns <- 
        rep(1:length(revised.durations),
            times = 1)
      rs <- 
        length(revised.sojourns)
      durations <- 
        revised.durations
      dd <- 
        length(durations)
      sojourns <- 
        revised.sojourns
      s <- 
        length(sojourns)
      
    }

    # 	print(counter)
    counter <- 
      counter + 1
    
  } # End Loop 1

  # Make table of durations and sojourns etc.
  trans.table <- 
    data.frame(
      counts       = y,
      counts.2     = counts.2,
      counts.3     = counts.3,
      vect.mag     = vect.mag,
      sojourns     = 0,
      durations    = 0,
      perc.soj     = NA,
      soj.type.all = NA,
      soj.mets.all = NA
    )

  tt <- 
    dim(trans.table)[1]
  durations.1 <- 
    rep(durations,
        times = durations)
  sojourns.1 <- 
    rep(sojourns,
        times = durations)
  trans.table$durations <- 
    durations.1
  trans.table$sojourns <- 
    sojourns.1

  # Get percent non zero in table.
  perc.soj <- 
    tapply(y > 0,
           INDEX = sojourns.1,
           FUN   = mean)
  perc.soj <- 
    rep(perc.soj,
        times = durations)
  trans.table$perc.soj <- 
    perc.soj

  ### Get inds.inactivities so can test nnet only to distinguish between
  # lifestyle and sedentary.
  # Now get inactivity indices.

  inds.inacts <- 
    (1:tt)[trans.table$perc.soj < 0.7]
  inactivities <- 
    trans.table[inds.inacts, ]
  i.a <- 
    dim(inactivities)[1]
  inact.trans.inds <-
    c(1, (1 + (1:i.a)[inactivities$sojourns[-1] != inactivities$sojourns[-i.a]]))
  inact.durations <- 
    inactivities$durations[inact.trans.inds]

  #### Get nnet inputs for vertical axis.
  nnetinputs <-
    as.vector(
      unlist(
        tapply(
          inactivities$counts,
          INDEX = inactivities$sojourns,
          FUN   = quantile,
          probs = c(.1, .25, .5, .75, .9)
        )
      )
    )
  length(nnetinputs)
  nnetinputs <- 
    matrix(
      nnetinputs,
      nrow  = length(nnetinputs) / 5,
      ncol  = 5,
      byrow = TRUE
    )
  nnetinputs <- 
    as.data.frame(nnetinputs)
  names(nnetinputs) <- 
    c("X10.", "X25.", "X50.", "X75.", "X90.")
  nnetinputs$acf <- 
    0
  g <- 
    1
  
  for (soj in unique(inactivities$sojourns)) {
    
    counts <- 
      inactivities$counts[inactivities$sojourns == soj]

    if (sum(counts) > 0) {
      
      temp <- 
        acf(counts,
            lag.max = 1,
            plot    = FALSE)
      nnetinputs$acf[g] <- 
        as.numeric(unlist(temp[1, 1])[1])
      
    }
    
    g <- 
      g + 1
    # 	print(g)
    
  }

  nnetinputs$acf[is.na(nnetinputs$acf)] <-
    mean(nnetinputs$acf,
         na.rm = TRUE)

  #### Get nnetinputs.2 - second axis.
  nnetinputs.2 <-
    as.vector(
      unlist(
        tapply(
          inactivities$counts.2,
          INDEX = inactivities$sojourns,
          FUN   = quantile,
          probs = c(.1, .25, .5, .75, .9)
        )
      )
    )
  length(nnetinputs.2)
  nnetinputs.2 <-
    matrix(
      nnetinputs.2,
      nrow  = length(nnetinputs.2) / 5,
      ncol  = 5,
      byrow = TRUE
    )
  nnetinputs.2 <- 
    as.data.frame(nnetinputs.2)
  names(nnetinputs.2) <- 
    c("X10.2", "X25.2", "X50.2", "X75.2", "X90.2")
  nnetinputs.2$acf.2 <- 
    0
  g <- 
    1
  
  for (soj in unique(inactivities$sojourns)) {
    
    counts <- 
      inactivities$counts.2[inactivities$sojourns == soj]

    if (sum(counts) > 0) {
      
      temp <- 
        acf(counts,
            lag.max = 1,
            plot    = FALSE)
      nnetinputs.2$acf.2[g] <- 
        as.numeric(unlist(temp[1, 1])[1])
      
    }
    
    g <- 
      g + 1
    # 	print(g)
    
  }

  nnetinputs.2$acf.2[is.na(nnetinputs.2$acf.2)] <-
    mean(nnetinputs.2$acf.2,
         na.rm = TRUE)

  #### Get nnetinputs.3 - third axis.
  nnetinputs.3 <-
    as.vector(
      unlist(
        tapply(
          inactivities$counts.3,
          INDEX = inactivities$sojourns,
          FUN   = quantile,
          probs = c(.1, .25, .5, .75, .9)
        )
      )
    )
  length(nnetinputs.3)
  nnetinputs.3 <- 
    matrix(
      nnetinputs.3,
      nrow  = length(nnetinputs.3) / 5,
      ncol  = 5,
      byrow = TRUE
    )
  nnetinputs.3 <- 
    as.data.frame(nnetinputs.3)
  names(nnetinputs.3) <- 
    c("X10.3", "X25.3", "X50.3", "X75.3", "X90.3")
  nnetinputs.3$acf.3 <- 
    0
  g <- 
    1
  
  for (soj in unique(inactivities$sojourns)) {
    
    counts <- 
      inactivities$counts.3[inactivities$sojourns == soj]

    if (sum(counts) > 0) {
      temp <- 
        acf(counts,
            lag.max = 1,
            plot    = FALSE)
      nnetinputs.3$acf.3[g] <-
        as.numeric(unlist(temp[1, 1])[1])
      
    }
    
    g <- 
      g + 1
    # print(g)
    
  }

  nnetinputs.3$acf.3[is.na(nnetinputs.3$acf.3)] <-
    mean(nnetinputs.3$acf.3,
         na.rm = TRUE)

  #### Get nnetinputs.vm - vector magnitude.
  nnetinputs.vm <-
    as.vector(
      unlist(
        tapply(
          inactivities$vect.mag,
          INDEX = inactivities$sojourns,
          FUN   = quantile,
          probs = c(.1, .25, .5, .75, .9)
        )
      )
    )
  length(nnetinputs.vm)
  nnetinputs.vm <- 
    matrix(
      nnetinputs.vm,
      nrow  = length(nnetinputs.vm) / 5,
      ncol  = 5,
      byrow = TRUE
    )
  nnetinputs.vm <- 
    as.data.frame(nnetinputs.vm)
  names(nnetinputs.vm) <- 
    c("X10.vm", "X25.vm", "X50.vm", "X75.vm", "X90.vm")
  nnetinputs.vm$acf.vm <- 
    0
  g <- 
    1
  
  for (soj in unique(inactivities$sojourns)) {
    
    counts <- 
      inactivities$vect.mag[inactivities$sojourns == soj]

    if (sum(counts) > 0) {
      
      temp <- 
        acf(counts,
            lag.max = 1,
            plot    = FALSE)
      nnetinputs.vm$acf.vm[g] <- 
        as.numeric(unlist(temp[1, 1])[1])
      
    }
    
    g <- 
      g + 1
    # print(g)
    
  }

  nnetinputs.vm$acf.vm[is.na(nnetinputs.vm$acf.vm)] <-
    mean(nnetinputs.vm$acf.vm,
         na.rm = TRUE)

  # Combine inputs so can center and scale.
  inputs <- 
    cbind(nnetinputs,
          nnetinputs.2)
  inputs <- 
    cbind(inputs,
          nnetinputs.3)
  inputs <- 
    cbind(inputs,
          nnetinputs.vm)
  inputs <- 
    cbind(inputs,
          inact.durations)
  inputs <- 
    scale(inputs,
          center = lyden_2014_cent_1,
          scale  = lyden_2014_scal_1)
  inputs <- 
    as.data.frame(inputs)

  # Predict type using all axes + vm. I intially had a lot of prediction nnets
  # here (i.e. different axis) but have removed them and only include the
  # one that looks "the best".  There are definitely others we can use/try.
  # Remove NA's.
  inputs.1 <- 
    inputs[, -(13)]
  inputs.1 <- 
    inputs.1[, -(1:2)]
  cool.all <- 
    predict(lyden_2014_net_soj3x_activity,
            newdata = inputs.1)

  # Add soj.type to trans table.
  junk.cool.all <- 
    as.vector(
      apply(cool.all,
            MARGIN = 1,
            FUN = which.max)
    )

  cool.all <- 
    rep(junk.cool.all,
        times = inact.durations)

  trans.table$soj.type.all[inds.inacts] <- 
    cool.all
  
  # Assign mets to types.
  trans.table$soj.mets.all[(trans.table$soj.type.all == 1) & 
                             (trans.table$perc.soj <= 0.12)] <- 
    1.5
  trans.table$soj.mets.all[(trans.table$soj.type.all == 1) & 
                             (trans.table$perc.soj > 0.12)] <- 
    1.7
  trans.table$soj.mets.all[(trans.table$soj.type.all == 3) & 
                             (trans.table$perc.soj <= 0.05)] <- 
    1
  trans.table$soj.mets.all[(trans.table$soj.type.all == 3) & 
                             (trans.table$perc.soj > 0.05)] <- 
    1.2

  # This identifies activities for nnet all - 6 means activity.
  trans.table$soj.type.all[trans.table$perc.soj >= 0.7] <- 
    6
  inds.activity.all <- # BUG -----
    (1:tt)[(trans.table$perc.soj >= 0.7) | 
             (trans.table$soj.type.all == 2) | 
             (trans.table$soj.type.all == 4)]

  act.trans.table.all <- 
    trans.table[inds.activity.all, ]
  dim(act.trans.table.all)
  activity.durations.all <- 
    table(act.trans.table.all$sojourns)

  quantiles.all <- 
    tapply(
      act.trans.table.all$counts,
      INDEX = act.trans.table.all$sojourns,
      FUN   = quantile,
      probs = c(.1, .25, .5, .75, .9)
    )
  nn.trans.table.all <- 
    as.data.frame(
      do.call("rbind",
              quantiles.all)
    )

  # I realize i am getting lag1 differently than I do for inactivities...
  # I should change to use function throughout.
  nn.trans.table.all$acf <- 
    tapply(act.trans.table.all$counts,
           INDEX = act.trans.table.all$sojourns,
           FUN   = acf.lag1)
  nn.trans.table.all <- 
    nn.trans.table.all[, c(1:6)]
  names(nn.trans.table.all) <- 
    c("X10.", "X25.", "X50.", "X75.", "X90.", "acf")
  nnetinputs.acts.all <- 
    scale(nn.trans.table.all,
          center = lyden_2014_cent,
          scale  = lyden_2014_scal)

  # Predict METs.
  act.mets.all <- 
    predict(lyden_2014_net_soj3x_met,
            newdata = nnetinputs.acts.all)
  act.mets.all <- 
    rep(act.mets.all,
        times = activity.durations.all)

  # Put back in table.
  trans.table$soj.mets.all[inds.activity.all] <- 
    act.mets.all

  # Get breaks from sitting.
  # 	trans.table$do.breaks <- 0
  trans.table$soj.breaks.all <- 
    0
  soj.posture <- 
    as.vector(trans.table$soj.mets.all)
  s.p <- 
    length(soj.posture)
  soj.one.posture <- 
    soj.posture[-s.p]
  soj.two.posture <- 
    soj.posture[-1]
  soj.trans <- 
    (soj.one.posture < 1.5) & (soj.two.posture >= 1.5)
  soj.trans <- 
    c(0, soj.trans)
  soj.trans.inds <- 
    (1:s.p)[soj.trans == 1]
  trans.table$soj.breaks.all <- 
    soj.trans
  # 	sum(trans.table$soj.breaks.all)
  names(trans.table)[8:10] <- 
    c("type", "METs", "break")

  trans.table <- 
    trans.table[, -c(8, 10)]
  
}

# Lyden_2014_soj3x helpers ----
acf.lag1 <- function(x) {
  
  n <- 
    length(x)
  a <- 
    mean(
      (x[-1] - mean(x[-1])) * (x[-n] - mean(x[-n]))
    )
  v <- 
    var(x)
  
  if ((v==0)|(is.na(v))) {
    
    val <- 
      0
    
  }
  
  if ((v!=0)&(is.na(v)==F)) {
    
    val <- 
      a / v
    
  }
  
  return(val)
  
}

staudenmayer_2015 <- function(raw_x,
                              raw_y,
                              raw_z,
                              vm,
                              freq = 100) {
  
  # DOI: 10.1152/japplphysiol.00026.2015
  # PMID: 26112238
  
  win.width <- 
    15
  n <- 
    length(raw_x)
  
  mins <- 
    ceiling(n / (freq * win.width))  # this is really the number of 15-sec windows in the file 
  
  min <- 
    rep(1:mins,
        each = win.width * freq)[1:n]
  v.ang <- 
    90 * (asin(raw_x / vm) / (pi / 2))
  ag_data_raw_wrist_Staud <- 
    data.frame(
      mean.vm = 
        tapply(vm,
               INDEX = min,
               FUN   = mean,
               na.rm = TRUE),
      sd.vm = 
        tapply(vm,
               INDEX = min,
               FUN   = sd,
               na.rm = TRUE),
      mean.ang = 
        tapply(v.ang,
               INDEX = min,
               FUN   = mean,
               na.rm = TRUE),
      sd.ang = 
        tapply(v.ang,
               INDEX = min,
               FUN   = sd,
               na.rm = TRUE),
      p625 = 
        tapply(vm,
               INDEX = min,
               FUN   = pow.625,
               samp_freq = freq),
      dfreq = 
        tapply(vm,
               INDEX = min,
               FUN   = dom.freq,
               samp_freq = freq),
      ratio.df = 
        tapply(vm,
               INDEX = min,
               FUN   = frac.pow.dom.freq,
               samp_freq = freq)
    )
  
  # apply the models (estimates are for each 15 second epoch)
  
  # MET estimates by random forest
  ag_data_raw_wrist_Staud$METs.rf <- 
    predict(staud_2015_rnf_met,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$METs.rf[ag_data_raw_wrist_Staud$sd.vm < .01] <- 
    1
  
  # MET estimates by linear regression
  ag_data_raw_wrist_Staud$METs.lm <- 
    predict(staud_2015_lnr_met,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$METs.lm[ag_data_raw_wrist_Staud$sd.vm < .01] <- 
    1
  
  # MET level estimates (rf and tree)
  ag_data_raw_wrist_Staud$MET.lev.rf <- 
    predict(staud_2015_rnf_metlevel,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$MET.lev.tr <- 
    predict(staud_2015_tre_metlevel,
            newdata = ag_data_raw_wrist_Staud,
            type = "class")
  
  # sedentary or not estimates (rf and tree)
  ag_data_raw_wrist_Staud$sed.rf <- 
    predict(staud_2015_rnf_sedentary,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$sed.tr <- 
    predict(staud_2015_tre_sedentary,
            newdata = ag_data_raw_wrist_Staud,
            type = "class")
  
  # locomotion or not estimates (rf and tree)
  ag_data_raw_wrist_Staud$loc.rf <- 
    predict(staud_2015_rnf_locomotion,
            newdata = ag_data_raw_wrist_Staud)
  ag_data_raw_wrist_Staud$loc.tr <- 
    predict(staud_2015_tre_locomotion,
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
    rep(0,
        times = length(ag_data_raw_wrist_Staud$METs.rf))
  junk[ag_data_raw_wrist_Staud$METs.rf > 1.5] <- 
    1
  junk[ag_data_raw_wrist_Staud$METs.rf >= 3] <- 
    2
  junk <- 
    rep(junk,
        each = win.width)[1:floor(n / freq)]
  junk <- 
    factor(junk,
           levels = c(0, 1, 2),
           labels = c("sedentary", "light", "mvpa"))
  
  return(junk)
  
}

# Staudenmayer_2015 helper functions. ----
pow.625 <- function(vm,
                    samp_freq = 80) {
  
  mods <- 
    Mod(fft(vm))
  mods <- 
    mods[-1]	
  n <- 
    length(mods)
  n <- 
    floor(n / 2)
  freq <- 
    samp_freq * (1:n) / (2 * n)
  mods <- 
    mods[1:n]
  inds <- 
    (1:n)[(freq > 0.6) & (freq < 2.5)]
  pow625 <- 
    sum(mods[inds]) / sum(mods)
  mods[is.na(mods)] <- 
    0
  
  if (sd(vm) == 0) {
    
    pow625 <- 0
    
  }
  
  return(pow625)
  
}

dom.freq <- function(vm,
                     samp_freq = 80) {
  
  if(length(vm) == 1) {
    
    return(NA)
    
  }
  
  mods <- 
    Mod(fft(vm))
  mods <- 
    mods[-1]	
  n <- 
    length(mods)
  n <- 
    floor(n / 2)
  freq <- 
    samp_freq * (1:n) / (2 * n)
  mods <- 
    mods[1:n]
  dom.ind <- 
    which.max(mods)
  d.f <- 
    as.vector(freq[which.max(mods)])
  
  return(d.f)
  
}

frac.pow.dom.freq <- function(vm,
                              samp_freq = 80) {
  
  mods <- 
    Mod(fft(vm))
  mods <- 
    mods[-1]	
  n <- 
    length(mods)
  n <- 
    floor(n / 2)
  freq <- 
    samp_freq * (1:n) / (2 * n)
  mods <- 
    mods[1:n]
  rat <- 
    max(mods) / sum(mods)
  mods[is.na(mods)] <- 
    0
  
  if (sd(vm) == 0) {
    
    rat <- 0
    
  }
  
  return(rat)
  
}

marcotte_2021_soj_g <- function(data                      = NA,
                                export_format             = 'session',
                                freq                      = 80,
                                step1_sd_threshold        = .00375,
                                step2_min_window_length   = 0,
                                step2_nest_length         = 5,
                                step3_nest_length         = 60,
                                step3_orig_soj_length_min = 180) {
  
  # data <-
  #   df_rw_raw %>%
  #   mutate(VM = sqrt(axis_x^2 + axis_y^2 + axis_z^2)) %>%
  #   select(
  #     Timestamp = datetime,
  #     AxisX = axis_x,
  #     AxisY = axis_y,
  #     AxisZ = axis_z,
  #     VM
  #   ) %>%
  #   as.data.table()
  # export_format <- "seconds"
  # freq <- 100
  # step1_sd_threshold <- 0.00375
  # step2_nest_length <- 5
  # step3_nest_length <- 60
  # step3_orig_soj_length_min <- 180
  
  # Remove last partial fraction of a second if number of observations is not a clean multiple of the sampling frequency
  if (nrow(data) %% freq != 0) {
    
    data <-
      data[1:(nrow(data) - (nrow(data) %% freq)), ]
    
  }
  
  #
  # Step 1 - Identify likely inactive periods----
  #
  message(
    paste0(
      "...Identifying likely inactive periods using sd_vm threshold = ",
      step1_sd_threshold
    )
  )
  data$index <- 
    rep(1:ceiling(nrow(data) / freq),
        each = freq)[1:nrow(data)]
  data_summary <- 
    data %>%
    group_by(index) %>%
    dplyr::summarize(sd_vm = sd(VM,
                                na.rm = TRUE))
  data_summary$step1_estimate <- 
    ifelse(test = data_summary$sd_vm <= step1_sd_threshold,
           yes  = 1,
           no   = 0) # 1 = inactive, 0 = unclassified

  seconds_index <- 
    seq(from = 1,
        to   = nrow(data),
        by   = freq)
  diffs <- 
    which((dplyr::lag(data_summary$step1_estimate) != data_summary$step1_estimate) == TRUE)
  diffs <- 
    c(1, diffs)
  data_summary$step2_sojourn_index <- 
    NA
  data_summary$step2_sojourn_duration <- 
    NA

  #
  # Step 2 - Segment remaining unlabeled periods into smaller windows, identify whether inactive or active----
  #
  message("...Segmenting remaining unlabeled periods into smaller windows")
  data_summary$step2_sojourn_index <- 
    data.table::rleid(data_summary$step1_estimate)
  data_summary$step2_sojourn_duration[diffs] <- 
    rle(data_summary$step2_sojourn_index)[[1]]
  data_summary$step2_sojourn_duration <- 
    zoo::na.locf(data_summary$step2_sojourn_duration)
  data_summary <- 
    data_summary %>%
    group_by(step2_sojourn_index) %>%
    mutate(
      step2_sojourn_index = 
        nest_sojourn(sojourn             = step2_sojourn_index,
                     orig_soj_length_min = step2_nest_length,
                     nest_length         = step2_nest_length)
    )
  # data_summary$step2_sojourn_index = sort(unlist(tapply(data_summary$step2_sojourn_index, data_summary$step2_sojourn_index, nest_sojourn, nest_length = step2_nest_length)))

  # Repopulate original data with step1 and 2 sojourn ID
  data$VM_sd_1sec <- 
    rep(data_summary$sd_vm,
        each = freq)[1:nrow(data)]
  data$step1_estimate <- 
    rep(data_summary$step1_estimate,
        each = freq)[1:nrow(data)]
  data$step2_sojourn_index <- 
    rep(data_summary$step2_sojourn_index,
        each = freq)[1:nrow(data)]
  data$step2_sojourn_duration <- 
    rep(data_summary$step2_sojourn_duration,
        each = freq)[1:nrow(data)]

  # Compute features within nested sojourns
  message("...Computing signal features in nested sojourn windows")
  ag_step2_summary <- 
    ag_feature_calc(
      ag_data_raw_wrist = 
        data %>% 
        dplyr::rename(sojourn = step2_sojourn_index,
                      seconds = step2_sojourn_duration),
      samp_freq = freq,
      window    = "sojourns"
    ) # , soj_colname = 'step2_sojourn_index', seconds_colname = 'step2_sojourn_duration')
  ag_step2_summary <- 
    ag_step2_summary %>% 
    dplyr::rename(step2_sojourn_index    = sojourn,
                  step2_sojourn_duration = seconds)
  ag_step2_summary$step2_durations <- 
    rle(data_summary$step2_sojourn_index)[[1]]
  ag_step2_summary$seconds <- 
    ag_step2_summary$step2_durations
  ag_step2_summary$step2_estimate <- 
    predict(MOCAModelData::sojg_stage2_unclassified_rf,
            newdata = ag_step2_summary,
            type    = "class")
  ag_step2_summary <- 
    ag_step2_summary %>% 
    dplyr::select(-seconds)

  # Append the step2 activity state estimate to the 1-sec summary dataframe
  data_summary$step2_estimate <- 
    rep(ag_step2_summary$step2_estimate,
        times = ag_step2_summary$step2_durations)
  data_summary$step3_sojourn_index <- 
    NA
  data_summary$step3_sojourn_duration <- 
    NA
  diffs <- 
    which((dplyr::lag(data_summary$step2_estimate) != data_summary$step2_estimate) == TRUE)
  diffs <- 
    c(1, diffs)

  # Verify that labels under step2_estimate are character dummy variables
  if (all(!is.na(as.numeric(levels(data_summary$step2_estimate))))) {
    
    data_summary$step2_estimate <- 
      factor(data_summary$step2_estimate,
             levels = c(1, 2),
             labels = c("Stationary", "Active"))
    
  }
  
  data_summary$step3_sojourn_index <- 
    data.table::rleid(data_summary$step2_estimate)
  data_summary$step3_sojourn_duration[diffs] <- 
    rle(data_summary$step3_sojourn_index)[[1]]
  data_summary$step3_sojourn_duration <- 
    zoo::na.locf(data_summary$step3_sojourn_duration)

  message("...Looking for too-short sojourn windows")
  if (any(data_summary$step3_sojourn_duration < step2_min_window_length)) {
    
    too_short <- 
      data_summary %>%
      group_by(step2_estimate,
               step3_sojourn_index,
               step3_sojourn_duration) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::arrange(step3_sojourn_index)

    # First, combine string of short sojourns together
    too_short$too_short <- 
      too_short$step3_sojourn_duration < step2_min_window_length
    too_short$too_short_index <- 
      data.table::rleid(too_short$too_short)

    # This section may introduce time jumbling. Verify this works fine with other min_window lengths
    temp <- 
      too_short %>%
      dplyr::group_by(too_short_index,
                      too_short) %>%
      dplyr::filter(too_short == TRUE) %>%
      dplyr::summarize(
        step3_sojourn_index    = dplyr::first(step3_sojourn_index),
        step3_sojourn_duration = sum(step3_sojourn_duration),
        step2_estimate         = 
          factor(max(as.numeric(step2_estimate),
                     na.rm = TRUE),
                 levels = c(1, 2),
                 labels = c("Stationary", "Active"))
      )

    too_short_updated <- 
      bind_rows(
        too_short %>% 
          dplyr::filter(too_short == FALSE),
        temp
      ) %>% 
      dplyr::arrange(step3_sojourn_index)

    # too_short_updated = too_short %>% dplyr::group_by(too_short_index, too_short) %>% dplyr::summarize(step3_sojourn_duration = sum(step3_sojourn_duration),
    #                                                                                                    step2_estimate = factor(max(as.numeric(step2_estimate), na.rm = T), levels = c(1,2), labels = c('Stationary','Active')))

    # Second, check to see if there are any remaining sojourns that are still too short
    if (any(too_short_updated$step3_sojourn_duration < step2_min_window_length)) {
      
      too_short_updated$too_short <- 
        too_short_updated$step3_sojourn_duration < step2_min_window_length
      too_short_updated$neighbor_lag <- 
        dplyr::lag(too_short_updated$step3_sojourn_duration)
      too_short_updated$neighbor_lead <- 
        dplyr::lead(too_short_updated$step3_sojourn_duration)

      too_short_updated$updated_duration <- 
        too_short_updated$step3_sojourn_duration

      too_short_indices <- 
        which(too_short_updated$too_short == TRUE)

      for (i in 1:length(too_short_indices)) {
        
        if (too_short_updated$neighbor_lag[too_short_indices[i]] <= too_short_updated$neighbor_lead[too_short_indices[i]]) {
          
          too_short_updated$updated_duration[(too_short_indices[i] - 1)] <- 
            too_short_updated$updated_duration[(too_short_indices[i] - 1)] + 
            too_short_updated$step3_sojourn_duration[(too_short_indices[i])]
          
        } else {
          
          too_short_updated$updated_duration[(too_short_indices[i] + 1)] <- 
            too_short_updated$updated_duration[(too_short_indices[i] + 1)] + 
            too_short_updated$step3_sojourn_duration[(too_short_indices[i])]
        }
        
      }

      too_short_updated$perc.original.duration <- 
        too_short_updated$step3_sojourn_duration / 
        too_short_updated$updated_duration
      # If the original sojourn length is not >= 70% of the newly merged 
      # duration, assign the estimate to be the other intensity
      # (Stationary vs Active).
      too_short_updated$step2_estimate <- 
        ifelse(
          test = too_short_updated$step3_sojourn_duration / too_short_updated$updated_duration < .7,
          yes = ifelse(test = too_short_updated$step2_estimate == "Stationary",
                       yes  = 2,
                       no   = too_short_updated$step2_estimate),
          no  = too_short_updated$step2_estimate
        )
      too_short_updated$step2_estimate <- 
        factor(too_short_updated$step2_estimate,
               levels = c(1, 2),
               labels = c("Stationary", "Active"))
      too_short_updated <- 
        too_short_updated %>%
        dplyr::select(-step3_sojourn_duration) %>%
        rename(step3_sojourn_duration = updated_duration)
      too_short_updated <- 
        too_short_updated[-too_short_indices, ]
      
    }

    too_short_updated$step3_sojourn_index <- 
      data.table::rleid(too_short_updated$step2_estimate)

    too_short_updated <- 
      too_short_updated %>%
      group_by(step3_sojourn_index,
               step2_estimate) %>%
      dplyr::summarize(step3_sojourn_duration = sum(step3_sojourn_duration))

    data_summary$step3_sojourn_index <- 
      rep(too_short_updated$step3_sojourn_index,
          times = too_short_updated$step3_sojourn_duration)[1:nrow(data_summary)]
    data_summary$step3_sojourn_state <- 
      rep(too_short_updated$step2_estimate,
          times = too_short_updated$step3_sojourn_duration)[1:nrow(data_summary)]
    data_summary$step3_sojourn_duration <- 
      rep(too_short_updated$step3_sojourn_duration,
          times = too_short_updated$step3_sojourn_duration)[1:nrow(data_summary)]
    
  } else {
    
    data_summary$step3_sojourn_state <- 
      data_summary$step2_estimate #################################### BUG FIX 
    
  }

  data_summary <- 
    data_summary %>%
    group_by(step3_sojourn_index) %>%
    mutate(
      step3_sojourn_index = 
        nest_sojourn(step3_sojourn_index,
                     orig_soj_length_min = step3_orig_soj_length_min,
                     nest_length         = step3_nest_length)
    )
  # data_summary$step3_sojourn_index = sort(unlist(tapply(data_summary$step3_sojourn_index, data_summary$step3_sojourn_index, nest_sojourn2, orig_soj_length_min = step3_orig_soj_length_min, nest_length = step3_nest_length)))

  data$step2_estimate <- 
    rep(data_summary$step2_estimate,
        each = freq)[1:nrow(data)]
  data$step3_sojourn_index <-
    rep(data_summary$step3_sojourn_index,
        each = freq)[1:nrow(data)]
  data$step3_sojourn_duration <- 
    rep(data_summary$step3_sojourn_duration,
        each = freq)[1:nrow(data)]

  # Compute features in final sojourns
  ag_step3_summary <- 
    ag_feature_calc(
      ag_data_raw_wrist = 
        data %>% 
        dplyr::rename(sojourn = step3_sojourn_index,
                      seconds = step3_sojourn_duration),
      samp_freq = freq,
      window    = "sojourns"
    ) # , soj_colname = 'step3_sojourn_index', seconds_colname = 'step3_sojourn_duration')

  ag_step3_summary <- 
    ag_step3_summary %>% 
    dplyr::rename(step3_sojourn_index    = sojourn,
                  step3_sojourn_duration = seconds)
  ag_step3_summary$step3_durations <- 
    rle(data_summary$step3_sojourn_index)[[1]]
  final_step2_estimate <- 
    data_summary %>%
    group_by(step3_sojourn_index) %>%
    summarize(step2_estimate = dplyr::first(step2_estimate)) %>%
    select(step2_estimate) %>%
    ungroup() %>%
    as.vector()
  ag_step3_summary$step2_estimate <- 
    final_step2_estimate$step2_estimate

  ag_step3_summary$seconds <- 
    ag_step3_summary$step3_sojourn_duration
  ag_step3_summary$step3_estimate_intensity <- 
    predict(MOCAModelData::sojg_stage3_intensity_rf,
            newdata = ag_step3_summary,
            type    = "class")
  # ag_step3_summary$step3_estimate_type = predict(MOCAModelData::sojg_stage3_activity_rf, newdata = ag_step3_summary, type = 'class')
  # ag_step3_summary$step3_estimate_locomotion = predict(MOCAModelData::sojg_stage3_locomotion_rf, newdata = ag_step3_summary, type = 'class')

  if (export_format == "session") {
    
    session_summary <- 
      data.frame(
        Date          = NA,
        Total_minutes = NA,
        Sedentary     = NA,
        Light         = NA,
        Moderate      = NA,
        Vigorous      = NA
      )
    temp <- 
      ag_step3_summary %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Date = lubridate::date(Timestamp)) %>%
      group_by(Date,
               step3_estimate_intensity) %>%
      dplyr::summarize(minutes = round(sum(step3_durations) / 60,
                                       digits = 2)) %>%
      tidyr::spread(step3_estimate_intensity,
                    minutes)

    session_summary <- 
      bind_rows(session_summary,
                temp) %>% 
      dplyr::filter(!is.na(Date))
    session_summary <- 
      session_summary %>%
      dplyr::ungroup() %>%
      tidyr::replace_na(replace = list(Sedentary = 0,
                                       Light     = 0,
                                       Moderate  = 0,
                                       Vigorous  = 0)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        Total_minutes = rowSums(across(Sedentary:Vigorous)),
        MVPA          = Moderate + Vigorous
      )

    return(session_summary)
    
  }

  if (export_format == "sojourn") {
    
    return(ag_step3_summary)
    
  }

  if (export_format == "seconds") {
    
    data_summary$step3_estimate_intensity <- 
      rep(ag_step3_summary$step3_estimate_intensity,
          times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_type = rep(ag_step3_summary$step3_estimate_type, times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_locomotion = rep(ag_step3_summary$step3_estimate_locomotion, times = ag_step3_summary$step3_durations)
    data_summary$Timestamp <- 
      data$Timestamp[seq(from = 1,
                         to = nrow(data),
                         by = freq)[1:nrow(data_summary)]]
    data_summary <- 
      data_summary %>% 
      dplyr::relocate(Timestamp)

    return(data_summary)
    
  }

  if (export_format == "raw") {
    
    data_summary$step3_estimate_intensity <- 
      rep(ag_step3_summary$step3_estimate_intensity,
          times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_type = rep(ag_step3_summary$step3_estimate_type, times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_locomotion = rep(ag_step3_summary$step3_estimate_locomotion, times = ag_step3_summary$step3_durations)
    data$step3_estimate_intensity <- 
      rep(data_summary$step3_estimate_intensity,
          each = freq)[1:nrow(data)]
    # data$step3_estimate_type = rep(data_summary$step3_estimate_type, each = freq)[1:nrow(data)]
    # data$step3_estimate_locomotion = rep(data_summary$step3_estimate_locomotion, each = freq)[1:nrow(data)]

    return(data)
    
  }
  
}

# Marcotte_2021 helper functions (also uses Staudenmaer_2015 helper functions) ----
ag_feature_calc <- function(ag_data_raw_wrist,
                            participant,
                            samp_freq          = 80,
                            window             = 15,
                            long_axis          = 'y',
                            angle_comp         ='Default_Staudenmayer',
                            soj_colname        = NA,
                            seconds_colname    = NA,
                            inactive_threshold = .00375) {
  
  # Only uncomment the below if trying to reference column indices with a non-numeric character element (e.g. .[[long_axis_index]])
  # if("data.table" %in% (.packages())){
  #   detach(package:data.table, unload = TRUE, force = T) # causes issues with referencing column indices with a non-numeric character element
  # }

  n <-
    dim(ag_data_raw_wrist)[1]

  # Assumes that the Timestamp column is first
  # long_axis_index = switch(long_axis,
  #                          'x' = str_which(colnames(ag_data_raw_wrist), 'AxisX'),
  #                          'y' = str_which(colnames(ag_data_raw_wrist), 'AxisY'),
  #                          'z' = str_which(colnames(ag_data_raw_wrist), 'AxisZ'))

  switch(
    angle_comp,
    "Rowlands" = {
      
      ag_data_raw_wrist$v.ang <- 
        (90 * asin(ag_data_raw_wrist[, long_axis_index] / ag_data_raw_wrist$VM)) / (pi / 2)
      ag_data_raw_wrist$SedSphere_v.ang <- 
        ifelse(
          test = ag_data_raw_wrist[, long_axis_index] > 1,
          yes  = asin(1) * 180 / pi,
          no   = 
            ifelse(
              test = ag_data_raw_wrist[, long_axis_index] < -1,
              yes  = asin(-1) * 180 / pi,
              no   = asin(pmin(pmax(ag_data_raw_wrist[, long_axis_index],
                                    -1.0),
                               1.0)) * 180 / pi
            )
        )
      
    },
    "Default_Staudenmayer" = {
      
      if (long_axis == "y") {
        
        ag_data_raw_wrist <- 
          ag_data_raw_wrist %>%
          dplyr::rowwise() %>%
          dplyr::mutate(v.ang = 90 * asin(AxisY / VM) / (pi / 2))
        
      } else if (long_axis == "x") {
        
        ag_data_raw_wrist <- 
          ag_data_raw_wrist %>%
          dplyr::rowwise() %>%
          dplyr::mutate(v.ang = 90 * asin(AxisX / VM) / (pi / 2))
        
      } else {
        
        # Assume long axis is z
        ag_data_raw_wrist <- 
          ag_data_raw_wrist %>%
          dplyr::rowwise() %>%
          dplyr::mutate(v.ang = 90 * asin(AxisZ / VM) / (pi / 2))
        
      }
    }
  )


  if (window == "sojourns") {
    
    # Redacted, but used to call flexible naming of sojourn and seconds column name
    #   soj_colindex = which(colnames(ag_data_raw_wrist) == soj_colname)
    #   seconds_colindex = which(colnames(ag_data_raw_wrist) == seconds_colname)

    # Compute features within sojourns using dplyr
    ag_data_raw_wrist.sum <- 
      ag_data_raw_wrist %>%
      dplyr::group_by(sojourn) %>%
      dplyr::summarize(
        Timestamp         = dplyr::first(Timestamp),
        sojourn           = dplyr::first(sojourn),
        seconds           = dplyr::first(seconds),
        perc.soj.inactive = mean(VM_sd_1sec <= inactive_threshold),
        mean.vm           = mean(VM,
                                 na.rm = TRUE),
        sd.vm             = sd(VM,
                               na.rm = TRUE),
        mean.ang          = mean(v.ang,
                                 na.rm = TRUE),
        sd.ang            = sd(v.ang,
                               na.rm = TRUE),
        p625              = pow.625(VM),
        p1020             = pow1020(VM),
        dfreq             = dom.freq(VM),
        ratio.df          = frac.pow.dom.freq(VM)
      )
    
  } else {
    
    epoch <- 
      ceiling(n / (samp_freq * window))
    ag_data_raw_wrist$epoch <- 
      rep(1:epoch,
          each = window * samp_freq)[1:n]

    # Compute features within epochs
    ag_data_raw_wrist.sum <- 
      ag_data_raw_wrist %>%
      dplyr::group_by(epoch) %>%
      dplyr::summarize(
        Timestamp = dplyr::first(Timestamp),
        mean.vm   = mean(VM,
                         na.rm = TRUE),
        sd.vm     = sd(VM,
                       na.rm = TRUE),
        mean.ang  = mean(v.ang,
                         na.rm = TRUE),
        sd.ang    = sd(v.ang,
                       na.rm = TRUE),
        p625      = pow.625(VM),
        p1020     = pow1020(VM),
        dfreq     = dom.freq(VM),
        ratio.df  = frac.pow.dom.freq(VM)
      )

    if (angle_comp == "Rowlands") {
      
      temp <- 
        ag_data_raw_wrist %>%
        dplyr::group_by(epoch) %>%
        dplyr::summarize(
          Timestamp          = dplyr::first(Timestamp),
          SedSphere_mean.ang = mean(SedSphere_v.ang,
                                    na.rm = TRUE),
          SedSphere_sd.ang   = sd(SedSphere_v.ang,
                                  na.rm = TRUE)
        )

      ag_data_raw_wrist.sum <- 
        dplyr::left_join(ag_data_raw_wrist.sum,
                         temp)
    }
  }

  return(ag_data_raw_wrist.sum)
  
}
nest_sojourn <- function(sojourn,
                         orig_soj_length_min = 180,
                         nest_length         = 60,
                         step_by             = .00001) {
  
  if (length(sojourn) > orig_soj_length_min) {
    
    n <- 
      ceiling(length(sojourn) / nest_length) - 1

    additives <- 
      seq(from = 0,
          to = n / 10,
          by = step_by)
    additives <- 
      rep(additives,
          each = nest_length,
          length.out = length(sojourn))

    if (length(sojourn) %% nest_length > 0) {
      
      short_window <- 
        length(sojourn) %% nest_length
      additives[(length(additives) - short_window):length(additives)] <- 
        additives[(length(additives) - short_window)]
      
    }
    
    sojourn <- 
      sojourn + additives
    
  }

  return(sojourn)
  
}
pow1020 <- function(signal,
                    samp_freq = 80) {
  
  mods <- 
    Mod(fft(signal))
  mods <- 
    mods[-1]
  n <- 
    length(mods)
  n <- 
    floor(n / 2)
  freq <- 
    samp_freq * (1:n) / (2 * n)
  mods <- 
    mods[1:n]
  # Refer to 2016 Straczkiewicz Physiol Meas paper on Driving Detection 
  # DADA algorithm.
  inds <- 
    (1:n)[(freq > 10) & (freq < 20)]
  pow1020 <- 
    sum(mods[inds]) / sum(mods)
  mods[is.na(mods)] <- 
    0
  
  if (sd(signal) == 0) {
    pow1020 <- 0
  }

  return(pow1020)
  
}