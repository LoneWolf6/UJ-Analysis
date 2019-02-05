#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

###########################################################################################
# R package UJAnalysis
# Leuphana Univerisity Lueneburg - Institute of Information Systems
# VB/BF
###########################################################################################

###
# functions necessary for generic UJ creation
###

# check colnames of input
check_colnames = function(input){
  cnames = colnames(input)
  cnames_necessary = c("id", "type", "value", "date")
  if(any(!cnames_necessary %in% cnames)){return(F)}
  else{return(T)}
}

# check type of input
check_type = function(input){
  type = class(input)
  if(type != "data.frame"){return(F)}
  else{return(T)}
}

# check if input$type is of type factor or character
check_type_of_type = function(input){
  if(is.factor(input$type)) cnames = c('id', 'date', unique(as.character(input$type)))
  if(is.character(input$type)) cnames = c('id', 'date', unique(input$type))
  if(!is.factor(input$type) && !is.character(input$type)) cnames = c('id', 'date')
  if(length(cnames) < 3) stop("The column 'type' is not of type character or factor. \n")
  return(cnames)
}

# function for the creation of the UJ
creation = function(input, cnames, d, store, where_all, store_add, additional, extraCol, where_extra, store_extra, where_add){
  if(any(grepl(input['type'], cnames, fixed = T))){
    where = grep(paste('^', input['type'], '$', sep = ''), colnames(d))
    where_all = c(where_all, where)
    if(is.na(input['value']) || input['value'] == ''){
      store = c(store, NA)
    }else{
      store = c(store, as.character(input['value']))
    }
  }else{
    cat('Type is not existent \n')
  }
  if(any(additional != F)){
    if(length(which(colnames(d) %in% unique(unlist(input[additional])))) > 0){
      where_add = which(colnames(d) %in% unique(unlist(input[additional])))
      if(is.na(input[additional]) || input[additional] == ''){
        store_add = c(store_add, NA)
      }else{
        store_add = c(store_add, as.character(input[additional]))
      }
    }else{
      cat('Additional variable is not existent \n')
    }
  }
  if(any(extraCol != F)){
    if(length(which(colnames(d) %in% extraCol)) > 0){
      where_extra = which(colnames(d) %in% extraCol)
      if(is.na(input[extraCol]) || input[extraCol] == ''){
        store_extra = c(store_extra, rep(NA, length(extraCol)))
      }else{
        store_extra = c(store_extra, as.character(input[extraCol]))
      }
    }else{
      cat('Extra variable is not existent \n')
    }
  }
  return(list(dat=store, where=where_all, add=store_add, where_add = where_add, extra=store_extra, where_extra=where_extra))
}

# adjusts parallel results for UJ creation
create_res = function(res_fin, input){
  for(i in 1:length(res_fin)){
    if(class(input$id) == "factor"){
      ids = which(res_fin[[i]]$id != as.numeric(levels(input$id)[i]))
    }else{
      ids = which(res_fin[[i]]$id != unique(input$id)[i])
    }
    if(length(ids) > 0) res_fin[[i]] = res_fin[[i]][-ids,]
  }
  res_fin = do.call("rbind", res_fin)
  return(res_fin)
}

# function for the handling of the mean and sum procedure when multiple observation of same type exists at the same point in time
mean_sum = function(dat, handling, colsProcess=F){
  if(colsProcess==F){
    dup = dat[dat[,2] %in% unique(dat[duplicated(dat[,2]),2]),]
    cols = unique(unlist(dup[,2]))
    val_res = c()
    for(j in 1:length(cols)){
      spot = which(dup[,2] == cols[j])
      if(any(is.na(suppressWarnings(as.double(unlist(dup[spot,1])))))){
        cat = T
        if(length(unlist(dup[spot,1])) > 2 && length(unique(unlist(dup[spot,1]))) > 1){
          val_res = c(val_res, Mode(unlist(dup[spot,1])))
        }else{
          val_res = c(val_res, unlist(dup[spot,1])[[1]])
        }
      }else{
        cat = F
        vals = suppressWarnings(as.double(unlist(dup[spot,1])))
      }
      if(handling == "sum" && cat == F) val_res = c(val_res, round(sum(vals), digits=3))
      if(handling == "mean" && cat == F) val_res = c(val_res, round(mean(vals),digits=3))
    }
    dat = dat[!duplicated(dat[,2], fromLast = F),]
    if(!is.null(val_res)){
      if(length(dat) == 6){
        dat$dat = as.character(val_res)
      }else{
        dat[which(dat[,2] %in% cols),1] = as.character(val_res)
      }
    }
  }
  if(colsProcess=='extraCol'){
    if(length(dat[,5][[1]]) == 1){
      dup = dat[dat[,6] %in% unique(dat[duplicated(dat[,6]),6]),]
      cols = unique(unlist(dup[,6]))
      val_res = c()
      for(j in 1:length(cols)){
        spot = which(dup[,6] == cols[j])
        if(any(is.na(suppressWarnings(as.double(unlist(dup[spot,5])))))){
          cat = T
          if(length(unlist(dup[spot,5])) > 2 && length(unique(unlist(dup[spot,5]))) > 1){
            val_res = c(val_res, Mode(unlist(dup[spot,5])))
          }else{
            val_res = c(val_res, unlist(dup[spot,5])[[1]])
          }
        }else{
          cat = F
          vals = suppressWarnings(as.double(unlist(dup[spot,5])))
        }
        if(handling == "sum" && cat == F) val_res = c(val_res, round(sum(vals), digits=3))
        if(handling == "mean" && cat == F) val_res = c(val_res, round(mean(vals),digits=3))
      }
      dat = dat[!duplicated(dat[,6], fromLast = F),]
      if(!is.null(val_res)){
        if(length(dat) == 6){
          dat$extra = as.character(val_res)
        }else{
          dat[which(dat[,6] %in% cols),5] = as.character(val_res)
        }
      }
      # if multiple lists
    }else{
      dup = unlist(dat[,6])
      cols = unique(dup[duplicated(unlist(dat[,6]))])
      dup = dat[unlist(lapply(dat[,6], function(x) any(cols %in% unique(dup[duplicated(unlist(dat[,6]))])))),]
      val_res = c()
      for(j in 1:length(cols)){
        spot = which(unlist(dup[,6]) == cols[j])
        if(any(is.na(suppressWarnings(as.double(unlist(dup[,5])[spot]))))){
          cat = T
          if(length(unlist(dup[,5])[spot]) > 2 && length(unique(unlist(dup[,5])[spot])) > 1){
            val_res = c(val_res, Mode(unlist(dup[,5])[spot]))
          }else{
            val_res = c(val_res, unlist(dup[,5])[spot][[1]])
          }
        }else{
          cat = F
          vals = suppressWarnings(as.double(unlist(dup[,5])[spot]))
        }
        if(handling == "sum" && cat == F) val_res = c(val_res, round(sum(vals), digits=3))
        if(handling == "mean" && cat == F) val_res = c(val_res, round(mean(vals),digits=3))
      }
      if(!is.null(val_res)){
        dup = as.data.frame(list(add=val_res, where=cols))
      }
      dat = dup
    }
  }
  return(dat)
}

# calculates the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# create the UJ including parallelization option
reshapeData = function(input, additional=F, extraCol=F, handling=F, handlingExtra=F,  parallel=F, cores=F, na.rm=F){
  # check status of input
  if(check_colnames(input) == F) stop("The data does not have the right amount of columns or the specific names are not existent. See manual for more information. \n")
  if(check_type(input) == F) stop("The input is not of type 'data.frame'. \n")
  # check type of input$type
  cnames = check_type_of_type(input)
  # check additional parameter
  if(any(additional != F)){
    if(!(is.character(additional))) stop("The additional parameter needs to  be a character string.")
    if(!any((additional %in% colnames(input)))) stop("The additional parameter has not been specified correctly or does not exist in input.")
    if(length(additional) == 1) cnames = append(cnames, unique(input[additional])[[1]])
    if(length(additional) > 1){
      for(i in 1:length(additional)){
        cnames = append(cnames, unique(input[additional[i]])[[1]])
      }
    }
  }
  # check extraCol parameter
  if(any(extraCol != F)){
    if(!(is.character(extraCol))) stop("The extraCol parameter needs to  be a character string.")
    if(!any((extraCol %in% colnames(input)))) stop("The extraCol parameter has not been specified correctly or does not exist in input.")
    cnames = append(cnames, extraCol)
  }
  res = data.frame(matrix(ncol=length(cnames), nrow=0))
  colnames(res) = cnames
  input = input[order(input$id, input$date),]
  ids = unique(input$id)
  count = 1
  if(class(input$date) == 'Date') res[,2] = as.Date(res[,2], origin="1970-01-01")
  if(class(input$date) == 'factor') res[,2] = as.Date(res[,2], origin="1970-01-01")
  if(class(input$date) == 'POSIXlt') res[,2] = as.Date.POSIXlt(res[,2])
  if(class(input$date) == 'POSIXlt') res[,2] = as.POSIXlt(res[,2])
  if(parallel == T){
    if(cores == F) cl = makeCluster(detectCores()-1)
    if((class(cores) != "numeric" | cores > detectCores()) & cores != F){
      stop("The provided argument 'cores' is either not numeric/integer or greater than 'detectCores()'. Please provide sufficient number of cores. \n")
    }
    if(cores != F & class(cores) == "numeric" & cores <= detectCores()){
      cores = round(cores)
      cl = makeCluster(cores)
    }
    registerDoSNOW(cl)
    pb <- timerProgressBar(min = 0, max = length(ids), style = 3)
    progress <- function(n) setTimerProgressBar(pb, n)
    opts <- list(progress = progress)
    res_fin = foreach(i=1:length(ids), .combine = rbind, .export=c("mean_sum", "creation", 'Mode'), .inorder=T, .options.snow = opts) %dopar% {
      sub = subset(input, input$id==ids[i])
      dates = unique(sub$date)
      for(k in 1:length(dates)){
        sub_data = subset(input, input$id==ids[i] & input$date==dates[k])
        if(nrow(sub_data) > 0){
          if(class(sub_data$id) == "factor"){
            res[count,1] = as.numeric(as.character(sub_data$id[1]))
          }else{
            res[count,1] = sub_data$id[1]
          }
          res[count,2] = sub_data$date[1]
          dat = do.call(rbind,apply(sub_data, 1, creation, additional=additional, cnames=cnames, d=res, extraCol=extraCol, store=c(), where_all=c(), store_add=c(), where_extra=c(), store_extra=c(), where_add =c()))
          #### conditions: procedure for multiple observations of same type at same point in time
          if(any(duplicated(dat[,2]))){
            # cond 1/2: take most recent value (default) or first value (first)
            if(handling == F) dat_dup = dat[!duplicated(dat[,2], fromLast = T),]
            if(handling == 'first') dat_dup = dat[!duplicated(dat[,2]),]
            # cond 3/4: mean value of existing values/sum of existing values
            if(handling == "sum") dat_dup = mean_sum(dat, "sum")
            if(handling == "mean") dat_dup = mean_sum(dat, "mean")
            # merge data back together and create the result
            if(length(dat_dup) == 6){
              res[count, dat_dup$where] = dat_dup$dat
            }else{
              res[count, unlist(dat_dup[,2])] = unlist(dat_dup[,1])
            }
          }else{
            res[count, unlist(dat[,2])] = unlist(dat[,1])
          }
          if(any(additional != F)){
            if(any(duplicated(unlist(dat[,4])))){
              if(length(additional) == 1){
                dat_add = dat[!duplicated(dat[,4], fromLast = T),]
                X = F
              }else{
                X = T
                un = unlist(dat[,4])
                un2 = unlist(dat[,3])
                dat_add = as.data.frame(list(add = un2[!duplicated(unlist(dat[,3]), fromLast = T)], where=un[!duplicated(unlist(dat[,4]), fromLast = T)]))
              }
              if(length(dat_add) == 6){
                res[count, dat_add$where_add] = dat_add$add
              }
              if(isTRUE(X)){
                res[count, unlist(dat_add[,2])] = unlist(dat_add[,1])
              }
              if(!isTRUE(X) && length(dat_add) != 6){
                res[count, unlist(dat_add[,4])] = unlist(dat_add[,3])
              }
            }else{
              res[count, unlist(dat[,4])] = unlist(dat[,3])
            }
          }
          if(any(extraCol != F)){
            if(any(duplicated(unlist(dat[,6])))){
              # cond 1/2: take most recent value (default) or first value (first)
              if(handlingExtra == F) dat_dup = as.data.frame(list(add = dat[!duplicated(dat[,6], fromLast = T),]$extra, where=dat[!duplicated(dat[,6], fromLast = T),]$where_extra))
              if(handlingExtra == 'first') dat_dup = as.data.frame(list(add = dat[!duplicated(dat[,6]),]$extra, where=dat[!duplicated(dat[,6]),]$where_extra))
              # cond 3/4: mean value of existing values/sum of existing values
              if(handlingExtra == "sum") dat_dup = mean_sum(dat, "sum", colsProcess = 'extraCol')
              if(handlingExtra == "mean") dat_dup = mean_sum(dat, "mean", colsProcess = 'extraCol')
              # merge data back together and create the result
              if(length(dat_dup) == 6){
                res[count, unlist(dat_dup$where_extra)] = unlist(dat_dup$extra)
              }else{
                res[count, unlist(dat_dup[,2])] = as.character(unlist(dat_dup[,1]))
              }
            }else{
              res[count, unlist(dat[,6])] = unlist(dat[,5])
            }
          }
          count = count + 1
        }else{
          next;
        }
      }
      return(list(res=res))
    }
    close(pb)
    stopCluster(cl)
    res_fin = create_res(res_fin, input)
    res_fin = res_fin[order(res_fin$id, res_fin$date),]
    rownames(res_fin) = 1:nrow(res_fin)
    res_fin[3:length(res_fin)] = lapply(res_fin[3:length(res_fin)], as.factor)
    for(i in 3:ncol(res_fin)){
      if(suppressWarnings(length(is.na(as.numeric(as.character((na.omit(res_fin[,i])))))) == 0)) next;
      if(suppressWarnings(any(is.na(as.numeric(as.character((na.omit(res_fin[,i])))))))){
        res_fin[,i] = as.factor(res_fin[,i])
      }else{
        res_fin[,i] = as.numeric(as.character((res_fin[,i])))
      }
    }
    if(na.rm==T) res_fin = res_fin[complete.cases(res_fin),]
    return(res_fin)
  }else{
    pb <- timerProgressBar(min = 0, max = length(ids), style = 3)
    for(i in 1:length(ids)){
      sub = subset(input, input$id==ids[i])
      dates = unique(sub$date)
      for(k in 1:length(dates)){
        sub_data = subset(input, input$id==ids[i] & input$date==dates[k])
        if(nrow(sub_data) > 0){
          if(class(sub_data$id) == "factor"){
            res[count,1] = as.numeric(as.character(sub_data$id[1]))
          }else{
            res[count,1] = sub_data$id[1]
          }
          res[count,2] = sub_data$date[1]
          dat = do.call(rbind,apply(sub_data, 1, creation, additional=additional, cnames=cnames, d=res, extraCol=extraCol, store=c(), where_all=c(), store_add=c(), where_extra=c(), store_extra=c(), where_add = c()))
          #### conditions: procedure for multiple observations of same type at same point in time
          if(any(duplicated(dat[,2]))){
            # cond 1/2: take most recent value (default) or first value (first)
            if(handling == F) dat_dup = dat[!duplicated(dat[,2], fromLast = T),]
            if(handling == 'first') dat_dup = dat[!duplicated(dat[,2]),]
            # cond 3/4: mean value of existing values/sum of existing values
            if(handling == "sum") dat_dup = mean_sum(dat, "sum")
            if(handling == "mean") dat_dup = mean_sum(dat, "mean")
            # merge data back together and create the result
            if(length(dat_dup) == 6){
              res[count, dat_dup$where] = dat_dup$dat
            }else{
              res[count, unlist(dat_dup[,2])] = unlist(dat_dup[,1])
            }
          }else{
            res[count, unlist(dat[,2])] = unlist(dat[,1])
          }
          if(any(additional != F)){
            if(any(duplicated(unlist(dat[,4])))){
              if(length(additional) == 1){
                dat_add = dat[!duplicated(dat[,4], fromLast = T),]
                X = F
              }else{
                X = T
                un = unlist(dat[,4])
                un2 = unlist(dat[,3])
                dat_add = as.data.frame(list(add = un2[!duplicated(unlist(dat[,3]), fromLast = T)], where=un[!duplicated(unlist(dat[,4]), fromLast = T)]))
              }
              if(length(dat_add) == 6){
                res[count, dat_add$where_add] = dat_add$add
              }
              if(isTRUE(X)){
                res[count, unlist(dat_add[,2])] = unlist(dat_add[,1])
              }
              if(!isTRUE(X) && length(dat_add) != 6){
                res[count, unlist(dat_add[,4])] = unlist(dat_add[,3])
              }
            }else{
              res[count, unlist(dat[,4])] = unlist(dat[,3])
            }
          }
          if(any(extraCol != F)){
            if(any(duplicated(unlist(dat[,6])))){
              # cond 1/2: take most recent value (default) or first value (first)
              if(handlingExtra == F) dat_dup = as.data.frame(list(add = dat[!duplicated(dat[,6], fromLast = T),]$extra, where=dat[!duplicated(dat[,6], fromLast = T),]$where_extra))
              if(handlingExtra == 'first') dat_dup = as.data.frame(list(add = dat[!duplicated(dat[,6]),]$extra, where=dat[!duplicated(dat[,6]),]$where_extra))
              # cond 3/4: mean value of existing values/sum of existing values
              if(handlingExtra == "sum") dat_dup = mean_sum(dat, "sum", colsProcess = 'extraCol')
              if(handlingExtra == "mean") dat_dup = mean_sum(dat, "mean", colsProcess = 'extraCol')
              # merge data back together and create the result
              if(length(dat_dup) == 6){
                res[count, unlist(dat_dup$where_extra)] = unlist(dat_dup$extra)
              }else{
                res[count, unlist(dat_dup[,2])] = as.character(unlist(dat_dup[,1]))
              }
            }else{
              res[count, unlist(dat[,6])] = unlist(dat[,5])
            }
          }
          count = count + 1
        }else{
          next;
        }
      }
      setTimerProgressBar(pb, i)
    }
    close(pb)
    res = res[order(res$id, res$date),]
    rownames(res) = 1:nrow(res)
    res[3:length(res)] = lapply(res[3:length(res)], as.factor)
    for(i in 3:ncol(res)){
      if(suppressWarnings(length(is.na(as.numeric(as.character((na.omit(res[,i])))))) == 0)) next;
      if(suppressWarnings(any(is.na(as.numeric(as.character((na.omit(res[,i])))))))){
        res[,i] = as.factor(res[,i])
      }else{
        res[,i] = as.numeric(as.character((res[,i])))
      }
    }
    if(na.rm==T) res = res[complete.cases(res),]
    return(res)
  }
}

###
# functions necessary for feature engineering
###

# verify which variables are numeric and qualify for feature engineering
check_num_feats = function(input, count){
  if(!is.na(as.numeric(as.character(na.omit(input))))){
    count = c(count, 1)
  }
  return(count)
}

# multiply all columns with each other that are numeric
createInteractionTerms = function(input, target, no.use=F, na.rm=F){
  if(missing(target)) stop("Please provide your target variable.")
  if(length(target) > 1) stop("Please provide only one target variable.")
  if(check_type(input) == F) stop("The input is not of type 'data.frame'. \n")
  input = input[order(input$id, input$date),]
  colnames(input)[which(colnames(input) == target)] = "target"
  feats_num = suppressWarnings(pbsapply(input, check_num_feats, count=c()))
  if(any(no.use != F)){
    feats = names(unlist(feats_num))[-c(which(names(unlist(feats_num)) == 'id'), which(names(unlist(feats_num)) == 'target'), which(names(unlist(feats_num)) %in% no.use))]
  }else{
    feats = names(unlist(feats_num))[-c(which(names(unlist(feats_num)) == 'id'), which(names(unlist(feats_num)) == 'target'))]
  }
  cat("Finalizing results - please wait just one moment")
  d = lapply(input[,feats], as.character)
  d = as.data.frame(lapply(d, as.numeric))
  res = do.call(cbind,combn(colnames(d), 2, function(x) list(d[x[1]]*d[x[2]])))
  colnames(res) = combn(colnames(d), 2, paste, collapse=":")
  res = lapply(res, as.factor)
  res_fin = cbind(input, res)
  colnames(res_fin)[which(colnames(res_fin) == "target")] = target
  if(na.rm==T) res_fin = res_fin[complete.cases(res_fin),]
  return(res_fin)
}

# manipulate parallel result for time window creation
manipulateParallelResult = function(res_fin, input){
  for(i in 1:length(res_fin)){
    for(k in 1:length(res_fin[[i]])){
      if(class(input$id) == "factor"){
        ids = which(as.data.frame(res_fin[[i]][k])[,1] != as.numeric(levels(input$id)[i]))
      }else{
        ids = which(as.data.frame(res_fin[[i]][k])[,1] != unique(input$id)[i])
      }
      if(length(ids) > 0) res_fin[[i]][k] = list(res_fin[[i]][[k]][-ids,])
    }
  }
  return(res_fin)
}

# specify a specific time window and then sum/sd/min/max/mean values only within this time frame (must be faster)
TimeWindow = function(input, parallel=F, cores=F, target, no.use=F, steps=2){
  if(missing(target)) stop("Please provide your target variable.")
  if(length(target) > 1) stop("Please provide only one target variable.")
  if(check_type(input) == F) stop("The input is not of type 'data.frame'. \n")
  cat("Please wait a moment - preparing calculation")
  input = input[order(input$id, input$date),]
  colnames(input)[which(colnames(input) == target)] = "target"
  feats_num = suppressWarnings(sapply(input, check_num_feats, count=c()))
  if(any(no.use != F)){
    feats = names(unlist(feats_num))[-c(which(names(unlist(feats_num)) == 'id'), which(names(unlist(feats_num)) == 'target'), which(names(unlist(feats_num)) %in% no.use))]
  }else{
    feats = names(unlist(feats_num))[-c(which(names(unlist(feats_num)) == 'id'), which(names(unlist(feats_num)) == 'target'))]
  }
  data = lapply(input[,feats], as.character)
  data = as.data.frame(lapply(data, as.numeric))
  data = cbind(input['id'], data)
  ids = unique(data$id)
  cols_sum = c()
  cols_max = c()
  cols_min = c()
  cols_sd = c()
  cols_mean = c()
  for(i in 1:(length(colnames(data))-1)){
    # include time bar here or make faster
    cols_sum = c(cols_sum, capture.output(cat("sumSteps",steps, colnames(data)[-1][i], sep="")))
    cols_max = c(cols_max, capture.output(cat("maxSteps",steps, colnames(data)[-1][i], sep="")))
    cols_min = c(cols_min, capture.output(cat("minSteps",steps, colnames(data)[-1][i], sep="")))
    cols_sd = c(cols_sd, capture.output(cat("sdSteps",steps, colnames(data)[-1][i], sep="")))
    cols_mean = c(cols_mean, capture.output(cat("meanSteps",steps, colnames(data)[-1][i], sep="")))
  }
  sum_res = data.frame()
  max_res = data.frame()
  min_res = data.frame()
  sd_res = data.frame()
  mean_res = data.frame()
  if(parallel == F){
    pb <- timerProgressBar(min = 0, max = length(ids), style = 3)
    for(i in 1:length(ids)){
      sub = subset(data, data$id==ids[i])
      sum = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      max = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      min = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      sd = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      mean = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      if(steps < nrow(sub)){
        cutpoints = seq(1, nrow(sub), steps)
        for(k in 1:length(cutpoints)){
          if(is.na(cutpoints[k+1])) next;
          sum[cutpoints[k+1]-1,] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, sum)))
          max[cutpoints[k+1]-1,] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, max)))
          min[cutpoints[k+1]-1,] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, min)))
          sd[cutpoints[k+1]-1,] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, sd)))
          mean[cutpoints[k+1]-1,] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, mean)))
        }
      }
      sum_res = rbind(sum_res, sum)
      max_res = rbind(max_res, max)
      min_res = rbind(min_res, min)
      sd_res = rbind(sd_res, sd)
      mean_res = rbind(mean_res, mean)
      setTimerProgressBar(pb, i)
    }
  }else{
    if(cores == F) cl = makeCluster(detectCores()-1)
    if((class(cores) != "numeric" | cores > detectCores()) & cores != F){
      stop("The provided argument 'cores' is either not numeric/integer or greater than 'detectCores()'. Please provide sufficient number of cores. \n")
    }
    if(cores != F & class(cores) == "numeric" & cores <= detectCores()){
      cores = round(cores)
      cl = makeCluster(cores)
    }
    registerDoSNOW(cl)
    pb <- timerProgressBar(min = 0, max = length(ids), style = 3)
    progress <- function(n) setTimerProgressBar(pb, n)
    opts <- list(progress = progress)
    sum_res = data.frame()
    max_res = data.frame()
    min_res = data.frame()
    sd_res = data.frame()
    mean_res = data.frame()
    res_fin = foreach(i=1:length(ids), .inorder=T, .options.snow = opts) %dopar% {
      sub = subset(data, data$id==ids[i])
      sum = matrix(nrow=nrow(sub), ncol=ncol(sub))
      sum[,1] = sub$id
      max = matrix(nrow=nrow(sub), ncol=ncol(sub))
      max[,1] = sub$id
      min = matrix(nrow=nrow(sub), ncol=ncol(sub))
      min[,1] = sub$id
      sd = matrix(nrow=nrow(sub), ncol=ncol(sub))
      sd[,1] = sub$id
      mean = matrix(nrow=nrow(sub), ncol=ncol(sub))
      mean[,1] = sub$id
      if(steps < nrow(sub)){
        cutpoints = seq(1, nrow(sub), steps)
        for(k in 1:length(cutpoints)){
          if(is.na(cutpoints[k+1])) next;
          sum[cutpoints[k+1]-1,2:ncol(sum)] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, sum)))
          max[cutpoints[k+1]-1,2:ncol(max)] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, max)))
          min[cutpoints[k+1]-1,2:ncol(min)] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, min)))
          sd[cutpoints[k+1]-1,2:ncol(sd)] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, sd)))
          mean[cutpoints[k+1]-1,2:ncol(mean)] = do.call(rbind,list(apply(sub[cutpoints[k]:(cutpoints[k+1]-1),-1], 2, mean)))
        }
      }
      sum_res = rbind(sum_res, sum)
      max_res = rbind(max_res, max)
      min_res = rbind(min_res, min)
      sd_res = rbind(sd_res, sd)
      mean_res = rbind(mean_res, mean)
      return(list(sum_res, max_res, min_res, sd_res, mean_res))
    }
    close(pb)
    stopCluster(cl)
    res_fin = manipulateParallelResult(res_fin, input)
    sum_res = data.frame()
    max_res = data.frame()
    min_res = data.frame()
    sd_res = data.frame()
    mean_res = data.frame()
    cat("Verifying results - Please wait just one moment.")
    for(i in 1:length(res_fin)){
      sum_res = rbind(sum_res, do.call("rbind", res_fin[[i]][1])[-1])
      max_res = rbind(max_res, do.call("rbind", res_fin[[i]][2])[-1])
      min_res = rbind(min_res, do.call("rbind", res_fin[[i]][3])[-1])
      sd_res = rbind(sd_res, do.call("rbind", res_fin[[i]][4])[-1])
      mean_res = rbind(mean_res, do.call("rbind", res_fin[[i]][5])[-1])
    }
  }
  # change colnames
  colnames(sum_res) = cols_sum
  colnames(min_res) = cols_min
  colnames(max_res) = cols_max
  colnames(sd_res) = cols_sd
  colnames(mean_res) = cols_mean
  res = cbind(sum_res, min_res, max_res, sd_res, mean_res)
  # convert to factor, cbind the real input, and change name back to real target name
  res = lapply(res, as.factor)
  res_fin = cbind(input, res)
  colnames(res_fin)[which(colnames(res_fin) == "target")] = target
  return(res_fin)
}

# binning for a chosen variable
binning = function(input, target=F, interval=F){
  if(check_type(input) == F) stop("The input is not of type 'data.frame'. \n")
  if(target == F) stop("Please provide a target variable for binning.")
  if(any(is.na(as.numeric(as.character(na.omit(input[,target])))))) stop("This target cannot be numeric. \n")
  if(interval == F) stop("Please provide a numeric interval for the target variable.")
  if(is.numeric(interval) == F) stop("Please provide a numeric value for the interval parameter.")
  col = which(colnames(input) == target)
  if(is.factor(input[,col])){
    bin_var = as.numeric(as.character(input[,col]))
  }else{
    bin_var = input[,col]
  }
  bins = cut(bin_var, interval)
  res_fin = cbind(input, bins)
  colnames(res_fin) = c(colnames(input), capture.output(cat("bin_", target, sep="")))
  return(res_fin)
}

###
# functions necessary for analysis
###
getModelString = function(input, target, exFeat){
  if(is.null(exFeat)){
    del = c('id', 'date', target)
  }else{
    del = c('id', 'date', exFeat, target)
  }
  vars = names(input[!names(input) %in% del])
  vars = paste(paste(sprintf("`%s`", vars), collapse="+"))
  formula = paste(target,vars,sep="~")
  return(formula)
}

# checks for whole number (integer)
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# searches for best svm cost and gamma parameters on a grid
getBestSVM =function(input, model_str,target, typeSVM, task){
  ikx = which(colnames(input)==target)
  costStepSize  = 10^seq(-2,4,0.3)
  gammaStepSize = 2^(-2:2)
  modelmatrix = sparse.model.matrix(formula(model_str),input)
  if(task == "classification") obj_radial = tune.svm(x=modelmatrix,y=as.factor(input[,ikx]),cost= costStepSize,gamma=gammaStepSize,kernel='radial',type=typeSVM,cross =2)
  if(task == "regression") obj_radial = tune.svm(x=modelmatrix,y=input[,ikx],cost= costStepSize,gamma=gammaStepSize,kernel='radial',type=typeSVM,cross =2)
  svm_kernel = 'radial'
  svm_gamma =  obj_radial$best.parameters[1]
  svm_costs = obj_radial$best.parameters[2]
  list('svm_kernel'=  svm_kernel,'svm_gamma'=svm_gamma,'svm_costs'=svm_costs)
}

# grid search for xgboost
getBestXgboost = function(mat, train, task, target){

  xgb_grid = expand.grid(nrounds = 1000, # maximum number of iterations
                         eta = c(0.1, 0.01, 0.0001), # learning rate
                         max_depth = c(2,6,10), # depth of tree
                         gamma = c(0,1), # regularizer
                         colsample_bytree = c(0.4,0.8), # number of features supplied to the tree
                         min_child_weight = c(1,20), # blocks the potential feature interactions to prevent overfitting
                         subsample = 1) # controls the number of features (variables) supplied to a tree

  if(task == "classification"){
    y=as.factor(train[,which(colnames(train) == target)])
    levels(y) = c("no", "yes")
    metric = "Accuracy"
    classProb = T
  }
  if(task == "regression"){
    y=train[,which(colnames(train) == target)]
    metric = "RMSE"
    classProb = F
  }

  cv_ctrl = trainControl(method = "repeatedcv", repeats = 1,number = 2,
                         classProbs = classProb,
                         allowParallel=T)

  xgb_tune = caret::train(x=mat,
                          y=y,
                          method="xgbTree",
                          trControl=cv_ctrl,
                          tuneGrid=xgb_grid,
                          metric=metric,
                          verbose=T)
  return(xgb_tune$bestTune)
}

# function for assessing the percentage of missing data
pMiss = function(x){sum(is.na(x))/length(x)*100}

# imputation function
imputeData = function(input, perc=F, missing, target, percEx){
  if(perc != F && is.numeric(perc) == F) stop("Please provide numeric value for percentage.")
  if(perc != F){
    if(percEx !=F){
      abovePerc = which(apply(input,2,pMiss) > perc)
      abovePerc = abovePerc[-which(names(abovePerc) %in% percEx)]
      input = input[,-abovePerc]
    }else{
      input = input[,-which(apply(input,2,pMiss) > perc)]
    }
  }
  if(missing == F || missing == 'median/mode'){
    input_impute = impute(input[,-which(colnames(input) %in% c("id", "date", target))], method = "median/mode", object = NULL)
    output = cbind(input[,which(colnames(input) %in% c("id", "date", target))], input_impute)
  }
  if(missing == 'knn'){
    input_impute = kNN(input, variable = colnames(input)[!(colnames(input) %in% c("id", "date", target))])
    output = input_impute[,1:ncol(input)]
  }
  output = output[complete.cases(output), ]
  return(output)
}

# performs k-fold cross validation using multiple machine learning techniques for comparison
analyzeUJ = function(input, target=F, type='cont', firstFeats=F, lastFeats=F, sumFeats = F, method='all', PCA=F, PCAOnly=F, comps=0, task='regression', interval=F, crossValidation=T, split=70, folds=10, optPara=F, missing=F,imp=F, perc=F, percEx=F, exFeat=NULL, ROC=F, proba=.5, scale=T){

  ######
  ## prepare data
  ######
  cat("Verifying and preparing data. \n")
  if(!(task %in% c('classification','regression'))) stop("Please provide correct task. \n")
  if(any(sumFeats != F) && any(!(sumFeats %in% colnames(input)))) stop("A specified feature does not exist as column in input data.")
  if(proba > 1 || proba < 0 || is.numeric(proba) == F) stop("Wrong definition of parameter proba. Please verify.")
  if(any(firstFeats !=F) && any(!(firstFeats %in% colnames(input)))) stop("A specified feature does not exist as column in input data.")
  if(any(lastFeats != F) && any(!(lastFeats %in% colnames(input)))) stop("A specified feature does not exist as column in input data.")
  if(missing != F && !(missing %in% c("median/mode","knn"))) stop("Please provide appropriate value for missing. \n")
  if(target == F) stop("Please provide your target variable.")
  if(imp != F && imp != T) stop("Please provide logical value True or False for the parameter imp.")
  if(!(is.numeric(split)) || split > 100 || split < 0) stop('The parameter split is not specified correctly.')
  if(!any(colnames(input) %in% target)) stop("Cannot find target variable in input.")
  if(task == "classification" && length(unique(input[,target])) != 2) stop("Currently, there is no multi-class classification available. Check your target variable.")
  if(check_type(input) == F) stop("The input is not of type 'data.frame'.")
  if(comps < 0 || !(is.numeric(comps))) stop("The parameter comps is not specified correctly.")
  if(!is.numeric(folds)) stop("The parameter 'folds' is not numeric. Provide appropriate parameter value.")
  if((!(isTRUE(PCA)) && !(isFALSE(PCA))) || (!(isTRUE(PCAOnly)) && !(isFALSE(PCAOnly)))) stop("Parameter PCA or PCAOnly are not set correctly.")
  if(percEx != F && any(!(percEx %in% colnames(input)))) stop("Specified features for exclusion of deletion depending on missing data does not exist in provided data.")
  # check for selected type of analysis
  if(!(type %in% c('cont', 'aggregate'))) stop("Please provide valid input for parameter 'type'.")
  # check exFeat
  if(!is.null(exFeat)){
    if(!(exFeat %in% colnames(input))) stop("Feature to be excluded does not exist as column name in input data.")
    exFeat = gsub(' |-', '', exFeat)
  }

  ################################################################################################
  # order input
  input = input[order(input$id, input$date),]
  # remove dashes and spaces from colnames
  colnames(input) = gsub(' |-', '', colnames(input))
  # cut input if interval provided
  if(interval != F){
    if(!(is.wholenumber(interval))) stop('Please specify correct interval. \n')
    new = as.data.frame(matrix(nrow=0, ncol=ncol(input)))
    colnames(new) = colnames(input)
    for(i in 1:length(unique(input$id))){
      sub = subset(input, input$id==unique(input$id)[i])
      sub$date = as.Date(sub$date, origin="1970-01-01")
      idx = which(sub$date >= sub$date[1] + interval)[1]
      if(is.na(idx)) next;
      new = rbind(new, sub[1:idx,])
    }
    input = new
  }
  # delete categorical variables with not enough levels
  idx = colnames(input)[-which(colnames(input) %in% c("id","date"))]
  levs = lapply(lapply(input[,idx], unique), function(x) length(x[!is.na(x)]) > 1)
  if(any(levs == F)){
    input = input[, !(colnames(input) %in% names(which(levs == F)))]
    #input = input[, !(colnames(input) %in% idx[which(levs == F)])]
  }
  # if aggregate, transform input
  if(type == 'aggregate'){
    # check if only one value of target for each user
    if(any(aggregate(input[target], by=list(input$id), function(x) length(unique(length(x))))[,2]) > 1) stop("There are different values in target for some points in time for an id. Not possible when specifying aggregate.")
    # modify according to colnames
    if(any(lastFeats != F)) lastFeats = gsub(' |-', '', lastFeats)
    if(any(firstFeats != F)) firstFeats = gsub(' |-', '', firstFeats)
    if(any(sumFeats != F)) sumFeats = gsub(' |-', '', sumFeats)
    # check if no categorical variables are defined for sumFeats
    if(any(sapply(input[,which(colnames(input) %in% sumFeats)], is.numeric)) ==F && sumFeats != F) stop("Categorical features were specified in sumFeats. Not possible.")
    # delete columns corresponding to amount of missing values
    if(perc != F && is.numeric(perc) == F) stop("Please provide numeric value for percentage.")
    if(perc != F){
      if(any(percEx !=F)){
        abovePerc = which(apply(input,2,pMiss) > perc)
        abovePerc = abovePerc[-which(names(abovePerc) %in% percEx)]
        input = input[,-abovePerc]
      }else{
        input = input[,-which(apply(input,2,pMiss) > perc)]
      }
    }
    targetVar = input[!duplicated(input$id),target]
    input = input[,-which(colnames(input) %in% c('date',target))]
    # check if defined and where
    indicator = which(sapply(list(lastFeats, firstFeats, sumFeats), function(x) !(F %in% x)))
    # aggregate data
    input_agg = cbind(aggregate(input[,sapply(input, is.numeric)], by=list(input$id), mean, na.rm=T)[,-1], aggregate(input[,sapply(input, is.factor)], by=list(input$id), Mode)[,-1])
    # include aggregation parameters if set
    if(length(indicator) > 0){
      for(i in 1:length(unique(input$id))){
        sub = subset(input, input$id==unique(input$id)[i])
        if(1 %in% indicator){
          last = which(colnames(input) %in% lastFeats)
          if(length(last) > 1){
            for(k in 1:length(last)){
              if(length(tail(na.omit(sub[,last[k]]),1)) > 0) input_agg[i, colnames(sub)[last[k]]] = tail(na.omit(sub[,last[k]]),1)
            }
          }
        }
        if(2 %in% indicator){
          first = which(colnames(sub) %in% firstFeats)
          if(length(first > 0)){
            for(k in 1:length(first)){
              if(length(tail(na.omit(sub[,first[k]]),1)) > 0) input_agg[[i, colnames(sub)[first[k]]]] = head(na.omit(sub[,first[k]]),1)
            }
          }
        }
        if(3 %in% indicator){
          sum = which(colnames(input) %in% sumFeats)
          if(length(sum) > 0){
            for(k in 1:length(sum)){
              if(length(tail(na.omit(sub[,sum[k]]),1)) > 0) input_agg[[i, colnames(sub)[sum[k]]]] = sum(sub[,sum[k]], na.rm=T)
            }
          }
        }
      }
    }
    input = input_agg
    input[target] = targetVar
    # delete categorical variables with not enough levels again (can happen because of aggregation)
    idx = colnames(input)[-which(colnames(input) %in% c("id","date"))]
    levs = lapply(lapply(input[,idx], unique), function(x) length(x[!is.na(x)]) > 1)
    if(any(levs == F)){
      input = input[, !(colnames(input) %in% names(which(levs == F)))]
    }
  }else{
    if(perc != F && is.numeric(perc) == F) stop("Please provide numeric value for percentage.")
    if(perc != F){
      if(any(percEx !=F)){
        abovePerc = which(apply(input,2,pMiss) > perc)
        abovePerc = abovePerc[-which(names(abovePerc) %in% percEx)]
        input = input[,-abovePerc]
      }else{
        input = input[,-which(apply(input,2,pMiss) > perc)]
      }
    }
  }
  if((isTRUE(missing) || any(is.na(input))) && imp == F) input = imputeData(input, perc=F, missing, target, percEx)
  # set classification or regression types
  if(task == 'classification'){
    typeSVM = 'C-classification'
    typeLASSO = 'binomial'
    typeTree = 'binary:logistic'

  }
  if(task == 'regression'){
    typeSVM = 'eps-regression'
    typeLASSO = 'gaussian'
    typeTree = 'reg:linear'
  }

  ######
  ## analysis
  ######

  # create folds for cross-validation
  if(isTRUE(crossValidation)){
    fold = createFolds(factor(input[,target]), k = folds, list = TRUE)
  }else{
    fold = 1
    folds = 1
    indexes = sample(1:nrow(input), size=(split/100)*nrow(input))
    test = input[-indexes,]
    train = input[indexes,]
  }
  # create model string
  modelString = getModelString(input, target, exFeat)
  res = list()
  # application of ML techniques
  cat("Executing specified methods: \n")
  pb <- timerProgressBar(min = 0, max = length(fold), style = 3)
  for(i in 1:length(fold)){
    if(isTRUE(crossValidation)){
      train = input[-fold[[i]],]
      test = input[fold[[i]],]
    }
    if((isTRUE(missing) || any(is.na(input))) && isTRUE(imp)){
      modelString = getModelString(input, target, exFeat)
      if(any(is.na(train))) train = imputeData(train, perc=F, missing, target, percEx)
      if(any(is.na(test))) test = imputeData(test, perc=F, missing, target, percEx)
      idx_train = colnames(train)[-which(colnames(train) %in% c("id","date"))]
      idx_test = colnames(test)[-which(colnames(test) %in% c("id","date"))]
      levs_train = lapply(lapply(train[,idx_train], unique), function(x) length(x[!is.na(x)]) > 1)
      levs_test = lapply(lapply(test[,idx_test], unique), function(x) length(x[!is.na(x)]) > 1)
      levs = append(levs_test, levs_train)
      if(target %in% names(levs)) levs = levs[-which(names(levs) == target)]
      if(any(levs == F)){
        train = train[, !(colnames(train) %in% names(which(levs == F)))]
        test = test[, !(colnames(test) %in% names(which(levs == F)))]
        modelString = getModelString(train, target, exFeat)
      }
      # adjust factor levels
      if(any(sapply(input, is.factor))){
        f = names(train[names(which(sapply(train, is.factor)))])
        for(j in 1:length(f)){
          levels(train[,f[j]]) = c(levels(train[,f[j]]), levels(test[,f[j]]))
          levels(test[,f[j]]) = c(levels(test[,f[j]]), levels(train[,f[j]]))
        }
      }
    }

    res$obs = c(res$obs, test[,target])

    # PCA
    if(PCA == T){
      resPCA = prcomp(train[,sapply(train, is.numeric)][,-which(names(train) %in% c('id', target))], scale=T)
      std_dev <- resPCA$sdev
      pr_var <- std_dev^2
      prop_varex <- pr_var/sum(pr_var)
      # if comps not given, choose as many components that explain 99% of the variance
      if(comps == 0) comps = which(cumsum(prop_varex)>=0.99)[1]
      PCAtrain = resPCA$x[,1:comps]
      PCAtest = predict(resPCA, newdata = test)[,1:comps]
      if(PCAOnly == T){
        train = cbind(train['id'], train[target], PCAtrain)
        test = cbind(test['id'], test[target], PCAtest)
      }
      if(PCAOnly == F){
        train = cbind(train['id'], train[target], train[,-which(names(train) %in% names(train[,sapply(train, is.numeric)]))], PCAtrain)
        test = cbind(test['id'], test[target], test[,-which(names(test) %in% names(test[,sapply(test, is.numeric)]))], PCAtest)
      }
      modelString = getModelString(train, target, exFeat)
    }

    # verify optPara
    if(optPara==T){
      if(method == 'all' || 'svm' %in% method){
        svm_parameters = getBestSVM(train, modelString, target, typeSVM, task=task)
      }

      if(method == 'all' || 'treeBoost' %in% method){
        mod.matrix = model.matrix(formula(modelString),rbind(train, test))
        mat = mod.matrix[1:nrow(train),]
        xgboost_parameters = getBestXgboost(mat, train, task=task, target=target)
      }
    }

    ######## Analysis

    # all/svm
    if(method == 'all' || 'svm' %in% method){
      if(task == "regression"){
        if(optPara == T){
          fit_svm = svm(formula = formula(modelString), data = train, type=typeSVM,gamma=svm_parameters$svm_gamma[[1]],costs =svm_parameters$svm_costs[[1]], scale=scale)
        }else{
          fit_svm = svm(formula = formula(modelString), data = train, scale=scale, type=typeSVM)
        }
        res$pred$pred_svm = predict(fit_svm, test)
      }
      if(task == 'classification'){
        if(optPara == T){
          fit_svm = svm(formula = formula(modelString), data = train, type=typeSVM,gamma=svm_parameters$svm_gamma[[1]],costs =svm_parameters$svm_costs[[1]], scale=scale, probability=T)
        }else{
          fit_svm = svm(formula = formula(modelString), data = train, scale=scale, type=typeSVM, probability=T)
        }
        res$perf_svm[[i]] = attr(predict(fit_svm, test, probability=T),"probabilities")[,1]
        res$pred$pred_svm = ifelse(res$perf_svm[[i]] >= proba, 1, 0)
      }
    }

    # all/lm
    if((method == 'all' || 'lm' %in% method) && task == 'regression'){
      if(nrow(input) < ncol(input)){
        res$pred$pred_lm = 'The data has more features than observations. Linear regression is not meaningful.'
      }else{
        fit_lm = glm(formula=formula(modelString), data = train)
        res$pred$pred_lm = predict(fit_lm, test)
      }
    }else{
      res$pred$pred_lm = 'Method not executed. Task is not regression, not possible, or not specified.'
    }

    # all/lasso
    if(method == 'all' || 'lasso' %in% method){
      mod.matrix = sparse.model.matrix(formula(modelString),rbind(train, test))
      mat = mod.matrix[1:nrow(train),]
      mat_test = mod.matrix[(nrow(train)+1):nrow(mod.matrix),]
      fit_lasso = cv.glmnet(mat, y = train[,target], alpha=1, family=typeLASSO, standardize=scale, lambda=10^seq(10,-2,length=1000))

      if(task == "regression") res$pred$pred_lasso = predict(fit_lasso, mat_test, type="link", s='lambda.min')

      if(task == 'classification'){
        res$perf_lasso[[i]] = predict(fit_lasso, type="response", mat_test, s="lambda.min")
        res$pred$pred_lasso = ifelse(res$perf_lasso[[i]] >= proba, 1, 0)
      }
    }

    # all/ridge
    if(method == 'all' || 'ridge' %in% method){
      mod.matrix = sparse.model.matrix(formula(modelString),rbind(train, test))
      mat = mod.matrix[1:nrow(train),]
      mat_test = mod.matrix[(nrow(train)+1):nrow(mod.matrix),]
      fit_ridge = cv.glmnet(mat, y = train[,target], alpha=0, standardize=scale, family=typeLASSO, lambda=10^seq(10,-2,length=1000))

      if(task == "regression") res$pred$pred_ridge = predict(fit_ridge, mat_test, type="link",s='lambda.min')

      if(task == 'classification'){
        res$perf_ridge[[i]] = predict(fit_ridge, type="response", mat_test, s='lambda.min')
        res$pred$pred_ridge = ifelse(res$perf_ridge[[i]] >= proba, 1, 0)
      }
    }

    # all/boost
    if(method == 'all' || 'treeBoost' %in% method){
      if(optPara==T){
        mod.matrix = sparse.model.matrix(formula(modelString),rbind(train, test))
        mat = mod.matrix[1:nrow(train),]
        mat_test = mod.matrix[(nrow(train)+1):nrow(mod.matrix),]
        fit = xgboost(data = mat, label=train[,which(colnames(train) == target)], nrounds = 1000, verbose=0, max.depth=as.numeric(xgboost_parameters[2]),
                      eta=as.numeric(xgboost_parameters[3]),gamma=as.numeric(xgboost_parameters[4]),colsample_bytree=as.numeric(xgboost_parameters[5]),
                      min_child_weight=as.numeric(xgboost_parameters[6]), subsample=as.numeric(xgboost_parameters[7]),objective = typeTree)
      }else{
        mod.matrix = sparse.model.matrix(formula(modelString),rbind(train, test))
        mat = mod.matrix[1:nrow(train),]
        mat_test = mod.matrix[(nrow(train)+1):nrow(mod.matrix),]
        fit = xgboost(data = mat, label=train[,which(colnames(train) == target)], nrounds = 1000, verbose=0, max.epth=3, objective = typeTree)
      }
      if(task == "regression") res$pred$pred_boost = predict(fit, mat_test, outputmargin=F)
      if(task == "classification"){
        res$perf_boost[[i]] = predict(fit, mat_test, outputmargin=F)
        res$pred$pred_boost = ifelse(res$perf_boost[[i]] >= proba, 1, 0)
      }
    }


    # all/log reg
    if((method == 'all' || 'log' %in% method) && task == 'classification'){
      if(nrow(input) < ncol(input)){
        res$pred$pred_log = 'The data has more features than observations. Logistic regression is not meaningful.'
      }else{
        fit_log = glm(formula=formula(modelString), data = train, family = binomial(link = "logit"))
        res$perf_log[[i]] = predict(fit_log, test, type="response")
        res$pred$pred_log = ifelse(res$perf_log[[i]] >= proba, 1, 0)
      }
    }else{
      res$pred$pred_log = 'Method not executed. Task is not classification, not possible, or not specified.'
    }

    # mean model
    mean_model = rep(mean(train[,target]), nrow(test))

    # predictions
    res$predSVM = c(res$predSVM, as.numeric(as.character(res$pred$pred_svm)))
    if(is.character(res$pred$pred_lm)){
      res$predLM = c(res$predLM, res$pred$pred_lm)
    }else{
      res$predLM = c(res$predLM, as.numeric(res$pred$pred_lm))
    }
    if(is.character(res$pred$pred_log)){
      res$predLOG = c(res$predLOG, res$pred$pred_log)
    }else{
      res$predLOG = c(res$predLOG, as.numeric(res$pred$pred_log))
    }
    res$predMean = c(res$predMean, mean_model)

    res$predBoost = c(res$predBoost, as.numeric(as.character(res$pred$pred_boost)))
    res$predLasso = c(res$predLasso, as.numeric(as.character(res$pred$pred_lasso)))
    res$predRidge = c(res$predRidge, as.numeric(as.character(res$pred$pred_ridge)))

    setTimerProgressBar(pb, i)

    # execute ML based on all data for inferential outcomes
    if(i == max(folds)){
      if((isTRUE(missing) || any(is.na(input))) && isTRUE(imp)) input = imputeData(input, perc=F, missing, target, percEx)
      inputUsed = input

      ########

      # PCA
      if(PCA == T){
        resPCA = prcomp(input[,sapply(input, is.numeric)][,-which(names(input) %in% c('id', target))], scale=T)
        std_dev <- resPCA$sdev
        pr_var <- std_dev^2
        prop_varex <- pr_var/sum(pr_var)
        # if comps not given, choose as many components that explain 99% of the variance
        if(comps == 0) comps = which(cumsum(prop_varex)>=0.99)[1]
        PCAinput = resPCA$x[,1:comps]
        colnames(PCAinput) = paste("PCA", rownames(resPCA$rotation)[1:comps], sep="_")

        if(PCAOnly == T){
          input = cbind(input['id'], input[target], PCAinput)
        }
        if(PCAOnly == F){
          input = cbind(input['id'], input[target], input[,-which(names(input) %in% names(input[,sapply(input, is.numeric)]))], PCAinput)
        }
        modelString = getModelString(input, target, exFeat)
      }

      # all/svm
      if(method == 'all' || 'svm' %in% method){
        if(optPara == T){
          svm_parameters = getBestSVM(input, modelString, target, typeSVM, task=task)
          res$fit$fit_svm = svm(formula = formula(modelString), data = input, type=typeSVM,gamma=svm_parameters$svm_gamma[[1]],costs =svm_parameters$svm_costs[[1]], scale=scale)
        }else{
          res$fit$fit_svm = svm(formula = formula(modelString), data = input, scale=scale, type=typeSVM)
        }
      }

      # all/lm
      if((method == 'all' || 'lm' %in% method) && task == 'regression'){
        if(nrow(input) < ncol(input)){
          res$fit$fit_lm = 'The data has more features than observations. Linear regression is not meaningful.'
        }else{
          res$fit$fit_lm = glm(formula=formula(modelString), data = input)
        }
      }else{
        res$fit$fit_lm = 'Method not executed. Task is not regression, not possible, or not specified.'
      }

      # all/lasso
      if(method == 'all' || 'lasso' %in% method){
        mat = sparse.model.matrix(formula(modelString),input)
        res$fit$fit_lasso = cv.glmnet(mat, y = input[,target], alpha=1, family=typeLASSO, standardize=scale)
      }

      # all/ridge
      if(method == 'all' || 'ridge' %in% method){
        mat = sparse.model.matrix(formula(modelString),input)
        res$fit$fit_ridge = cv.glmnet(mat, y = input[,target], alpha=0, family=typeLASSO, standardize=scale)
      }

      # all/boost
      if(method == 'all' || 'treeBoost' %in% method){
        if(optPara==T){
          mat = model.matrix(formula(modelString),input)
          xgboost_parameters = getBestXgboost(mat, train=input, task=task, target=target)
          res$fit$fit_boost = xgboost(data = mat, label=input[,which(colnames(input) == target)], nrounds = 1000, verbose=0, max.depth=as.numeric(xgboost_parameters[2]),
                                      eta=as.numeric(xgboost_parameters[3]),gamma=as.numeric(xgboost_parameters[4]),colsample_bytree=as.numeric(xgboost_parameters[5]),
                                      min_child_weight=as.numeric(xgboost_parameters[6]), subsample=as.numeric(xgboost_parameters[7]),objective = typeTree)
        }else{
          mat = sparse.model.matrix(formula(modelString),input)
          dat_xg = xgb.DMatrix(data = mat, label=input[,which(colnames(input) == target)])
          res$fit$fit_boost = xgboost(data = dat_xg, nrounds = 1000, verbose=0, max.epth=3, objective = typeTree)
        }
      }

      # all/log reg
      if((method == 'all' || 'log' %in% method) && task == 'classification'){
        if(nrow(input) < ncol(input)){
          res$fit$fit_log = 'The data has more features than observations. Logistic regression is not meaningful.'
        }else{
          res$fit$fit_log = glm(formula=formula(modelString), data = input, family = binomial(link = "logit"))
        }
      }else{
        res$fit$fit_log = 'Method not executed. Task is not classification, not possible, or not specified.'
      }
    }
  }

  # create performance measures based on task
  if(task == 'regression'){
    # performance measures
    if(length(res$predSVM) > 0){
      res$maeSVM = mae(res$predSVM, res$obs)
      res$rmseSVM = rmse(res$predSVM, res$obs)
    }else{
      res$maeSVM = NA
      res$rmseSVM = NA
    }
    if(length(res$predLM) > 0 && (is.character(res$predLM) == F)){
      res$maeLM = mae(res$predLM, res$obs)
      res$rmseLM = rmse(res$predLM, res$obs)
    }else{
      res$maeLM = NA
      res$rmseLM = NA
    }
    if(length(res$predMean) > 0){
      res$maeMean = mae(res$predMean, res$obs)
      res$rmseMean = rmse(res$predMean, res$obs)
    }else{
      res$maeMean = NA
      res$rmseMean = NA
    }
    if(length(res$predLasso) > 0){
      res$maeLasso = mae(as.numeric(res$predLasso), res$obs)
      res$rmseLasso = rmse(as.numeric(res$predLasso), res$obs)
    }else{
      res$maeLasso = NA
      res$rmseLasso = NA
    }
    if(length(res$predRidge) > 0){
      res$maeRidge = mae(as.numeric(res$predRidge), res$obs)
      res$rmseRidge = rmse(as.numeric(res$predRidge), res$obs)
    }else{
      res$maeRidge = NA
      res$rmseRidge = NA
    }
    if(length(res$predBoost) > 0){
      res$maeBoost = mae(as.numeric(res$predBoost), res$obs)
      res$rmseBoost = rmse(as.numeric(res$predBoost), res$obs)
    }else{
      res$maeBoost = NA
      res$rmseBoost = NA
    }
    # create performance table
    perf = data.frame(matrix(ncol=2, nrow=6))
    colnames(perf) = c('MAE', 'RMSE')
    rownames(perf) = c('LM', 'SVM', 'LASSO','RIDGE','TreeBoost','MEAN')
    perf['MAE'] = c(res$maeLM, res$maeSVM, res$maeLasso, res$maeRidge, res$maeBoost,res$maeMean)
    perf['RMSE'] = c(res$rmseLM, res$rmseSVM, res$rmseLasso, res$rmseRidge,res$rmseBoost, res$rmseMean)
  }

  if(task == 'classification'){
    if(length(res$predSVM) > 0){
      res$confSVM = suppressWarnings(confusionMatrix(as.factor(res$predSVM), as.factor(res$obs)))
    }else{
      res$confSVM = 'Not specified or not possible.'
    }
    if(length(res$predLOG) > 0 && (is.character(res$predLOG) == F)){
      res$confLOG = suppressWarnings(confusionMatrix(as.factor(res$predLOG), as.factor(res$obs)))
    }else{
      res$confLOG = 'Not specified or not possible.'
    }
    if(length(res$predLasso) > 0){
      res$confLasso = suppressWarnings(confusionMatrix(as.factor(res$predLasso), as.factor(res$obs)))
    }else{
      res$confLasso = 'Not specified or not possible.'
    }
    if(length(res$predBoost) > 0){
      res$confBoost = suppressWarnings(confusionMatrix(as.factor(res$predBoost), as.factor(res$obs)))
    }else{
      res$confBoost = 'Not specified or not possible.'
    }
    if(length(res$predRidge) > 0){
      res$confRidge = suppressWarnings(confusionMatrix(as.factor(res$predRidge), as.factor(res$obs)))
    }else{
      res$confRidge = 'Not specified or not possible.'
    }
    perf=list(ConfusionMatrixLOG = res$confLOG, ConfusionMatrixTreeBoost = res$confBoost,
              ConfusionMatrixSVM = res$confSVM, ConfusionMatrixLasso = res$confLasso,
              ConfusionMatrixRidge = res$confRidge)

    if(ROC==T && length(unique(input[,target])) == 2 && length(res$perf_lasso) > 0){
      perf_lasso = prediction(unlist(res$perf_lasso), res$obs)
      ROCLasso = performance(perf_lasso,"tpr","fpr")
      AUCLasso = performance(perf_lasso,"auc")
      perf[['ROCLasso']] = ROCLasso
      perf[['AUCLasso']] = AUCLasso
    }
    if(ROC==T && length(unique(input[,target])) == 2 && length(res$perf_ridge) > 0){
      perf_ridge = prediction(unlist(res$perf_ridge), res$obs)
      ROCRidge = performance(perf_ridge,"tpr","fpr")
      AUCRidge = performance(perf_ridge,"auc")
      perf[['ROCRidge']] = ROCRidge
      perf[['AUCRidge']] = AUCRidge
    }
    if(ROC==T && length(unique(input[,target])) == 2 && length(res$perf_log) > 0){
      perf_log = prediction(unlist(res$perf_log), res$obs)
      ROCLog = performance(perf_log,"tpr","fpr")
      AUCLog = performance(perf_log,"auc")
      perf[['ROCLog']] = ROCLog
      perf[['AUCLog']] = AUCLog
    }
    if(ROC==T && length(unique(input[,target])) == 2 && length(res$perf_svm) > 0){
      perf_svm = prediction(unlist(res$perf_svm), res$obs)
      ROCSVM = performance(perf_svm,"tpr","fpr")
      AUCSVM = performance(perf_svm,"auc")
      perf[['ROCSVM']] = ROCSVM
      perf[['AUCSVM']] = AUCSVM
    }
    if(ROC==T && length(unique(input[,target])) == 2 && length(res$perf_boost) > 0){
      perf_boost = prediction(unlist(res$perf_boost), res$obs)
      ROCTreeBoost = performance(perf_boost,"tpr","fpr")
      AUCTreeBoost = performance(perf_boost,"auc")
      perf[['ROCTreeBoost']] = ROCTreeBoost
      perf[['AUCTreeBoost']] = AUCTreeBoost
    }
  }

  # if no cross-validation, set fold to the training index
  if(!(isTRUE(crossValidation))) fold = indexes
  # create results
  TargetPreds=list(SVM=res$predSVM, LM=res$predLM, Lasso=res$predLasso,Ridge=res$predRidge,Tree=res$predBoost,LOG=res$predLOG,Mean=res$predMean)
  if(task == "regression") TargetPredictions = TargetPreds
  if(task == "classification"){
    if("Method not executed. Task is not classification, not possible, or not specified." %in% res$predLOG) res$perf_log = res$predLOG
    TargetPredictions = list()
    TargetPredictions$probs = list(SVM=unlist(res$perf_svm), LM=res$predLM, Lasso=unlist(res$perf_lasso),Ridge=unlist(res$perf_ridge),Tree=unlist(res$perf_boost),LOG=unlist(res$perf_log))
    TargetPredictions$preds = TargetPreds[-which(names(TargetPreds) == "Mean")]
  }
  result = list(input=inputUsed, fold=fold, TargetObservations=res$obs,
                TargetPredictions=TargetPredictions,
                performance=perf, results=list(LM=res$fit$fit_lm, Log=res$fit$fit_log, SVM=res$fit$fit_svm, Lasso=res$fit$fit_lasso, Ridge=res$fit$fit_ridge, Tree=res$fit$fit_boost))
  return(result)
}
