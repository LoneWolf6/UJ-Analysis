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
  if(na.rm) res_fin = res_fin[complete.cases(res_fin),]
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
    for(i in seq_along(ids)){
      sub = subset(data, data$id==ids[i])
      sum = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      max = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      min = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      sd = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      mean = matrix(nrow=nrow(sub), ncol=ncol(sub)-1)
      if(steps < nrow(sub)){
        cutpoints = seq(1, nrow(sub), steps)
        for(k in seq_along(cutpoints)){
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
        for(k in seq_along(cutpoints)){
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