# function for the aggregation of the observations
aggCreationSum = function(x){
  if(any(is.na(suppressWarnings(as.numeric(as.character(x)))))){
    x = paste(tail(x,1), collapse="")
  }else{
    x = sum(as.numeric(as.character(x)))
  }
  return(paste(x))
}

# function for the aggregation of the observations
aggCreationMean = function(x){
  if(any(is.na(suppressWarnings(as.numeric(as.character(x)))))){
    x = paste(tail(x,1), collapse="")
  }else{
    x = mean(as.numeric(as.character(x)))
  }
  return(paste(x))
}

# calculates the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# creates the UJ
reshapeData = function(input, additional = F, extraCol = F, handling = F, handlingExtra = F, na.rm = F){
  # check status of input
  if(check_colnames(input) == F) stop("The data does not have the right amount of columns or the specific names are not existent. See manual for more information. \n")
  if(check_type(input) == F) stop("The input is not of type 'data.frame'. \n")
  if(!(handling %in% c("sum", "mean", "first", F))) stop("Handling not correctly specified. \n")
  if(!(handlingExtra %in% c("sum", "mean", "first", F))) stop("HandlingExtra not correctly specified. \n")
  # check type of input$type
  #cnames = check_type_of_type(input)
  # check additional parameter
  if(any(additional != F)){
    if(!(is.character(additional))) stop("The additional parameter needs to  be a character string.")
    if(!any((additional %in% colnames(input)))) stop("The additional parameter has not been specified correctly or does not exist in input.")
  }
  # check extraCol parameter
  if(any(extraCol != F)){
    if(!(is.character(extraCol))) stop("The extraCol parameter needs to  be a character string.")
    if(!any((extraCol %in% colnames(input)))) stop("The extraCol parameter has not been specified correctly or does not exist in input.")
  }
  
  input = input[order(input$id, input$date),]
  
  dt = as.data.table(input)
  
  if(handling == 'first'){
    DT = data.table::dcast(dt, id + date ~ type, value.var = "value", fill=paste(NA), fun.aggregate = function(x) paste(head(x,1), collapse=""))
  }
  
  if(handling == F){
    DT = data.table::dcast(dt, id + date ~ type, value.var = "value", fill=paste(NA), fun.aggregate = function(x) paste(tail(x,1), collapse=""))
  }
  
  if(handling == 'mean'){
    DT = data.table::dcast(dt, id + date ~ type, value.var = "value", fill=paste(NA), fun.aggregate = function(x) aggCreationMean(x))
  }
  
  if(handling == 'sum'){
    DT = data.table::dcast(dt, id + date ~ type, value.var = "value", fill=paste(NA), fun.aggregate = function(x) aggCreationSum(x))
  }
  
  
  if(any(extraCol != F)){
    
    if(handlingExtra == 'first'){
      for(i in 1:length(extraCol)){
        dt_add = data.table::dcast(dt, id + date ~ get(paste(extraCol[i])), value.var = extraCol[i], fill=paste(NA), fun.aggregate = function(x) paste(head(x,1), collapse=""))
        dt_add = dt_add[,!c("id", "date")]
        DT = cbind(DT, dt_add)
      }
    }
    
    if(handlingExtra == F){
      for(i in 1:length(extraCol)){
        dt_add = data.table::dcast(dt, id + date ~ get(paste(extraCol[i])), value.var = extraCol[i], fill=paste(NA), fun.aggregate = function(x) paste(tail(x,1), collapse=""))
        dt_add = dt_add[,!c("id", "date")]
        DT = cbind(DT, dt_add)
      }
    }
    
    if(handlingExtra == 'mean'){
      for(i in 1:length(extraCol)){
        dt_add = data.table::dcast(dt, id + date ~ get(paste(extraCol[i])), value.var = extraCol[i], fill=paste(NA), fun.aggregate = function(x) aggCreationMean(x))
        dt_add = dt_add[,!c("id", "date")]
        DT = cbind(DT, dt_add)
      }
    }
    
    if(handlingExtra == 'sum'){
      for(i in 1:length(extraCol)){
        dt_add = data.table::dcast(dt, id + date ~ get(paste(extraCol[i])), value.var = extraCol[i], fill=paste(NA), fun.aggregate = function(x) aggCreationSum(x))
        dt_add = dt_add[,!c("id", "date")]
        DT = cbind(DT, dt_add)
      }
    }
  }
  
  if(any(additional != F)){
    cname = c(colnames(DT), additional)
    for(i in 1:length(additional)){
      if(handling == 'first'){
        dt_add = dt[, head(get(paste(additional[i])),1), by=list(id, date)]
        dt_add = dt_add[,!c("id", "date")]
        cbind(DT, dt_add)
      }
    }
    
    if(handling == F){
      for(i in 1:length(additional)){
        dt_add = dt[, .(new = tail(get(paste(additional[i])),1)), by=list(id, date)]
        dt_add = dt_add[,!c("id", "date")]
        DT = cbind(DT, dt_add)
      }
    }
    
    if(handling == 'mean' | handling == "sum"){
      for(i in 1:length(additional)){
        
        if(any(is.na(suppressWarnings(as.numeric(as.character(dt[,get(paste(additional[i]))])))))){
          dt_add = dt[, .(new = tail(get(paste(additional[i])),1)), by=list(id, date)]
          dt_add = dt_add[,!c("id", "date")]
          DT = cbind(DT, dt_add)
        }else{
          if(handling == "mean"){
            dt_add = dt[, .(new = mean(as.numeric(as.character(get(paste(additional[i])))))), by=list(id, date)]
            dt_add = dt_add[,!c("id", "date")]
            DT = cbind(DT, dt_add)
          }
          if(handling == "sum"){
            dt_add = dt[, .(new = sum(as.numeric(as.character(get(paste(additional[i])))))), by=list(id, date)]
            dt_add = dt_add[,!c("id", "date")]
            DT = cbind(DT, dt_add)
          }
          
        }
      }
    }
    setnames(DT, cname)
  }
  
  for(col in names(DT)[-which(colnames(DT) %in% c("id", "date"))]) set(DT, i=which(DT[[col]]==""), j=col, value=NA)
  
  res = data.frame(DT)
  res = res[order(res$id, res$date),]
  
  res = type.convert(res)
  if(na.rm) res = res[complete.cases(res),]
  
  return(res)
}