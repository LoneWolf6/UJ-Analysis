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

ROCValues=function(x,y,at){
  # check if i have exectly this value or apprearing more than 1 time
  idx = x == at
  if(sum(idx)>=1){
    return(y[max(which(idx))] ) ##return max
  }

  # look for left index
  left = x<=at
  left = max(which(left))

  # look for right index
  right = x>at
  right = min(which(right))

  # get y values
  left_y = y[left]
  right_y = y[right]

  # get x values
  left = x[left]
  right= x[right]

  # linear interpolation
  inter = left_y + ( (right_y-left_y)/(right-left) ) * (at-left)
}

# performs k-fold cross validation using multiple machine learning techniques for comparison
analyzeUJ = function(input, target=F, type='cont', firstFeats=F, lastFeats=F, sumFeats = F, method='all',
                     PCA=F, PCAOnly=F, comps=0, task='regression', interval=F, crossValidation=T, split=70,
                     folds=10, optPara=F, missing=F,imp=F, perc=F, percEx=F, exFeat=NULL, ROC=T, proba=.5, scale=T,
                     holdout = FALSE, holdoutSize = 20){

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
  if(!is.numeric(holdoutSize) || !holdoutSize < 100 || holdoutSize == 0) stop("Please provide appropriate value for holdoutSize. \n")
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
    setDT(input)
    input$date = as.Date(input$date, origin="1970-01-01")
    input = input[, .SD[1:which(date >= date[1] + interval)[1]], by=id]
    setorder(input, id, date)
    setDF(input)
  }

  # delete categorical variables with not enough levels
  idx = colnames(input)[-which(colnames(input) %in% c("id","date"))]

  levs = lapply(lapply(input[,idx], unique), function(x) length(x[!is.na(x)]) > 1)
  if(any(levs == F)){
    input = input[, !(colnames(input) %in% names(which(levs == F)))]
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
    if(any(sapply(input[,which(colnames(input) %in% sumFeats)], is.numeric)) == F && sumFeats != F) stop("Categorical features were specified in sumFeats. Not possible.")
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
      if(length(colnames(input)) < 4) stop("No features are left after deletion based on missing values")
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

  # for analyses set holdout set aside if holdout = TRUE
  if(holdout){
    setDT(input)
    set.seed(19)
    holdoutUsers = sample(unique(input$id), size = holdoutSize/100*length(unique(input$id)))
    holdoutSet = input[id %in% holdoutUsers]
    input = input[!(id %in% holdoutUsers)]
    setDF(input)
    setDF(holdoutSet)
    if((isTRUE(missing) || any(is.na(holdoutSet)))) holdoutSet = imputeData(holdoutSet, perc=F, missing, target, percEx)
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
    set.seed(123)
    indexes = sample(1:nrow(input), size=(split/100)*nrow(input))
    test = input[-indexes,]
    train = input[indexes,]
  }
  # create model string
  modelString = getModelString(input, target, exFeat)
  res = list()
  mean_model = list()
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
    res$obclass[[i]] = test[,target]

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
    if(optPara){
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

    # foldid for glmnet package
    set.seed(123)
    foldid = sample(1:10, size=nrow(train), replace=TRUE)

    # all/svm
    if(method == 'all' || 'svm' %in% method){
      if(task == "regression"){
        if(optPara == T){
          fit_svm = svm(formula = formula(modelString), data = train, type=typeSVM,gamma=svm_parameters$svm_gamma[[1]],costs =svm_parameters$svm_costs[[1]], scale=scale)
        }else{
          fit_svm = svm(formula = formula(modelString), data = train, scale=scale, type=typeSVM)
        }
        res$pred$pred_svm[[i]] = predict(fit_svm, test)
      }
      if(task == 'classification'){
        if(optPara == T){
          fit_svm = svm(formula = formula(modelString), data = train, type=typeSVM,gamma=svm_parameters$svm_gamma[[1]],costs =svm_parameters$svm_costs[[1]], scale=scale, probability=T)
        }else{
          fit_svm = svm(formula = formula(modelString), data = train, scale=scale, type=typeSVM, probability=T)
        }
        res$perf_svm[[i]] = attr(predict(fit_svm, test, probability=T),"probabilities")[,1]
        res$pred$pred_svm[[i]] = ifelse(res$perf_svm[[i]] >= proba, 1, 0)
      }
    }else{
      res$pred$pred_svm = 'Method not executed. Task is not regression, not possible, or not specified.'
    }

    # all/lm
    if((method == 'all' || 'lm' %in% method) && task == 'regression'){
      if(nrow(input) < ncol(input)){
        res$pred$pred_lm = 'The data has more features than observations. Linear regression is not meaningful.'
      }else{
        fit_lm = glm(formula=formula(modelString), data = train)
        res$pred$pred_lm[[i]] = predict(fit_lm, test)
      }
    }else{
      res$pred$pred_lm = 'Method not executed. Task is not regression, not possible, or not specified.'
    }

    # all/lasso
    if(method == 'all' || 'lasso' %in% method){
      mod.matrix = sparse.model.matrix(formula(modelString),rbind(train, test))
      mat = mod.matrix[1:nrow(train),]
      mat_test = mod.matrix[(nrow(train)+1):nrow(mod.matrix),]
      fit_lasso = cv.glmnet(mat, y = train[,target], alpha=1, foldid=foldid, family=typeLASSO, standardize=scale, lambda=10^seq(10,-2,length=1000))

      if(task == "regression") res$pred$pred_lasso[[i]] = predict(fit_lasso, mat_test, type="link", s='lambda.min')

      if(task == 'classification'){
        res$perf_lasso[[i]] = predict(fit_lasso, type="response", mat_test, s="lambda.min")
        res$pred$pred_lasso[[i]] = ifelse(res$perf_lasso[[i]] >= proba, 1, 0)
      }
    }else{
      res$pred$pred_lasso = 'Method not executed. Task is not regression, not possible, or not specified.'
    }

    # all/ridge
    if(method == 'all' || 'ridge' %in% method){
      mod.matrix = sparse.model.matrix(formula(modelString),rbind(train, test))
      mat = mod.matrix[1:nrow(train),]
      mat_test = mod.matrix[(nrow(train)+1):nrow(mod.matrix),]
      fit_ridge = cv.glmnet(mat, y = train[,target], alpha=0, foldid=foldid, standardize=scale, family=typeLASSO, lambda=10^seq(10,-2,length=1000))

      if(task == "regression") res$pred$pred_ridge[[i]] = predict(fit_ridge, mat_test, type="link",s='lambda.min')

      if(task == 'classification'){
        res$perf_ridge[[i]] = predict(fit_ridge, type="response", mat_test, s='lambda.min')
        res$pred$pred_ridge[[i]] = ifelse(res$perf_ridge[[i]] >= proba, 1, 0)
      }
    }else{
      res$pred$pred_ridge = 'Method not executed. Task is not regression, not possible, or not specified.'
    }

    # all/boost
    if(method == 'all' || 'treeBoost' %in% method){
      if(optPara){
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
      if(task == "regression") res$pred$pred_boost[[i]] = predict(fit, mat_test, outputmargin=F)
      if(task == "classification"){
        res$perf_boost[[i]] = predict(fit, mat_test, outputmargin=F)
        res$pred$pred_boost[[i]] = ifelse(res$perf_boost[[i]] >= proba, 1, 0)
      }
    }else{
      res$pred$pred_boost = 'Method not executed. Task is not regression, not possible, or not specified.'
    }


    # all/log reg
    if((method == 'all' || 'log' %in% method) && task == 'classification'){
      if(nrow(input) < ncol(input)){
        res$pred$pred_log = 'The data has more features than observations. Logistic regression is not meaningful.'
      }else{
        fit_log = glm(formula=formula(modelString), data = train, family = binomial(link = "logit"))
        res$perf_log[[i]] = predict(fit_log, test, type="response")
        res$pred$pred_log[[i]] = ifelse(res$perf_log[[i]] >= proba, 1, 0)
      }
    }else{
      res$pred$pred_log = 'Method not executed. Task is not classification, not possible, or not specified.'
    }

    if(task == 'regression') mean_model[[i]] = rep(mean(train[,target]), nrow(test))


    setTimerProgressBar(pb, i)

    # execute ML based on all data for inferential outcomes

    # foldid for glmnet package
    set.seed(123)
    foldid = sample(1:10, size=nrow(input), replace=TRUE)

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
        res$fit$fit_lasso = cv.glmnet(mat, y = input[,target], alpha=1, foldid=foldid, family=typeLASSO, standardize=scale)
      }

      # all/ridge
      if(method == 'all' || 'ridge' %in% method){
        mat = sparse.model.matrix(formula(modelString),input)
        res$fit$fit_ridge = cv.glmnet(mat, y = input[,target], alpha=0, foldid=foldid, family=typeLASSO, standardize=scale)
      }

      # all/boost
      if(method == 'all' || 'treeBoost' %in% method){
        if(optPara){
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

  # create performance measures
  if(task == 'regression'){
    for(i in 1:folds){

      if(length(res$pred$pred_svm) > 0 && !(is.character(res$pred$pred_svm))){
        res$maeSVM[[i]] = mae(unlist(res$pred$pred_svm[[i]]), res$obclass[[i]])
        res$rmseSVM[[i]] = rmse(unlist(res$pred$pred_svm[[i]]), res$obclass[[i]])
      }else{
        res$maeSVM = NA
        res$rmseSVM = NA
      }

      if(length(res$pred$pred_lm) > 0 && !(is.character(res$pred$pred_lm))){
        res$maeLM[[i]] = mae(unlist(res$pred$pred_lm[[i]]), res$obclass[[i]])
        res$rmseLM[[i]] = rmse(unlist(res$pred$pred_lm[[i]]), res$obclass[[i]])
      }else{
        res$maeLM = NA
        res$rmseLM = NA
      }


      if(length(mean_model) > 0){
        res$maeMean[[i]] = mae(unlist(mean_model[[i]]), res$obclass[[i]])
        res$rmseMean[[i]] = rmse(unlist(mean_model[[i]]), res$obclass[[i]])
      }else{
        res$maeMean = NA
        res$rmseMean = NA
      }


      if(length(res$pred$pred_lasso) > 0 && !(is.character(res$pred$pred_lasso))){
        res$maeLasso[[i]] = mae(as.numeric(unlist(res$pred$pred_lasso[[i]])), res$obclass[[i]])
        res$rmseLasso[[i]] = rmse(as.numeric(unlist(res$pred$pred_lasso[[i]])), res$obclass[[i]])
      }else{
        res$maeLasso = NA
        res$rmseLasso = NA
      }


      if(length(res$pred$pred_ridge) > 0 && !(is.character(res$pred$pred_ridge))){
        res$maeRidge[[i]] = mae(as.numeric(unlist(res$pred$pred_ridge[[i]])), res$obclass[[i]])
        res$rmseRidge[[i]] = rmse(as.numeric(unlist(res$pred$pred_ridge[[i]])), res$obclass[[i]])
      }else{
        res$maeRidge = NA
        res$rmseRidge = NA
      }


      if(length(res$pred$pred_boost) > 0 && !(is.character(res$pred$pred_boost))){
        res$maeBoost[[i]] = mae(unlist(res$pred$pred_boost[[i]]), res$obclass[[i]])
        res$rmseBoost[[i]] = rmse(unlist(res$pred$pred_boost[[i]]), res$obclass[[i]])
      }else{
        res$maeBoost = NA
        res$rmseBoost = NA
      }
    }

    # create performance table
    perf = list()
    perf$mean = data.frame(matrix(ncol=2, nrow=6))
    colnames(perf$mean) = c('MAE', 'RMSE')
    rownames(perf$mean) = c('LM', 'SVM', 'LASSO','RIDGE','Tree','MEAN')
    perf$mean['MAE'] = c(mean(res$maeLM), mean(res$maeSVM), mean(res$maeLasso), mean(res$maeRidge), mean(res$maeBoost),mean(res$maeMean))
    perf$mean['RMSE'] = c(mean(res$rmseLM), mean(res$rmseSVM), mean(res$rmseLasso), mean(res$rmseRidge),mean(res$rmseBoost), mean(res$rmseMean))
    perf$folds = list(LM=list(MAE=res$maeLM, RMSE=res$rmseLM), SVM=list(MAE=res$maeSVM, RMSE=res$rmseSVM),
                      Lasso=list(MAE=res$maeLasso, RMSE=res$rmseLasso), Ridge=list(MAE=res$maeRidge, RMSE=res$rmseRidge),
                      Tree=list(MAE=res$maeBoost, RMSE=res$rmseBoost), Mean=list(MAE=res$maeMean, RMSE=res$rmseMean))
  }

  if(task == 'classification'){

    if(length(res$pred$pred_lasso) > 0 && !(is.character(res$pred$pred_lasso))){
      res$confusionMatrixLasso = matrix(0,nrow=2, ncol=2)
      for(f in 1:folds){
        res$confMatLasso[[f]] = suppressWarnings(confusionMatrix(as.factor(unlist(res$pred$pred_lasso[[f]])), as.factor(unlist(res$obclass[[f]]))))
        res$confusionMatrixLasso = res$confusionMatrixLasso + res$confMatLasso[[f]]$table
      }
    }else{
      res$confMatLasso = 'Not specified or not possible.'
      res$confusionMatrixLasso = 'Not specified or not possible.'
    }

    if(length(res$pred$pred_svm) > 0 && !(is.character(res$pred$pred_svm))){
      res$confusionMatrixSVM = matrix(0,nrow=2, ncol=2)
      for(f in 1:folds){
        res$confMatSVM[[f]] = suppressWarnings(confusionMatrix(as.factor(unlist(res$pred$pred_svm[[f]])), as.factor(unlist(res$obclass[[f]]))))
        res$confusionMatrixSVM = res$confusionMatrixSVM + res$confMatSVM[[f]]$table
      }
    }else{
      res$confMatSVM = 'Not specified or not possible.'
      res$confusionMatrixSVM = 'Not specified or not possible.'
    }

    if(length(res$pred$pred_log) > 0 && !(is.character(res$pred$pred_log))){
      res$confusionMatrixLog = matrix(0,nrow=2, ncol=2)
      for(f in 1:folds){
        res$confMatLog[[f]] = suppressWarnings(confusionMatrix(as.factor(unlist(res$pred$pred_log[[f]])), as.factor(unlist(res$obclass[[f]]))))
        res$confusionMatrixLog = res$confusionMatrixLog + res$confMatLog[[f]]$table
      }
    }else{
      res$confMatLog = 'Not specified or not possible.'
      res$confusionMatrixLog = 'Not specified or not possible.'
    }

    if(length(res$pred$pred_boost) > 0 && !(is.character(res$pred$pred_boost))){
      res$confusionMatrixBoost = matrix(0,nrow=2, ncol=2)
      for(f in 1:folds){
        res$confMatBoost[[f]] = suppressWarnings(confusionMatrix(as.factor(unlist(res$pred$pred_boost[[f]])), as.factor(unlist(res$obclass[[f]]))))
        res$confusionMatrixBoost = res$confusionMatrixBoost + res$confMatBoost[[f]]$table
      }
    }else{
      res$confMatBoost = 'Not specified or not possible.'
      res$confusionMatrixBoost ='Not specified or not possible.'
    }

    if(length(res$pred$pred_ridge) > 0 && !(is.character(res$pred$pred_ridge))){
      res$confusionMatrixRidge = matrix(0,nrow=2, ncol=2)
      for(f in 1:folds){
        res$confMatRidge[[f]] = suppressWarnings(confusionMatrix(as.factor(unlist(res$pred$pred_ridge[[f]])), as.factor(unlist(res$obclass[[f]]))))
        res$confusionMatrixRidge = res$confusionMatrixRidge + res$confMatRidge[[f]]$table
      }
    }else{
      res$confMatRidge = 'Not specified or not possible.'
      res$confusionMatrixRidge = 'Not specified or not possible.'
    }


    if(isTRUE(ROC) && length(unique(input[,target])) == 2 && length(res$perf_lasso) > 0){
      AUCLasso = c()
      ROCLasso = list()
      for(f in 1:folds){
        perf_lasso = prediction(unlist(res$perf_lasso[[f]]), res$obclass[[f]])
        ROCLasso[[f]] = performance(perf_lasso,"tpr","fpr")
        AUCLasso = c(AUCLasso, performance(perf_lasso,"auc")@y.values[[1]])
        AUCLassoMean=sum(AUCLasso)/folds
      }
    }else{
      AUCLasso='Not specified or not possible.'
      AUCLassoMean='Not specified or not possible.'
      ROCLasso='Not specified or not possible.'
    }


    if(isTRUE(ROC) && length(unique(input[,target])) == 2 && length(res$perf_ridge) > 0){
      AUCRidge = c()
      ROCRidge = list()
      for(f in 1:folds){
        perf_ridge = prediction(unlist(res$perf_ridge[[f]]), res$obclass[[f]])
        ROCRidge[[f]] = performance(perf_ridge,"tpr","fpr")
        AUCRidge = c(AUCRidge, performance(perf_ridge,"auc")@y.values[[1]])
        AUCRidgeMean=sum(AUCRidge)/folds
      }
    }else{
      AUCRidge='Not specified or not possible.'
      AUCRidgeMean='Not specified or not possible.'
      ROCRidge='Not specified or not possible.'
    }


    if(isTRUE(ROC) && length(unique(input[,target])) == 2 && length(res$perf_log) > 0){
      AUCLog = c()
      ROCLog = list()
      for(f in 1:folds){
        perf_log = prediction(unlist(res$perf_log[[f]]), res$obclass[[f]])
        ROCLog[[f]] = performance(perf_log,"tpr","fpr")
        AUCLog = c(AUCLog, performance(perf_log,"auc")@y.values[[1]])
        AUCLogMean=sum(AUCLog)/folds
      }
    }else{
      AUCLog='Not specified or not possible.'
      AUCLogMean='Not specified or not possible.'
      ROCLog='Not specified or not possible.'
    }


    if(isTRUE(ROC) && length(unique(input[,target])) == 2 && length(res$perf_svm) > 0){
      AUCSVM = c()
      ROCSVM = list()
      for(f in 1:folds){
        perf_svm = prediction(unlist(res$perf_svm[[f]]), res$obclass[[f]])
        ROCSVM[[f]] = performance(perf_svm,"tpr","fpr")
        AUCSVM = c(AUCSVM, performance(perf_svm,"auc")@y.values[[1]])
        AUCSVMMean=sum(AUCSVM)/folds
      }
    }else{
      AUCSVM='Not specified or not possible.'
      AUCSVMMean='Not specified or not possible.'
      ROCSVM='Not specified or not possible.'
    }


    if(isTRUE(ROC) && length(unique(input[,target])) == 2 && length(res$perf_boost) > 0){
      AUCBoost = c()
      ROCBoost = list()
      for(f in 1:folds){
        perf_boost = prediction(unlist(res$perf_boost[[f]]), res$obclass[[f]])
        ROCBoost[[f]] = performance(perf_boost,"tpr","fpr")
        AUCBoost = c(AUCBoost, performance(perf_boost,"auc")@y.values[[1]])
        AUCBoostMean=sum(AUCBoost)/folds
      }
    }else{
      AUCBoost='Not specified or not possible.'
      AUCBoostMean='Not specified or not possible.'
      ROCBoost='Not specified or not possible.'
    }

    perf=list(LOG = list(ConfMatFolds=res$confMatLog, ConfMatSum=res$confusionMatrixLog, AUCFolds=AUCLog , AUCMean=AUCLogMean, ROCValues=ROCLog),
              Tree = list(ConfMatFolds=res$confMatBoost, ConfMatSum=res$confusionMatrixBoost, AUCFolds=AUCBoost , AUCMean=AUCBoostMean, ROCValues=ROCBoost),
              SVM = list(ConfMatFolds=res$confMatSVM, ConfMatSumum=res$confusionMatrixSVM, AUCFolds=AUCSVM , AUCMean=AUCSVMMean, ROCValues=ROCSVM),
              Lasso = list(ConfMatFolds=res$confMatLasso, ConfMatSum=res$confusionMatrixLasso, AUCFolds=AUCLasso, AUCMean=AUCLassoMean, ROCValues=ROCLasso),
              Ridge = list(ConfMatFolds=res$confMatRidge, ConfMatSum=res$confusionMatrixRidge, AUCFolds=AUCRidge , AUCMean=AUCRidgeMean, ROCValues=ROCRidge))
  }


  # if no cross-validation, set fold to the training index
  if(!(isTRUE(crossValidation))) fold = indexes
  # create results
  if(task == "regression"){
    TargetPredictions = list(SVM= unlist(res$pred$pred_svm), LM=unlist(res$pred$pred_lm), Lasso=unlist(res$pred$pred_lasso),
                             Ridge=unlist(res$pred$pred_ridge),Tree=unlist(res$pred$pred_boost),Mean=unlist(mean_model))
  }
  if(task == "classification"){
    if("Method not executed. Task is not classification, not possible, or not specified." %in% res$pred$pred_log) res$perf_log = res$pred$pred_log
    if("Method not executed. Task is not classification, not possible, or not specified." %in% res$pred$pred_ridge) res$perf_ridge = res$pred$pred_ridge
    if("Method not executed. Task is not classification, not possible, or not specified." %in% res$pred$pred_lasso) res$perf_lasso = res$pred$pred_lasso
    if("Method not executed. Task is not classification, not possible, or not specified." %in% res$pred$pred_svm) res$perf_svm = res$pred$pred_svm
    if("Method not executed. Task is not classification, not possible, or not specified." %in% res$pred$pred_boost) res$perf_boost = res$pred$pred_boost
    TargetPredictions = list()
    TargetPredictions$probs = list(SVM=unlist(res$perf_svm), Lasso=unlist(res$perf_lasso),Ridge=unlist(res$perf_ridge),Tree=unlist(res$perf_boost),LOG=unlist(res$perf_log))
    TargetPredictions$preds = TargetPreds=list(SVM=unlist(res$pred$pred_svm), Lasso=unlist(res$pred$pred_lasso),Ridge= unlist(res$pred$pred_ridge),
                                               Tree=unlist(res$pred$pred_boost),LOG=unlist(res$pred$pred_log))
  }
  if(!holdout) holdoutSet = "Not specified"
  result = list(input=inputUsed, fold=fold, TargetObservations=res$obs,
                TargetPredictions=TargetPredictions, holdoutSet = holdoutSet,
                performance=perf, results=list(LM=res$fit$fit_lm, Log=res$fit$fit_log, SVM=res$fit$fit_svm, Lasso=res$fit$fit_lasso, Ridge=res$fit$fit_ridge, Tree=res$fit$fit_boost))

  return(result)
}

# plot roc function
plotROC = function(values, type='avg', err=T){
  if(!(is.list(values$ROCValues))) stop("ROC values not existent in input.")
  if(length(values$ROCValues) < 1) stop("Something went wrong. No ROC values in input.")
  if(length(values$AUCFolds) < 1) stop("Something went wrong. No AUC values in input.")
  if(length(values$AUCMean) < 1) stop("Something went wrong. No AUC values in input.")
  if(!(length(values$AUCFolds) == length(values$ROCValues))) stop("Something went wrong. AUC values and ROC values do not have the same length.")

  folds=length(values$ROCValues)

  ROC_vals = list()
  for(k in 1:folds){
    res = c(0)
    for(i in seq(0,1,length.out = 1000)){
      res = c(res,ROCValues(unlist(values$ROCValues[[k]]@x.values),
                            unlist(values$ROCValues[[k]]@y.values),i))
    }
    ROC_vals[[k]] = res
  }

  ROCs = matrix(unlist(ROC_vals), ncol=1001, byrow=T)

  if(type == 'avg' && isTRUE(err)){
    sd = apply(ROCs, 2, sd)
    ROCplot = ggplot() +
      geom_line(data=data.frame(TPR=colMeans(ROCs), FPR=seq(0,1,length.out = 1001)), aes(x=FPR, y=TPR, color='a'), size=0.8) +
      geom_abline(linetype = 2) +
      geom_ribbon(aes(ymin=colMeans(ROCs)+sd, ymax=colMeans(ROCs)-sd, x=seq(0,1,length.out = 1001)), alpha=0.2) +
      scale_color_manual(name = "", labels=paste('AUC: ', round(values$AUCMean,3), sep=""), values = c('a'="black")) +
      theme(legend.background=element_blank(), axis.text=element_text(size=14),
            axis.title=element_text(size=18), legend.text=element_text(size=18), legend.position = c(0.8, 0.2))
    return(ROCplot)
  }

  if(type == 'avg' && !(isTRUE(err))){
    ROCplot = ggplot() +
      geom_line(data=data.frame(TPR=colMeans(ROCs), FPR=seq(0,1,length.out = 1001)), aes(x=FPR, y=TPR, color='a'), size=0.8) +
      geom_abline(linetype = 2) +
      scale_color_manual(name = "", labels=paste('AUC: ', round(values$AUCMean,3), sep=""), values = c('a'="black")) +
      theme(legend.background=element_blank(), axis.text=element_text(size=14),
            axis.title=element_text(size=18), legend.text=element_text(size=18), legend.position = c(0.8, 0.2))
    return(ROCplot)
  }

  if(type == 'each'){
    ROCs2 = data.frame(t(ROCs))
    ROCs2['FPR'] = seq(0,1,length.out = 1001)
    ROCs2 = melt(ROCs2, id.vars="FPR")
    cols = palette(rainbow(folds))

    ROCplot = ggplot(ROCs2, aes(x=FPR, y=value, color=variable)) + geom_path() +
      labs(x="FPR", y="TPR") +
      scale_color_manual(name = "", labels=paste('AUC: ', round(values$AUCFolds,3), sep=""), values = cols) +
      theme(legend.background=element_blank(), axis.text=element_text(size=14),
            axis.title=element_text(size=18), legend.text=element_text(size=18), legend.position = c(0.9, 0.2)) +
      geom_abline(linetype = 2)
    return(ROCplot)
  }
}
