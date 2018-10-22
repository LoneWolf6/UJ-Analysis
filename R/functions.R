load("~/ownCloud/1_RESEARCH/Mental Health/FE paper/data/4dat_after_feature.RData")

analyzeUJ = function(input, target=F, type='cont', firstFeats=F, lastFeats=F, sumFeats = F, method='all', PCA=F, PCA_only=F, comps=0, task='regression', interval=F, crossValidation=T, split=70, folds=10, optPara=F, missing=F,imp=F, perc=F, percEx=F, exFeat=NULL, ROC=F, proba=.5, scale=T){

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
  if(!is.numeric(folds)) stop("The parameter 'folds' is not numeric. Provide appropriate parameter value.")
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
    fold = createFolds(input[,target], k = folds, list = TRUE, returnTrain = FALSE)
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
    
    
    # PCA (describe PCA, PCA_only, and comp in manual)
    if(PCA = T){
      resPCA = prcomp(train[,sapply(train, is.numeric)][,-which(names(train) %in% c('id', target))], scale=T)
      std_dev <- resPCA$sdev
      pr_var <- std_dev^2
      prop_varex <- pr_var/sum(pr_var)
      # if comps not given, choose as many components that explain 99% of the variance
      if(comps == 0) comps = which(cumsum(prop_varex)>=0.99)[1]
      PCAtrain = resPCA$x[,1:comps]
      PCAtest = predict(resPCA, newdata = test)[,1:comps]
      if(PCA_only == T){
        train = PCAtrain
        test = PCAtest
      }
      if(PCA_only == F){
        train = cbind(train, PCAtrain)
        test = cbind(test, PCAtest)
      }
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
