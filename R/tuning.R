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
                         subsample = 1) # subsample training ratio

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

  cv_ctrl = trainControl(method = "cv", number = 10,
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
