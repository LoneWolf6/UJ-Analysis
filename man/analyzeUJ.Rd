\name{analyzeUJ}
\alias{analyzeUJ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Analyze User Journey
%%  ~~function to do ... ~~
}
\description{
This function applies multiple machine learning techniques for regression or classification. It can select features that contribute to the prediction performance. Additionally, all machine learning techniques are evaluated by RMSE/MAE for regression or ROC/AUC for classification, which are returned as well as the predictions and effects for each model. Note that if categorical variables are included in the data, features with not enough levels will be automatically excluded. You can verify the utilized features in the resulting list.
}
\usage{
analyzeUJ(input, target, type, firstFeats, lastFeats, sumFeats, method, PCA, PCAOnly, comps, task, interval, crossValidation, split, folds, optPara, missing, imp, perc, percEx, exFeat, ROC, proba, scale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
an object of type data.frame. Preferably the output of reshapeData() without missing values.
}
  \item{target}{
a mandatory character string defining the dependent variable.
}
  \item{type}{
a mandatory character string defining the type of analysis. This is dependent on the setting of the study to be analyzed. If only one and only one target value is available for each individual, set type to 'aggregate'. In this case, the target variable in the input should be repeated for all observations corresponding to the user. All observations for each individual are then aggregated (default: mean for numeric variables and mode for categorical variable) and utilized for prediction. If target values exist for each individual and each point in time, set type to 'cont'. Default is 'cont'.
}
  \item{firstFeats}{
an optional character string defining the variables that should not be aggreagted by mean/mode if type=aggregate. Here, the first value will be utilized neglecting the rest of the values.
}
  \item{lastFeats}{
an optional character string defining the variables that should not be aggreagted by mean/mode if type=aggregate. Here, the last value will be utilized neglecting the rest of the values.
}
  \item{sumFeats}{
an optional character string defining the variables that should not be aggreagted by mean/mode if type=aggregate. Here, the sum of the corresponding feature will be utilized.
}
\item{method}{
an optional character string defining the methods to be run in this analysis. Can be 'all', 'svm', 'lm','log', 'lasso', "ridge",  or 'treeBoost'. Default is 'all'.

all: all possible methods are executed.

svm: support vector machine according to the \link[e1071]{svm} function. Depending on the specified class, the type is C-classification or eps-regression with default parameters.

lm: linear regression according to the \link[stats]{lm} function.

log: logistic regression according to the \link[stats]{glm} function.

lasso: lasso regression according to the \link[glmnet]{glmnet} function. The optimal lambda is found by cross-validation with a predifined lambda sequence of 10^seq(10,-2,length=1000).

ridge: ridge regression according to the \link[glmnet]{glmnet} function. The optimal lambda is found by cross-validation with a predifined lambda sequence of 10^seq(10,-2,length=1000).

treeBoost: extreme gradient boosting trees according to the \link[xgb.train]{xgboost} function with maximum depth of 3, maximum number of boosting iterations of 1000, and the rest of the parameters are default.

(Multiple parameters are tuned if optPara=T (for more information, see optPara))
}
  \item{PCA}{
an optional logical value True or False. Defines if Principal Component Analysis (PCA) is executed for numeric predictors only.
}
  \item{PCAOnly}{
an optional logical value True or False. Defines if only principal components are utilized as features (True) for the predictive models or if the non-numeric features are also included (False). Default is False.
}
  \item{comps}{
an optional non-negative numeric value defining the number of components to utilize. If not specified, as many components are utilized until 99 percent of the variance is explained.
}
  \item{task}{
an optional character string defining the task of analysis. Can be either 'classification' or 'regression'. However, this parameter should be set according to the goal of the analysis. Classification currently only works for binary classification. Default is 'regression'.
}
  \item{interval}{
an optional non-negative numeric value defining the interval for the analysis in days. Default is the consideration of all points in time.
}
  \item{crossValidation}{
an optional logical value True or False. Defines if cross-validation is executed. Default is True.
}
  \item{split}{
an optional non-negative numeric value defining the split for training and test data if crossValidation is False. Specify the percentage as the training data. The rest will be utilized as test data. Default is 70.
}
  \item{folds}{
an optional non-negative numeric value defining the folds for k-fold cross validation. Default is 10.
}
  \item{optPara}{
an optional logical value True or False. Defines if some specific machine learning techniques will be tuned before application. Can increase run time tremendously. Default is False.

svm: grid-based search for cost (10^seq(-5,5,0.1)) and gamma parameters (2^(-3:3)) and radial basis kernel.

treeBoost: grid based search for eta, max_depth, gamma, colsample_bytree, and min_child_weight.

}
  \item{missing}{
an optional logical value True or False. Defines if missing values are imputed. Can be "median/mode" or "knn". If median/mode, categorical variables are imputed by mode and numeric variables by median corresponding to the \link[imputeMissings]{impute}function. If knn, missings values are imputed by k-Nearest Neighbour method according to the \link[VIM]{kNN} function with default values. The knn method can increase computation time depending on the amount of observations and features. Default is False. However, if missing values are still detected, median/mode is applied. After imputation procedure, complete cases are used for analysis.
}
  \item{imp}{
an optional logical value True or False. If missing values exist, this parameter defines if imputation should be executed before the split into training and test set or after the split on each corresponding training and test set. Default is False and imputation will be executed before the split.
}
  \item{perc}{
an optional non-negative numeric value. If missing is True or missing values are detected, this value defines the percentage of missing values up to which features are excluded from the analysis. Default is none.
}
  \item{percEx}{
an optional character string defining the variables that should not be deleted even though amount of missing values is higher than provided percentage (perc). Default is none.
}
  \item{exFeat}{
an optional character string defining the features not to be included in the analysis. Has to be specified by name of column of corresponding feature.
}
  \item{ROC}{
an optional logical value True or False. If True, ROC curve will be included in output if task is classification and only two labels exist. Default is False.
}
  \item{proba}{
an optional numeric value between 0 and 1. If task is classification, this is the threshold for the classifying the observations. Confusion Matrix is based on this threshold. Default is .5.
}
  \item{scale}{
an optional logical value True or False. Numeric features will be scaled. Default is True.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Function returns a list containing the following components:

\item{input}{A data frame consisting of the utilized input data for analysis}

\item{fold}{A list consisting of the indices for each fold of k-fold cross-validation}

\item{TargetObservations}{A numeric vector containing the target observations}

\item{TargetPredictions}{A numeric vector containing the target predictions for each executed method}

\item{performance}{
Regression: A data frame consisting of the cross-validated prediction error indicated by mean absolute error and root mean square error for all specified methods and an addtional "mean model", which utilizes the mean of the training data as prediction. These performance measures are included or each fold and averaged across folds.

Classification: A list consisting of confusion matrices (for each fold and summed up), values for plotting the receiver operating characteristics curves (ROC), and area under the curve (AUC) values (for each fold and averaged) according to the \link[ROCR]{performance} function (if ROC=T) for each executed method.
}

\item{results}{A list containing the trained methods based on all data}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[caret]{createFolds}}, \code{\link[imputeMissings]{impute}}, \code{\link[stats]{glm}}, \code{\link[glmnet]{glmnet}}, \code{\link[e1071]{svm}}, \code{\link[VIM]{kNN}}
}
\examples{
# create data frame with mandatory columns
data = data.frame('id'=length(rep(c(1:5), each=600)),
                  'type'=rep(c('Var1', 'Var2', 'Var3'), times=1000),
                  'value'=rep(c(1:5), times=600),
                  'date'=rep(seq(as.Date("2000/1/1"), by = "day", length.out=60), each=50))

# use function to create rectangle version of user journey
dat = reshapeData(data, parallel=T, cores=2, na.rm=F)

# use function to analyze data
res = analyzeUJ(dat, target="Var1", missing="median/mode", task="regression")
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
