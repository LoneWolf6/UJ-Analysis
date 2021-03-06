\name{plotROC}
\alias{plotROC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot ROC curves
%%  ~~function to do ... ~~
}
\description{
This function plots ROC curves and AUC values based on the results of analyzeUJ function.
}
\usage{
plotROC(values, type, err)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
the values created in analyzeUJ function for a specific method. Input could be, for example, result$performance$Lasso.
}

  \item{type}{
an optional character string defining the type of ROC curve desired. Default is 'avg'.

avg: ROC curve averaged over all folds of k-fold cross-validation.

each: ROC curves for each fold of k-fold cross-validation.
}

  \item{err}{
  an optional logical value True or False. Defining if error bar around averaged ROC curve is desired (only if type=avg). Default is True.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Function plots the desired ROC curve including AUC value.

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

%\seealso{
%\code{\link[caret]{createFolds}}, \code{\link[imputeMissings]{impute}}, \code{\link[stats]{glm}}, \code{\link[glmnet]{glmnet}}, \code{\link[e1071]{svm}}, \code{\link[VIM]{kNN}}
%}
\examples{
data = data.frame('id'=rep(c(1:5), each=600),
                  'type'=rep(c('Var1', 'Var2', 'Var3'), times=1000),
                  'value'=sample(c(0,1), 3000, T),
                  'date'=rep(seq(as.Date("2000/1/1"), by = "day", length.out=60), each=50))


# use function to create rectangle version of user journey
dat = reshapeData(data, parallel=T, cores=2, na.rm=F)

# use function to analyze data
res = analyzeUJ(dat, target="Var1", missing="median/mode", task="classification", ROC=T)

# use plot function
plotROC(res$performance$Lasso, type="avg", err=T)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
