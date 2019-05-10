\name{reshapeData}
\alias{reshapeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reshape data - create User Journey
%%  ~~function to do ... ~~
}
\description{
This function creates a rectangle (wide) version of the User Journey based on specific columns that need to be provided. The rows then represent observations of different variables at a specific point in time for a particular user. The chosen time window between observation is based on the date provided by the user.
}
\usage{
reshapeData(input, additional, extraCol, handling, handlingExtra, na.rm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
an object of type data.frame including the mandatory column names 'id', 'type', 'value', and 'date'.
}
  \item{additional}{
an optional character string defining an additional column or multiple columns of input to be considered. Then, each unique level of the corresponding column are treated as additional feature in the output. If multiple observations occur at the same point in time for a specific user, the features will be aggregated as defined in parameter handling.
}
  \item{extraCol}{
an optional character string defining an additional column or multiple columns of input to be considered. Then, each column becomes a feature with its corresponding values in the output. These extra features will be handled as specified in handlingExtra.
}
  \item{handling}{
an optional character string that defines how multiple observations of same type at same point in time are handled. Can be "first", "sum", or "mean". Default is the consideration of the most recent value only. For categorical variables, if  "sum" or "mean" are specified, the most recent value is utilized.
}
  \item{handlingExtra}{
an optional character string that defines how multiple observations of same type at same point in time are handled for the extra features. Can be "first", "sum", or "mean". Default is the consideration of the most recent value only. For categorical variables, if  "sum" or "mean" are specified, the most recent value is utilized.
}
  \item{na.rm}{
an optional logical value True or False. If True, all rows with missing values are omitted. Default is False.
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of type data.frame.
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
% \code{\link[caret]{createFolds}}
\seealso{
\code{}
}
\examples{
# create data frame with mandatory columns
data = data.frame('id'=rep(c(1:5), each=6),
      'type'=rep(c('Var1', 'Var2', 'Var3'), times=10),
      'value'=rep(c(1:5), times=6),
      'date'=rep(seq(as.Date("2000/1/1"), by = "day", length.out=15), each=2))

# use function to create rectangle version of user journey
dat = reshapeData(data, na.rm=F)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
