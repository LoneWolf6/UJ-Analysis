\name{TimeWindow}
\alias{TimeWindow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time Window Aggregation
%%  ~~function to do ... ~~
}
\description{
This function creates time-based minimum, maximum, mean, sum, and standard deviation of numeric columns of a data frame. These values are calculated based on specific time steps provided by the user.
}
\usage{
TimeWindow(input, parallel, cores, target, no.use, steps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
an object of type data.frame. Preferably the output of reshapeData().
}
  \item{parallel}{
an optional logical value True or False. Parameter specifies if parallel computation is enabled according to the foreach package. Can result in tremendous improvement of computation time. Default is False.
}
  \item{cores}{
an optional non-negative integer. The desired number of cores utilized if parallel = T. Default is the number of cores available -1.
}
  \item{target}{
a mandatory character string defining the dependent variable which is therefore not included in the computation.
}
  \item{no.use}{
an optional character string that defines other variables that should not be used.
}
  \item{steps}{
an optional non-negative numeric value defining the time window. Steps defines how many previous points in time are considered (including current point in time). Default is 2.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of type data.frame in the same form as data including additional features.
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
\code{\link[foreach]{foreach}}, \code{\link[doSNOW]{registerDoSNOW}}, \code{\link[parallel]{makeCluster}}, \code{\link[parallel]{detectCores}}
}
\examples{
# create data frame with mandatory columns
data = data.frame('id'=rep(c(1:5), each=8),
      'type'=rep(c('Var1', 'Var2'), times=20),
      'value'=rep(c(1:5), times=8),
      'date'=rep(seq(as.Date("2000/1/1"), by = "day", length.out=20), each=2))

# create rectangle version of user journey
dat = reshapeData(data)

# use function to create new features based on time windows
dat_feat = TimeWindow(dat, parallel=T, cores=2, target='Var1', no.use=F, steps=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
