\name{binning}
\alias{binning}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Binning
%%  ~~function to do ... ~~
}
\description{
This function creates particular bins of equal intervals based on the specified numeric column of a data frame.
}
\usage{
binning(input, target, interval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
an object of type data.frame. Preferably the output of reshapeData().
}
  \item{target}{
a mandatory character string defining the variable that is to be used for binning.
}
  \item{interval}{
a mandatory non-negative numeric value indicating the amount of bins.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of type data.frame in the same form as data including additional binned column.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# create data frame with mandatory columns
data = data.frame('id'=rep(c(1:5), each=4),
      'type'=rep(c('Var1', "Var2"), times=10),
      'value'=rep(c(1:5), times=4),
      'date'=rep(seq(as.Date("2000/1/1"), by = "day", length.out=10), each=2))

# create rectangle version of user journey
dat = reshapeData(data)

# use function to perform binning on a chosen variable
dat_bin = binning(dat, target='Var1', interval=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
