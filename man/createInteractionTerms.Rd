\name{createInteractionTerms}
\alias{createInteractionTerms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Interaction Terms
%%  ~~function to do ... ~~
}
\description{
This function creates interaction terms between numeric columns. Essentially, it multiplies each column with one another and adds these to the input data.
}
\usage{
createInteractionTerms(input, target, no.use, na.rm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
an object of type data.frame. Preferably the output of reshapeData().
}
  \item{target}{
a mandatory character string defining the variable that is to be analyzed eventually and therefore not included in the computation.
}
  \item{no.use}{
an optional character string that defines other variables that should not be used.
}
  \item{na.rm}{
optional logical value True or False. If True, all rows with missing values are omitted. Default is False.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of type data.frame in the same form as data including the additonally created columns.
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
data = data.frame('id'=rep(c(1:5), each=6),
      'type'=rep(c('Var1', 'Var2', 'Var3'), times=10),
      'value'=rep(c(1:5), times=6),
      'date'=rep(seq(as.Date("2000/1/1"), by = "day", length.out=15), each=2))

# create rectangle version of user journey
dat = reshapeData(data)

# use function to create interaction terms and exclude the variable 'mood' from the computation
datInteract = createInteractionTerms(dat, target='mood', no.use=F)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
