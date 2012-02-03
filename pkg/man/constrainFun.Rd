\name{constrainFun}
\alias{constrainFun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
constrainFun(parameter.val, full, fm2, comp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{parameter.val}{
%%     ~~Describe \code{parameter.val} here~~
}
  \item{full}{
%%     ~~Describe \code{full} here~~
}
  \item{fm2}{
%%     ~~Describe \code{fm2} here~~
}
  \item{comp}{
%%     ~~Describe \code{comp} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(parameter.val, full, fm2, comp)
  {
  row <- which(fm2$Gamma == comp)
  fm2[row, 2:3] <- c(parameter.val, "F")
  full$G.param <- fm2
  con.mod <- update.asreml(object = full, trace = FALSE)
  if(con.mod$converge) return((-2*(full$loglik - con.mod$loglik))) else return(-998)
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
