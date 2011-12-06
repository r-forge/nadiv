\name{makeA}
\alias{makeA}
\title{Creates the additive genetic relationship matrix
}
\description{Using a pedigree, this returns the kinship matrix.  Note, elements are the coefficient of coancestry, not two times this value.  The latter being what is typically used when conducting genetic analysis using the 'animal model'.
}
\usage{
makeA(pedigree)
}
\arguments{
  \item{pedigree }{
A pedigree where the columns are ordered ID, Dam, Sire}
}
\details{
Used as a support function to \code{\link{makeD}}.  Uses the algorithm of Meuwissen and Luo (1992)}
\value{Returns A, or the numerator relationship matrix, in sparse matrix form.}

\references{Meuwissen, T.H.E. & Luo, Z. 1992. Computing inbreeding coefficients in large populations. Genetics, Selection, Evolution 24, 305-313.}

\author{\email{matthewwolak@gmail.com}}

\seealso{\code{\link{makeD}}}

\examples{
data(Mrode2)
Mrode2<-Mrode2
makeA(Mrode2)
}

