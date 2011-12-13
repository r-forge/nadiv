\name{makeA}
\alias{makeA}
\title{Creates the additive genetic relationship matrix
}
\description{Using a pedigree, this returns the additive relationship matrix.
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
\value{Returns A, or the numerator relationship matrix, in sparse and dense matrix form.

  \item{Asparse }{A in sparse matrix form}
  \item{Adense }{A in dense matrix form}
}

\references{Meuwissen, T.H.E. & Luo, Z. 1992. Computing inbreeding coefficients in large populations. Genetics, Selection, Evolution 24, 305-313.}

\author{\email{matthewwolak@gmail.com}}

\seealso{\code{\link{makeD}}}

\examples{
 makeA(Mrode2)
}

