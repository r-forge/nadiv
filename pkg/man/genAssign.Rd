\name{genAssign}
\alias{genAssign}

\title{
Generation assignment}
\description{
Given a pedigree, the function assigns the generation number each individual belongs in.}
\usage{
genAssign(pedigree)
}
\arguments{
  \item{pedigree }{A pedigree with columns organized: ID, Dam, Sire
}
}
\details{
Zero is the base population.  

Adapted from \code{kindepth} function in \code{kinship}
} 

\seealso{\code{\link[kinship]{kinship}}}

\value{
A vector of values is returned.  This vector is in the same order as the ID column of the pedigree.
}


