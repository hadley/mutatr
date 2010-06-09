\name{ancestor_iterator}
\alias{ancestor_iterator}
\title{Ancestor iterator.}
\usage{ancestor_iterator(proto)}

\description{
  Ancestor iterator. This iterator has two methods \code{has_next} and
  \code{get_next} and iterates deep-first through the ancestry graph of
  a mutatr object.
}
\arguments{
  \item{proto}{the object with the ancestors}
}
\examples{A <- Object$clone()
B <- A$clone()
C <- B$clone()
ai <- mutatr:::ancestor_iterator(C)
while(ai$has_next()) print(ai$get_next())}
