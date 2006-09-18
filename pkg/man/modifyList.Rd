\name{modifyList}
\alias{modifyList}
\title{Recursively Modify Elements of a list}
\description{
  Modifies a possibly nested list recursively by changing a subset of
  elements at each level to match a second list.
}

\usage{
modifyList(x, val)
}

\arguments{
  \item{x}{ named list, possibly empty }
  \item{val}{ named list with components to replace corresponding
    components in \code{x} } }

\value{

  A modified version of \code{x}, with the modifications determined as
  follows (here, list elements are identified by their names).  Elements
  in \code{val} which are missing from \code{x} are added to \code{x}.
  For elements that are common to both but are not both lists
  themselves, the component in \code{x} is replaced by the one in
  \code{val}.  For common elements that are both lists, \code{x[[name]]}
  is replaced by \code{modifyList(x[[name]], val[[name]])}.
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\keyword{utilities}
