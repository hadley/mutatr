\name{get_slot}
\alias{get_slot}
\title{Get slot...}
\usage{get_slot(obj, name, scope=obj)}

\description{
  Get slot
}

\details{
  Function that powers inheritance.  Given an object and a slot name
  iterators through ancestors looking for slots that match that name.

  If the slot is a function, it adjusts the function scope (with
  \code{object_scope}) so that the self context is set correctly.
}
\keyword{internal}
\arguments{
  \item{obj}{object in which to look for slot}
  \item{name}{name of slot to look for}
  \item{scope}{self object to use for object context}
}
