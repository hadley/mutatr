\name{core-bootstrap}
\title{Create the core object.}

\description{
  Create the core object.
}

\details{
  Boostrapping core - mostly implements usual R access, but uses object
  scoping.  Functions CAN NOT call other object functions because scope
  will be incorrect (functions not explicitly accessed via $.core will
  still have lexical scope).

  Should implement: * protos, append_proto, set_proto, get_proto *
  has_slot, get_slot, set_slot, slot_names * clone * do
}
\keyword{internal}
\alias{Core}
\alias{$.core}
\alias{print.core}
\alias{format.core}
