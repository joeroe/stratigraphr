% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cql.R
\name{cql_boundary}
\alias{cql_boundary}
\alias{cql_sigma_boundary}
\alias{cql_tau_boundary}
\alias{cql_zero_boundary}
\alias{cql_transition}
\title{Describe a boundary constraint in CQL}
\usage{
cql_boundary(name, ..., prior = NULL)

cql_sigma_boundary()

cql_tau_boundary()

cql_zero_boundary()

cql_transition(name, prior = NULL)
}
\arguments{
\item{name}{Character. Label for a boundary or transition.}

\item{...}{\code{cql} objects contained within a boundary.}

\item{prior}{\code{cql} object. Expression describing the prior likelihood of
a boundary or transition.}
}
\value{
A \code{cql} object.
}
\description{
The CQL command \code{Boundary} describes constraints within an ordered sequence
(see \code{\link[=cql_sequence]{cql_sequence()}}). Groups of events between two boundaries are assumed
to be sampled from the same prior distribution. \code{Boundary} alone models a
uniform prior. Other types of boundary, i.e. \code{Sigma_Boundary}, \code{Tau_Boundary},
and \code{Zero_Boundary} are not yet implemented in stratigraphr.

Boundaries can contain a \code{Transition} command, in which case they describe
a non-instantaneous transition or 'trapezium' prior.
}
\examples{
# Uniform model with unknown boundaries
cql(
  cql_boundary("P1 start"),
  cql_phase("P1", list(
    cql_r_date("A", 5050, 30),
    cql_r_date("B", 5000, 30),
    cql_r_date("C", 4950, 30)
  )),
  cql_boundary("P1 end")
)

# Uniform model with boundaries with a prior likelihood
cql(
  cql_boundary("P1 start",
               prior = cql_date("P1S-Prior", cql_u("", 5200, 5100))),
  cql_phase("P1", list(
    cql_r_date("A", 5050, 30),
    cql_r_date("B", 5000, 30),
    cql_r_date("C", 4950, 30)
  )),
  cql_boundary("P1 end",
               prior = cql_date("P1E-Prior", cql_u("", 4800, 4900)))
)

# 'Trapezium' model
cql(
  cql_boundary("P1 start",
    cql_transition("")
  ),
  cql_phase("P1", list(
    cql_r_date("A", 5050, 30),
    cql_r_date("B", 5000, 30),
    cql_r_date("C", 4950, 30)
  )),
  cql_boundary("P1 end",
    cql_transition("")
  )
)
}
\references{
\url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_oper.html#group}

\url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_commands.html}
}
\seealso{
Other CQL functions: 
\code{\link{cql_n}()},
\code{\link{cql_options}()},
\code{\link{cql_other}},
\code{\link{cql_phase}()},
\code{\link{cql_r_date}()},
\code{\link{cql_sequence}()},
\code{\link{cql}()},
\code{\link{write_oxcal}()}
}
\concept{CQL functions}
