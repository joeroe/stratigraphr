% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cql.R
\name{cql_options}
\alias{cql_options}
\title{Set OxCal options in CQL}
\usage{
cql_options(
  bcad = TRUE,
  convergence_data = FALSE,
  curve = "intcal20.14c",
  cubic = TRUE,
  ensembles = 30,
  floruit = FALSE,
  intercept = FALSE,
  k_iterations = 30,
  plus_minus = FALSE,
  raw_data = FALSE,
  resolution = 5,
  round = FALSE,
  round_by = 0,
  sd1 = TRUE,
  sd2 = TRUE,
  sd3 = FALSE,
  uniform_span_prior = TRUE,
  use_f14c = TRUE,
  year = 1950.5,
  ...
)
}
\arguments{
\item{bcad}{Logical. Whether BC/AD are used in the log file output.}

\item{convergence_data}{Logical. Whether sample convergence data is included in the output data file.}

\item{curve}{Character. The default calibration curve.}

\item{cubic}{Logical. Whether cubic (as opposed to linear) interpolation is used for calibration curves.}

\item{ensembles}{Integer. The number of age-depth ensembles stored during the analysis.}

\item{floruit}{Logical. Whether quantile ranges are calculated instead of the default highest posterior density (hpd).}

\item{intercept}{Logical. Whether the intercept method is used for radiocarbon calibration ranges.}

\item{k_iterations}{Integer. The default number of MCMC passes.}

\item{plus_minus}{Logical. Whether + and - are used in place of BC and AD in log files.}

\item{raw_data}{Logical. Whether raw calibration curve data is included in the output data file.}

\item{resolution}{Integer. The default bin size for probability distributions of Date and Interval type.}

\item{round}{Logical. Whether ranges are rounded off.}

\item{round_by}{Integer. Resolution of rounding (0 for automatic).}

\item{sd1}{Logical. Whether 68.2\% (1 σ) ranges are given in the log and tab delimited files.}

\item{sd2}{Logical. Whether 95.4\% (2 σ) ranges are given in the log and tab delimited files.}

\item{sd3}{Logical. Whether 99.7\% (3 σ) ranges are given in the log and tab delimited files.}

\item{uniform_span_prior}{Logical. Whether the two extra prior factors suggested by Nicholls and Jones 2001 are used.}

\item{use_f14c}{Logical. Whether all calibrations take place in F14C space (rather than BP space).}

\item{year}{Numeric. The datum point for ages - the default is mid AD 1950.}

\item{...}{Additional named arguments converted to OxCal options. See details.}
}
\value{
A \code{cql} object.
}
\description{
This function generates a CQL command that sets OxCal's global options.
}
\details{
Parameter descriptions and defaults are taken from the OxCal v4.4
documentation (see references).

Options not explicitly included here can be specified as additional named
arguments to this function. The name must exactly match the name of the
option expected by OxCal; unknown or misspelled options are silently ignored.
}
\references{
\url{https://c14.arch.ox.ac.uk/oxcalhelp/hlp_analysis_detail.html#opt}
}
\seealso{
Other CQL functions: 
\code{\link{cql_boundary}()},
\code{\link{cql_n}()},
\code{\link{cql_other}},
\code{\link{cql_phase}()},
\code{\link{cql_r_date}()},
\code{\link{cql_sequence}()},
\code{\link{cql}()},
\code{\link{write_oxcal}()}
}
\concept{CQL functions}
