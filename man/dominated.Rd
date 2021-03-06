% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moo_domination.R
\name{dominated}
\alias{dominated}
\alias{nondominated}
\title{Check for Pareto-dominance}
\usage{
dominated(x)

nondominated(x)
}
\arguments{
\item{x}{[\code{matrix}]\cr
Numeric \eqn{(m \times n)} matrix where \eqn{m} is the number of objectives
and \eqn{n} is the number of points.}
}
\value{
Logical vector where the \eqn{i}th component is \code{TRUE} if
  \code{x[i]} is dominated or nondominated respectively and \code{FALSE}
  otherwise.
}
\description{
These functions take a numeric matrix as input where each column corresponds to
a point and return a logical vector. The \eqn{i}th position of the latter is
\code{TRUE} if the \eqn{i}th point is dominated by at least one other point for
\code{dominated}. Likewise, the \eqn{i}th entry of \code{nondominated} is \code{TRUE}
if it is not dominated by any other point.
}
\examples{
x = matrix(c(1, 1, 2, 2, 1, 3), byrow = FALSE, ncol = 3L)
dominated(x)
nondominated(x)
}
\seealso{
Other Pareto-dominance checks: 
\code{\link{dominates}()},
\code{\link{set_dominates}()},
\code{\link{which_dominated}()}

Other multi-objective tools: 
\code{\link{dominates}()},
\code{\link{set_dominates}()},
\code{\link{which_dominated}()}
}
\concept{Pareto-dominance checks}
\concept{multi-objective tools}
\keyword{optimize}
