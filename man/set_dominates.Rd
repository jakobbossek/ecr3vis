% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moo_domination.R
\name{set_dominates}
\alias{set_dominates}
\title{Check if one set Pareto-dominates another}
\usage{
set_dominates(x, y)
}
\arguments{
\item{x}{[\code{matrix}]\cr
First set of points.}

\item{y}{[\code{matrix}]\cr
Second set of points.}
}
\value{
Single logical value.
}
\description{
The function checks whether every point from the second set of points
is dominated by at least one point from the first set.
}
\examples{
x = matrix(c(1, 1, 2, 2, 1, 3), byrow = FALSE, ncol = 3L)
y = matrix(c(3, 4, 2, 2, 5, 6, 7, 7), byrow = FALSE, ncol = 4L)
set_dominates(x, y)
set_dominates(y, x)
}
\seealso{
Other Pareto-dominance checks: 
\code{\link{dominated}()},
\code{\link{dominates}()},
\code{\link{which_dominated}()}

Other multi-objective tools: 
\code{\link{dominated}()},
\code{\link{dominates}()},
\code{\link{which_dominated}()}
}
\concept{Pareto-dominance checks}
\concept{multi-objective tools}
