% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{op-missing-default}
\alias{op-missing-default}
\alias{\%missing\%}
\title{Default value for `missing_arg`}
\usage{
x \%missing\% y
}
\arguments{
\item{x, y}{If `x` is missing, will return `y`; otherwise returns `x`.}
}
\description{
This infix function makes it easy to replace `missing_arg`s with a default
value. It's inspired by the way that rlang's `%||%` infix operator works.
}
\examples{
x <- rlang::missing_arg()
y <- x \%missing\% 2
}
