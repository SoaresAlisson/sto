% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{gsub2}
\alias{gsub2}
\title{transform string to first capitalized, except join words*.
A gsub to be used easily with native pipe |>
gsub2 is just a wrapper around gsub}
\usage{
gsub2(x, arg1, arg2 = "", ic = FALSE, ...)
}
\arguments{
\item{x}{data to search}

\item{arg1}{pattern}

\item{arg2}{replacement}

\item{...}{arguments passed to gsub.}
}
\description{
transform string to first capitalized, except join words*.
A gsub to be used easily with native pipe |>
gsub2 is just a wrapper around gsub
}
\examples{
c("a", "b", "c", "d") |> gsub2("a", "x")
# in the case of character
"a b c d" |> gsub2("a", "x")
# If no second argument is provided, than it will erase the pattern:
"'bla bla1 'bla" |> gsub2("'")
}
