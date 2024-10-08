% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stopwords.R
\name{gen_stopwords}
\alias{gen_stopwords}
\title{Generates a stopwords list of terms
Function to generate a list of stopwords for a given language using grammar categories.}
\usage{
gen_stopwords(lang = "pt", categories = "IN CC CD", vec = "vec", include = "")
}
\arguments{
\item{lang}{language, like "en", "pt"}

\item{include}{include additional words to the stop words list}

\item{cat}{grammar categories, following penn bank
#param vec as vector: "list", "n_vec" return a named vector, "vec" (pattern) return an unnamed vector.}
}
\description{
Generates a stopwords list of terms
Function to generate a list of stopwords for a given language using grammar categories.
}
\examples{
gen_stopwords()
gen_stopwords(lang = "pt")
gen_stopwords(lang = "pt", categories = "V")
gen_stopwords(lang = "pt", categories = "V", vec = "list")
gen_stopwords(lang = "pt", categories = "V", vec = "n_vec")
gen_stopwords(lang = "pt", categories = "V", vec = "vec")
}
