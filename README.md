
<!-- README.md is generated from README.Rmd. Please edit that file -->

# String Operations - sto

<!-- badges: start -->
<!-- badges: end -->

The goal of {sto} is to make easier some of the tedious operations
involving strings and text analysis. It comes with three main types of
functions: 1) You can use it to quickly create vectors from strings,
load a series os packages more easily, and perform other operations like
`grep`, `grepl` and `gsub` with native pipe. 2) {sto} also provides
functions for fine grain generate stopword lists by language and grammar
categories. Most of the existing stopwords packages only provides a
vector of words. 3) Additionally, {sto} comes with a rule-based proper
name/entity extractor that returns vectors, lists, or graphs
representing binary co-occurrence in sentences or paragraphs.

**Keywords**: strings; text analysis; text mining

## Installing String Operations

Install using {devtools} package

``` r
install.packages("devtools") # if devtools is not installed yet
devtools::install_github("SoaresAlisson/sto")
```

Or install using {pak} package

``` r
install.packages("pak", dependencies = TRUE) # if {pak} is not installed yet
pak::pkg_install("SoaresAlisson/sto")
```

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

See the vignettes.
