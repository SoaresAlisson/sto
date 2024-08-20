# Functions

#' To easily paste and collpase chars objects into one string
#' A wrapper for glue::glue
#' @export
f <- glue::glue
library(magrittr)

#' transform string to firts capitalized, except join words*
# to_title <- function
# stringr::str_to_title(orgaoJulgadorPorExtenso, locale = "pt"),
#         orgaoJulgadorPorExtenso = gsub(" D([aeo]) ", " d\\1 ", orgaoJulgadorPorExtenso

#' A gsub to be used easily with native pipe |>
#' gsub2 is just a wrapper around gsub
#' @param x data to search
#' @param arg1 pattern
#' @param arg2 replacement
#' @param ... arguments passed to gsub.
#' @export
#' @examples
#' c("a", "b", "c", "d") |> gsub2("a", "x")
#' # in the case of character
#' "a b c d" |> gsub2("a", "x")
#' # If no second argument is provided, than it will erase the pattern:
#' "'bla bla1 'bla" |> gsub2("'")
gsub2 <- function(x, arg1, arg2 = "", ic = FALSE, ...) {
  gsub(arg1, arg2, x, ignore.case = ic)
}

#' A grep to be used with native pipe |>
#'
#' grep2 is a wraper around the traditional grep, that works with native pipe,
#' returns value and are ignore case as default
#' @param x data to search
#' @param arg1 pattern
#' @param ic ignore case
#' @param ... arguments passed to grep. See ?grep for more details
#' @export
#' @examples
#' c("a", "b", "c", "d") |> grep2("a")
grep2 <- function(x, arg1, ic = T, value = TRUE, ...) {
  grep(arg1, x, ignore.case = ic, value = value, ...)
}

#' grepl to be used with native pipe |>
#' @param x data to search
#' @param arg1 pattern
#' @param ic ignore case
#' @param ... arguments passed to grepl.
#' @export
#' @examples
#' c("a", "b", "c", "d") |> grepl2("a")
grepl2 <- function(x, arg1, ic = TRUE, ...) {
  grepl(arg1, x, ignore.case = ic, ...)
}


#' convert a string to vector of elements.
#' Easily transform a character string into a vector of elements.
#' @param char character to be transformed
#' @param sep separator
#' @param wss whitespace substitution. Which character will be used in the end to be converted into white space
#' @param print if TRUE, will return a string that can be pasted in the console
#' @export
#' @examples
#' "a b c d" |> s2v(r
#' # strips white spaces
#' "a b c     d" |> s2v()
#' "a b c\nd\te" |> s2v()
#' # And specifying the separator character:
#' "a|b|c|d e" |> s2v(sep = "\\|")
#' # To use the output verbatim as a string to copy and use in the code
#' "a b c d" |> s2v(print = TRUE)
s2v <- function(char, sep = " |\\n|\\t|\\r", wss = "_", print = FALSE) {
  vec <- char |>
    strsplit(sep) |>
    unlist() |>
    gsub2(" +", " ") |> # strip extra white spaces
    gsub2("[,;]", " ") |> # strip comma
    stringi::stri_remove_empty() |>
    gsub2("^ | $", "") |> # strip extra white spaces
    gsub2(wss, " ")

  if (!print) {
    return(vec)
  } else {
    vec |>
      paste(collapse = "', '") |>
      gsub2("^", "c('") |>
      gsub2("$", "')")
  }
}


#' list of strings to list of vectors
#' string to vec from list of nested elements of text
ls2v <- function(char, sep = " |\\n|\\t|\\r", wss = "_") {
  # apply_strsplit_recursive <- function(x, split = " |\\n|\\t|\\r", wss = "_") {
  if (is.atomic(char)) {
    char |>
      strsplit(sep) |>
      unlist() |>
      gsub2(" +", " ") |> # strip extra white spaces
      gsub2("[,;]", " ") |> # strip comma
      stringi::stri_remove_empty() |>
      gsub2("^ | $", "") |> # strip extra white spaces
      gsub2(wss, " ")
  } else {
    lapply(char, ls2v, sep = sep)
  }
}
# char |>
# list_sw |> strsplit(sep)
# list_sw |> lapply(strsplit, sep)
# list_sw[7] |> lapply(strsplit, sep)
# apply_strsplit_recursive(list_sw)
#|>
# }


#' load libraries from string
#' load packages from a char separated by space or commas
#' @export
#' @examples
#' sto::ll("rvest stringr dplyr")
ll <- function(char, ...) {
  gsub(",|;", " ", char) |>
    s2v() |>
    stringi::stri_remove_empty() |>
    lapply(library, character.only = TRUE, ...)
}

#' install libraries from string
#' given a char with package names, separated by spaces and/or commas, install all the packages
#' @param char string with package names
#' @param installer choose which installer to be used: "install.package" (default), "pak", "dev_git" for devtools::install_github
#' @param ... Additional parameters
il <- function(char, installer = "install.package", ...) {
  #  message("to be installed")
  vec_packages <- gsub(",|;", " ", char) |>
    stringi::stri_remove_empty() |>
    s2v()
  if (installer == "install.package") {
    vec_packages |> lapply(install.packages, ...)
  } else if (installer == "dev_git") {
    vec_packages |> lapply(devtools::install_github, ...)
  } else if (installer == "pak") {
    vec_packages |> lapply(pak::pkg_install, ...)
  }
}
#' generate shuffle times
#' easily generate shuffle time, good to use in webscraping
#' @export
shuffle_time <- function(min_time, max_time) {
  miliseconds <- sample(1:60, 1)
  seconds <- sample(min_time:max_time, 1)
  paste0(seconds, ".", miliseconds) |> as.numeric()
}


#' count a vector of elements, arragange it or not, and returns a tibble
#' @export
#' @examples
#' vec <- s2v("a a b c a b a z z z c d e")
#' vec |> count_vec()
count_vec <- function(vec, sort_n = TRUE) {
  vec_count <- vec |>
    unlist() |>
    plyr::count()
  if (sort_n) {
    vec_count <- vec_count |> dplyr::arrange(-freq)
  }
  tibble::as_tibble(vec_count)
}

#' extract all chars
#' Inspired by stringr::str_extract_all(), but with ignore case option
#' @export
#' @examples
#' "bla bla bla Foo ble ble" |> s_extract_all("foo")
#' c("bla bla bla Foo ble", "Lorem FOO ipsum") |> s_extract_all("foo", unl = T)
s_extract_all <- function(txt, pattern, IC = TRUE, unl = FALSE) {
  # x = "bla bla bla Foo ble ble"
  # pattern <- "foo"
  rgx <- paste0("(.*)(", pattern, ")(.*)")
  list_str <- lapply(txt, \(x) {
    gsub(rgx, "\\2", x, ignore.case = IC)
  })
  if (unl) {
    list_str <- list_str |> unlist()
    return(list_str)
  } else {
    return(list_str)
  }
}

# para erros na requisisção, ver tribunaisTrabalho TODO2
