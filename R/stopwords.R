#' show all stopwords categories of a language
#'
#' @param lang language, like "en", "pt"
#' @param add include additional words to the stop words list
#' @param as_vector return as vector
#' @return an list object of stopwords
#' @export
#'
#' @examples
#' show_sw("pt")
#' show_sw("en")
#' # as vector and adding some more stopwords
#' show_sw("en", T, "Abul Zoe")
show_sw <- function(lang = "en", as_vector = FALSE, add = "") {
  # lang = "pt"
  file_name <- paste0("stopwords_", lang, ".yml") |> tolower()
  yaml_file_path <- system.file("stopwords", file_name, package = "sto")

  if (!yaml_file_path |> file.exists()) {
    paste0(
      'Error in "', lang,
      '": language not found. Please specify a valid language, or consider contributing to this project with a new stopwords language'
    ) |>
      stop()
  }

  list_sw <- yaml_file_path |>
    yaml::read_yaml() |>
    ls2v()

  list_sw[["added"]] <- s2v(add) 

  if (as_vector) {
    sw <- list_sw |>
      unlist() |>
      unique() |>
      sort()
  } else {
    sw <- list_sw
  }
  return(sw)
}

#' check if vector or unit. If unit, break it into a vector
# add_words_check <- function(input) {
#   if (length(input) == 1) {
#     input <- s2v(input)
#   }
#
# }


#' Generates a stopwords list of terms
#' Function to generate a list of stopwords for a given language using grammar categories.
#'
#' @param lang language, like "en", "pt"
#' @param cat grammar categories, following penn bank
#' #param vec as vector: "list", "n_vec" return a named vector, "vec" (pattern) return an unnamed vector.
#' @param include include additional words to the stop words list
#' @export
#' @examples
#' gen_stopwords()
#' gen_stopwords(lang = "pt")
#' gen_stopwords(lang = "pt", categories = "IN V")
#' gen_stopwords(lang = "pt", categories = "V", vec = "list")
#' gen_stopwords(lang = "pt", categories = "V", vec = "n_vec")
#' gen_stopwords(lang = "pt", categories = "V", vec = "vec")
#' Easily adding more stopwords:
#' gen_stopwords(lang = "en", categories = "PP", add = "word1 word2")
gen_stopwords <- function(lang = "pt", categories = "CC CD DT", vec = "vec", add = NULL) {
  # lang = "PT"

  # folder <- devtools::package_file("data/stopwords/")
  # file.exists(folder)
  file_name <- paste0("stopwords_", lang, ".yml") |> tolower()
  # file_name2 <- paste0("/data/stopwords/", file_name)
  # file_searched <- list.files(folder, pattern = file_name, full.names = T)
  # file_searched <- devtools::package_file(paste0( "/data/stopwords/", file_name))
  # file_searched <- devtools::package_file(paste0("/data/stopwords/", file_name))
  yaml_file_path <- system.file("stopwords", file_name, package = "sto")

  if (! file.exists(yaml_file_path )) {
    paste0(
      'Error in "', lang,
      '": language not found. Please specify a valid language.'
    ) |>
      stop()
  }

  # # reading the yml file
  list_sw <- yaml_file_path |>
    yaml::read_yaml() |>
    ls2v()

  if (length(add) == 1) add <- s2v(add)
  # if (! is.null(add) ) list_sw[["added"]] <- add 
  if (! is.null(add) ) list_sw[["added"]] <- s2v(add) #|> stringr::str_to_title()

  categ_vec <- categories |>
    toupper() |>
    s2v()

  # test if categories in parameters really exists
  categ_vec_in_list_sw <- categ_vec %in% names(list_sw)

  if (any(! categ_vec_in_list_sw )) {

   cat_not_found <- categ_vec[!categ_vec_in_list_sw]

    paste0( 'Error in "categories" parameter. "', 
      cat_not_found ,'" no found. Please specify a valid category.'
    ) |>
      stop()
  }

  # append categories of the user
  categ_vec <- c(categ_vec, "added")

  sw <- list_sw[categ_vec]

  if (vec == "n_vec") {
    sw <- unlist(sw)
  } else if (vec == "vec") {
    sw <- unlist(sw) |>
      unname() |>
      unique()
  } else if ( vec == "list" ) {
    sw <- sw
  } else {
    stop(paste("Parameter invalid: ", vec))
  }
  return(sw)
}

#' to generate a dictionary of specialized words
#' you can use regex and the function check the dictionary of the language and returns the matched words.
#' It is also useful to text your regex pattern.
gen_dict <- function() {
}

#' returns a vector with words of a language.
#' The intent behind it is to test regex patterns
all_words <- function(lang) {
  lang
}
