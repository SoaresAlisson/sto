#' show all stopwords categories of a language
#'
#' @param lang language, like "en", "pt"
#' @return an list object of stopwords
#' @export
#'
#' @examples
#' show_sw("pt")
#' show_sw("en")
show_sw <- function(lang, as_vector = FALSE) {
  # lang = "pt"
  file_name <- paste0("stopwords_", lang, ".yml") |> tolower()
  yaml_file_path <- system.file("stopwords", file_name, package = "sto")

  if (!yaml_file_path |> file.exists()) {
    paste0(
      'Error in "', lang,
      '": language not found. Please specify a valid language'
    ) |>
      stop()
  }

  list_sw <- yaml_file_path |>
    yaml::read_yaml() |>
    ls2v()
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
#' gen_stopwords(lang = "pt", categories = "V")
#' gen_stopwords(lang = "pt", categories = "V", vec = "list")
#' gen_stopwords(lang = "pt", categories = "V", vec = "n_vec")
#' gen_stopwords(lang = "pt", categories = "V", vec = "vec")
gen_stopwords <- function(lang = "pt", categories = "IN CC CD", vec = "vec", include = "") {
  # lang = "PT"

  # folder <- devtools::package_file("data/stopwords/")
  # file.exists(folder)
  file_name <- paste0("stopwords_", lang, ".yml") |> tolower()
  # file_name2 <- paste0("/data/stopwords/", file_name)
  # file_searched <- list.files(folder, pattern = file_name, full.names = T)
  # file_searched <- devtools::package_file(paste0( "/data/stopwords/", file_name))
  # file_searched <- devtools::package_file(paste0("/data/stopwords/", file_name))
  yaml_file_path <- system.file("stopwords", file_name, package = "sto")

  if (!yaml_file_path |> file.exists()) {
    paste0(
      'Error in "', lang,
      '": language not found. Please specify a valid language'
    ) |>
      stop()
  }

  # system.file( package = "sto")
  # # reading the yml filei
  list_sw <- yaml_file_path |>
    yaml::read_yaml() |>
    ls2v()
  # lapply(lapply(s2v)
  # list_sw[7] |> lapply(s2v)

  list_sw[["included"]] <- s2v(include) #|> stringr::str_to_title()

  categ_vec <- categories |>
    toupper() |>
    s2v()

  categ_vec <- c(categ_vec, "included")

  sw <- list_sw[categ_vec]

  if (vec == "n_vec") {
    sw <- unlist(sw)
  } else if (vec == "vec") {
    sw <- unlist(sw) |>
      unname() |>
      unique()
  }

  return(sw)

  # message(folder)
  # return(folder)
  # print("ola")
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
