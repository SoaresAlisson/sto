library(rvest)
library(stringr)
library(magrittr)

# URL <- "https://economia.uol.com.br/noticias/redacao/2024/08/18/lula-diz-que-silvio-santos-tinha-medo-de-ser-preso-por-caso-panamericano.htm"
# pagina <- rvest::read_html(URL)
# texto <- pagina |> rvest::html_nodes("p") |> rvest::html_text()
# saveRDS(texto, "./dev/texto.rds")
# texto <- readRDS("./dev/texto.rds")
s2v <- function(char) {
  char |>
    strsplit(" ") |>
    unlist()
}
sw <- "de as ao em como porque sem ele por este esse a o os as no quando" |> s2v()
texto <- "texto.txt" |> readLines()
# list.files()
# regex <- "\\W{2,}"
# rgx_word <- "(\\b[:upper:][:lower:]+\\b)"
# rgx_word <- "(\\b[:upper:][[:upper:][:lower:]\\.\\-]+\\b)"
# rgx_word <- "(\\b[:upper:][[:upper:][:lower:]\\.\\-]+\\b ?)"

#' a lowercase connector between two propernames
connector <- function(lang = "pt") {
  if (lang == "pt") {
    conn <- "da das de do dos"
  } else if (lang == "es") {
    conn <- "del"
  } else if (lang == "en") {
    conn <- "of"
  } else {
    paste("Lang not found:", lang) |> stop()
  }

  conn |>
    s2v() %>%
    paste(., collapse = "|") %>%
    gsub("(.*)", "(\\1)", .) |>
    tolower()
}

# rgx_word <- "(\\b[A-Z<U+00C0>-<U+00DA>][[A-Z<U+00C0>-<U+00DA>][a-z<U+00E0>-<U+00FA>]\\.\\-]+\\b ?)+"
# rgx_word <- "(\\b[A-Z][[A-Z][a-z]\\.\\-]+\\b ?)+"
rgx_word <- "(\\b[A-Z][[A-Z][a-z]\\.\\-]+\\b)" # TODO
rgx_ppn <- paste0("(", rgx_word, "+ ?)+", "(", connector, " ", rgx_word, ")*")
# rgx_ppn <- paste0("(", rgx_word, "+ ?)( ", connector, " ", rgx_word, "+ ?)*")
rgx_abbrev <- "([:upper:]\\.){2,}"

#' A rule based entity extractor
#' extracts the entity from a text using regex. This regex captures all uppercase words, words that begin with upper case. If there is sequence of this patterns together, this function also captures.
#' In the case of proper names with common lower case connectors like "Wwwww of Wwwww" this function also captures the connector and the subsequent uppercase words.
#' @param text an input text
#' @param connectors a vector of lowercase connectors. Use use your own, or use the function "connector" to obtain some patterns.
#' @export
#'
extract_entity <- function(text, connector = connector("pt"), sw) {
  rgx_ppn <- paste0("(", rgx_word, "+ ?)+", "(", connector, " ", rgx_word, ")*")

  # text <- texto
  text_vec <- text |>
    str_extract_all(rgx_ppn) |>
    unlist() |>
    unique() |>
    trimws()
  # deleting stopword elements
  text_vec[!text_vec %in% stringr::str_to_title(sw)]
}
texto |> extract_entity()

extract_relation <- function(text, using = "sentences") {
  list_w <- text |>
    tokenizers::tokenize_sentences() |>
    lapply(extract_entity)
  list_length <- list_w |>
    lapply(length) |>
    unlist()
  list_w[list_length > 1] #|> lapply(combn, 2, simplify = TRUE)
}

#' Extract a non directional graph based on coocurrence in sentence or paragraph.
#' It extracts only if two entities are mentioned in the same sentence or paragraph
#' extrac_graph(texto)
extrac_graph <- function(text) {
  list_ent <- text |> extract_relation()
  graph <- tibble::tibble(n1 = as.character(""), n2 = as.character(""))
  list_length <- list_ent |> length()
  lapply(list_ent, \(e) {
    items <- e |> combn(2, simplify = FALSE)
    items_length <- length(items)
    lapply(1:items_length, \(x) {
      line <- unlist(c(items[x][1], items[x][2]))
      graph <- rbind(graph, line)
      graph
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(n1 != "")
  }) |>
    dplyr::bind_rows()
}

# [A-z<U+00C0>-<U+00FA>] // accepts lowercase and uppercase characters
# [A-z<U+00C0>-<U+00FF>] // as above, but including letters with an umlaut (includes [ ] ^ \ <U+00D7> <U+00F7>)
# [A-Za-z<U+00C0>-<U+00FF>] // as above but not including [ ] ^ \
# [A-Za-z<U+00C0>-<U+00D6><U+00D8>-<U+00F6><U+00F8>-<U+00FF>] //
# \u00C0-\u017F
