rgx_abbrev <- "([:upper:]\\.){2,}"
rgx_word <- "(\\b[A-ZÀ-Ÿ][[A-ZÀ-Ÿ][a-zà-ÿ]\\.\\-]+\\b)"


#' A lowercase connectors between two proper names
#'
#' In some languages there is a lowercase connector between two or more proper names.
#' This function returns a regex pattern by language with lowercase allowed connectors.
#' @param lang language, It can be en, es, pt and misc (with many languages)
#' @export
#' @examples
#'
#' connectors("es")
#' connectors("pt")
#' connectors("en")
#' connectors("misc")
connectors <- function(lang = "pt") {
  if (lang == "pt") {
    conn <- "da das de do dos"
  } else if (lang == "es") {
    conn <- "del"
  } else if (lang == "en") {
    conn <- "of of_the"
  } else if (lang == "misc") {
    conn <- "of the of_the von van del"
  } else {
    paste("Lang not found:", lang) |> stop()
  }

  con_v <- conn |> s2v()

  paste(con_v, collapse = "|") |>
    gsub2("(.*)", "(\\1)")
}

#' A rule based entity extractor
#' extracts the entity from a text using regex. This regex captures all uppercase words, words that begin with upper case. If there is sequence of this patterns together, this function also captures.
#' In the case of proper names with common lower case connectors like "Wwwww of Wwwww" this function also captures the connector and the subsequent uppercase words.
#' @param text an input text
#' @param connectors a vector of lowercase connectors. Use use your own, or use the function "connector" to obtain some patterns.
#' @param sw a vector of stopwords
#' @export
#' @examples
#' "John Does lives in New York in United States of America" |> extract_entity()
#' "João Ninguém mora em São José do Rio Preto" |> extract_entity(connector = connectors("pt"))
# text |> extract_entity()
extract_entity <- function(text, connector = connectors("misc"), sw = "the") {
  rgx_ppn <- paste0("(", rgx_word, "+ ?)+", "", connector, "? (", rgx_word, " ?)*")

  # text <- texto
  text_vec <- text |>
    stringr::str_extract_all(rgx_ppn) |>
    unlist() |>
    unique() |>
    trimws()
  # deleting stopword elements
  text_vec[!text_vec %in% stringr::str_to_title(sw)]
}



#' tokenize and selects only sentences/paragraphs with more than one entity per sentence or paragraph
#' @export
#' @examples
#' "John Does lives in New York in United States of America." |> extract_relation()
#' "João Ninguém mora em São José do Rio Preto. Ele foi para o Rio de Janeiro." |> extract_relation(connector = connectors("pt"))
extract_relation <- function(text, using = "sentences",
                             connectors = connectors("misc"),
                             sw = gen_stopwords("en")) {
  message("iniciando")
  if (using == "sentences" || using == "sent") {
    message("Using sentences")
    list_w <- text |>
      tokenizers::tokenize_sentences()
  } else if (using == "paragraph" || using == "par") {
    message("Using paragraph")
    list_w <- text |>
      tokenizers::tokenize_paragraphs()
  } else {
    stop("Parameter invalid")
  }

  list_w <- list_w |>
    lapply(extract_entity, using, connectors, sw)

  list_length <- list_w |>
    lapply(length) |>
    unlist()

  # selecting only sentences with more than one entity
  list_w[list_length > 1] #|> lapply(combn, 2, simplify = TRUE)
}

#' Extract a non directional graph based on co-occurrence in sentence or paragraph.
#' It extracts only if two entities are mentioned in the same sentence or paragraph
#' @param text an input text
#' @param using sentence or paragraph to tokenize
#' @param connectors lowercase connectors, like the "von" in "John von Neumann".
#' @param sw stopwords
#' @export
#' @examples
#' "John Does lives in New York in United States of America" |> extract_relation()
# extract_graph(text)
extract_graph <- function(text, using = "sentences", connectors = connectors("misc"), sw = "") {
  list_ent <- text |> extract_relation(using, using, connectors, sw)
  graph <- tibble::tibble(n1 = as.character(""), n2 = as.character(""))
  # list_length <- list_ent |> length()
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
