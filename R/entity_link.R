rgx_abbrev <- "([:upper:]\\.){2,}"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][[A-ZÀ-Ÿ][a-zà-ÿ]\\.\\-]+\\b)"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][A-ZÀ-Ÿa-zà-ÿ0-9\\.\\-]+\\b)"
# unicode in order https://symbl.cc/en/unicode-table/#spacing-modifier-letters
rgx_word <- "(\\b[A-ZÀ-ß][A-ZÀ-ßa-zà-ÿ0-9\\.\\-]+\\b)"



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
#' connectors("port")
#' connectors("en")
#' connectors("misc")
connectors <- function(lang = "pt") {
  if (lang %in% s2v("pt por port portugues português portuguese")) {
    conn <- "da das de do dos"
  } else if (lang %in% s2v("es spa spanish espanol español")) {
    conn <- "del"
  } else if (lang %in% s2v("en eng english")) {
    conn <- "of of_the"
  } else if (lang == "misc") {
    conn <- "of the of_the von van del"
  } else {
    paste("Lang not found:", lang) |> stop()
  }

  conn |> s2v()
}

#' A rule based entity extractor
#' extracts the entity from a text using regex. This regex captures all uppercase words, words that begin with upper case. If there is sequence of this patterns together, this function also captures.
#' In the case of proper names with common lower case connectors like "Wwwww of Wwwww" this function also captures the connector and the subsequent uppercase words.
#' @param text an input text
#' @param connect a vector of lowercase connectors. Use use your own, or use the function "connector" to obtain some patterns.
#' @param sw a vector of stopwords
#' @export
#' @examples
#' "John Does lives in New York in United States of America." |> extract_entity()
#' "João Ninguém mora em São José do Rio Preto. Ele esteve antes em Sergipe" |> extract_entity(connect = connectors("pt"))
# text |> extract_entity()
extract_entity <- function(text, connect = connectors("misc"), sw = "the") {
  # connectors <- connectors |> s2v()
  connector <- paste(connect, collapse = "|") |> gsub2("(.*)", "(\\1)")

  # rgx_ppn <- paste0("(", rgx_word, "+ ?)+", "", connector, "? (", rgx_word, " ?)*")
  rgx_ppn <- paste0("(", rgx_word, "+ ?)+", "(", connector, "? (", rgx_word, " ?)+)*")

  # text <- texto
  text_vec <- text |>
    stringr::str_extract_all(rgx_ppn) |>
    unlist() |>
    # unique() |>
    stringr::str_trim()
  # trimws()
  # deleting stopword elements
  text_vec[!text_vec %in% stringr::str_to_title(sw)]
}



#' tokenize and selects only sentences/paragraphs with more than one entity per sentence or paragraph
#' @param text an input text
#' @param using sentence or paragraph to tokenize
#' @param connect lowercase connectors, like the "von" in "John von Neumann". To use pre built connectors use `connectors()``
#' @param sw stopwords vector. To use pre built stopwords use `gen_stopwords()`
#' @export
#' @examples
#' "John Does lives in New York in United States of America." |> extract_relation()
#' "João Ninguém mora em São José do Rio Preto. Ele foi para o Rio de Janeiro." |> extract_relation(connector = connectors("pt"))
extract_relation <- function(text, using = "sentences",
                             connect = connectors("misc"),
                             sw = gen_stopwords("en")) {
  if (using == "sentences" || using == "sent") {
    message("Tokenizing by sentences")
    list_w <- text |>
      tokenizers::tokenize_sentences()
  } else if (using == "paragraph" || using == "par") {
    message("Tokenizing by paragraph")
    list_w <- text |>
      tokenizers::tokenize_paragraphs()
  } else {
    stop(paste("Parameter invalid: ", using))
  }

  list_w <- lapply(
    X = list_w, \(txt) {
      extract_entity(txt,
        connect = connect, sw = sw
      )
    }
  )

  list_length <- list_w |>
    lapply(length) |>
    unlist()

  # selecting only sentences with more than one entity
  list_w[list_length > 1] #|> lapply(combn, 2, simplify = TRUE)
}

#' Extract a non directional graph based on co-occurrence in the token.
#' It extracts only if two entities are mentioned in the same token (sentence or paragraph)
#' @param text an input text
#' @param using sentence or paragraph to tokenize
#' @param connect lowercase connectors, like the "von" in "John von Neumann".
#' @param sw stopwords vector.
#' @param loop if TRUE, it will not remove loops, a node pointing to itself.
#' @export
#' @examples
#' text <- "John Does lives in New York in United States of America. He  is a passionate jazz musician, often playing in local clubs."
#' extract_graph(text)
extract_graph <- function(text, using = "sentences",
                          connect = connectors("misc"),
                          sw = gen_stopwords("en"),
                          loop = FALSE) {
  list_ent <- text |> extract_relation(using, connect, sw)
  graph <- tibble::tibble(n1 = as.character(""), n2 = as.character(""))
  # list_length <- list_ent |> length()
  graph <- lapply(list_ent, \(e) {
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

  if (!loop) {
    graph <- graph |>
      dplyr::mutate(loop = (n1 == n2)) |>
      dplyr::filter(loop == FALSE) |>
      dplyr::select(-loop)
  }
  return(graph)
}

#' plot a network of coocurrence of terms
#'
#' plot a graph of co-occurrence of terms, as returned by extract_graph
#' @param text an input text
#' @param df a dataframe of co-occurrence, extracted with `extract_graph()` and `count(n1, n2)`
#' @param head_n number of nodes to show - the more frequent
#' @export
plot_graph <- function(text, df, head_n = 30, color = "lightblue") {
  # graph <-  g_N |> head(head_n)
  graph <- df |> head(head_n)
  vert <- unique(c(graph$n1, graph$n2))

  # frequency of nodes/terms
  freqPPN <- lapply(vert, \(v) {
    text |> stringr::str_extract_all(v)
  }) |>
    unlist() |>
    count_vec()

  graph |>
    igraph::graph_from_data_frame(directed = FALSE, vertices = freqPPN) |>
    ggraph::ggraph(layout = "graphopt") +
    ggraph::geom_edge_link(ggplot2::aes(edge_width = n, edge_alpha = 0.5),
      angle_calc = "along",
      label_dodge = grid::unit(4.5, "mm"),
      color = color,
      # c("lightblue", "blue", "royalblue")[1],
      end_cap = ggraph::circle(6, "mm")
    ) + # afastamento do nó
    ggraph::geom_node_text(ggplot2::aes(label = name, size = freq), repel = TRUE) +
    # ggraph::geom_node_label(ggplot2::aes(label = name), repel=TRUE,  point.padding = unit(0.2, "lines")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
