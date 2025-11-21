#' Convert a string in CSV like format into a dataframe/tibble
#'
#' @description
#' There is many ways to create a dataframe/tibble, one of them is tibble::tribble,
#' but you must delimit strings between quotes, each item, each cell,  what can be
#' very annoyng. With s2df, in the other hand, is possible to create a tibble of
#' strings much more easier. It can be used, for example, in a pipe to replace
#' many items in a text.
#'
#' @param input_string A character string in the format "col1;col2\nvalue1;value2".
#'   The first line should be the header row.
#' @param header if the fist line must be considered the header or not. Default TRUE
#' @param sep the separator of values. Default `;`.
#'
#' @return A `tibble` with columns named according to the header and rows populated with data from the subsequent lines.
#'
#' @export
#'
#' @examples
#' s <- "col1;col2
#' bla1;10
#' bla2;23
#' bla3;32
#' "
#' s2df(s)
#'
#' # example without column name specified
#' s2 <- "bla1,10\nbla2,23\nbla3,32"
#' s2df(s2, header = FALSE, sep = ",")
#'
#' # example of text substitution
#' substitutions <- s2df("replace_this;by\nNew York;NewYork\nSoviet.Union;SovietUnion")
#' substitutions
#' text <- "Ice-cream in Soviet Union or New York?"
#' stringr::str_replace_all(text,  tibble::deframe(substitutions))
#'
s2df <- function(input_string, header = TRUE, sep = ";") {
  # if input_string is empty
  if (length(input_string) <= 1) {
    stop("The input provided is empty")
  }

  # Split the input string by new lines to get individual rows
  rows <- strsplit(input_string, "\n")[[1]]

  # Remove empty rows if any
  rows <- Filter(Negate(is.null), rows)

  if (header) {
    # Extract headers and data
    df_header <- unlist(strsplit(rows[1], sep))
    data_lines <- rows[-1]
  } else {
    data_lines <- rows
  }

  # Split each data line into columns and create a matrix
  data_matrix <- do.call(
    rbind,
    lapply(data_lines, function(line) {
      unlist(strsplit(line, sep))
    })
  )

  # Convert the matrix to a tibble
  result_tibble <- tibble::as_tibble(data_matrix)

  if (header) {
    # Set the column names
    colnames(result_tibble) <- df_header
  } else {
    df_header <- colnames(result_tibble)
  }

  # converting columns only with integer into numeric
  for (i in df_header) {
    test_all_numeric <- dplyr::pull(result_tibble, i) |>
      grepl("^\\d+$", x = _) |>
      all()

    if (test_all_numeric) {
      # result_tibble <-
      result_tibble[i] <- dplyr::pull(result_tibble, i) |> as.numeric()
      # result_tibble |> dplyr::mutate(!!i := as.numeric(!!{ df_header[i] }))
    }
  }

  # TODO trimws
  return(result_tibble)
}
