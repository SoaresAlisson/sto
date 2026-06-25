#' Transform the text into proper name style
#'
#' @description
#' Traditional funcions like toupper() or tolower() transform the text into
#' upper case or lower case. The stringr::str_to_title() transforms all text into lower case, with the exception of the first word. This function transform the text into proper name style.
#'
#' @param text the text to be transformed into name style
#' @param prep a vector of prepositions to be in lower case
#'
#' @export
#'
#' @examples
#' "augusto dos anjos" |> s2ppn(prep = "dos")
#' "AUGUSTO DOS ANJOS" |> s2ppn(prep = "dos")
#' "AUGUSTO Dos anjos" |> s2ppn(prep = s2v("da de di do das dos"))
s2ppn <- function(text, prep = s2v("da de di do das dos von van of the")) {
  vec_title <- prep |> stringr::str_to_title()
  replacements <- stats::setNames(prep, paste0("\\b", vec_title, "\\b"))

  text |>
    stringr::str_to_title() |>
    stringr::str_replace_all(pattern = replacements)
}
