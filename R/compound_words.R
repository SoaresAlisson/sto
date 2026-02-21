#' Make compounded words into one word
#'
#' @description
#' Transform compounded words into one. E.g. "At New York", "It was in Soviet Union" becomes "At New_York" and "It was in Soviet_Union". The compunded words must be supplied in parameter `subs`.
#'
#' @param txt text to be processed
#' @param subs string with compounded words. E.g. "New_York Soviet_Union"
#'
#' @export
#'
#' @examples
#' txt <- "ice-cream in Soviet Union or New York?"
#' s <- "Soviet_Union New_York"
#' compounded_words(txt, s)
compound_words <- function(txt, subs) {
  # if (nchar(subs) == 0) {
  if (subs == "") {
    stop("subs parameter is empty")
  }

  df_sub <- tibble::tibble(
    c1 = s2v(subs),
    c2 = s2v(subs, keep_wss = TRUE)
  )

  txt |>
    stringr::str_replace_all(tibble::deframe(df_sub))
}
