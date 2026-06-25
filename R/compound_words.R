#' Make compounded words into one word
#'
#' @description
#' Transform compounded words into one. E.g. "At New York", "It was in Soviet
#' Union" becomes "At New_York" and "It was in Soviet_Union". The compounded words
#' must be supplied in parameter `subs`. It can be used when counting words,
#' ploting wordclouds, cooccurence networks, help in naives Bayes
#' text classification.
#'
#' @param txt text to be processed
#' @param subs string with compounded words. E.g. "New_York Soviet_Union"
#'
#' @export
#'
#' @examples
#' # text
#' txt <- "ice-cream in Soviet Union or New York?"
#' # words to be taken as a single
#' subs <- "Soviet_Union New_York"
#' compound_words(txt, subs)
#'
#' # or using a vector of words
#' subs <- c("Soviet Union", "New York")
#' txt2 <- compound_words(txt, subs)
#' txt2
#' txt2 |> strsplit(" ")
compound_words <- function(txt, subs) {
  # if (nchar(subs) == 0) {
  is_subs_scalar <- (length(subs) == 1)

  if (is_subs_scalar) {
    if (subs == "") {
      stop("subs parameter is empty")
    }

    df_sub <- tibble::tibble(
      c1 = s2v(subs),
      c2 = s2v(subs, keep_wss = TRUE)
    )
  } else {
    df_sub <- tibble::tibble(
      c1 = subs,
      c2 = gsub(x = subs, " ", "_")
    )
  }

  txt |>
    stringr::str_replace_all(tibble::deframe(df_sub))
}
