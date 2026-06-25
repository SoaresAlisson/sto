#' a regex pattern to capture brazilian names, like "Fulano de Tal", "Ciclano dos Santos"
regex_NomeProprio <- "((\\b[A-ZÀ-Ÿ][a-zà-ÿ\\-]+ (d[aeo]s? )?)+([A-ZÀ-Ÿ][a-zà-ÿ\\-]+)*)|(\\b[A-ZÀ-Ÿ\\.]{2,}\\b)"

# 	"([A-ZÀ-ÿ][a-zà-ÿ\\-]+ (d[aeo]s? )?){1,}([A-ZÀ-ÿ][a-zà-ÿ\\-])*"
regex_CPF <- "\\b[0-9]{3}.[0-9]{3}.[0-9]{3}-[0-9]{2}\\b"

# 'rule-based (regex) proper-name extractor
#' extract proper names from strings using regular expresssions.
#' The function suposes any sequence of upper letter followed by lower case is a proper name.
#' It is expected to return more things that wanted. Post-processing.
#' @param string input text
#' @param connector typical connector between names, like "of" "von", "van", "del". regex.
#' @export
#' @examples
#' "O Joaquim José da Silva Xavier, tambén conhecido como Tiradentes foi um auferes." |> extract_ppn()
#' "José da Silva e Fulano de Tal foram, bla Maria Silva. E depois disso, bla Joaquim José da Silva Xavier no STF" |> extract_ppn()
extract_ppn <- function(string, connector = "d[aeo]s?") {
  regex_propName <- paste0(
    "((\\b[A-ZÀ-Ÿ][a-zà-ÿ\\-]+ (",
    connector,
    " )?)+([A-ZÀ-Ÿ][a-zà-ÿ\\-]+)*)|(\\b[A-ZÀ-Ÿ\\.]{2,}\\b)"
  )

  string |>
    stringr::str_extract_all(regex_propName) |>
    # lapply(trimws)
    lapply(stringr::str_trim)
  # gsub2("^ | $")
  # trimws()
}

#' Extract abbreviations from text
#' @param txt input text
#' @param stopwords a vector of stop words. Pay attention to the capitalization.
#' @export
#' @examples
#' "Lorem Ipsum STF ergo S.T.F. foo." |> extract_abbrev()
#' "Lorem Ipsum FEDERAL SERVICE dolor sit S.T.F. foo." |> extract_abbrev()
#' "Lorem THE FEDERAL SERVICE dolor sit S.T.F. foo." |> extract_abbrev()
#' # now using the stopwords:
#' "Lorem THE FEDERAL SERVICE dolor sit S.T.F. foo." |> extract_abbrev(stopwords = "THE")
#' "Lorem THE FEDERAL SERVICE dolor sit WE S.T.F. foo." |> extract_abbrev(stopwords = c("THE", "WE"))
extract_abbrev <- function(txt, stopwords = "", connectors = "do|de|o|a") {
  # "Lorem Ipsum STF ergo T.J." |>
  rgx <- "\\b[A-ZÀ-Ÿ\\.?]+\\b"
  # rgx <- "\\b[A-Z\\.?]+\\b"
  rgx2 <- paste0("(", rgx, " )+")
  # remove stopwords
  # txt <- "Lorem Ipsum THE FEDERAL SERVICE dolor sit S.T.F. foo. WE"
  # txt <- if (stopwords != "") {stringr::str_remove_all(string = txt, stopwords)}
  if (any(stopwords != "")) {
    rgx_sw <- paste(stopwords, collapse = "\\b|\\b")
    rgx_sw <- paste0("\\b", rgx_sw, "\\b")
    txt <- stringr::str_remove_all(string = txt, rgx_sw)
    # txt <- lapply(
    #  stopwords,
    #  \(sw) {
    #    stringr::str_remove_all(string = txt, sw)
    #  }
    # ) |>
    #  unlist()
  }


  txt |>
    stringr::str_extract_all(rgx2) |>
    lapply(stringr::str_trim)
  # lapply(trimws)
}


#' Convert the string into proper name
#' @export
#' @examples
#' "jose dos santos" |> s2ppn()
s2ppn <- function(char) {
  # 		str = "jose dos santos"
  s2ppn_element <- function(char) {
    str2 <- char |>
      stringr::str_to_title() |>
      strsplit(" ") |>
      unlist()

    ifelse(grepl("^D[aeos]+$", str2), tolower(str2), str2) |>
      paste(collapse = " ") |>
      # trimws()
      lapply(stringr::str_trim)
  }

  # gsub2("(D[EAOS]*)")
  char |>
    sapply(s2ppn_element) |>
    unlist()
}
