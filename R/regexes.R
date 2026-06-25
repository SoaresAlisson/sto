#' regex to capture websites address
#'
#' @export
#'
#' @examples
#' text <- "Contact us at www.example.com or https://www.john.doe.com"
#' stringr::str_extract_all(text, rgx_url)[[1]]
rgx_url <- "https?://[\\w.-]+|www\\.[\\w.-]+"

#' regex to capture e-mail
#'
#' @export
#'
#' @examples
#' text <- "Contact us at support@example.com or john.doe@company.co.no"
#' stringr::str_extract_all(text, rgx_email)[[1]]
rgx_email <- "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"

rgx_abbrev <- "([:upper:]\\.){2,}"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][[A-ZÀ-Ÿ][a-zà-ÿ]\\.\\-]+\\b)"
# rgx_word <- "(\\b[A-ZÀ-Ÿ][A-ZÀ-Ÿa-zà-ÿ0-9\\.\\-]+\\b)"
# unicode in order https://symbl.cc/en/unicode-table/#spacing-modifier-letters
rgx_word <- "(\\b[A-ZÀ-ß][A-ZÀ-ßa-zà-ÿ0-9\\.\\-]+\\b)"
