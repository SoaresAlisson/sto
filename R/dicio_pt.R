# To test your regex, if it is capturing
# palavras lista de palavras em Português. "https://raw.githubusercontent.com/fserb/pt-br/master/palavras" #
# dicio lista de palavras com maior certeza. "https://raw.githubusercontent.com/fserb/pt-br/master/dicio" # 4.84 MB
# verbos lista de verbos.  36.2 KB: - "https://raw.githubusercontent.com/fserb/pt-br/master/verbos"
# conjugações todas as conjugações dos verbos. "2.12 MB" - "https://raw.githubusercontent.com/fserb/pt-br/master/conjuga%C3%A7%C3%B5es"

#' read a file .dic from Hunspell and get only the words to build a dict in the language
#' @args arqs the name of one file
#' \dontrun{
#' grep("dic$",x = arqs, value = T)  |> build_dict()  |> head(5)
#' }
build_dict <- function(arqs) {
 arqs  |> 
    # grep("dic$",x = _, value = T)  |> 
    # grep("dic$",x = _, value = T)  |> 
    # readLines(n=90L,skip=2L ) |> 
    readLines() |> 
    grep2("\\t") |>
    gsub(r"((.*)(\t.*))", "\\1", x=_) |> 
    gsub( r"((.*)(\/.*))", "\\1",x = _) |>
    grep2("[:alpha:]") 
}

