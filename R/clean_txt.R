# talvez desncessária
ws_remove <- function(x) {
  x |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_squish() |>
    #  para retirar isso  "Non-breaking space"
    #  charToRaw, o espaço em branco é "c2 a0" . O stringr já retira isso
    gsub2("[^ -~]+", " ")
}
