write a function in R lang that take string like this and turn it into a tibble
header = FALSE 
header = TRUE
sep = ";"
s <- "col1;col2
bla1;10
bla2;23
bla3;32
"
input_string <- s

s2df <- function(txt, sep = ";|\\n") {
  txt |> strsplit(sep)
}

s |> s2df()
