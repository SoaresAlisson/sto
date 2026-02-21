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

s2 <- "bla1,10\nbla2,23\nbla3,32"
s2df(s2, header = FALSE, sep = ",")

# example of text substitution
substitutions <- s2df("replace_this;by\nNew York;NewYork\nSoviet.Union;SovietUnion")
substitutions
text <- "Ice-cream in Soviet Union or New York?"
stringr::str_replace_all(text,  tibble::deframe(substitutions))

