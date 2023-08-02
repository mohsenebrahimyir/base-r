n = 2

for (i in seq_len(n)) "man khoshghlab hastam\n" |> cat()

text <- "man khoshghlab hastam
man khoshghlab hastam
man khoshghlab hastam
man khoshghlab hastam"

split_newline <- function(text) (text |> strsplit("\n"))[[1]]

text |> split_newline() |> length() |> cat()




