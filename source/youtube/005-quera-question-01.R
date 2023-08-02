## First Solution
n = 3
o = c()
for (i in seq_len(n)) { ## seq_len = 1:n
  o[i] = "o"
}
cat("W", o, "w!", sep = "")

## Second Solution
wow <- function(n) "W%sw!" |> sprintf(strrep("o", n)) |> cat()
wow(2)
