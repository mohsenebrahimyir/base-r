name <- "Reza"

if (name == "Mohsen" | name == "Reza") {
  "Salam %s" |> sprintf(name) |> print()
} else {
  print("Salam gharibe")
}

ifelse(
  (name == "Mohsen" | name == "Reza"), {
    "Salam %s" |> sprintf(name) |> print()
  }, {
    print("Salam gharibe")
  })
