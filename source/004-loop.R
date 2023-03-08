names <- c("Mohsen Ebrahimi", "Reza Naseri", "Sara lotfi")
for (name in names) {
  "%s,\nYou are invited to the Wedding\n" |> 
    sprintf(name) |> 
    cat()
}
