#' Helper function.
fix_names_encoding <- function(x) {
  x <- gsub("Ã¦", "æ", x)
  x <- gsub("Ã¸", "ø", x)
  x <- gsub("Ã¥", "å", x)
  x <- gsub("Ã†", "Æ", x)
  x <- gsub("Ã˜", "Ø", x)
  x <- gsub("Ã…", "Å", x)
  return(x)
}

#' Helper function.
fix_names_join <- function(x, to_lower = TRUE) {
  x <- gsub("æ", "ae", x)
  x <- gsub("ø", "oe", x)
  x <- gsub("å", "aa", x)
  x <- gsub("Æ", "Ae", x)
  x <- gsub("Ø", "Oe", x)
  x <- gsub("Å", "Aa", x)
  x <- gsub("-", "", x)
  x <- gsub(" ", "", x)

  if(to_lower){
    x <- tolower(x)
  }

  return(x)
}

