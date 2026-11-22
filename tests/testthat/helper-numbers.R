# Formatted numbers back to numbers (minus signs, padding, leading zeros)
as_num <- function(x) {
  x <- gsub(intToUtf8(8722), "-", x, fixed = TRUE)
  x <- gsub("[^0-9.-]", "", x)
  as.numeric(x)
}

ci_bounds <- function(x) {
  x <- gsub(intToUtf8(8722), "-", x, fixed = TRUE)
  parts <- strsplit(gsub("[^0-9.,-]", "", x), ",", fixed = TRUE)
  t(vapply(parts, as.numeric, numeric(2)))
}
