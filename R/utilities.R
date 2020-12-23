#' Return x if not null, y otherwise (ruby style or)
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else
    x
}

#' Create an odata_expr
#' @param x Character containing the odata code
#' @noRd
odata_expr <- function(x) {
  structure(x, class = "odata_expr")
}

#' Check if something is an odata expr
#' @param x The argument to check
#' @noRd
is.odata_expr <- function(x) {
  "odata_expr" %in% class(x)
}


#' Print an odata expr
#' @param expr Expression to print
#' @noRd
print.odata_expr <- function(expr) {
  cat("odata_expr: ", expr)
}

#' Concatenate two characters
#' @param s1 First character
#' @param s2 Second character
#' @noRd
`%s+%` <- function(s1, s2) {
  paste0(s1, s2)
}

#' Check whether something is a Date
#' @param x Argument to check
#' @noRd
is.Date <- function(x) {
  "Date" %in% class(x)
}
