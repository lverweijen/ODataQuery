# NSE-test

#' Macro to convert R to OData syntax
#'
#' @export
#' @param expr Expression to convert to OData
#'
#' @details
#' to_odata takes unquote R code and quotes its input. Use !! to unquote an argument.
#' to_odata_ requires its argument to be quoted already.
#'
#' Only a subset of R is supported.
#'
#' * arithmatic
#' The operators +, -, *, / and %% are mapped to add, sub, mul, divby and mod respectively
#'
#' * strings (characters in R)
#' toupper, tolower, startsWith, endsWith, nchar, paste, paste0, trimws
#'
#' * arrays (lists in R)
#' list, append, length
#'
#' * Formulae become lambdas in OData
#' (x ~ x$Name == "John")
#'
#' Every unknown function is passed as is.
#' If the function name is surrounded by percent signs it's treated as an infix operator.
#'
#' @examples
#' to_odata(Field == value)
#' address <- "Bakerstreet 4"
#' to_odata(!!address %in% Adresses)
#' to_odata(Friends$any(f ~ f$FirstName == 'John'))
#'
to_odata <- function(expr) {
  to_odata_(rlang::enexpr(expr))
}

#' @rdname to_odata
#' @export
to_odata_ <- function(expr) {
  expr <- process_dollar(expr)
  env <- odata_env(expr)
  represent_value(rlang::eval_bare(expr, env))
}

#' Create an environment to evaluate expression in
#' @param expr Expression for which to generate environment
#' @noRd
odata_env <- function(expr) {
  fnames <- all.names(expr, functions = TRUE)
  snames <- all.names(expr, functions = FALSE)
  fnames <- fnames[!(fnames %in% snames)]

  res <- new.env()

  for (fname in unique(fnames)) {
    if (startsWith(fname, "%") && endsWith(fname, "%"))
      res[[fname]] <- binop_function(" " %s+% substr(fname, 2, nchar(fname) - 1) %s+% " ")
    else
      res[[fname]] <- prefix_function(fname)
  }

  for (sname in unique(snames)) {
    res[[sname]] <- odata_expr(sname)
  }

  rlang::env_clone(standard_env, res)
}

#' Translate $ to /
#' @param expr Expression in which to replace dollar
#' @noRd
process_dollar <- function(expr) {
  if (rlang::is_call(expr)) {
    if (expr[[1]] == "$") {
      expr[[2]] <- process_dollar(expr[[2]])
      return (as.name(paste0(expr[[2]], "/", expr[[3]])))
    } else {
      expr[] <- lapply(expr[], process_dollar)
      return (expr)
    }
  } else {
    return (expr)
  }
}

#' Create a prefix function
#' @param f Function name in OData
#' @noRd
prefix_function <- function(f) {
  rlang::new_function(
    rlang::exprs(... = ),
    rlang::expr(
      odata_expr(
        paste0(
          !!f, '(',
          paste(lapply(list(...), represent_value), collapse=','),
          ')')
        )
    ),
    rlang::caller_env()
  )
}

#' Create an infix function
#' @param sep Separator to use in OData syntax
#' @noRd
binop_function <- function(sep) {
  rlang::new_function(
    rlang::exprs(e1 =, e2 =),
    rlang::expr(
      odata_expr(
        paste0(represent_value(e1), !!sep, represent_value(e2))
        )
    ),
    rlang::caller_env()
  )
}

#' Convert paste0 call to concat
#' @param ... Arguments to join
#' @noRd
odata_paste0 <- function(...) {
  Reduce(prefix_function('concat'), list(...))
}

#' Convert paste call to concat
#' @param ... Arguments to join
#' @param sep Separator to join arguments with
#' @noRd
odata_paste <- function(..., sep = " ") {
  Reduce(function(e1, e2) {odata_paste0(odata_paste0(e1, sep, e2))}, list(...))
}

#' Convert - call to odata. Can be used for subtraction or negation.
#' @param x First operand if y is passed, otherwise argument to negate.
#' @param y Second operand to minus function
#' @noRd
odata_minus <- function(x, y) {
  if (missing(y))
    prefix_function('-')(x)
  else
    binop_function(' sub ')(x, y)
}

#' Convert c call to OData
#'
#' Mainly used in combination with %in%
#' @param ... Contents of container
#' @noRd
odata_c <- function(...) {
  odata_expr(
    '(' %s+% paste(lapply(list(...), represent_value), collapse=",") %s+% ')'
  )
}

#' Convert list to an array
#' @param ... Contents of array
#' @noRd
odata_list <- function(...) {
  # represent_value(list(...))
  odata_expr(
    '[' %s+% paste(lapply(list(...), represent_value, double = TRUE), collapse=",") %s+% ']'
    )
}

#' Standard library of R functions and their OData equivalent
#' @noRd
standard_env <- rlang::child_env(
  .parent = rlang::empty_env(),
  `+` = binop_function(' add '),
  `-` = odata_minus,
  `*` = binop_function(' mul '),
  `/` = binop_function(' divby '),
  `%%` = binop_function(' mod '),
  `==` = binop_function(' eq '),
  `!=` = binop_function(' ne '),
  `<` = binop_function(' lt '),
  `>` = binop_function(' gt '),
  `<=` = binop_function(' le '),
  `>=` = binop_function(' ge '),
  `&&` = binop_function(' and '),
  `&` = binop_function(' and '),
  `||` = binop_function(' or '),
  `|` = binop_function(' or '),
  `!` = prefix_function('not'),
  paste0 = odata_paste0,
  paste = odata_paste,
  trimws = prefix_function('trim'),
  startsWith = prefix_function('startswith'),
  endsWith = prefix_function('endswith'),
  nchar = prefix_function("length"),
  `(` = prefix_function(""),
  `$` = binop_function('.'),
  `~` = binop_function(':'),
  list = odata_list,
  c = odata_c,
  `:` = function(e1, e2) {represent_value(as.list(e1:e2))},
  append = prefix_function(" concat "),
)

#' @title Create a combined filter
#' @export
#'
#' @param ... Raw odata queries or query options.
#'
#' @details
#' This function can be used with raw values or query options
#'
#' 1) Raw odata queries
#' Raw OData can be passed as string.
#' It's the responsibility of the caller that the argument is valid syntax
#' and values are escaped.
#'
#' 2) Query options
#' Query options can be passed as named parameters.
#'
#' Query options should be of the following form: `property.operator = value`
#'
#' * Property should be a property of the entity or individual.
#'
#' * Operation can have any of the following values:
#'
#'   * eq Whether property is equal to value.
#'   * ne Whether property is not equal to value.
#'   * gt Whether property is greater than value.
#'   * ge Whether property is greater than or equal to value.
#'   * lt Whether property is lower than value.
#'   * le Whether property is lower than or equal to value.
#'   * has Whether property has value as enumeration property.
#'   * startswith Whether property starts with value.
#'   * endswith Whether property ends with value.
#'   * contains Whether property contains value.
#'
#' * Value should be a string, double or boolean
#'   and will be escaped automatically.
#'
#'@md
#' @seealso <https://docs.oasis-open.org/odata/odata/v4.0/errata03/os/complete/part2-url-conventions/>
#'
#' @examples
#' and_query("Column eq OtherColumn",
#'           FirstName.startswith = 'A',
#'           LastName.eq = 'Scott')
#'
#' or_query("ExpireDate eq null",
#'          ExpireDate.lt = "2020-07-07")
#'
#' not_query(or_query(Age.lt = 21, Age.gt = 65))
#'
and_query <- function(...) {
  binop_query(" and ", ...)
}

#' @rdname and_query
#' @export
or_query <- function(...) {
  binop_query(" or ", ...)
}

#' @rdname and_query
#' @export
not_query <- function(...) {
  odata_expr(paste("not", and_query(...)))
}

#' Helper for creating and / or queries
#' @noRd
binop_query <- function(op, ...) {
  args <- list(...)

  # Find arg names
  if (is.null(names(args))) {
    argnames <- rep("", length(args))
  } else {
    argnames <- names(args)
  }

  query <- paste(Map(handle_parameter, argnames, args), collapse = op)
  odata_expr(paste0("(", query, ")"))
}


#' Handle parameter
#' @noRd
handle_parameter <- function(name, value) {
  INFIX <- c("eq", "ne", "gt", "ge", "lt", "le", "has")
  PREFIX <- c("startswith", "endswith", "contains")

  # Handle raw odata query
  if (is.null(name) || nchar(name) == 0)
    return(value)

  parts <- strsplit(name, ".", fixed = TRUE)[[1]]

  if (length(parts) != 2)
    stop("argument name must have format: \"property.operation\"")

  property <- parts[[1]]
  operator <- parts[[2]]

  if (operator %in% INFIX)
    result <- paste(property, operator, represent_value(value))
  else if (operator %in% PREFIX)
    result <- paste0(operator, "(", property, ",", represent_value(value), ")")
  else
    stop(paste0("Unknown operator \"", operator, "\". ",
                "Operator should be one of: ",
                paste0("\"", c(INFIX, PREFIX), "\"", collapse = ", ")), ".")

  result
}

#' Represent a primitive in OData (sanitized)
#' @param x Object to represent as odata query
#' @param double Whether a character should use double instead of single quotes
#' @noRd
represent_value <- function(x, double = FALSE) {
  if (is.odata_expr(x))
    return(x)
  else if (is.list(x))
    result <- jsonlite::toJSON(x, auto_unbox = TRUE)
  else if (length(x) > 1)
    result <- '(' %s+% paste(lapply(x, represent_value), collapse=",") %s+% ')'
  else if (is.name(x))
    result <- as.character(x)
  else if (is.Date(x))
    result <- as.character(x)
  else if (is.character(x)) {
    # Escape single or double quotes
    if (!double)
      result <- paste0("'", gsub("'", "''", x), "'")
    else
      result <- paste0("\"", gsub("\"", "\\\"", x), "\"")
  }
  else if (is.numeric(x))
    result <- x
  else if (is.logical(x))
    result <- tolower(x)
  else if (is.null(x))
    result <- "null"
  else {
    stop(paste("unknown type", class(x)))
  }

  odata_expr(result)
}

