# NSE-test

to_odata <- function(expr) {
  to_odata_(rlang::enexpr(expr))
}

to_odata_ <- function(expr) {
  expr <- process_dollar(expr)
  env <- odata_env(expr)
  rlang::eval_bare(expr, env)
}

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

method_function <- function(method) {
  rlang::new_function(
    rlang::exprs(obj =, ... =),
    rlang::expr(
      odata_expr(
        paste0(represent_value(obj), ".", !!method, '(',
               paste(lapply(list(...), represent_value), collapse = ","),
               ')')
        )
    ),
    rlang::caller_env()
  )
}

odata_paste0 <- function(...) Reduce(prefix_function('concat'), list(...))

odata_paste <- function(..., sep=" ") {
  Reduce(function(e1, e2) {odata_paste0(odata_paste0(e1, sep, e2))}, list(...))
}

odata_minus <- function(x, y) {
  if (missing(y))
    prefix_function('-')(x)
  else
    binop_function(' sub ')(x, y)
}

odata_c <- function(...) {
  odata_expr(
    '(' %s+% paste(lapply(list(...), represent_value, double = TRUE), collapse=",") %s+% ')'
  )
}

odata_list <- function(...) {
  # represent_value(list(...))
  odata_expr(
    '[' %s+% paste(lapply(list(...), represent_value, double = TRUE), collapse=",") %s+% ']'
    )
}

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
  `:` = function(e1, e2) {represent_value(e1:e2)},
  append = prefix_function(" concat "),
)

represent_value <- function(x, double = FALSE) {
  if (is(x, "odata_expr"))
    return(x)
  else if (is.name(x))
    result <- as.character(x)
  else if (is.list(x) || length(x) > 1)
    result <- jsonlite::toJSON(x, auto_unbox = TRUE)
  else if (is(x, "Date"))
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

odata_expr <- function(x) {
  structure(x, class = "odata_expr")
}

print.odata_expr <- function(x) {
  cat("odata_expr: ", x)
}

`%s+%` <- function(s1, s2) {
  paste0(s1, s2)
}
