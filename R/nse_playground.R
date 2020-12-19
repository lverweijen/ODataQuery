# NSE-test

to_odata <- function(expr) {
  expr <- rlang::enexpr(expr)
  env <- odata_env(expr)
  rlang::eval_bare(expr, env)
}

odata_env <- function(expr) {
  fnames <- all.names(expr, functions = TRUE)
  snames <- all.names(expr, functions = FALSE)
  fnames <- fnames[!(fnames %in% snames)]
  
  res <- new.env()
  
  for (fname in unique(fnames)) {
    res[[fname]] <- prefix_function(fname)
  }
  
  for (sname in unique(snames)) {
    res[[sname]] <- structure(sname, class="odata_query")
  }
  
  rlang::env_clone(standard_env, res)
}

prefix_function <- function(f) {
  rlang::new_function(
    rlang::exprs(... = ),
    rlang::expr(
      structure(
        paste0(
          !!f, '(', 
          paste(lapply(list(...), represent_value), collapse=','), 
          ')'),
        class="odata_query")
    ),
    rlang::caller_env()
  )
}

binop_function <- function(sep) {
  rlang::new_function(
    rlang::exprs(e1 =, e2 =),
    rlang::expr(
      structure(
        paste0(represent_value(e1), !!sep, represent_value(e2)),
        class="odata_query")
    ),
    rlang::caller_env()
  )
}

method_function <- function(method) {
  rlang::new_function(
    rlang::exprs(obj =, ... =),
    rlang::expr(
      structure(
        paste0(represent_value(obj), ".", !!method, '(', 
               paste(lapply(list(...), represent_value), collapse = ","), 
               ')'),
        class="odata_query")
    ),
    rlang::caller_env()
  )
}

odata_paste0 <- function(...) Reduce(prefix_function('concat'), list(...))

odata_paste = function(..., sep=" ") {
  Reduce(function(e1, e2) {odata_paste0(odata_paste0(e1, sep, e2))}, list(...))
}

standard_env <- rlang::child_env(
  .parent = rlang::empty_env(),
  `+` = binop_function(' add '),
  `-` = binop_function(' sub '),  # TODO negation
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
  `||` = binop_function(' or '),
  `%in%` = function(e1, e2) {binop_function(' has ')(e2, e1)},
  `!` = prefix_function('not'),
  paste0 = odata_paste0,
  paste = odata_paste,
  trimws = prefix_function('trim'),
  startsWith = prefix_function('startswith'),
  endsWith = prefix_function('endswith'),
  `(` = prefix_function(""),
  `$` = binop_function('.'),
  `~` = binop_function(':'),
  all = method_function("all"),
  any = method_function("any"),
)

represent_value <- function(x) {
  if (class(x) == "odata_query")
    return(x)
  else if (is.character(x))
    # Escape single quotes
    result <- paste0("'", gsub("'", "''", x), "'")
  else if (is.numeric(x))
    result <- x
  else if (is.logical(x))
    result <- tolower(x)
  else if (is.null(x))
    result <- "null"
  else
    stop("unknown type")
  
  structure(result, class = "odata_query")
}
