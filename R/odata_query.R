# odata_query

ODataQuery <- R6::R6Class(
  "ODataQuery",
  
  public = list(
    base = NULL,
    query_options = NULL,
    
    initialize = function(base) {
      self$base = base
      self$query_options = list()
    },
    
    print = function() {
      cat("ODataQuery:", self$url(), '\n')
    },
    
    url = function() {
      query_string <- paste0('$', names(self$query_options), '=', self$query_options,
                             collapse='&')
      result <- paste0(self$base, '?', query_string)
      return(result)
    },
    
    path = function(...) {
      resource <- self$clone()
      resource$base <- paste(self$base, ..., sep = '/')
      return(resource)
    },
    
    get = function(pk) {
      resource <- self$clone()
      resource$base <- paste0(self$base, '(', represent_value(pk), ')')
      return(resource)
    },
    
    query = function(...) {
      query_options <- self$query_options
      new_options <- list(...)
      query_options[names(new_options)] <- new_options
      resource <- self$clone()
      resource$query_options <- query_options
      return(resource)
    },
    
    all = function(remove_meta = TRUE) {
      result <- OData::retrieveData(self$url())
      if(remove_meta) {
        result <- result$value
        result <- lapply(result, function(x) {x[!startsWith(names(x), '@')]})
      }
      return(result)
    },
    
    one = function(remove_meta = TRUE) {
      if(remove_meta) {
        result <- self$all(remove_meta)
      } else {
        result <- self$all(remove_meta)$value
      }
      
      stopifnot(length(result) == 1)
      return(result[[1]])
    }
  ))

represent_value <- function(x) {
  if(is.character(x))
    return(paste0("'", x, "'"))
  else if (is.numeric(x))
    return(x)
  else if (is.logical(x))
    return(tolower(x))
  stop("unknown type")
}

binop_query <- function(op, ...) {
  args <- list(...)
  nargs <- if(is.null(names(args))) {rep("", length(args))} else {names(args)}
  left_hand <- sub('.', ' ', nargs, fixed = TRUE)
  right_hand <- ifelse(nchar(nargs) > 0, 
                       lapply(args, represent_value), 
                       args)
  query <- paste(left_hand, right_hand, collapse = op)
  return(paste0('(', query, ')'))
}

and_query <- function(...) {
  return(binop_query(' and ', ...))
}

or_query <- function(...) {
  return(binop_query(' or ', ...))
}

not_query <- function(...) {
  return(paste('not', and_query(...)))
}
