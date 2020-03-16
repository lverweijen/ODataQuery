# odata_query

ODataQuery <- R6::R6Class(
  "ODataQuery",
  
  active = list(
    url = function(value) {
      stopifnot(missing(value))
      query_string <- paste0('$', names(self$query_options), '=', self$query_options,
                             collapse='&')
      result <- paste0(self$service, self$resource, '?', query_string)
      return(result)
    }
  ),
  
  public = list(
    service = NULL,
    resource = NULL,
    query_options = NULL,
    
    initialize = function(service, resource, query_options) {
      self$service <- service
      self$resource <- if(missing(resource)) {''} else resource
      self$query_options <- if(missing(query_options)) {list()} else {query_options}
    },
    
    print = function() {
      cat("ODataQuery:", self$url, '\n')
    },
    
    path = function(...) {
      resource <- paste(self$resource, ..., sep='/')
      return(ODataQuery$new(self$service, resource))
    },
    
    get = function(pk) {
      resource <-paste0(self$base, '(', represent_value(pk), ')')
      return(ODataQuery$new(self$service, resource))
    },
    
    query = function(...) {
      new_options <- list(...)
      query_options <- self$query_options
      query_options[names(new_options)] <- new_options
      return(ODataQuery$new(self$service, self$resource, query_options))
    },
    
    all = function(remove_meta = TRUE) {
      result <- OData::retrieveData(self$url)
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
