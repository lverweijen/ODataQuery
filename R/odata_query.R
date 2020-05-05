# odata_query
# General methods to make OData url handling easier

ODataQuery <- R6::R6Class(
  "ODataQuery",
  
  active = list(
    url = function(value) {
      stopifnot(missing(value))
      query_string <- paste0(names(self$query_options), '=', self$query_options,
                             collapse='&')
      
      if(length(self$query_options) > 0) {
        result <- paste0(self$service, self$resource, '?', query_string)
      } else {
        result <- paste0(self$service, self$resource)
      }
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
      return(invisible(self))
    },
    
    print_data = function(top = 5) {
      self$print()
      query <- self
      if (is.null(top))
        query <- self$query(top = top)
      print(query$all())
      return(invisible(self))
    },
    
    path = function(...) {
      resource <- paste(self$resource, ..., sep='/')
      return(ODataQuery$new(self$service, resource))
    },

    get = function(...) {
      args <- list(...)
      left_hand = ifelse(nchar(names(args)) > 0, paste0(names(args), '='), names(args))
      right_hand = lapply(args, represent_value)
      body <- paste0(left_hand, right_hand, collapse=",")
      resource <-paste0(self$resource, '(', body, ')')
      return(ODataQuery$new(self$service, resource))
    },

    func = function(fname, remove_meta = TRUE) {
      #' Bind an OData function to R
      #' 
      #' @description This turns an OData function into an R function
      #' Parameters are serialized to json.
      #' Scalar arguments should be passed as atomic vectors.
      #' Array or object arguments should be passed as list.
      #' 
      #' @param fname The name of the function that should be called.
      #' @param remove_meta logical Whether metadata should be removed from output.
      
      force(fname)
      force(remove_meta)
     
      # Create a closure
      function(...) {
        args <- list(...)
        nargs <- names(args)
        
        left_hand <- ifelse(nchar(nargs) == 0, '', paste(nargs, '='))
        right_hand <- lapply(args, function(x) {
          if(is.list(x)) {
            RJSONIO::toJSON(x, pretty = FALSE, collapse = '')
          } else {
            as.character(x)
            }
        })
        
        arg_string <- paste(left_hand, right_hand, collapse=',')
        
        url <- paste(self$url, fname, sep='/')
        url <- paste0(url, '(', arg_string, ')')
        result <- OData::retrieveData(url)
        
        if(remove_meta)
          result <- lapply(result, function(x) {x[!startsWith(names(x), '@')]})
        
        return(result)
      }
    },

    query = function(...) {
      new_options <- list(...)
      query_options <- self$query_options
      query_options[names(new_options)] <- new_options
      return(ODataQuery$new(self$service, self$resource, query_options))
    },

    top = function(n) {
      return(self$query(`$top` = n))
    },

    skip = function(n) {
      return(self$query(`$skip` = n))
    },

    select = function(...) {
      return(self$query(`$select` = paste(..., sep=',')))
    },

    filter = function(...) {
      return(self$query(`$filter` = and_query(...)))
    },

    expand = function(...) {
      return(self$query(`$expand` = paste(..., sep=',')))
    },

    orderby = function(...) {
      keys <- c(...)
      orders <- ifelse(startsWith(keys, '-'),
                       paste(substr(keys, 2, 999), 'desc'),
                       keys)
      orderby <- paste(orders, collapse=',')
      return(self$query(`$orderby` = orderby))
    },

    search = function(s) {
      return(self$query(`$search` = s))
    },

    all = function(remove_meta = TRUE) {
      result <- OData::retrieveData(self$url)
      if(remove_meta && 'value' %in% names(result)) {
        result <- result$value
        result <- lapply(result, function(x) {x[!startsWith(names(x), '@')]})
      }
      return(result)
    },
    
    one = function(default, remove_meta = TRUE) {
      if(remove_meta) {
        result <- self$all(remove_meta)
      } else {
        result <- self$all(remove_meta)$value
      }
      
      if(length(result) > 1) {
        stop('Multiple items: ', length(result))
      } else if(length(result) < 1) {
        if(missing(default)) {
          stop("No items found")
        } else {
          return(default)
        }
      } else {
        return(result[[1]])  
      }
    }
  ))

#' Sanitize parameters for use in url
represent_value <- function(x) {
  if(is.character(x))
    # Escape single quotes
    result <- paste0("'", gsub("'", "''", x), "'")
  else if (is.numeric(x))
    result <- x
  else if (is.logical(x))
    result <- tolower(x)
  else if(is.null(x))
    result <- "null"
  else
    stop("unknown type")

  return(result)
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

#' Create an and-filter with parameters sanitized
and_query <- function(...) {
  return(binop_query(' and ', ...))
}

#' Create an or-filter with parameters sanitized
or_query <- function(...) {
  return(binop_query(' or ', ...))
}

#' Create a negative filter with parameters sanitized
not_query <- function(...) {
  return(paste('not', and_query(...)))
}
