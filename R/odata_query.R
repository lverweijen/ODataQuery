# odata_query
# General methods to make OData url handling easier


#' Class that wraps an OData query
#'
#' @export
ODataQuery <- R6::R6Class(
  "ODataQuery",
  
  active = list(
    url = function(value) {
      stopifnot(missing(value))
      query_string <- paste0(names(self$query_options), '=', self$query_options,
                             collapse='&')
      
      if (length(self$query_options) > 0) {
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
      self$query_options <- if (missing(query_options)) {list()} else {query_options}
    },
    
    print = function() {
      cat("ODataQuery:", self$url, '\n')
      invisible(self)
    },
    
    print_data = function(top = 5) {
      self$print()
      query <- self
      if (is.null(top))
        query <- self$query(top = top)
      print(query$all())
      invisible(self)
    },
    
    path = function(...) {
      resource <- paste(self$resource, ..., sep='/')
      ODataQuery$new(self$service, resource)
    },

    get = function(...) {
      args <- list(...)
      left_hand = ifelse(nchar(names(args)) > 0, paste0(names(args), '='), names(args))
      right_hand = lapply(args, represent_value)
      body <- paste0(left_hand, right_hand, collapse=",")
      resource <-paste0(self$resource, '(', body, ')')
      ODataQuery$new(self$service, resource)
    },

    func = function(fname, metadata, simplify) {
      odata_function(self$url, metadata = metadata, simplify = simplify)
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

    compute = function(...) {
      args <- list(...)
      right_hand <- ifelse(nchar(names(args)) == 0,
                           '',
                           paste("AS", names(args)))
      query <- paste(args, right_hand, collapse=',')
      return(self$query(`$compute` = query))
    },

    #' @inheritsParams retrieve_data
    retrieve = function(...) {
      retrieve_data(self$url, ...)
    },
    
    #' @inheritsParams retrieve_all()
    all = function(...) {
      retrieve_all(self$url, ...)
    },
    
    #' @inheritsParams retrieve_one()
    one = function(default, ...) {
      retrieve_one(self$url, ...)
    }
  ))

#' Retrieve data
#'
#' @param metadata Which metadata is included
#' @param simplify Simplifies nested lists into vectors and data frames
#' @export
retrieve_data <- function(url, metadata = c("none", "minimal", "all"), simplify) {
  metadata <- match.arg(metadata)
  req <- httr::GET(url, httr::add_headers("Accept" = paste0("application/json;odata.metadata=", metadata)))
  httr::stop_for_status(req)
  json <- httr::content(req, as = "text")
  data <- jsonlite::fromJSON(json, simplifyVector = simplify)
  
  if (!is.null(data$value)) {
    value <- data$value
    annotations <- data[startsWith(names(data), '@')]
    attributes(value) <- c(attributes(value), annotations)
  } else {
    value <- data
  }
  
  value
}

#' Retrieve data. If data is paged, concatenate pages.
#'
#' @inheritsParams retrieve_data
#' @export
retrieve_all <- function(url, ...) {
  pages <- list()
  next_link <- url
  
  i <- 1
  while (!is.null(next_link)) {
    data <- retrieve_data(next_link, ...)
    pages[[i]] <- data
    next_link <- attr(data, '@odata.nextLink')
    i <- i + 1
  }

  if (is.data.frame(pages[[1]]))
    result <- do.call(rbind, pages)
  else
    result <- do.call(c, pages)

  result
}

#' Retrieve single instance.
#'
#' @param default The default if nothing was found. If not specified, an error is thrown in this case.
#' @inheritsParams retrieve_data
#' @return Single value or default if none. Otherwise an error is thrown.
#' @export
retrieve_one <- function(url, default = stop("value not found"), ...) {
  value <- retrieve_data(url, ...)
  n <- nrow(value) %||% length(value)

  if (n < 1)
    result <- default
  else if (n > 1)
    result <- stop("multiple values")
  else if (is.data.frame(value))
    result <- lapply(value, function(col) col[[1]])
  else
    result <- value[[1]]
  
  result
}

#' Make an OData function available to R
#' 
#' @description This turns an OData function into an R function
#' Parameters are serialized to json.
#' Scalar arguments should be passed as atomic vectors.
#' Array or object arguments should be passed as list.
#' 
#' @inheritsParams retrieve_data
#' #return An R function
#' @export
odata_function <- function(url, metadata, simplify) {
  force(metadata)
  force(simplify)

  # Create a closure
  function(...) {
    args <- list(...)
    nargs <- names(args)

    left_hand <- ifelse(nchar(nargs) == 0, '', paste(nargs, '='))
    right_hand <- lapply(args, jsonlite::toJSON, auto_unbox = TRUE)
    arg_string <- paste(left_hand, right_hand, collapse=',')

    url <- paste(self$url, fname, sep='/')
    url <- paste0(url, '(', arg_string, ')')
    retrieve_data(url, metadata=metadata, simplify=simplify)
  }
}

#' Create an and-filter with parameters sanitized
#' @export
and_query <- function(...) {
  return(binop_query(' and ', ...))
}

#' Create an or-filter with parameters sanitized
#' @export
or_query <- function(...) {
  return(binop_query(' or ', ...))
}

#' Create a negative filter with parameters sanitized
#' @export
not_query <- function(...) {
  return(paste('not', and_query(...)))
}

#' Helper for creating and / or queries
#' @noRd
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

#' Sanitize parameters for use in url
#' @noRd
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
# vim: ts=2 sts=2 sw=2 expandtab

