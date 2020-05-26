# odata_query
# General methods to make OData url handling easier


#' @title ODataQuery
#'
#' @description R6 class that represents an OData query
#'
#' @details
#'
#' This class has methods to build and navigate OData services:
#'
#' \itemize{
#' \item Use methods such as `$path()` and `$get()` to find a path.
#' \item Use methods such as `$select()` and `$filter()` to make your query.
#' \item Use methods such as`$retrieve()`, `$all()` and `$one()`to obtain
#' the results.
#' }
#' @export
ODataQuery <- R6::R6Class("ODataQuery",
  active = list(
    #' @field url Generate (encoded) url
    #' @param value Read-only
    url = function(value) {
      stopifnot(missing(value))
      query_string <- paste0(names(self$query_options), "=", self$query_options,
                             collapse = "&")

      if (length(self$query_options) > 0) {
        result <- paste0(self$service, self$resource, "?", query_string)
      } else {
        result <- paste0(self$service, self$resource)
      }
      URLencode(result, repeated = TRUE)
    }
  ),

  public = list(

    #' @field service Service endpoint
    service = NULL,

    #' @field resource Resource name
    resource = NULL,

    #' @field query_options Options to query on
    query_options = NULL,

    #' @description Create a class representing a query.
    #'
    #' @param service The url of the endpoint to connect to.
    #' This url should not end with backslash.
    #' @param resource Don't use. Use $path() instead.
    #' @param query_options Don't use. Use methods like $select(),
    #' $filter(), $query() instead.
    initialize = function(service, resource = "", query_options = list()) {
      self$service <- service
      self$resource <- resource
      self$query_options <- query_options
    },

    #' @description Print query
    #'
    #' @param top Number of results to print
    #' @param ... Additional parameters are passed to print
    # inheritDotParams print
    print = function(top = 3, ...) {
      cat("ODataQuery:", self$url, "\n")

      if (top > 0) {
        df <- self$top(top)$retrieve(metadata = "minimal",
                                     simplifyVector = TRUE)
        print(df, ...)
      }

      invisible(self)
    },

    #' @description Supply path to the resource
    #'
    #' @param ... Components that lead to resource path
    path = function(...) {
      resource <- paste(self$resource, ..., sep = "/")
      ODataQuery$new(self$service, resource)
    },

    #' @description Query an individual record by ID parameters
    #'
    #' @param ... ID-parameters (named or unnamed)
    get = function(...) {
      args <- list(...)
      left_hand <- ifelse(nchar(names(args)) > 0,
                          paste0(names(args), "="),
                          names(args))
      right_hand <- lapply(args, represent_value)
      body <- paste0(left_hand, right_hand, collapse = ",")
      resource <- paste0(self$resource, "(", body, ")")
      ODataQuery$new(self$service, resource)
    },

    #' @description Path to an OData function
    #'
    #' @param fname Name of the function
    #' @param ... Options passed to retrieve_data
    # inheritDotParams retrieve_data
    #' @return closure
    func = function(fname, ...) {
      url <- paste(self$url, fname, sep = "/")
      odata_function(url, ...)
    },

    #' @description Supply custom query options that do not start with $
    #'
    #' @param ... Named lists where the names are custom query options
    query = function(...) {
      new_options <- list(...)
      query_options <- self$query_options
      query_options[names(new_options)] <- new_options
      return(ODataQuery$new(self$service, self$resource, query_options))
    },

    #' @description Limit the number of results to n
    #'
    #' @param n Number of records to return at most
    top = function(n) {
      stopifnot(is.numeric(n))
      return(self$query(`$top` = n))
    },

    #' @description Skip first few items
    #'
    #' @param n Number of items to skip
    skip = function(n) {
      stopifnot(is.numeric(n))
      return(self$query(`$skip` = n))
    },

    #' @description Select fields. If not present, all fields are returned.
    #'
    #' @param ... Fields to select
    select = function(...) {
      return(self$query(`$select` = paste(..., sep = ",")))
    },

    #' @description Apply filter to result
    #'
    #' @param ... Passed to and_query
    #' @inheritParams and_query()
    filter = function(...) {
      return(self$query(`$filter` = and_query(...)))
    },

    #' @description Expand on expansion properties
    #'
    #' @param ... Properties to extend on
    expand = function(...) {
      return(self$query(`$expand` = paste(..., sep = ",")))
    },

    #' @description Order results by one or more keys
    #' @param ... Keys to order by. To order in descending order, the key can
    #' be prefixed by a negative sign.
    orderby = function(...) {
      keys <- c(...)
      orders <- ifelse(startsWith(keys, "-"),
                       paste(substr(keys, 2, 999), "desc"),
                       keys)
      orderby <- paste(orders, collapse = ",")
      return(self$query(`$orderby` = orderby))
    },

    #' @description Search the entity
    #'
    #' @param s Search string as defined by the endpoint.
    search = function(s) {
      return(self$query(`$search` = s))
    },

    #' @description Compute properties
    #'
    #' Add additional properties to query computed from other attributes
    #'
    #' @param ... Named list of properties to compute
    compute = function(...) {
      args <- list(...)
      right_hand <- ifelse(nchar(names(args)) == 0,
                           "",
                           paste("AS", names(args)))
      query <- paste(args, right_hand, collapse = ",")
      return(self$query(`$compute` = query))
    },

    #' @description Retrieve data
    #'
    #' @param ... Passed to retrieve_data
    #' @inheritParams retrieve_data(...)
    retrieve = function(...) {
      retrieve_data(self$url, ...)
    },

    #' @description Retrieve all data pages
    #'
    #' Return concatenation of value of all pages
    #'
    #' @param ... Passed to retrieve_all
    # inheritDotParams retrieve_all(...)
    all = function(...) {
      retrieve_all(self$url, ...)
    },

    #' @description Retrieve individual
    #'
    #' @param ... Passed to retrieve_one
    #' @inheritParams retrieve_one(...)
    one = function(...) {
      retrieve_one(self$url, ...)
    }
  ))

#' Retrieve data
#'
#' @param metadata Which metadata is included
#' @param simplifyVector Simplifies nested lists into vectors and data frames
#' @inheritDotParams jsonlite::fromJSON
#' @export
retrieve_data <- function(url, metadata = c("none", "minimal", "all"),
                          simplifyVector = FALSE, ...) {

  metadata <- match.arg(metadata)
  accept <- paste0("application/json;odata.metadata=", metadata)
  useragent <- "https://github.com/lverweijen/odata_r"

  req <- httr::GET(url, httr::add_headers(Accept = accept,
                                          UserAgent = useragent))
  httr::stop_for_status(req)

  json <- httr::content(req, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(json, simplifyVector = simplifyVector, ...)
}

#' Retrieve data. If data is paged, concatenate pages.
#'
#' @inheritParams retrieve_data
#' @export
retrieve_all <- function(url, ...) {
  pages <- list()
  next_link <- url

  i <- 1
  while (!is.null(next_link)) {
    data <- retrieve_data(next_link, ...)
    pages[[i]] <- data$value %||% data
    next_link <- data[["@odata.nextLink"]]
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
#' @param default The default if nothing was found.
#' If not specified, an error is thrown in this case.
#' @inheritParams retrieve_data
#' @return Single value or default if none. Otherwise an error is thrown.
#' @export
retrieve_one <- function(url, default = stop("value not found"), ...) {
  data <- retrieve_data(url, ...)

  # Data is already a singleton
  if (is.null(data[["value"]]))
    return(data)

  # Make data a singleton
  value <- data$value
  n <- nrow(value) %||% length(value)

  if (n < 1)
    result <- default
  else if (n > 1)
    result <- stop("multiple values")
  else if (is.data.frame(value))
    result <- c(data[names(data) != "value"],
                lapply(value, function(col) col[[1]]))
  else
    result <- c(data[names(data) != "value"], value[[1]])

  result
}

#' Make an OData function available to R
#'
#' @description This turns an OData function into an R function
#' Parameters are serialized to json.
#' Scalar arguments should be passed as atomic vectors.
#' Array or object arguments should be passed as list.
#'
#' @inheritParams retrieve_data
#' #return An R function
#' @export
odata_function <- function(url, metadata = c("none", "minimal", "all"), ...) {
  force(metadata)
  force(list(...))

  # Create a closure
  function(...) {
    args <- list(...)
    nargs <- names(args)

    left_hand <- ifelse(nchar(nargs) == 0, "", paste(nargs, "="))
    right_hand <- lapply(args, jsonlite::toJSON, auto_unbox = TRUE)
    arg_string <- paste(left_hand, right_hand, collapse = ",")
    encoded_args <- URLencode(paste0("(", arg_string, ")"))

    url <- paste0(url, encoded_args)
    retrieve_data(url, ...)
  }
}

#' Create an and-filter with parameters sanitized
#' @export
and_query <- function(...) {
  return(binop_query(" and ", ...))
}

#' Create an or-filter with parameters sanitized
#' @export
or_query <- function(...) {
  return(binop_query(" or ", ...))
}

#' Create a negative filter with parameters sanitized
#' @export
not_query <- function(...) {
  return(paste("not", and_query(...)))
}

#' Helper for creating and / or queries
#' @noRd
binop_query <- function(op, ...) {
  args <- list(...)

   if (is.null(names(args))) {
     nargs <- rep("", length(args))
  } else {
    nargs <- names(args)
  }

  left_hand <- sub(".", " ", nargs, fixed = TRUE)
  right_hand <- ifelse(nchar(nargs) > 0,
                       lapply(args, represent_value),
                       args)
  query <- paste(left_hand, right_hand, collapse = op)
  return(paste0("(", query, ")"))
}

#' Sanitize parameters for use in url
#' @noRd
represent_value <- function(x) {
  if (is.character(x))
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

  return(result)
}

#' Return x if not null, y otherwise (ruby style or)
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else
    x
}

# vim: ts=2 sts=2 sw=2 expandtab
