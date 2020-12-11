# odata_query
# General methods to make OData url handling easier
#' @md


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
      if (!missing(value))
        stop("url is read-only")

      query_string <- paste0(names(self$query_options), "=", self$query_options,
                             collapse = "&")

      if (length(self$query_options) > 0) {
        result <- paste0(self$service, self$resource, "?", query_string)
      } else {
        result <- paste0(self$service, self$resource)
      }
      utils::URLencode(result, repeated = TRUE)
    },

    #' @field service Service endpoint
    service = function(value) {
      if (missing(value))
        private$service
      else
        stop("service is read-only")
    },

    #' @field resource Resource name
    resource = function(value) {
      if (missing(value))
        private$resource
      else
        stop("resource is read-only")
    }
  ),

  private = list(
    service = NULL,
    resource = NULL,
    query_options = NULL
  ),

  public = list(
    #' @description Create a class representing a query.
    #'
    #' @param service The url of the endpoint to connect to.
    #' This url should not end with backslash.
    #' @param resource Name of resource. It\'s recommended to use $path()
    #' instead.
    #' @param query_options List of query options.
    #' It\'s recommended to use methods like $select(), $filter() and $query()
    #' instead.
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    initialize = function(service, resource = "", query_options = list()) {
      stopifnot(length(service) == 1 && is.character(service))
      stopifnot(length(resource) == 1 && is.character(resource))
      stopifnot(is.list(query_options))

      self$service <- paste0(trimws(service, whitespace = "[/\\s]"), "/")
      self$resource <- resource
      self$query_options <- query_options
    },

    #' @description Print query, useful when debugging.
    #'
    #' @param top Number of records to print. If NULL, print everything.
    #' @param ... Additional parameters are passed to print
    # inheritDotParams print
    #'
    #' @examples
    #' \dontrun{
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' service$print(10)$path("People")$print(NULL)
    #' }
    print = function(top = 3, ...) {
      cat("ODataQuery:", self$url, "\n")

      if (top > 0) {
        df <- self$top(top)$retrieve(metadata = "minimal",
                                     simplifyVector = TRUE)
        print(df, ...)
      } else if (is.null(top)) {
        df <- self$retrieve(metadata = "minimal", simplifyVector = TRUE)
        print(df, ...)
      }

      invisible(self)
    },

    #' @description Supply path to the resource
    #'
    #' @param ... Components that lead to resource path
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    path = function(...) {
      resource <- paste(self$resource, ..., sep = "/")
      resource <- trimws(resource, whitespace = "[/\\s]")
      ODataQuery$new(self$service, resource)
    },

    #' @description Query an individual record by ID parameters
    #'
    #' @param ... ID-parameters (named or unnamed)
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' russellwhyte <- people_entity$get("russellwhyte")
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
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' get_nearest_airport <- service$func('GetNearestAirport',
    #'                                     simplifyVector = TRUE)
    #' \dontrun{
    #' get_nearest_airport(lat = 33, lon = -118)
    #' }
    func = function(fname, ...) {
      url <- paste(self$url, fname, sep = "/")
      odata_function(url, ...)
    },

    #' @description Supply custom query options that do not start with $
    #'
    #' @param ... Named lists where the names are custom query options
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$query(filter = "FirstName eq 'scott'")
    query = function(...) {
      new_options <- list(...)
      query_options <- self$query_options
      query_options[names(new_options)] <- new_options
      return(ODataQuery$new(self$service, self$resource, query_options))
    },

    #' @description Limit the number of results to n
    #'
    #' @param n Number of records to return at most
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$top(10)
    top = function(n) {
      stopifnot(is.numeric(n) && length(n) == 1)
      return(self$query(`$top` = n))
    },

    #' @description Skip first few items
    #'
    #' @param n Number of items to skip
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$skip(10)
    skip = function(n) {
      stopifnot(is.numeric(n) && length(n) == 1)
      return(self$query(`$skip` = n))
    },

    #' @description Select fields. If not present, all fields are returned.
    #'
    #' @param ... Fields to select
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$select("FirstName", "LastName")
    select = function(...) {
      return(self$query(`$select` = paste(..., sep = ",")))
    },

    #' @description Apply filter to result
    #'
    #' @param ... Can be a raw odata query or query options. It's recommended to use
    #' query options because these will automatically escape parameters.
    #' The parameters are passed on to `and_query`.
    #' @inheritParams and_query()
    #' @seealso [and_query()] for details.
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$filter(FirstName.eq = 'Scott')
    filter = function(...) {
      return(self$query(`$filter` = and_query(...)))
    },

    #' @description Expand on expansion properties
    #'
    #' @param ... Properties to extend on
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$expand("Friends")
    expand = function(...) {
      return(self$query(`$expand` = paste(..., sep = ",")))
    },

    #' @description Order results by one or more keys
    #' @param ... Keys to order by. To order in descending order, the key can
    #' be prefixed by a negative sign.
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$orderby('Concurrency')
    #' people_entity$orderby('-Concurrency')
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
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$search('Boise')
    search = function(s) {
      return(self$query(`$search` = s))
    },

    #' @description Compute properties
    #'
    #' Add additional properties to query computed from other attributes.
    #'
    #' @param ... Named list of properties to compute
    #'
    #' @examples
    #' # Not really supported by this particular service.
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$compute(a = "5 MUL Concurrency")
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
    #'
    #' @examples
    #' \dontrun{
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity$retrieve()
    #' }
    retrieve = function(...) {
      retrieve_data(self$url, ...)
    },

    #' @description Retrieve all data pages
    #'
    #' Return concatenation of value of all pages
    #'
    #' @param ... Passed to retrieve_all
    # inheritDotParams retrieve_all(...)
    #'
    #' @examples
    #' \dontrun{
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity$all()
    #' people_entity$all(simplifyVector = TRUE)
    #' }
    all = function(...) {
      retrieve_all(self$url, ...)
    },

    #' @description Retrieve individual
    #'
    #' @param ... Passed to retrieve_one
    #' @inheritParams retrieve_one(...)
    #'
    #' @examples
    #' \dontrun{
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity$top(1)$one(default = NA)
    #' }
    one = function(...) {
      retrieve_one(self$url, ...)
    }
  ))

#' Retrieve data
#'
#' @param url Which url to fetch data from
#' @param metadata Whether and how metadata is included
#' @param simplifyVector Simplifies nested lists into vectors and data frames
#' @inheritDotParams jsonlite::fromJSON
#' @return Data including metadata
#' @export
#' @family retrieve
#'
#' @examples
#' \dontrun{
#' url <- "https://services.odata.org/V4/TripPinServiceRW"
#' retrieve_data(url)
#' }
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
#' Only return the value.
#'
#' @param url Which url to fetch data from
#' @inheritDotParams retrieve_data
#' @export
#' @family retrieve
#'
#' @examples
#' \dontrun{
#' url <- "https://services.odata.org/V4/TripPinServiceRW/People"
#' retrieve_all(url)
#' }
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
#' @param url Which url to fetch data from
#' @param default The default if nothing was found.
#' If not specified, an error is thrown in this case.
#' @inheritDotParams retrieve_data
#' @return Single value or default if none. If the result consistents of
#' multiple records, an error is thrown.
#' @export
#' @family retrieve
#'
#' @examples
#' \dontrun{
#' url <- https://services.odata.org/V4/TripPinServiceRW/People?$top=1
#' retrieve_one(url)
#'
#' url <- "https://services.odata.org/V4/TripPinServiceRW/People('russellwhyte')"
#' retrieve_one(url)
#' }
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
#' @param url Which url to fetch data from
#' @param metadata Whether and how metadata is included
#' @inheritDotParams retrieve_data
#' @return An R function
#' @export
#' @family retrieve
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
    encoded_args <- utils::URLencode(paste0("(", arg_string, ")"))

    url <- paste0(url, encoded_args)
    retrieve_data(url, ...)
  }
}

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
  return(binop_query(" and ", ...))
}

#' @rdname and_query
#' @export
or_query <- function(...) {
  return(binop_query(" or ", ...))
}

#' @rdname and_query
#' @export
not_query <- function(...) {
  return(paste("not", and_query(...)))
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

#' Return x if not null, y otherwise (ruby style or)
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else
    x
}

# vim: ts=2 sts=2 sw=2 expandtab
