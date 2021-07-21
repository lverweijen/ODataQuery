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
#' \item Use methods such as `$retrieve()`, `$all()` and `$one()` to obtain
#' the results.
#' }
#' @export
ODataQuery <- R6::R6Class("ODataQuery",
  active = list(
    #' @field url Generate (encoded) url
    #' @param value Read-only
    url = function(value) {
      stopifnot(missing(value))
      query_string <- paste0(names(private$query_options), "=", private$query_options,
                             collapse = "&")

      if (length(private$query_options) > 0) {
        result <- paste0(private$service, private$resource, "?", query_string)
      } else {
        result <- paste0(private$service, private$resource)
      }
      utils::URLencode(result, repeated = TRUE)
    }
  ),

  public = list(
    #' @description Create a class representing a query.
    #'
    #' @param service The url of the endpoint to connect to.
    #' This url should not end with backslash.
    #' @param .resource Should not be used directly. Use $path() instead.
    #' @param .query_options Should not be used directly.
    #' Use methods such as $select(), $filter() and $query() instead.
    #' @param httr_args Additional parameters to pass to httr::GET
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    initialize = function(service, .resource = "", .query_options = list(), httr_args = list()) {
      stopifnot(length(service) == 1 && is.character(service))
      stopifnot(length(.resource) == 1 && is.character(.resource))
      stopifnot(is.list(.query_options))
      stopifnot(is.list(httr_args))

      private$service <- paste0(trimws(service, whitespace = "[/\\s]"), "/")
      private$resource <- .resource
      private$query_options <- .query_options
      private$httr_args <- httr_args
    },

    #' @description Print query, useful when debugging.
    #'
    #' @param top Number of records to print.
    #' @param ... Additional parameters are passed to print
    # inheritDotParams print
    #'
    #' @examples
    #' \dontrun{
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' service$print(10)$path("People")$print()
    #' }
    print = function(top = 0, ...) {
      cat("ODataQuery:", self$url, "\n")

      if (top > 0) {
        df <- self$top(top)$retrieve(metadata = "minimal")
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
      resource <- paste(private$resource, ..., sep = "/")
      resource <- trimws(resource, whitespace = "[/\\s]")
      ODataQuery$new(private$service, resource, httr_args = private$httr_args)
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
      resource <- paste0(private$resource, "(", body, ")")
      ODataQuery$new(private$service, resource, httr_args = private$httr_args)
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
    #' get_nearest_airport <- service$func('GetNearestAirport')
    #' \dontrun{
    #' get_nearest_airport(lat = 33, lon = -118)
    #' }
    func = function(fname, ...) {
      url <- paste(self$url, fname, sep = "/")
      odata_function(url, ..., httr_args = private$httr_args)
    },

    #' @description Supply custom query options that do not start with $
    #'
    #' @param ... Named lists where the names are custom query options
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$query(filter = "FirstName eq 'scott'")$url
    query = function(...) {
      new_options <- list(...)
      query_options <- private$query_options
      query_options[names(new_options)] <- new_options
      return(ODataQuery$new(private$service, private$resource, query_options,
                            httr_args = private$httr_args))
    },

    #' @description Limit the number of results to n
    #'
    #' @param n Number of records to return at most
    #'
    #' @examples
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity <- service$path("People")
    #' people_entity$top(10)
    top = function(n = 10) {
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
    skip = function(n = 10) {
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
    #' @param count Whether to include a count of the total number of records
    #' @param ... Passed to retrieve_data
    #' @inheritParams retrieve_data(...)
    #'
    #' @examples
    #' \dontrun{
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity$retrieve()
    #' }
    retrieve = function(count = FALSE, ...) {
      if (missing(count)) {
        url <- self$url
      } else {
        url <- self$query(`$count` = represent_value(count))$url
      }
      retrieve_data(url, ..., httr_args = private$httr_args)
    },

    #' @description Retrieve all data pages
    #'
    #' Return concatenation of value of all pages
    #'
    #' @param ... Passed to retrieve_all
    #' @inheritParams retrieve_all(...)
    #'
    #' @examples
    #' \dontrun{
    #' service <- ODataQuery$new("https://services.odata.org/V4/TripPinServiceRW")
    #' people_entity$all()
    #' people_entity$all(jsonlite_args = list(simplifyVector = False))
    #' }
    all = function(...) {
      retrieve_all(self$url, ..., httr_args = private$httr_args)
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
      retrieve_one(self$url, ..., httr_args = private$httr_args)
    }
  ),

  private = list(
    # service Service endpoint
    service = NULL,

    # resource Resource name
    resource = NULL,

    # query_options Options to query on
    query_options = NULL,

    # httr_args Options to query on
    httr_args = NULL
  ),

  cloneable = FALSE
)
