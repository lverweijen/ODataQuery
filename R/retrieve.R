#' Retrieve data
#'
#' @param url Which url to fetch data from
#' @param metadata Whether and how metadata is included
#' @param httr_args List of additional arguments passed on to httr::GET
#' @param jsonlite_args List of additional arguments passed on to jsonlite::fromJSON
#' @return Data including metadata
#' @export
#' @family retrieve
#'
#' @examples
#' \dontrun{
#' url <- "https://services.odata.org/V4/TripPinServiceRW"
#' retrieve_data(url)
#' }
retrieve_data <- function(url,
                          metadata = c("none", "minimal", "all"),
                          httr_args = list(),
                          jsonlite_args = list()) {
  metadata <- match.arg(metadata)
  accept <- paste0("application/json;odata.metadata=", metadata)
  useragent <- "https://github.com/lverweijen/odata_r"

  args <- list(url, httr::add_headers(Accept = accept, UserAgent = useragent))
  req <- do.call(httr::GET, c(args, httr_args))
  httr::stop_for_status(req)

  json <- httr::content(req, as = "text", encoding = "UTF-8")
  parsed <- do.call(jsonlite::fromJSON, c(json, jsonlite_args))
  parsed
}

#' Retrieve data. If data is paged, concatenate pages.
#' Only return the value without metadata.
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
#' @return Single value or default if none. If the result consists of
#' multiple records, an error is thrown.
#' @export
#' @family retrieve
#'
#' @examples
#' \dontrun{
#' url <- "https://services.odata.org/V4/TripPinServiceRW/People?$top=1"
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
  outer_dots <- force(list(...))

  # Create a closure
  function(...) {
    args <- list(...)
    nargs <- names(args)

    left_hand <- ifelse(nchar(nargs) == 0, "", paste(nargs, "="))
    right_hand <- lapply(args, jsonlite::toJSON, auto_unbox = TRUE)
    arg_string <- paste(left_hand, right_hand, collapse = ",")
    encoded_args <- utils::URLencode(paste0("(", arg_string, ")"))

    url <- paste0(url, encoded_args)
    do.call(retrieve_data, c(url, outer_dots))
  }
}
