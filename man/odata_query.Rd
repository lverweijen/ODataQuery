\name{odata_query}
\alias{odata_query}
\title{Odata Query}
\usage{
ODataService(url)
}
\description{
Provides access to OData using an easy to use interface.
}
\examples{
URL <- "https://services.odata.org/V4/TripPinServiceRW"
trip_service <- ODataQuery$new(URL)
people_resource <- trip_service$path("People") 
person <- people_resource$query(filter=and_query(FirstName.eq='Scott'))$one()
}
