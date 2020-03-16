# odata_r
Query on R OData

This package aims to make OData services more accesible to R users.

## Example usage

```{R}
# Initalisation
URL <- "https://services.odata.org/V4/TripPinServiceRW"
trip_service <- ODataQuery$new(URL)
people_resource <- trip_service$path("People") 

# Find url of people
trip_service$path("People")$url()
# [1] "https://services.odata.org/V4/TripPinServiceRW/People?$="

# Get a vector of all persons
trip_service$path("People")$all()

# Find a single person
people_resource$query(filter=and_query(FirstName.eq='Scott'))$one()

# Include that persons' friends
people_resource$query(filter=and_query(FirstName.eq='Scott'), expand="Friends")$one()

# Find friends of that person
people_resource$get("scottketchum")$path('Friends')$all()
```

