# odata_r
Query on R OData

This package aims to make OData services more accesible to R users.

## Example usage

```{R}
# Initialisation
URL <- "https://services.odata.org/V4/TripPinServiceRW"
trip_service <- ODataQuery$new(URL)
people_entity <- trip_service$path("People")

# Find all people whose name starts with an R
people_entity$
  select("UserName", "FirstName", "LastName")$
  filter(FirstName.startswith = "R")$
  all(simplifyVector = TRUE)

# Find a single person
people_entity$
  filter(FirstName.eq = 'Scott')$
  one()

# Show friends of that person
people_entity$
  get("scottketchum")$
  path('Friends')$
  all(simplifyVector = TRUE)
```

See vignette [demo](vignettes/demo.Rmd) for more examples.

## Other R packages

- [OData](https://cran.r-project.org/web/packages/OData/) - very basic
- [cbsodataR](https://cran.r-project.org/web/packages/cbsodataR/) - cbs data only
