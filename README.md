# ODataQuery #
Query on R OData

This package aims to make OData services more accesible to R users.

## Installation ##

```R
packages.install("devtools")
devtools::install_github("https://github.com/lverweijen/ODataQuery")
```

## Example usage ##

```R
# Initialisation
URL <- "https://services.odata.org/V4/TripPinServiceRW"
trip_service <- ODataQuery$new(URL)
people_entity <- trip_service$path("People")

# Find all people whose name starts with an R
people_entity$
  select("UserName", "FirstName", "LastName")$
  filter(FirstName.startswith = "R")$
  all(simplifyVector = TRUE)

# Find a person named Scott
people_entity$
  filter(FirstName.eq = 'Scott')$
  one()

# Find Scott's friends
people_entity$
  get("scottketchum")$
  path('Friends')$
  all(simplifyVector = TRUE)
```

See vignette [demo](vignettes/demo.Rmd) for more examples.

## Other R packages dealing with OData ##

- [OData](https://cran.r-project.org/web/packages/OData/) - very basic
- [cbsodataR](https://cran.r-project.org/web/packages/cbsodataR/) - for data from Statistics, the Netherlands

