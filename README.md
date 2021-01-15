# ODataQuery #
Query on R OData

This package aims to make OData services more accesible to R users.

## Installation ##

```R
install.packages("devtools")
devtools::install_github("https://github.com/lverweijen/ODataQuery")
```

## Example usage ##

```R
library(ODataQuery)

url <- "https://services.odata.org/V4/TripPinServiceRW"
trip_service <- ODataQuery$new(url)
people_entity <- trip_service$path("People")

# Find all people whose name starts with an R
people_entity$
  select("UserName", "FirstName", "LastName")$
  where(startsWith(FirstName, "R"))$
  all()

# Find a person named Scott
people_entity$where(FirstName == "Scott")$one()

# Find Scott's friends
people_entity$get("scottketchum")$path("Friends")$all()
```

See vignettes [demo](vignettes/demo.Rmd) and [querying](vignettes/querying.Rmd) for more examples.

## Other R packages dealing with OData ##

- [OData](https://cran.r-project.org/web/packages/OData/) - very basic
- [cbsodataR](https://cran.r-project.org/web/packages/cbsodataR/) - for data from Statistics, the Netherlands in particular
- [odataR](https://github.com/HanOostdijk/odataR/) - alternative to this package

