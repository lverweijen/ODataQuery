[![CRAN](http://www.r-pkg.org/badges/version/ODataQuery)](http://cran.r-project.org/package=ODataQuery)
[![status](https://tinyverse.netlify.com/badge/ODataQuery)](https://CRAN.R-project.org/package=ODataQuery)
[![Downloads](http://cranlogs.r-pkg.org/badges/ODataQuery)](http://cran.r-project.org/package=ODataQuery)


# ODataQuery #
Query on R OData

This package aims to make OData services more accessible to R users.

## Installation ##

Cran version:

```R
install.packages("ODataQuery")
```


Development version:

```R
install.packages("devtools")
devtools::install_github("https://github.com/lverweijen/ODataQuery")
```

## Example usage ##

```R
# Initialisation
url <- "https://services.odata.org/V4/TripPinServiceRW"
trip_service <- ODataQuery$new(url)
people_entity <- trip_service$path("People")

# Find all people whose name starts with an R
people_entity$
  select("UserName", "FirstName", "LastName")$
  filter(to_odata(startsWith(FirstName, "R")))$
  all()

# Find a person named Scott
people_entity$
  filter(to_odata(FirstName == "Scott"))$
  one()

# Find Scott's friends
people_entity$
  get("scottketchum")$
  path("Friends")$
  all()
```

See vignettes [demo](vignettes/demo.Rmd) and [querying](vignettes/querying.Rmd) for more examples.

## Other R packages dealing with OData ##

- [OData](https://CRAN.R-project.org/package=OData) - very basic
- [cbsodataR](https://CRAN.R-project.org/package=cbsodataR) - for data from Statistics, the Netherlands in particular
- [odataR](https://github.com/HanOostdijk/odataR/)

