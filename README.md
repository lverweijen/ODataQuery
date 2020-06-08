# ODataQuery
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

# Find a person named Scott
people_entity$
  filter(FirstName.eq = 'Scott')$
  one()

# Find Scott's friends
people_entity$
  get("scottketchum")$
  path('Friends')$
  all(simplifyVector = TRUE)

# Statistics, the Netherlands
opendata_service <- ODataQuery$new("http://beta-odata4.cbs.nl/")
entity_81589NED  <- opendata_service$path("CBS", "81589NED", Observations")
dataset_81589NED <- entity_81589NED$all(simplifyVector = TRUE)

# Northwind (Older OData api v2)
northwind_service <- ODataQuery$new("https://services.odata.org/V2/Northwind/Northwind.svc/")
customer_entity <- northwind_service$path("Customers")
customer_entity$
  select("CompanyName", "Address", "Phone")$
  filter(Country.eq = "Germany", City.eq = "Berlin")
```

See vignette [demo](vignettes/demo.Rmd) for more examples.

## Other R packages dealing with OData

- [OData](https://cran.r-project.org/web/packages/OData/) - very basic
- [cbsodataR](https://cran.r-project.org/web/packages/cbsodataR/) - for data from Statistics, the Netherlands

