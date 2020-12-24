# ODataQuery 0.5.0

* New function `to_odata` that translates R code for use in an OData query.
* `and_query`, `or_query` and `not_query` return an `odata_query` from now on.

# ODataQuery 0.4.0

* Change interaction with jsonlite
Functions that retrieve data no longer take additional arguments that are passed to jsonlite.
Additional parameters to jsonlite can from now on be supplied by a parameter `jsonlite_args`.

* Pass additional options to `httr::GET`
Additional parameters to jsonlite can from now on be supplied by a parameter `httr_args`.
This can be useful if you are behind a proxyserver or if authentication is required.

* Method `retrieve` on ODataQuery takes a new parameter `count`.

