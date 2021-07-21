# ODataQuery 0.5.3

* Concessions to get it published on cran:
- By default print, doesn't print any records.
- `prefix_functions` accepts dummy dot-arguments but raises an error when called with more than one arguments

# ODataQuery 0.5.2

* Make all fields of ODataQuery private except `url`.
* Constructor arguments `.resource` and `.query_options` that should not be used directly get a leading dot.
* ODataQuery is no longer cloneable (It's immutable now anyway).

# ODataQuery 0.5.1

* `top` and `skip` get 10 as default parameter.
* `print` by default prints the whole data.frame.

# ODataQuery 0.5.0

* New function `to_odata` that translates R code for use in an OData query.
* `and_query`, `or_query` and `not_query` now return an `odata_query`.

# ODataQuery 0.4.0

* Change interaction with jsonlite.
Functions that retrieve data no longer take additional arguments that are passed to jsonlite.
Additional parameters to jsonlite can from now on be supplied by a parameter `jsonlite_args`.

* Pass additional options to `httr::GET`.
Additional parameters to jsonlite can from now on be supplied by a parameter `httr_args`.
This can be useful if you are behind a proxyserver or if authentication is required.

* Method `retrieve` on ODataQuery takes a new parameter `count`.
