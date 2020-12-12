# ODataQuery 0.4.0

* Change interaction with jsonlite
Functions that retrieve data no longer take additional arguments that are passed to json_lite.
Additional parameters to jsonlite can from now on be supplied by a parameter jsonlite_args

* Pass additional options to httr::GET
Additional parameters to jsonlite can from now on be supplied by a parameter httr_args

* Parameter ODataQuery takes a new parameter count.
This can be useful if you are behind a proxyserver.

