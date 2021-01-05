library(ODataQuery)

# Root with /
service <- ODataQuery$new("testurl.org/")
expect_equal(service$url, "testurl.org/")

# Root without /
service <- ODataQuery$new("testurl.org")
expect_equal(service$url, "testurl.org/")

# Resource or singleton
item_resource <- service$path("Items")
expect_equal(item_resource$url, "testurl.org/Items")

# Queries
expect_equal(item_resource$select("First", "Second", "Third")$url,
             "testurl.org/Items?$select=First,Second,Third")

expect_equal(item_resource$skip(10)$top(5)$url,
             "testurl.org/Items?$skip=10&$top=5")

expect_equal(item_resource$expand("Prices")$url,
             "testurl.org/Items?$expand=Prices")

expect_equal(item_resource$filter("Quantity > 0", Value.gt = 100)$url,
             "testurl.org/Items?$filter=(Quantity%20%3E%200%20and%20Value%20gt%20100)")

expect_equal(item_resource$orderby("Price", "Quality")$url,
             "testurl.org/Items?$orderby=Price,Quality")
