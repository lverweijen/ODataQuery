library(ODataQuery)

# Function to check
expect_equal_character <- function(x, y) {
  expect_equal(as.character(x), as.character(y))
}

## Test literals with to_odata_
expect_equal_character(
  to_odata_(5),
  "5")

expect_equal_character(
  to_odata_("5"),
  "'5'")

expect_equal_character(
  to_odata_(TRUE),
  "true")

expect_equal_character(
  to_odata_(FALSE),
  "false")

expect_equal_character(
  to_odata_(NULL),
  "null")

expect_equal_character(
  to_odata_(list(1, "lala")),
  "[1,\"lala\"]")

expect_equal_character(
  to_odata_(c(1)),
  "1")

## Test literals with to_odata
expect_equal_character(
  to_odata(c(1)),
  "(1)")

expect_equal_character(
  to_odata(c(1, 2, "hello")),
  "(1,2,'hello')")

expect_equal_character(
  to_odata(1:5),
  "[1,2,3,4,5]")

expect_equal_character(
  to_odata(5:3),
  "[5,4,3]")

## Test binary operators
expect_equal_character(
  to_odata(5 + 4),
  "5 add 4")

expect_equal_character(
  to_odata(3 <= x),
  "3 le x"
)

expect_equal_character(
  to_odata(a + b + d),
  "a add b add d")

expect_equal_character(
  to_odata(5 + 4 * 3 - 2 / 7 == (a - b) / e * d %% 23),
  "5 add 4 mul 3 sub 2 divby 7 eq (a sub b) divby e mul d mod 23")

expect_equal_character(
  to_odata((1 + 2) * (3 + 4)),
  "(1 add 2) mul (3 add 4)")

expect_equal_character(
  to_odata(a == 3 && b != 6 || d < 9 && ! lala >= bar),
  "a eq 3 and b ne 6 or d lt 9 and not(lala ge bar)")

## Test strings
expect_equal_character(
  to_odata(paste(a, "b", e, sep=", ")),
  "concat(concat(concat(concat(a,', '),'b'),', '),e)")

expect_equal_character(
  to_odata(startsWith(Naam, "R_")),
  "startswith(Naam,'R_')")

## Test attributes
expect_equal_character(
  to_odata(Friends$all(f ~ f$FirstName == 'John')),
  "Friends/all(f:f/FirstName eq 'John')")

expect_equal_character(
  to_odata(Friends$any(f ~ f$FirstName == 'John')),
  "Friends/any(f:f/FirstName eq 'John')")

expect_equal_character(
  to_odata(a$b$d - (3)),
  "a/b/d sub (3)")

expect_equal_character(
  to_odata(a(b(d(x, 2), 3), 4)),
  "a(b(d(x,2),3),4)")

expect_equal_character(
  to_odata(a$b$h$d$e(3$f$g)),
  "a/b/h/d/e(3/f/g)")

expect_equal_character(
  to_odata(Items$any(d ~ d$Quantity > 100)),
  "Items/any(d:d/Quantity gt 100)")

## Test lists
expect_equal_character(
  to_odata(list(1, a, 2, 3, "hello")),
  "[1,a,2,3,\"hello\"]")

expect_equal_character(
  to_odata(Name %in% list('Milk', 'Cheese')),
  "Name in [\"Milk\",\"Cheese\"]")

expect_equal_character(
  to_odata(list(FirstName, LastName) %in% list(list("John","Doe"), list("Jane","Smith"))),
  "[FirstName,LastName] in [[\"John\",\"Doe\"],[\"Jane\",\"Smith\"]]")

## Other tests
expect_equal_character(
  to_odata(5 + "chicken"),
  "5 add 'chicken'")

expect_equal_character(
  to_odata(5 %add% "chicken"),
  "5 add 'chicken'")

expect_equal_character(
  to_odata("hello" %in% c("hello", "goodbye")),
  "'hello' in ('hello','goodbye')")

expect_equal_character(
  to_odata(c(hello) == c("hello")),
  "(hello) eq ('hello')")

## Quick queries
expect_equal_character(
  and_query("Column eq OtherColumn",
            FirstName.startswith = 'A',
            LastName.eq = 'Scott'),
  "(Column eq OtherColumn and startswith(FirstName,'A') and LastName eq 'Scott')")

expect_equal_character(
  or_query("ExpireDate eq null",
           ExpireDate.lt = "2020-07-07"),
  "(ExpireDate eq null or ExpireDate lt '2020-07-07')")

expect_equal_character(
 not_query(or_query(Age.lt = 21, Age.gt = 65)),
 "not ((Age lt 21 or Age gt 65))")

