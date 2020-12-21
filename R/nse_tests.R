# nse_tests

# to_odata(5 + 4)

# to_odata(3 <= x)

# to_odata(5 + 4 * 3 - 2 / 7 == (a - b) / c * d %% 23)
# "5 add 4 mul 3 sub 2 divby 7 eq (a sub b) divby c mul d mod 23"

# to_odata(a == 3 and b != 6 || d < 9 && not lala >= bar)
# [1] "a eq 3 and b ne 6 or d lt 9 and not(lala ge bar)"

# to_odata(paste(a, "b", c, sep=", "))
# "concat(concat(concat(concat(a,', '),'b'),', '),c)"

# to_odata(all(Friends, f ~ f$FirstName == 'John'))
# "Friends.all(f:f.FirstName eq 'John')"

# to_odata(any(Friends, f ~ f$FirstName == 'John'))
# "Friends.any(f:f.FirstName eq 'John')"

# to_odata(a$b$c - (3))
# "a.b.c sub (3)"

# > to_odata(startsWith(Naam, "R_"))
# [1] "startswith(Naam,'R_')"

# to_odata(a(b(c(x, 2), 3), 4))
# "a(b(c(x,2),3),4)"

# > to_odata(list(1, a, 2, 3, "hello"))
# [1] "[1,a,2,3,\"hello\"]"

# to_odata(a + b + c)

# process_dollar(rlang::expr(a$b$c$d$e(3$f$g)))
# a$b$c$d$e(`3/f/g`)

# Name in ('Milk', 'Cheese')
# > to_odata(Name %in% list('Milk', 'Cheese'))
# odata_expr:  Name in ["Milk","Cheese"]

# > to_odata(5 + "chicken")
# odata_expr:  5 add 'chicken'
# > to_odata(5 %add% "chicken")
# odata_expr:  5 add 'chicken'

# > to_odata(list(FirstName, LastName) %in% list(list("John","Doe"), list("Jane","Smith")))
# odata_expr:  [FirstName,LastName] in [["John","Doe"],["Jane","Smith"]]

# Error
# > to_odata(Items$any(d ~ d$Quantity > 100))
# odata_expr:  Items/any(d:d/Quantity gt 100)

# Good
# to_odata(any(d ~ d$Quantity > 100))
# odata_expr:  any(d:d.Quantity gt 100)

# > to_odata(5:3)
# odata_expr:  [5,4,3]

# > to_odata("hello" %in% c("hello", "goodbye"))
# odata_expr:  'hello' in ("hello","goodbye")

# > to_odata(c(hello) == c("hello"))
# odata_expr:  (hello) eq ("hello")