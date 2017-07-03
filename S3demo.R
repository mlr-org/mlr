baz <- function(x) UseMethod("baz", x)
baz.A <- function(x) "A"
baz.B <- function(x) "B"

ab <- structure(1, class = c("A", "B"))
ba <- structure(1, class = c("B", "A"))
baz(ab)
baz(ba)



baz.C <- function(x) c("C", NextMethod())
ca <- structure(1, class = c("C", "B", "A"))
cb <- structure(1, class = c("C", "B"))
baz(ca)
baz(cb)

baz.D <- function(x) {
  class(x) <- "F" ## does not work
  x$l = "3"
  list(x$l,NextMethod())  ## NextMethod is like super() in Java
}
da <- structure(1, class = c("D", "A"))
db <- structure(1, class = c("D", "B"))
baz(da)
baz(db)



baz.E <- function(x) NextMethod()
ca <- structure(1, class = c("E", "B", "A"))
cb <- structure(1, class = c("E", "A", "B"))
baz(ca)
baz(cb)
