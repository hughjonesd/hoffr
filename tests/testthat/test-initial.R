
# desired behaviour.
#
#

context("Basic hofs")


make_hof <- function() {
  hof( function(a = 1, b) {
    sprintf("a is %s and b is %s", a, b)
  })
}


test_that("hofs can be called like normal functions", {
  tmp <- make_hof()
  expect_equal(
        tmp(a = 3, b = 2)
        , "a is 3 and b is 2")
  expect_equal(
        tmp(b = 2, a = 4)
        , "a is 4 and b is 2")
})


test_that("hofs can be applied partially", {
  tmp <- make_hof()
  expect_equal(
        tmp(a = 3)(b = 2)
        , "a is 3 and b is 2")
  expect_equal(
        tmp(a = 5)(b = 6)
        , "a is 5 and b is 6")
})

test_that("Partially applied copies have separate environments", {
  tmp <- make_hof()
  tmp3 <- tmp(a = 3)
  expect_false(identical(environment(tmp), environment(tmp3)))
  expect_equal(tmp(5), "a is 1 and b is 5")
})


test_that("Arguments can be given by position", {
  # no defaults:
  tmp <- hof( function(a, b) sprintf("a is %s and b is %s", a, b))
  expect_equal(
        tmp(3, 2)
        , "a is 3 and b is 2")
  tmp3 <- tmp(3)
  expect_equal(
        tmp3(2)
        , "a is 3 and b is 2")
  expect_equal(
        tmp(b = 3, 2)
        , "a is 2 and b is 3")
  tmpb3 <- tmp(b = 3)
  expect_equal(
    tmpb3(2)
    , "a is 2 and b is 3")
})

test_that("Partial functions can be passed to new variables", {
  tmp <- make_hof()
  tmpa5 <- tmp(a = 5)
  expect_equal(
        tmpa5(b = 5)
        , "a is 5 and b is 5")
  expect_equal(
        tmpa5(b = 1)
        , "a is 5 and b is 1")
})


test_that("Default arguments work in hofs", {
  tmp <- make_hof()
  expect_equal(
        tmp(b = 5)
        , "a is 1 and b is 5")
})


test_that("Non-existent arguments throw an error", {
  tmp <- make_hof()
  expect_error(
        tmp(bad_argument = 1)
        )
})


test_that("Supplied arguments are evaluated eagerly", {
  tmp <- make_hof()
  expect_error(
        tmpx <- tmp(a = x)
        )
})


test_that("Default arguments are evaluated lazily", {
  tmp <- hof(function(x, y = x) paste0(x, y))
  expect_equal(
        tmp(1, 2)
        , "12")
  expect_equal(
        tmp(1)
        , "11")
  expect_equal(
        tmp(2)
        , "22")
})


test_that("hofs can be created with ...", {
  dotty <- hof(function (..., sep = "/", collapse) {
    paste(..., sep = sep, collapse = collapse)
  })
  expect_equal(
        dotty("a", c("b", "c"), collapse = "...")
        , "a/b...a/c")
  dottydash <- dotty(sep = "-")
  expect_equal(
        dottydash("a", c("b", "c"), collapse = "...")
        , "a-b...a-c")
})

test_that("Function arguments stick when hofs are passed to new envs", {
  tmp <- make_hof()
  tmpa3 <- tmp(a = 3)
  newcontext <- function (pass_me_in) {
    pass_me_in(b = 2)
  }
  expect_equal(newcontext(tmpa3), "a is 3 and b is 2")

  x <- 55
  tmpa55 <- tmp(a = x)
  expect_equal(newcontext(tmpa55), "a is 55 and b is 2")

})


test_that("Existing functions can be passed to hof", {
  apply_fnl <- hof(apply)
  m <- matrix(1:4, 2, 2)
  expect_equal(
        apply_fnl(m, 2, sum)
        , apply(m, 2, sum))
  row_apply <- apply_fnl(MARGIN = 2)
  expect_equal(
        row_apply(m, sum)
        , apply(m, 2, sum))
})



