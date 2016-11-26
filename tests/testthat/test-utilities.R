context("Data utilities")

test_that("string of doubles is changed to vector", {
  expect_equal(changeCsStringToDoubleVectorOrLeaveNull("1,2,3"), c(1,2,3))
  expect_equal(changeCsStringToDoubleVectorOrLeaveNull("1.5,2.9"), c(1.5,2.9))
  expect_equal(changeCsStringToDoubleVectorOrLeaveNull("10000000"), c(10000000))
  expect_equal(changeCsStringToDoubleVectorOrLeaveNull(""), NULL)
  expect_warning(changeCsStringToDoubleVectorOrLeaveNull("1-2-3"),
                 "NAs introduced by coercion")
})

test_that("empty string changed to null", {
  expect_equal(changeEmptyStringToNull(""), NULL)
  expect_equal(changeEmptyStringToNull("Lorem Ipsem"), "Lorem Ipsem")
})

test_that("string changed to vector", {
  expect_equal(changeCsStringToVectorOrLeaveNull("foo,bar,baz,quux"),
               c("foo", "bar", "baz", "quux"))
  expect_equal(changeCsStringToVectorOrLeaveNull("foo"), c("foo"))
  expect_equal(changeCsStringToVectorOrLeaveNull(""), NULL)
})

test_that("string changed to logical", {
  expect_equal(changeStringToLogical("T"), T)
  expect_equal(changeStringToLogical("F"), F)
  expect_error(changeStringToLogical("H"), "argument is not interpretable")
  expect_error(changeStringToLogical(""), "argument is not interpretable")
})

test_that("NULL as string changed to NULL", {
  expect_equal(changeStringNullToNull("NULL"), NULL)
  expect_equal(changeStringNullToNull("FOOBAR"), "FOOBAR")
  expect_equal(changeStringNullToNull(""), "")
})
