test_that("find in cwb registry", {
  skip_on_cran() # depends on local cwb install
  expect_true(in_cwb_registry("BNC"))
  expect_false(in_cwb_registry("BCN"))
  expect_false(in_cwb_registry(""))
  expect_error(in_cwb_registry(character(0)))
})

test_that("check if attribute exists", {
  skip_on_cran() # depends on local cwb install
  expect_true(has_cwb_attr("BNC", "word"))
  expect_true(has_cwb_attr("BNC", "text_id"))
  expect_false(has_cwb_attr("BNC", "wodr"))
  expect_false(has_cwb_attr("BNC", "textid"))
  expect_error(has_cwb_attr("BNC", character(0)))
})
