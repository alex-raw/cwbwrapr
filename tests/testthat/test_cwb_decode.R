test_that("decode command", {
  cmd <- cwb_decode("BNC", c("word", "hw"), c("text_id", "text_genre"))
  expect_length(cmd, 1)
  expect_vector(cmd, "")
})

test_that("corpus name is checked correctly", {
  expect_error(cwb_decode("", c("word", "hw"), c("text_id", "text_genre")))
})
