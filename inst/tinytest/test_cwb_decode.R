ignore(expect_error)(cwb_decode("BNC", c("word", "hw"), c("text_id", "text_genre")))
ignore(expect_error)(cwb_decode("", c("word", "hw"), "text_id"))

cmd <- cwb_decode("BNC", c("word", "hw"), "text_id")
ignore(expect_equal)(length(cmd), 1)
ignore(expect_true)(is.vector(cmd))
