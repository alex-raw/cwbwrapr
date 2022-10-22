# depends on local cwb install
ignore(expect_true)(cwbwrapr:::in_cwb_registry("BNC"))
ignore(expect_false)(cwbwrapr:::in_cwb_registry("BCN"))
ignore(expect_false)(cwbwrapr:::in_cwb_registry(""))
ignore(expect_error)(cwbwrapr:::in_cwb_registry(character(0)))

ignore(expect_true)(cwbwrapr:::has_cwb_attr("BNC", "word"))
ignore(expect_true)(cwbwrapr:::has_cwb_attr("BNC", "text_id"))
ignore(expect_false)(cwbwrapr:::has_cwb_attr("BNC", "wodr"))
ignore(expect_false)(cwbwrapr:::has_cwb_attr("BNC", "textid"))
ignore(expect_error)(cwbwrapr:::has_cwb_attr("BNC", character(0)))
