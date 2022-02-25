cwb_decode <- function(corpus, p_attrs, s_attrs) {
  if (system2("cwb-lexdecode", c("-S", corpus), stdout = "/dev/null")) {
    stop(corpus, " not in CWB registry")
  }

  paste(
    "cwb-decode", corpus,
    paste(" -S", s_attrs, collapse = ""),
    paste(" -P", p_attrs, collapse = "")
  )
}
