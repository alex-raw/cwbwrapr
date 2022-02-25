in_cwb_registry <- function(corpus) {
  if (!length(corpus)) stop("`corpus` has length 0")

  !as.logical(system2(
    "cwb-describe-corpus", corpus, stdout = "/dev/null", stderr = "/dev/null"
  ))
}

has_cwb_attr <- function(corpus, attribute) {
  if (!length(corpus)) stop("`corpus` has length 0")
  if (!length(attribute)) stop("`attribute` has length 0")

  stats <- system2("cwb-describe-corpus", c("-s", corpus), stdout = TRUE)
  # might want to distinction between s-ATT and p-ATT
  any(grepl(paste("ATT", attribute), stats))
}
