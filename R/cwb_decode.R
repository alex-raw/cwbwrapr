#' Decode entire corpus
#'
#' A convenience wrapper generating `system()` calls to cwb-decode
#' Requires a working installation of cwb
#'
#' @param corpus character name of the corpus in the cwb registry
#' @param p_attrs character vector with p-attributes
#' @param s_attrs character vector with s-attributes
#'
#' @examples
#' \dontrun{
#' cmd <- cwb_decode("BROWN", c("word", "lemma"), c("text_id", "text_genre"))
#' stopifnot(is.character(cmd), length(cmd) == 1)
#' }
#'
#' @export
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
