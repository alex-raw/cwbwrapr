#' Decode entire corpus
#'
#' A convenience wrapper generating `system()` calls to cwb-decode
#' Requires a working installation of cwb
#'
#' @param corpus character name of the corpus in the cwb registry
#' @param p_attrs character vector with p-attributes, defaults to "word"
#' @param s_attrs character vector with s-attributes, defaults to "text_id"
#'
#' @examples
#' \dontrun{
#' cmd <- cwb_decode("BROWN", c("word", "lemma"), c("text_id", "text_genre"))
#' stopifnot(is.character(cmd), length(cmd) == 1)
#' }
#'
#' @export
cwb_decode <- function(corpus, p_attrs = "word", s_attrs = "text_id") {
  stopifnot(in_cwb_registry(corpus))

  if (length(s_attrs) > 1) {
    stop("multiple s_attrs currently not supported")
    # TODO: multiple s_attrs don't work due to identical regions in -S
    # attributes tabulating multiple s_attrs would require parsing xml output
    # due to limitations of cwb-decode
  }

  paste(
    "cwb-decode", corpus,
    paste(" -S", s_attrs, collapse = ""),
    paste(" -P", p_attrs, collapse = "")
  )
}
