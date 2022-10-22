#' Query CQP from R
#'
#' A convenience wrapper generating `system()` calls to cqp -c
#' Requires a working installation of cwb
#'
#' @param query character string with the cqp commands to be used
#' if multiple commands are stringed together, `;` has to be used as separator
#' @param corpus character optionally provide corpus to be used
#' @param n integer left and right context
#' @param path name of the file to save the results to

#' @examples
#' \dontrun{
#' cqp_query <- '"example"'
#' query_cqp(cqp_query, "BROWN")
#' }
#'
#' @export
query_cqp <- function(query, corpus = NULL, n = 5, path = NULL) {
  if (!is.null(corpus)) query <- paste(corpus, query, sep = ";")
  if (is.null(path)) path <- tempfile()
  cmd <- sprintf(
    r"('%s; %s; tabulate Last match[-%d]..match[%d] word > "%s";' | cqp -c)",
    corpus, query, n, n, path
  )
  system2("echo", cmd)
  readLines(path)
}
