#' Query CQP from R
#'
#' A convenience wrapper generating `system()` calls to cqp -c
#' Requires a working installation of cwb
#'
#' @param query character string with the cqp commands to be used
#' if multiple commands are stringed together, `;` has to be used as separator
#' @param corpus character string; optionally provide corpus to be activated
#' @param filename name of the file to save the results to

#' @examples
#'\dontrun{
#' cqp_query <- '
#' "example"
#' '
#' filename <- "lol"
#' query_cqp(cqp_query, "BROWN", "lol_a_file.txt")
#'}

#' @export
query_cqp <- function(query, corpus = NULL, filename) {
  system(paste0(
    "echo '", corpus, "; ", query,
    '; cat Last > "', filename, "\";' | cqp -c"
  ))
}
