#' Import plain text frequency lists
#'
#' @description Convenience wrapper to import typical output from CWB tools.
#' The expected file format is tab delimited, only containing data, no quotes,
#' missing values, comments or header.
#'
#' @param path character. path to file
#' @param header logical. whether or not the first line should be used as column
#' names. Note: `names` in cols take precedence.
#' @param cols list. a list with column types (e.g. `0`, for numeric,
#' `""` for character), if named, the names will be used as column/vector names.
#' By default, column 1 is expected to contain integer frequencies and column 2
#' strings with types
#' @param sep character. see `scan`
#' @param comment.char character. off by default. replace if necessary
#' @param quote character. see `scan`
#' @param na.strings character. see `scan`
#' @param nlines integer. number of lines in file, see `nlines` in `scan()`
#' @param ... further arguments to be passed to `scan`
#'
#' @return data.frame
#'
#' @seealso [scan()]
#' @details This is a wrapper around `scan` that behaves similarly to
#' `read.table`.
#' The main difference is that, if available, `wc -l` is run on the file before
#' in order to get the number of lines to pass to `scan`.
#' This reduces memory overhead substantially and can also be a bit faster.
#'
#' @examples
#' \dontrun{
#' path <- "brown_word_per_id.txt"
#' out <- read_freqs(path, list(f = 0L, type = "", text_id = ""))
#' }
#' @export
read_freqs <- function(.x, header = FALSE, cols = list(0L, ""),
                       sep = "\t", comment.char = "", na.strings = "",
                       quote = "", allowEscapes = FALSE,
                       nlines = sh_count_lines(.x), ...) {

  .colnames <- names(cols)
  if (!is.null(.colnames) && isTRUE(header)) {
    warning("Both `header=TRUE` and names found in `cols`: using `names(cols)`")
  }

  res <- scan(.x, quiet = TRUE,
    what = cols, nlines = nlines, comment.char = comment.char, quote = quote,
    na.strings = na.strings, sep = sep, allowEscapes = allowEscapes, ...)

  .colnames <- if (is.null(.colnames) && isTRUE(header)) {
    vapply(res, \(x) as.character(x[[1]]), "")
  } else {
    c("f", "type")
  }

  res <- data.frame(res)
  colnames(res) <- .colnames
  res
}

sh_count_lines <- function(path) {
  cmd <- c("-l", path, " | awk '{print $1}'")
  n <- system2("wc", cmd, stdout = TRUE, stderr = NULL) |>
    tryCatch(warning = \(w) 0) |>
  as.integer()
}

#' @rdname read_freqs
#' @export
# TODO: write wrapper to cover this and read_freqs or merge using argument
fread_freqs <- function(..., header = FALSE, sep = "\t", quote = "",
                        na.strings = NULL, stringsAsFactors = FALSE) {
  data.table::fread(
    ...,
    header = header,
    sep = sep,
    quote = quote,
    na.strings = na.strings,
    stringsAsFactors = stringsAsFactors
  )
}

