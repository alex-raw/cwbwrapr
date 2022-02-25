#' Import plain text frequency lists
#'
#' @description Convenience wrapper to import typical output from CWB tools.
#' The expected file format is tab delimited, only containing data, no quotes,
#' missing values, comments or header.
#'
#' @param path character. path to file
#' @param cols list. a list with column types (e.g. `0`, for numeric,
#' `""` for character), if named, the names will be used as column/vector names.
#' By default, column 1 is expected to contain integer frequencies and column 2
#' strings with types
#' @param df boolean. If TRUE (default) a `data.frame` is returned.
#' @param n number of lines in file, see `nlines` in `scan()`
#' @param comment.char character. off by default. replace if necessary
#' @param quiet boolean. If TRUE, number of records is printed after import
#' @param ... further arguments to be passed to `scan` or `fread`
#' @param header see `scan` or `fread`
#' @param sep see `scan` or `fread`
#' @param quote see `scan` or `fread`
#' @param na.strings see `scan` or `fread`
#' @param stringsAsFactors see `scan` or `fread`
#'
#' @return If df == TRUE, a `data.frame`, else a `list`
#'
#' @seealso [scan()]
#' @details This is just a wrapper around `scan` that behaves similarly to
#' `read.table`.
#' The main difference is that `wc -l` is run on the file before
#' in order to get the number of lines to pass to `scan`.
#' This reduces memory overhead substantially and can also be a bit faster.
#'
#' @examples
#' \dontrun{
#' path <- "brown_word_per_id.txt"
#' out <- read_freqs(path, list(f = 0L, type = "", text_id = ""))
#' }
#' @export
read_freqs <- function(path, cols = list(f = 0L, type = ""), df = TRUE,
                       n = sh_count_lines(path), comment.char = "",
                       quiet = TRUE, ...) {
  out <- scan(
    path,
    what = cols,
    nlines = n,
    sep = "\t",
    quote = "",
    na.strings = "",
    allowEscapes = FALSE,
    quiet = quiet,
    ...
  )
  if (df) data.frame(out) else out
}

sh_count_lines <- function(path) {
  as.integer(
    system2("wc", c("-l", path, " | awk '{print $1}'"), stdout = TRUE)
  )
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

# read.table.cwb <- function(path, n, header = FALSE, comment.char = "", ...) {
#   if (missing(n)) stop("Need to specify number of columns to read")
#   read.table(path, header = header, comment.char = comment.char,
#     colClasses = c("integer", rep("character", n)),
#     sep = "\t",
#     quote = "",
#     na.strings = "",
#     fill = TRUE,
#     stringsAsFactors = FALSE,
#     row.names = NULL,
#     allowEscapes = FALSE,
#     ...
#   )
# }
