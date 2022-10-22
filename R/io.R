#' Read and write plain text frequency lists
#'
#' @description Convenience wrapper to import typical output from CWB tools.
#' The expected file format is tab delimited, only containing data, no quotes,
#' missing values, comments or header.
#'
#' @param .x character. path to file or connection, see `scan()`
#' @param header logical. whether or not the first line should be used as column
#' names. Note: `names` in cols take precedence.
#' @param cols list. a list with column types (e.g. `0`, for numeric,
#' `""` for character), if named, the names will be used as column/vector names.
#' By default, column 1 is expected to contain integer frequencies and column 2
#' strings with types
#' @param sep character.
#' @param comment.char character.
#' @param quote character.
#' @param na.strings character.
#' @param nlines integer. number of lines in file, see `nlines` in `scan()`
#' @param skip integer. how many lines to skip, see `scan()`
#' @param ... further arguments to be passed to `scan`
#'
#' @return data.frame or data.table
#'
#' @seealso [scan(), fread()]
#' @details These are convenience wrappers around `scan` or `fread` with sane
#' defaults for common frequency list formats.
#' In `read_freqs`, `wc -l` is run if available to pass the line number to scan
#' This reduces memory overhead substantially and can also be a bit faster.
#'
#' @examples
#' \dontrun{
#' path <- "brown_word_per_id.txt"
#' out <- read_freqs(path, list(f = 0L, type = "", text_id = ""))
#' }
#' @export
read_freqs <- function(file, header = FALSE, cols = list(0L, ""),
                       sep = "\t", comment.char = "", na.strings = "",
                       quote = "", allowEscapes = FALSE,
                       nlines = sh_count_lines(file), ...) {
  .scan <- \(...) scan(..., sep = sep, comment.char = comment.char,
    na.strings = na.strings, quote = quote, allowEscapes = allowEscapes,
    quiet = TRUE)

  .colnames <- names(cols)
  if (!is.null(.colnames) && isTRUE(header)) {
    warning("Both `header=TRUE` and names found in `cols`: using `names(cols)`")
  }

  .colnames <- if (is.null(.colnames) && isTRUE(header)) {
     .scan(file, what = "", nlines = 1)
  } else {
    c("f", "type")
  }

  res <- .scan(file, what = cols, nlines = nlines, skip = header)

  data.frame(res) |>
    `colnames<-`(.colnames)
}

#' @rdname read_freqs
#' @export
write_freqs <- function(..., sep = "\t", quote = FALSE, na = "") {
  write.table(..., sep = sep, quote = quote, na = "", row.names = FALSE)
}

#' @rdname read_freqs
#' @export
fread_freqs <- function(..., header = FALSE, sep = "\t", quote = "",
                        na.strings = NULL, stringsAsFactors = FALSE) {
  do.call(data.table::fread, as.list(match.call()[-1]))
}

#' @rdname read_freqs
#' @export
fwrite_freqs <- function(..., sep = "\t", sep2 = " ", quote = FALSE) {
  data.table::fwrite(..., sep = sep, sep2 = sep2, quote = quote)
}

#' Read and write VRT or WPL files
#'
#' @description Import VRT (verticalized text) or WLP (word per line) files
#'
#' @param .x character. path to file or connection, see `scan()`
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
#' @seealso [read.table()]
#' @details These formats are used by common corpus indexing software, like CWB
#' (.vrt) and Sketchengine (.wpl). These functions are convenience wrappers with
#' to read from and create such file formats.
#'
#' @export
read_vrt <- function(file, no_xml = FALSE) {
  # TODO: https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf
}

#' @rdname read_vrt
#' @export
read_wpl <- function(file, no_xml = FALSE) {
  # TODO: https://www.sketchengine.eu/documentation/preparing-corpus-text/
}

#' @rdname read_vrt
#' @export
write_vrt <- function(file) {
  # TODO: https://cwb.sourceforge.io/files/CWB_Encoding_Tutorial.pdf
  # a. xml must be on separate line

  # https://fedora.clarin-d.uni-saarland.de/teaching/Comparing_Corpora_Tutorials/Tutorial_VRT.html
  # 1. element and attribute names may only consists of the characters [a-zA-Z0-9_], i.e. no white spaces, no dashes or diacritics to name just the most frequent error sources
  # 2. values are principally free text; BUT ATTENTION: " signals the end of a value so it cannot be part of a value (you can use singel quotes instead).
  # 3. XML-elements have to be on a separate single line - XML-elements spanning more than one line cannot be properly identified and lead to errors in the processing
  # 4. pointed brackets (<>) in token level annotation may also lead to errors in the processing as they might be misinterpreted as a broken XML-element
  # 5. there must be at least one <text>-element with an obligatory unique identifier (ID) as attribute-value pair.
  # 6. the attribute for the ID has to represented with the attribute id in lower case letters and has to be the first attribute-value pair of the <text>-element
  # 7. attribute-value pairs you want to use in the frequency distribution implemented in CQPweb (e.g. information on register, year) may consist only of the characters [a-zA-Z0-9_] again no white space, dashes or diacritics (among others)
  # 8. for importing of pre-encoded corpora <s>-elements are required

}

#' @rdname read_vrt
#' @export
write_wpl <- function(file) {
  # TODO: https://www.sketchengine.eu/documentation/preparing-corpus-text/
  # Sketch engine:
  # glue tag -> single <g/> to signalize that there shouldn't be a space
  # ambiguity tags
  # brush   NN;VV    brush
}
