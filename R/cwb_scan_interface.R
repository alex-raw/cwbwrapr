#' Create a data grid
#'
#' Create a grid with combinations of parameters to scan. This
#' includes a look-up table to replace alternative attribute names. Depends on
#' installation
#'
#' @param parameters Named list of parameters for "corpus", "s_attr", "p_attr"
#' @import data.table
#' @export

get_data_grid <- function(parameters) {
  # create parameter combinations from list
  data.table::data.table(
    expand.grid(parameters, stringsAsFactors = FALSE)
    )[
    corpus %in% c("DTA", "DTA2017", "BASE") &
    s_attr == "text_id", s_attr := "file_id"
    ][
    corpus %in% c("BNC", "BNC-BABY") &
    p_attr == "lemma", p_attr := "hw"
  ][]
}

#' Call cwb-scan-corpus with vectors of parameters
#'
#' This calls cwb-scan-corpus from parameters provided by vectors and all their
#' combinations. Currently one p_attribute and one s_attribute are hardcoded.
#'
#' @param dir_path Name of the directory to save files in
#' @param corpus Name of the encoded cwb corpus.
#' @param p_attr positional attribute to scan
#' @param s_attr structural attribute to scan
#' @param constraint additional character string with constraints, see `man cwb-scan-corpus`
#' @export

cwb_scan <- Vectorize(
  function(dir_path, corpus, p_attr, s_attr, constraint = NULL) {
    # TODO: test constraint
    # call cwb-scan-corpus and save result to file in directory
    if (!dir.exists(dir_path)) dir.create(dir_path)
    filename <- paste0(dir_path, corpus, ".", p_attr, ".", s_attr)
    system(paste(
      "cwb-scan-corpus -o", filename, corpus, p_attr, s_attr, constraint)
    )
}, vectorize.args = c("corpus", "p_attr", "s_attr", "constraint"))

#' CWB-Scan interface for R
#'
#' A convenience wrapper for scripting with cwb-scan. Creates data with
#' cwb-scan-corpus. `scan_import()` additionally imports the entire directory
#' with fread as data.table using `fread()` and `rbindlist()`.
#'
#' @param dir_path Name of directory to save results in
#' @param parameters list of character vectors with corpora, p_attributes and
#' s_attributes
#' @param col_names column names for the parameters. by default read from the
#' names of the list provided in parameters
#'
#' @examples
#' lol <- list(
#'   corpus = c("BASE", "BROWN", "FROWN"),
#'   p_attr = c("word", "lemma"),
#'   s_attr = c("text_id")
#' )
#'
#' full <- scan_import("data/", lol)
#' full <- import_from_dir("data/", names(lol))
#' call_scan("data/", lol)
#' @export

call_scan <- function(dir_path, parameters) invisible(
  with(get_data_grid(parameters), cwb_scan(dir_path, corpus, p_attr, s_attr))
)

#' @rdname call_scan
#' @export

import_from_dir <- function(dir_path, col_names) {
  # import scanned files; create columns with corpus and attributes
  files <- dir(dir_path, full.names = TRUE)
  data.table::rbindlist(idcol = "corpus",
    sapply(files, freq_list_to_dt, simplify = FALSE)
  )[, (col_names) := lapply(FUN = as.factor,
        data.table::tstrsplit(split = ".", fixed = TRUE,
        gsub(paste0(dir_path, "/"), "", corpus)
  ))]
}

freq_list_to_dt <- function(dir_path) {
  data.table::fread(dir_path,
    sep = "\t",
    quote = "",
    na.string = "",
    header = FALSE,
    fill = TRUE,
    strip.white = TRUE,
    stringsAsFactors = TRUE,
    col.names = c("count", "p_attr", "s_attr")
  )
}

#' @rdname call_scan
#' @export

scan_import <- function(dir_path, parameters, col_names = names(parameters)) {
  # scan and create file; read them into data.table
  call_scan(dir_path, parameters)
  import_from_dir(dir_path, col_names)
}
