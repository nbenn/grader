#' @export
read_answers <- function(filename, col_spec, ...) {

  dat <- readr::read_csv(
    filename,
    na = "-",
    col_types = readr::cols(.default = readr::col_guess()),
    ...
  )
}
