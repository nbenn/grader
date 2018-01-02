#' @export
read_answers <- function(filename, col_spec, ...) {

  dat <- readr::read_csv(
    filename,
    na = "-",
    col_types = readr::cols(.default = readr::col_character()),
    ...
  )

  read <- lapply(names(col_spec), function(col) {
    get(col_spec[[col]], mode = "function")(dat[[col]])
  })

  c(list(Info = dat[, !names(dat) %in% names(col_spec)]),
    setNames(read, names(col_spec)))
}

moodle_num <- function(x) {
  readr::parse_double(x)
}

moodle_str <- function(x) {
  readr::parse_character(x)
}

moodle_mc <- function(x) {
  readr::parse_character(x)
}

moodle_match <- function(x) {
  sep <- lapply(strsplit(x, "; ", fixed = TRUE), function(x) {
    if (length(x) == 1L && is.na(x)) return(NA)
    y <- strsplit(x, "\n -> ", fixed = TRUE)
    stopifnot(all(sapply(y, length) == 2L))
    setNames(lapply(y, `[[`, 2), sapply(y, `[[`, 1))
  })
  sort_rbind(sep)
}

moodle_cloze <- function(x) {
  sep <- lapply(strsplit(x, "; ", fixed = TRUE), function(x) {
    if (length(x) == 1L && is.na(x)) return(NA)
    res <- readr::parse_character(sub("^ ", "", sub("^part [0-9]+:", "", x)))
    nme <- regmatches(x, regexpr("^part [0-9]+", x))
    stopifnot(length(res) == length(nme))
    setNames(res, nme)
  })
  sort_rbind(sep)
}

moodle_tf <- function(x) {
  sep <- lapply(strsplit(x, "; ", fixed = TRUE), function(x) {
    if (length(x) == 1L && is.na(x)) return(NA)
    y <- strsplit(x, "\n: ", fixed = TRUE)
    stopifnot(all(sapply(y, length) == 2L),
              sapply(y, `[[`, 2) %in% c("Yes", "No"))
    setNames(lapply(y, `[[`, 2), sapply(y, `[[`, 1))
  })
  sort_rbind(sep)
}

sort_rbind <- function(x) {
  quests <- unique(unlist(lapply(x, names)))
  sorted <- lapply(x, function(y) {
    res <- y[match(quests, names(y))]
    res[sapply(res, is.null)] <- NA_character_
    as.character(res)
  })
  res <- do.call(rbind, sorted)
  colnames(res) <- quests
  as.data.frame(res)
}