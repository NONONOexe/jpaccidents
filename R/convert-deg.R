convert_deg <- function(dms) {
  dms_num <- as.numeric(dms)
  dms_str <- number(dms_num, accuracy = .00000001, digits = 8)

  d <- dms_str |>
    str_replace("^(.+)\\..{8}$", "\\1") |>
    str_replace_na("0")
  m <- dms_str |>
    str_replace("^.+\\.(.{2}).{6}$", "\\1") |>
    str_replace_na("0")
  s <- dms_str |>
    str_replace("^.+\\..{2}(.{2})(.{4})$", "\\1.\\2") |>
    str_replace_na("0")

  deg <- if_else(is.na(dms), dms_num, dms2deg(d, m, s))

  return(deg)
}
