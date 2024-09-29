#' Convert DMS (degree-minute-second) to decimal degrees
#'
#' This function converts DMS (degree-minute-second) to decimal degrees.
#' The input DMS values must be in the format of "DDDMMSSsss" where:
#'   * D: Degrees (1--3 digits)
#'   * M: Minutes (2 digits)
#'   * S: Seconds (2 digits)
#'   * s: Milliseconds (3 digits)
#'
#' @param dms A numeric vector of DMS values in the format of "DDDMMSSsss"
#' @return A numeric vector of decimal degrees.
#' @export
#' @examples
#' convert_deg(c("431007628", "1410328320"))
#' convert_deg(c(431007628, 1410328320))
convert_deg <- function(dms) {
  # Convert the input to character
  dms_str <- trimws(format(dms, scientific = FALSE))

  # Check if the input is a number consisting of 8 to 10 digits
  if (!all(grepl("^[0-9]{8,10}$", dms_str))) {
    warning("The input must be a number consisting of 8 to 10 digits.")
    dms_str[!grepl("^[0-9]{8,10}$", dms_str)] <- NA
  }

  # Extract degrees, minutes, and seconds
  d <- as.numeric(substr(dms_str, 1, nchar(dms_str) - 7))
  m <- as.numeric(substr(dms_str, nchar(dms_str) - 6, nchar(dms_str) - 5))
  s_int <- substr(dms_str, nchar(dms_str) - 4, nchar(dms_str) - 3)
  s_dec <- substr(dms_str, nchar(dms_str) - 2, nchar(dms_str))
  s <- as.numeric(s_int) + as.numeric(s_dec) / 1000

  # Check if minute and second are between 0 and 60
  m_invalid <- !is.na(m) & (m < 0 | 60 <= m)
  s_invalid <- !is.na(s) & (s < 0 | 60 <= s)
  if (any(m_invalid)) {
    warning("Minutes must be between 0 and 60.")
    m[m_invalid] <- NA
  }
  if (any(s_invalid)) {
    warning("Seconds must be between 0 and 60.")
    s[s_invalid] <- NA
  }

  # Calculate decimal degrees
  deg <- d + m / 60 + s / 3600

  return(deg)
}
