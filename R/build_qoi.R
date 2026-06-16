#' @export
build_qoi <- function(eoi,findtype,UFI,ULI,USI) {
  eoi <- trimws(eoi %||% "")
  findtype <- trimws(findtype %||% "")
  UFI <- trimws(UFI %||% "")
  ULI <- trimws(ULI %||% "")
  USI <- trimws(USI %||% "")

  # Only include bits that exist
  parts <- c(
    "The date (year)",
    if (nzchar(eoi)) paste("of",eoi,"of") else "associated with the event of interest from the life-cycle of",
    if (nzchar(findtype)) paste("the", findtype) else "an artefact",
    if (nzchar(UFI)) paste0("(", UFI, ")") else NULL,

    if (nzchar(ULI)) (if(tolower(eoi)=="deposition") paste0("in context ", ULI) else paste0("associated with context ", ULI)) else NULL,
    if (nzchar(USI)) paste0("(", USI,")") else NULL
  )
  paste(parts, collapse = " ")
}
