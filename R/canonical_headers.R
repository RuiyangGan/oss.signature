get_headers_string <- function(headers) {
  canon_headers <- list()
  headers_keys <- names(headers)
  for (ii in seq_along(headers)) {
    lower_key <- tolower(headers_keys[ii])
    if (grepl("^x-oss", lower_key)) {
      canon_headers[[lower_key]] <- headers[[ii]]
    }
  }
  lc <- Sys.getlocale(category = "LC_COLLATE")
  Sys.setlocale(category = "LC_COLLATE", locale = "C")
  on.exit(Sys.setlocale(category = "LC_COLLATE", locale = lc))
  if (length(canon_headers) > 0) {
    canon_headers <- unlist(canon_headers[order(names(canon_headers))])
    # trim leading, trailing, and all non-quoted duplicated spaces
    trimmed_headers <- gsub("[[:space:]]{2,}", " ", trimws(canon_headers))
    header_string <- paste0(
      names(canon_headers), ":", trimmed_headers, "\n", collapse = "")
    return(header_string)
  } else {
    return("")
  }
}
