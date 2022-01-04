is_blank <- function(value) {
  return(is.null(value) || value == "")
}

vmsg <- function(verbose, msg, ...) {
  if (isTRUE(verbose)) {
    message(sprintf(msg, ...))
  }
}

param_to_query_string <- function(query_args) {
  param_string <- as.character(
    mapply(function(k, v) {
      ifelse(v != "", paste(k, v, sep = "="), k)
      }, names(query_args), query_args)
  )
  return(param_string)
}
