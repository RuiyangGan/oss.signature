#' 生成需要计算签名的请求头字符串
#'
#' @description 生成需要计算签名的请求头字符串
#' @param headers 一个named list of character string, 包含所有在请求中的头文件信息
#' @details 需要计算签名的请求头为CanonicalizedOSSHeaders, 所有以x-oss-为前缀的HTTP Header被称为CanonicalizedOSSHeaders
#' @references \href{https://help.aliyun.com/document_detail/31951.html#title-wbj-1a2-eyc}{CanonicalizedOSSHeaders构建方式}
#' @return 一个需要包括在签名计算中的请求头字符串, 编码为UTF-8
#' @export
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
