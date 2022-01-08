subresource_key_set <- c(
  "response-content-type", "response-content-language",
  "response-cache-control", "logging", "response-content-encoding",
  "acl", "uploadId", "uploads", "partNumber", "group", "link",
  "delete", "website", "location", "objectInfo", "objectMeta",
  "response-expires", "response-content-disposition", "cors", "lifecycle",
  "restore", "qos", "referer", "stat", "bucketInfo", "append", "position",
  "security-token", "live", "comp", "status", "vod", "startTime", "endTime",
  "x-oss-process", "symlink", "callback", "callback-var", "tagging",
  "encryption", "versions", "versioning", "versionId", "policy",
  "requestPayment", "x-oss-traffic-limit", "qosInfo", "asyncFetch",
  "x-oss-request-payer", "sequential", "inventory", "inventoryId",
  "continuation-token", "callback", "callback-var", "worm", "wormId",
  "wormExtend", "replication", "replicationLocation", "replicationProgress",
  "transferAcceleration"
)

get_subresource_string <- function(query_args) {
  if (missing(query_args) | length(query_args) == 0 | is.null(query_args)) {
    return("")
  }
  subresource_args <- list()
  query_arg_name <- names(query_args)
  for (ii in seq_along(query_args)) {
    if (query_arg_name[ii] %in% subresource_key_set) {
      subresource_args[[query_arg_name]] <- query_args[[ii]]
    }
  }
  lc <- Sys.getlocale(category = "LC_COLLATE")
  Sys.setlocale(category = "LC_COLLATE", locale = "C")
  on.exit(Sys.setlocale(category = "LC_COLLATE", locale = lc))
  subresource_args <- unlist(subresource_args[order(names(subresource_args))])
  if (length(subresource_args) > 0) {
    subresource_string <- sprintf(
      "?%s", paste0(param_to_query_string(subresource_args),
                    collapse = "&"))
  } else {
    subresource_string <- ""
  }
  return(subresource_string)
}

#' 生成需要计算签名的资源字符串
#'
#' @description 生产需要计算签名的资源字符串
#' @param bucket 被请求OSS资源的 bucket
#' @param path 被请求OSS资源的路径(object)
#' @param query_args 额外的请求参数, 包含自资源的请求参数等
#' @details 用户发送请求中想访问的OSS目标资源被称为CanonicalizedResource; 需要访问的OSS资源格式为/bucket/path, 如果请求的资源包括子资源（SubResource），则将所有的子资源按照字典序，从小到大排列并以&为分隔符生成子资源字符串
#' @return 一个需要被包括在签名计算中的资源字符串
#' @seealso \href{https://help.aliyun.com/document_detail/31951.html#title-wnt-ob2-f9f}{构建CanonicalizedResource的方法}
get_resource_string <- function(bucket = "",
                                path = "",
                                query_args = NULL) {
  if (is.null(bucket)) {
    return(paste0("/", get_subresource_string(query_args)))
  } else {
    return(sprintf(
      "/%s/%s%s", bucket, path, get_subresource_string(query_args)
    ))
  }
}
