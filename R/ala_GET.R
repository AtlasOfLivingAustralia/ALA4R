# Wrapper for getting data
#
# Try using crul
ala_GET <- function(url, path, params = list()) {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      useragent = ala_config()$user_agent
    )
  )
  res <- cli$get(path = path, query = params, encode = "json")
  if (res$status_code == "504") {
    stop("Status code 504 returned for url",
         res$request$url)
  } else if (res$status_code != 200) {
    stop("Status code ", res$status_code, "returned for url ",
         res$request$url)
  }
  data <- res$parse("UTF-8")
  return(fromJSON(data, flatten = TRUE))
}

cache_filename <- function(args, ext) {
  filename <- paste0(digest(sort(args)), ext)
  file.path(ala_config()$cache_directory, filename)
}
