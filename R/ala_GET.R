# Wrapper for getting data
# 
# Try using crul
ala_GET <- function(url, path, params = list(), caching = "off") {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      useragent = ala_config()$user_agent
    )
  )
  filename <- cache_filename(url, path, params, ext = ".txt")
  if (caching == "on" && file.exists(filename)) {
    # read from existing file (should check this exists and is not empty)
    message("Reading existing file")
    return(fromJSON(readLines(filename)))
  } else {
    res <- cli$get(path = path, query = params, encode = "json")
    #print(res$url)
    if (res$status_code == "504") {
      stop("Status code 504 returned for url",
           res$request$url)
    } else if (res$status_code != 200) {
      stop("Status code ", res$status_code, "returned for url ", res$request$url)
    }
    data <- res$parse("UTF-8")
    if (caching != "off") {
      message("Writing to file")
      write(x = data, file = filename)
    }
    return(fromJSON(data, flatten = TRUE))
  }
}

cache_filename <- function(url, path = NULL, params = NULL, ext) {
  # build_url string
  url <- parse_url(url) 
  url$path <- path
  url$params <- params
  filename <- paste0(digest(build_url(url)), ext)
  file.path(ala_config()$cache_directory, filename)
}