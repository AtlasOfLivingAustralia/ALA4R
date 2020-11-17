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
  
  # workaround for fq troubles
  if (length(params$fq) > 1) {
    cli$url <- build_fq_url(url, path, params)
    res <- cli$get()
  } else {
    cli$url <- url
    res <- cli$get(path = path, query = params, encode = "json")
  }
  
  #print(res$request$url)
  if (res$status_code == "504") {
    stop("Status code 504 returned for url",
         res$request$url)
  } else if (res$status_code == "403") {
    stop("Status code 403 was returned. This may be because the email you",
         " provided is not registered with the ALA. 
         Please check and try again. ")
  }  else if (res$status_code != 200) {
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

build_fq_url <- function(url, path, params = list()) {
  url <- parse_url(url)
  url$path <- path
  url$query <- params[names(params) != "fq"]
  join_char <- ifelse(length(url$query) > 0, "&fq=", "?fq=")
  fq <- paste(params$fq, collapse = "&fq=")
  paste0(build_url(url), join_char, URLencode(fq))
}
