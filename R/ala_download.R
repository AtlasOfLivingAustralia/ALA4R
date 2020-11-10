# Download a file
# so far needs to handle zip files and csv
ala_download <- function(url, path, params = list(), ext = ".csv",
                         cache_file = NULL) {
  assert_that(is.character(url))
  message("Downloading from url ", url)
  cli <- HttpClient$new(
    url = url,
    headers = list(
      useragent = ala_config()$user_agent
    )
  )

  res <- cli$get(path = path, query = params, disk = cache_file)
  if (ext == ".csv") {
    df <- read.csv(res$content, stringsAsFactors = FALSE)
    close(file(cache_file))
  } else {
    # for zipped files just return the path
    return(cache_file)
  }
  return(df)
}
