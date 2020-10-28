# Download a file
# so far needs to handle zip files and csv
ala_download <- function(url, path, params = list(), file_type = "csv") {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      useragent = ala_config()$user_agent
    )
  )
  f <- tempfile()
  res <- cli$get(path = path, query = params, disk = f)
  if (file_type == "csv") {
    df <- read.csv(res$content, stringsAsFactors = FALSE)
    close(file(f))
  } else {
    df <- read.csv(unz(f, "data.csv"), stringsAsFactors = FALSE)
    close(file(f))
  }
  return(df)
  
}
