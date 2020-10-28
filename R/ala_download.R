# Download a file

ala_download <- function(url, path, params = list()) {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      useragent = ala_config()$user_agent
    )
  )
  f <- tempfile()
  res <- cli$get(path = path, query = params, disk = f)
  df <- read.csv(res$content)
  close(file(f))
  return(df)
}
