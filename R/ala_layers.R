#' Search ALA spatial layers
#' 
#' @return dataframe of all spatial layers held by the ALA
#' @export ala_layers

ala_layers <- function() {
  # web service returns all layers so might as well do that
  url <- getOption("ALA4R_server_config")$base_url_spatial
  result <- ala_GET(url, "ws/layers")
  names(result) <- rename_columns(names(result), type = "layer")
  result <- result[names(result) %in% wanted_columns("layer")]
  result
}