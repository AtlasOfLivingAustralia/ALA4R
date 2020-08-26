#' Intersect environmental or contextual layers at a given a set of points
#' (coordinates)
#'
#' This function allows the user to sample environmental/contextual layers at
#' arbitrary locations. It complements
#' the \code{\link{occurrences}} function, which allows values of the same set
#' of layers to be downloaded at species occurrence locations.
#' NOTE: Requests are currently processed in a *single queue* on the ALA
#' servers. Processing times may be slow if there are many requests in the
#' queue. Note also that the actual processing of batch requests is inherently
#' slow: a large number of points may take quite some time. Be warned.
#'
#' @references The associated ALA web service:
#' \url{https://api.ala.org.au/#ws84}
#' @references Descriptions of the spatial layers:
#' \url{https://spatial.ala.org.au/ws/layers/index/}
#' @param pnts numeric: vector of latitude/longitude pairs, or a 2 column
#' data.frame or matrix of lat, lons. NOTE: the number of locations must be
#' less than 100000
#' @param layers string vector: ids of layers to be intersected. The list of
#' possible layers is available from \code{ala_fields("layers")}. Names can be
#' passed as full layer names (e.g. "Radiation - lowest period (Bio22)") rather
#' than id ("el871"). Note: if more than one location has been provided in
#' \code{pnts}, the number of layers must be less than 700
#' @param SPdata.frame logical: should the output should be returned as a
#' SpatialPointsDataFrame of the sp package or simply as a data.frame?
#' @param use_layer_names logical: if TRUE, layer names will be used as column
#' names in the returned data frame (e.g. "radiationLowestPeriodBio22").
#' Otherwise, layer id value will be used for column names (e.g. "el871")
#' @param verbose logical: show additional progress information?
#' [default is set by \code{\link{ala_config}}]
#' @return A SpatialPointsDataFrame containing the intersecting data
#' information. Missing data or incorrectly identified layer id values will
#' result in NA data
#' @seealso \code{\link{ala_config}}
#' @examples
#' \dontrun{
#'  ## single point with multiple layers
#'  layers <- c("cl22", "cl23", "el773")
#'  pnts <- c(-23.1, 149.1)
#'  intersect_points(pnts, layers)
#'
#'  ## equivalent direct web service call:
#'  ## https://spatial.ala.org.au/ws/intersect/cl22,cl23,el773/-23.1/149.1
#'
#'  ## multiple points as a grid sampling multiple layers
#'  layers <- c("cl22", "cl23", "el773")
#'  pnts <- data.frame(expand.grid(lat=seq(-29, -19, by=2),
#'  lon=seq(130, 140, by=2)))
#'  intersect_points(pnts, layers)
#' }

## Previous limits of 1000 points and 299 layers have been increased here to
## reflect the increase on the service end. The batch version uses POST now to
## avoid 414 (URL too long) errors.

#' @export
intersect_points <- function(pnts, layers, SPdata.frame = FALSE,
                             use_layer_names = TRUE,
                             verbose = ala_config()$verbose) {
    ## input parameter checking
    assert_that(is.numeric(pnts) || all(apply(pnts, 2, is.numeric)))
    assert_that(is.character(layers))
    assert_that(is.flag(SPdata.frame))
    assert_that(is.flag(verbose))

    num_points_limit <- 100000
    num_layers_limit <- 700

    if (!is.data.frame(pnts) & !is.matrix(pnts)) {
      # convert to matrix
      # must be an even number of points
      if (length(pnts) %% 2 == 1) {
        stop("vector of points must be paired locations ...
               the length of this vector must be even")
      }
      pnts <- matrix(pnts, nrow = length(pnts) / 2, byrow = TRUE)
    }

    # check the dimensions
    if (dim(pnts)[2] != 2) {
      stop("data.frame or matrix of points must have 2 columns ordered
             lat, lon")
    }
    if (nrow(pnts) > (num_points_limit + 1)) {
      stop("number of locations checked must be less than ",
           num_points_limit)
    }
    # build points string
    pnts_str <- paste(paste(pnts[, 1], pnts[, 2], sep = ","), collapse = ",",
                      sep = "")

    ##format the layers string
    valid_layers <- ala_fields("layers")$id
    ## replace long names with ids
    layers <- fields_name_to_id(fields = layers, fields_type = "layers")

    if (length(layers) > (num_layers_limit - 1)) {
      #ensure no more than 300 layers when bulk
      stop("the number of layers must be <", num_layers_limit)
    }

    unknown <- setdiff(layers, valid_layers) #get the different layers
    if (length(unknown) > 0) {
      # warn user of bad layer ids
      warning(paste(paste(unknown, collapse = ", "), "are invalid layer ids"))
      layers <- layers[-which(layers %in% unknown)] #remove offending layers
    }
    # nothing returned if no valid IDs provided
    if (length(layers) < 1) {
      stop("all layer ids provided were invalid")
    }

    layers_str <- paste(layers, collapse = ",", sep = "")

    url <- build_url_from_parts(getOption("ALA4R_server_config")$
                                  base_url_spatial, c("intersect", "batch"))
    body <- paste0("fids=", layers_str, "&points=", pnts_str)

    # include points and layers in cache filename
    cache_file <- build_url_from_parts(getOption("ALA4R_server_config")$
                                         base_url_spatial,
                                       c("intersect", "batch"),
                                       query = list(fids = layers_str,
                                                    points = pnts_str))

    this_cache_file <- ala_cache_filename(cache_file)

    if ((ala_config()$caching %in% c("off", "refresh")) ||
        (! file.exists(this_cache_file))) {
      ## fetch the data from the server
      status_url <- jsonlite::fromJSON(cached_post(url, body = body,
                                                   type = "text"))$statusUrl
      # get the data url
      data_url <- cached_get(status_url, type = "json", caching = "off")
      while (data_url$status != "finished") {
        #keep checking the status until finished
        if (data_url$status == "error") {
          stop("batch intersect has returned an error; please check
                       your inputs. ", getOption("ALA4R_server_config")$notify)
        }
        if (verbose) {
          if (data_url$status == "waiting" & data_url$waiting == "In queue") {
            message("Your job is in queue... please wait")
          } else {
            message("Your job is processing... please be
                                    patient")
          }
        }
        Sys.sleep(5)
        data_url <- cached_get(status_url, type = "json",
                               caching = "off") #get the data url
      }
      download_to_file(data_url$downloadUrl, outfile = this_cache_file,
                       binary_file = TRUE)
    } else {
      ## use the existing cached file
      if (verbose) message(sprintf("Using cached file %s",
                                   this_cache_file))
    }
    suppressWarnings(out <- read_csv_quietly(unz(this_cache_file, "sample.csv"),
                            as.is = TRUE, na.strings = c("NA", "n/a")))

    if (SPdata.frame) { #if output is requested as a SpatialPointsDataFrame
      ## coerce to SpatialPointsDataFrame class
      if (nrow(out) > 0) {
        out <- SpatialPointsDataFrame(coords = out[, c("longitude",
                                                       "latitude")],
                                      proj4string =
                                        CRS("+proj=longlat +ellps=WGS84"),
                                      data = out)
      }
    } else {
      out[out == "n/a"] <- NA
    }
    ## final formatting before return
    if (use_layer_names) {
      names(out) <- make.names(fields_id_to_name(names(out), "layers"))
    }
    ## rename vars for consistency
    names(out) <- rename_variables(names(out), type = "layers")
    ##return the output
    out
}
