
### addImageQuery ############################################################
##############################################################################
#' Add image query functionality to leaflet/mapview map.
#'
#' @details
#' This function enables Raster*/stars objects added to leaflet/mapview maps to
#' be queried. Standard query is on 'mousmove', but can be changed to 'click'.
#' Note that for this to work, the \code{layerId} needs to be the same as the
#' one that was set in \code{\link[leaflet]{addRasterImage}} or
#' \code{\link{addStarsImage}}. Currently only works for
#' numeric values (i.e. numeric/integer and factor values are supported).
#'
#' @param map the map with the RasterLayer to be queried.
#' @param x the RasterLayer that is to be queried.
#' @param band for stars layers, the band number to be queried.
#' @param group the group of the RasterLayer to be queried.
#' @param layerId the layerId of the RasterLayer to be queried. Needs to be the
#'   same as supplied in \code{\link[leaflet]{addRasterImage}} or
#'   \code{\link{addStarsImage}}.
#' @param project whether to project the RasterLayer to conform with leaflets
#'   expected crs. Defaults to \code{TRUE} and things are likely to go haywire
#'   if set to \code{FALSE}.
#' @param type whether query should occur on 'mousemove' or 'click'. Defaults
#'   to 'mousemove'.
#' @param digits the number of digits to be shown in the display field.
#' @param position where to place the display field. Default is 'topright'.
#' @param prefix a character string to be shown as prefix for the layerId.
#' @param className a character string to append to the control legend.
#' @param ... currently not used.
#'
#' @return
#' A leaflet map object.
#'
#' @examples
#' if (interactive()) {
#'   if (requireNamespace("plainview")) {
#'     library(leaflet)
#'     library(plainview)
#'
#'     leaflet() %>%
#'       addProviderTiles("OpenStreetMap") %>%
#'       addRasterImage(poppendorf[[1]], project = TRUE, group = "poppendorf",
#'                      layerId = "poppendorf") %>%
#'       addImageQuery(poppendorf[[1]], project = TRUE,
#'                     layerId = "poppendorf") %>%
#'       addLayersControl(overlayGroups = "poppendorf")
#'   }
#' }
#'
#' @importFrom raster projectExtent projectRaster as.matrix
#'
#' @export addImageQuery
#' @name addImageQuery
#' @rdname addImageQuery
addImageQuery = function(map,
                         x,
                         band = 1,
                         group = NULL,
                         layerId = NULL,
                         project = TRUE,
                         type = c("mousemove", "click"),
                         digits,
                         position = 'topright',
                         prefix = 'Layer',
                         className = "",
                         ...) {
  
  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  
  type = match.arg(type)
  if (missing(digits)) digits = "null"
  if (is.null(group)) group = "stars"
  if (is.null(layerId)) layerId = group
  
  jsgroup <- gsub(".", "", make.names(group), fixed = TRUE)
  
  tmp <- leafem:::makepathStars(as.character(jsgroup))
  pathDatFn <- tmp[[2]][1]
  # starspathDatFn <- tmp[[3]][1]
  # datFn <- tmp[[4]][1]
  
  if (project) {
    if (inherits(x, "stars")) {
      if (utils::packageVersion("stars") >= "0.4-1") {
        projected = stars::st_warp(x, crs = 4326)
      } else {
        projected <- sf::st_transform(x, crs = 4326)
      }
    }
    if (inherits(x, "SpatRaster")) {
      projected <- terra::project(x, y = "EPSG:4326")
      }
    if (inherits(x, "Raster")) {
      projected = raster::projectRaster(
        x
        , raster::projectExtent(x, crs = sf::st_crs(4326)$proj4string)
        , method = "ngb"
      )
    }
  } else {
    projected <- x
  }
  
  pre <- paste0('var data = data || {}; data["', layerId, '"] = ')
  writeLines(pre, pathDatFn)
  cat('[', image2Array(projected, band = band), '];',
      file = pathDatFn, sep = "", append = TRUE)
  
  ## check for existing layerpicker control
  ctrlid = leafem:::getCallEntryFromMap(map, "addControl")
  ctrl_nm = paste("imageValues", layerId, sep = "-")
  imctrl = unlist(sapply(ctrlid, function(i) {
    ctrl_nm %in% map$x$calls[[i]]$args
  }))
  ctrlid = ctrlid[imctrl]
  
  # map = leaflet::clearControls(map)
  
  if (length(ctrlid) == 0) {
    # must add empty character instead of NULL for html with addControl
    map = leaflet::addControl(
      map,
      html = "",
      layerId = ctrl_nm,
      position = position,
      className = paste("info legend", className)
    )
  }
  
  sm <- leafem:::createFileId() #sample(1:1000, 1)
  map$dependencies <- c(map$dependencies,
                        leafem:::starsDataDependency(jFn = pathDatFn,
                                            counter = 1,
                                            group = paste0(layerId,"_",sm)))
  map$dependencies = c(map$dependencies,
                       list(htmltools::htmlDependency(
                         version = "0.0.1",
                         name = "joda",
                         src = system.file("htmlwidgets/lib/joda",
                                           package = "leafem"),
                         script = c("joda.js",
                                    "addImageQuery-bindings.js"))
                       ))
  
  bounds <- as.numeric(sf::st_bbox(projected))
  
  leaflet::invokeMethod(
    map
    , NULL
    , "addImageQuery"
    , layerId
    , bounds
    , type
    , digits
    , prefix
  )
}

###################################


stars2Array = function(x, band = 1) {
  if(length(dim(x)) == 2) layer = x[[1]] else layer = x[[1]][, , band]
  paste(
    sapply(seq(nrow(x[[1]])), function(i) {
      paste0(
        '['
        , gsub(
          "NA"
          , "null"
          , paste(as.numeric(layer[i, ]), collapse = ",")
        )
        , ']'
      )
    }),
    collapse = ","
  )
}


###################################


SpatRaster2Array = function(x) {
  x = terra::as.matrix(x, wide = TRUE)
  paste(
    sapply(seq(ncol(x)), function(i) {
      paste0(
        '['
        , gsub(
          "NA"
          , "null"
          , paste(as.matrix(x)[, i], collapse = ",")
        )
        , ']'
      )
    }),
    collapse = ","
  )
}


###################################


rasterLayer2Array = function(x) {
  x = as.matrix(x)
  paste(
    sapply(seq(ncol(x)), function(i) {
      paste0(
        '['
        , gsub(
          "NA"
          , "null"
          , paste(as.matrix(x)[, i], collapse = ",")
        )
        , ']'
      )
    }),
    collapse = ","
  )
}


###################################


image2Array = function(x, band = 1) {
  switch(class(x)[1],
         "SpatRaster" = SpatRaster2Array(x),
         "stars" = stars2Array(x, band = band),
         "RasterLayer" = rasterLayer2Array(x))
}


###################################


# Function that modifies existing leaflet::addLegend by adding an option for decreasing order
addLegend_decreasing <- function (map,
                                  position = c("topright", "bottomright", "bottomleft",
                                               "topleft"),
                                  pal,
                                  values,
                                  na.label = "NA",
                                  bins = 7,
                                  colors,
                                  opacity = 0.5,
                                  labels = NULL,
                                  labFormat = labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors))
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula"))
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins))
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1)
        pretty(values, bins)
      else bins
      
      if (length(bins) > 2)
        if (!all(abs(diff(bins, differences = 2)) <=
                 sqrt(.Machine$double.eps)))
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2,
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values)))
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels))
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                 na_color = na.color, na_label = na.label, opacity = opacity,
                 position = position, type = type, title = title, extra = extra,
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}


###################################


# Function to change CRS of coords, but leave as data.frame instead of converting to {sf} obj
st_changeCoords = function(data, coords, crs_in, crs_out) {
  # returns data.frame w/ cols named 'x' and 'y' for transformed coords
  
  dat.sf <- sf::st_as_sf(data, coords = coords, crs = crs_in) %>% 
    sf::st_transform(dat.sf, crs = crs_out) %>% 
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) %>% 
    st_drop_geometry()
  
  return(dat.sf)
}


###################################


# function to convert list object from rerddapXtracto::rxtracto() into a {terra} SpatRaster object
array2rast <- function(lon, lat, var, time, extent) {
  #lon: a vector of longitude values
  #lat: a vector of latitude values
  #var: an array of values for the variable of interest where the 1st two dimensions denote the spatial grid and the third dimensions represents the number of datetimes
  #time: a vector of dates associated with each 2D array from `var`
  #extent: a SpatRaster extent on which to spatially define the raster; ordered as xmin, xmax, ymin, ymax
  
  dims <- dim(var)
  
  # check dims in case "altitude/depth" is included; this code will only select 1st slice of altitude
  if (length(dims) == 4) {
    var <- var[,,1,]
  } else {
    var <- var
  }
  
  dims1 <- dim(var)
  
  rast1 <- terra::rast(t(var), crs = 'EPSG:4326', extent = extent) %>%
      terra::flip(direction = "vertical")
  names(rast1) <- time
  
  return(rast1)
}


###################################


# function based on that from Michael Sumner Github gist (related to Pull Request for {terra})
# https://gist.github.com/mdsumner/aaa6f1d2c1ed107fbdd7e83f509a7cf3
get_elev <- function(x, method = "bilinear", maxcell = 25e6, silent = TRUE) {
  if (terra::ncell(x) > maxcell) {
    stop("number of cells in x exceeds 'maxcell'")
  }
  
  src <- "/vsicurl/https://gebco2023.s3.valeria.science/gebco_2023_land_cog.tif"
  # src <- "/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2021_GEOTIFF/GEBCO_2021.tif"
  
  terra::project(terra::rast(src), x, by_util = TRUE, method = method)
}


###################################


predict.hgpr <- function(cov_list, model_fit, covars, mesh.seq, nbasis, degree) {
  #cov_list = list of different environ covars as SpatRasters
  #model_fit = a fitted HGPR INLA model object
  #covars = character vector of covar names as used in model_fit
  #mesh.seq = list of covar ranges as used for model_fit
  #nbasis = number of basis functions used for model_fit
  #degree = degree value used for model_fit
  #alpha = alpha value used for model_fit
  #age.class = logical; TRUE or FALSE whether the model accounts for age class differences
  
  
  # Define 1D mesh per covar
  mesh.list <- vector("list", length(covars))
  for (i in 1:length(covars)) {
    mesh.list[[i]] <- inla.mesh.1d(seq(mesh.seq[[i]][1], mesh.seq[[i]][2],
                                       length.out = nbasis),
                                   degree = degree,
                                   boundary = 'free')
  }
  
  # Define vector of month.years for indexing
  my.ind <- names(cov_list$npp)

  rast.hgpr <- rep(cov_list$bathym, nlyr(cov_list$npp))
  names(rast.hgpr) <- my.ind
  
  
  
  # Subset covars by month.year
    vars <- data.frame(log.bathym = as.vector(terra::values(cov_list$bathym)) %>%
                         abs() %>%
                         log(),
                       log.npp = as.vector(terra::values(cov_list$npp)) %>%
                         log(),
                       log.sst = as.vector(terra::values(cov_list$sst)) %>%
                         log()) %>%
      mutate(row_id = 1:nrow(.)) %>%
      drop_na(log.bathym, log.npp, log.sst)
    
    vars2 <- data.frame(Intercept = 1,
                        log.sst = as.vector(terra::values(cov_list$sst)) %>%
                          log(),
                        log.sst2 = as.vector(terra::values(cov_list$sst)) %>%
                          log() %>%
                          . ^ 2) %>%
      mutate(row_id = 1:nrow(.)) %>%
      filter(row_id %in% vars$row_id)
    
    
    
    # Generate matrices for covariate raster data (for prediction)
    A.mat <- vector("list", length(covars))
    for (j in 1:length(covars)) { #one matrix for model estimation and another for generating predictions for plotting
      A.mat[[j]] <- inla.spde.make.A(mesh.list[[j]], loc = vars[[covars[[j]]]])
    }
    
    
    # Define coeff values from HGPR
      coeff1 <- model_fit$summary.random[1:3] %>%
        map(., ~pull(.x, mean))
      
      # Define coeff values of fixed terms from HGPR
      coeff2 <- model_fit$summary.fixed$mean
      
      # Make predictions on intensity of use from model for GP terms
      hgpr.pred <- A.mat %>%
        map2(.x = ., .y = coeff1,
             ~{.x %*% .y %>%
                 as.vector()}
        ) %>%
        bind_cols() %>%
        rowSums()  #sum up all predictions across covars
      
      # Make predictions using linear terms
      hgpr.pred2 <- as.matrix(vars2[,1:3]) %*% coeff2
      
      # Store results in raster stack
      terra::values(rast.hgpr) <- NA  # initially store all NAs for locs w/o predictions
      terra::values(rast.hgpr)[vars$row_id] <- hgpr.pred + hgpr.pred2[,1]
      
      
    
    return(rast.hgpr)
  
}


###################################


#Function to normalize data (e.g., predicted raster values) via min-max scaling
# In this case, min and max values will be determined from entire set of predictions (provided by user)
# This function is intended to be used for values from SpatRaster stack

normalize <- function(x) {
  
  range <- values(x) %>%
    as.vector() %>%
    range(na.rm = TRUE)
  
  (x - range[1]) / (range[2] - range[1])
}


###################################

# Function to download MUR SST data from ERDDAP
sst.url <- function(date, extent) {
  paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41mday.nc?sst%5B(", date,
         "):1:(",
         date,
         ")%5D%5B(",
         extent$ymin,
         "):1:(",
         extent$ymax,
         ")%5D%5B(",
         extent$xmin,
         "):1:(",
         extent$xmax,
         ")%5D"
  )
}


###################################

# Function to download NPP data from ERDDAP
npp.url <- function(date, extent) {
  paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1ppmday.nc?productivity%5B(",
         date,
         "):1:(",
         date,
         ")%5D%5B(0.0):1:(0.0)%5D%5B(",
         extent$ymax,
         "):1:(",
         extent$ymin,
         ")%5D%5B(",
         extent$xmin,
         "):1:(",
         extent$xmax,
         ")%5D"
  )
}


###################################


# Process/filter bathymetric and SST layers
process_rasters <- function(bathym.rast, sst.rast, npp.rast) {
  
  # Set all positive bathymetric values (i.e., elevation) as NA
  bathym.rast[bathym.rast > 0] <- NA
  
  # Transform raster layers to match coarsest spatial resolution (i.e., NPP)
  bathym.rast <- terra::resample(bathym.rast, npp.rast, method = "average")
  sst.rast <- terra::resample(sst.rast, npp.rast, method = "average")
  
  
  # Deal w/ bathym depth exactly equal to 0 (since a problem on log scale)
  bathym.rast[bathym.rast > -0.001] <- NA
  
  return(list(bathym = bathym.rast,
              npp = npp.rast,
              sst = sst.rast))
}
