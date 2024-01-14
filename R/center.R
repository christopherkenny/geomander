#' Get the kind of center of each shape
#'
#' Returns points within the shape, near the center.
#' Uses the centroid if that's in the shape, or point on surface if not.
#'
#' @param shp An sf dataframe
#' @templateVar epsg TRUE
#' @template template
#'
#' @return An sf dataframe where geometry is the center(ish) of each shape in shp
#' @export
#'
#' @concept center
#'
#' @examples
#' data(towns)
#' st_centerish(towns)
#'
st_centerish <- function(shp, epsg = 3857) {
  cent <- geos_centerish(shp, epsg = epsg)

  sf::st_geometry(shp) <- sf::st_as_sfc(cent)
  shp
}

#' Get the kind of center of each shape
#'
#' Returns points within the shape, near the center.
#' Uses the centroid if that's in the shape, or point on surface if not.
#'
#' @param shp An sf dataframe
#' @templateVar epsg TRUE
#' @template template
#'
#' @return A geos geometry list
#' @export
#'
#' @concept center
#'
#' @examples
#' data(towns)
#' geos_centerish(towns)
#'
geos_centerish <- function(shp, epsg = 3857) {
  shp <- make_planar_pair(x = shp, epsg = epsg)$x

  cent <- geos::geos_centroid(shp)

  if (nrow(shp) > 1) {
    outside <- !geos::geos_within(cent, shp)

    if (any(outside)) {
      cent[outside] <- geos::geos_point_on_surface(shp[outside, ])
    }
  } else {
    outside <- !geos::geos_within(geom1 = cent, geom2 = shp)

    if (is.na(outside)) {
      outside <- TRUE
    }

    if (outside) {
      cent <- geos::geos_point_on_surface(shp)
    }
  }

  cent
}

#' Get the centroid of the maximum inscribed circle
#'
#' Returns the centroid of the largest inscribed circle for each shape
#'
#' @param shp An sf dataframe
#' @templateVar tolerance TRUE
#' @templateVar epsg TRUE
#' @template template
#'
#' @return An sf dataframe where geometry is the circle center of each shape in shp
#' @export
#'
#' @concept center
#'
#' @examples
#' data(towns)
#' st_circle_center(towns)
#'
st_circle_center <- function(shp, tolerance = 0.01, epsg = 3857) {
  cent <- geos_circle_center(shp, tolerance = tolerance, epsg = epsg)

  sf::st_geometry(shp) <- sf::st_as_sfc(cent)
  shp
}

#' Get the centroid of the maximum inscribed circle
#'
#' Returns the centroid of the largest inscribed circle for each shape
#'
#' @param shp An sf dataframe
#' @templateVar tolerance TRUE
#' @templateVar epsg TRUE
#' @template template
#'
#' @return A geos geometry list
#' @export
#'
#' @concept center
#'
#' @examples
#' data(towns)
#' geos_circle_center(towns)
#'
geos_circle_center <- function(shp, tolerance = 0.01, epsg = 3857) {
  shp <- make_planar_pair(x = shp, epsg = epsg)$x

  geos::geos_centroid(geos::geos_maximum_inscribed_crc(shp, tolerance = tolerance))
}
