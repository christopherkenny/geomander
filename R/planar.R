make_planar_pair <- function(x, y = NULL, epsg = 3857) {
  if (is.null(epsg) || isFALSE(epsg)) {
    return(list(x = x, y = y))
  }

  x_is_ll <- isTRUE(sf::st_is_longlat(x))
  y_is_ll <- isTRUE(sf::st_is_longlat(y))

  x_crs <- sf::st_crs(x)
  y_crs <- sf::st_crs(y)

  if (is.null(x_crs) || is.na(x_crs)) {
    cli::cli_warn('Planarizing skipped. {.arg x} missing CRS.')
    return(list(x = x, y = y))
  }
  if ((is.null(y_crs) || is.na(y_crs)) & !is.null(y)) {
    cli::cli_warn('Planarizing skipped. {.arg y} missing CRS.')
    return(list(x = x, y = y))
  }

  if (!is.null(y)) {
    if (!x_is_ll && !y_is_ll) { # both not-ll
      if (x_crs != y_crs) { # diff crs -> use x crs
        y <- sf::st_transform(y, x_crs)
      }
    } else if (!x_is_ll) { # y ll, x not-ll -> use x crs
      y <- sf::st_transform(y, x_crs)
    } else if (!y_is_ll) { # x ll, y not-ll -> use y crs
      x <- sf::st_transform(x, y_crs)
    } else { # both ll -> use epsg
      x <- sf::st_transform(x, epsg)
      y <- sf::st_transform(y, epsg)
    }
  } else {
    if (x_is_ll) {
      x <- sf::st_transform(x, epsg)
    }
  }

  list(x = x, y = y)
}
