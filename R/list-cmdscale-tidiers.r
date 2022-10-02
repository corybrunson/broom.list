#' @templateVar class cmdscale
#' @template title-desc-tidy-list
#'
#' @details
#' 
#' When [cmdscale()] is instructed to return any of several optional elements,
#' or when `list. = TRUE`, the output is not the default point coordinate matrix
#' but a 5-element list with a consistent naming scheme (though some elements
#' will be `NULL` if their parameters are not set to `TRUE`). These tidiers rely
#' on this list structure to organize the model output into a tibble.
#' 
#' @aliases tidy.cmdscale
#' @param x A list with components `points`, `eig`, `x`, `ac`, and `GOF` as
#'   returned by [stats::cmdscale()] under certain conditions.
#' @param matrix Character specifying which list element should be tidied,
#'   matched to the following options (defaults to `"points"`):
#'
#'   - `"points"`: returns information about the coordinates in the
#'   representation space.
#'
#'   - `"x"`: returns information about the doubly-centered symmetric matrix
#'   used in the calculation.
#'
#'   - `"eig"`: returns information about the eigenvalues.
#' 
#' @template param-unused-dots

#' @return A [tibble::tibble] with columns depending on the element of the CMDS
#'   being tidied.
#'   
#'   If `matrix` is `"points"`, each row corresponds to one object (point) in
#'   the original data object. The columns are:
#'   

#'   \item{`point`}{ID (character name or integer number) of the point in the
#'                  original data.}
#'   \item{`PCo*`}{The principal coordinates of this point.}
#'

#'   If `matrix` is `"x"`, each row corresponds to one pair of points in the
#'   original data. The columns are:
#'   

#'   \item{`point1`}{The first point name, in order of the original data.}
#'   \item{`point2`}{The second point name, in order of the original data.}
#'   \item{`DC`}{The corresponding entry in the doubly-centered symmetric
#'               matrix.}
#'   

#'   If `matrix` is `"eig"`, each row contains information about one dimension
#'   of the eigendecomposition that powers the scaling process. The columns are:
#'   

#'   \item{`PCo`}{An integer indicating the principal coordinate.}
#'   \item{`eig`}{The eigenvalue along this principal coordinate.}
#'   \item{`percent`}{Fraction of variation along this principal coordinate
#'                    (a numeric value between 0 and 1).}
#'   \item{`cumulative`}{Cumulative fraction of variation along principal
#'                       coordinates up to this one
#'                       (a numeric value between 0 and 1).}
#'

#' @example inst/examples/ex-list-cmdscale-tidiers-cities.r
#' @family list tidiers
#' @seealso [generics::tidy()], [stats::cmdscale()]
tidy_cmdscale <- function(x, matrix = "points", ...) {
  if (length(matrix) > 1) {
    stop("Must specify a single matrix to tidy.")
  }
  matrix <- match.arg(matrix, c("points", "eig", "x"))
  
  if (matrix == "points") {
    res <- as.data.frame(x$points)
    names(res) <- paste0("PCo", seq(ncol(res)))
    res <- tibble::rownames_to_column(res, "point")
  } else if (matrix == "x") {
    if (is.null(x$x)) stop("Matrix `x` is NULL.")
    res <- as.data.frame(x$x)
    names(res) <- seq(ncol(res))
    res <- tibble::rownames_to_column(res, "point1")
    res <- tidyr::pivot_longer(
      res,
      cols = c(dplyr::everything(), -tidyselect::all_of("point1")),
      names_to = "point2", values_to = "DC"
    )
    res <- mutate(res, dplyr::across(c("point1", "point2"), as.integer))
    res <- res[res$point1 < res$point2, ]
    res <- mutate(res, dplyr::across(
      c("point1", "point2"),
      ~ rownames(x$points)[.]
    ))
  } else if (matrix == "eig") {
    ks <- seq(ncol(x$points))
    res <- data.frame(
      PCo = seq_along(x$eig)[ks],
      eig = x$eig[ks]
    )
    # following `broom:::tidy_svd()`
    # for each artificial coordinate, eigenvalue = variance
    res$percent = res$eig / sum(res$eig)
    res$cumulative = cumsum(res$percent)
  }
  as_tibble(res)
}

#' @templateVar class cmdscale
#' @template title-desc-tidy-list
#'
#' @inherit tidy_cmdscale params examples
#' 
#' @return A [tibble::tibble] with exactly one row and the following columns:
#' 
#'   \item{n}{Number of points.}
#'   \item{k}{Dimension of data representation space.}
#'   \item{ac}{Additive constant \eqn{c}.}
#'   \item{GOF1,GOF2}{Goodness of fit as calculated from the eigenvalues.}
#' 
#' @aliases glance.cmdscale
#' @family list tidiers
#' @seealso [generics::glance()], [stats::cmdscale()]
glance_cmdscale <- function(x, ...) {
  as_glance_tibble(
    n = nrow(x$points),
    k = ncol(x$points),
    ac = x$ac,
    GOF1 = x$GOF[[1L]],
    GOF2 = x$GOF[[2L]],
    na_types = "iirrr"
  )
}
