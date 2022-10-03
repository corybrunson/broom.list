#' @templateVar class cancor
#' @template title-desc-tidy-list
#'
#' @aliases tidy.cancor
#' @param x A list with components `cor`, `xcoef`, `ycoef`, `xcenter`, and
#'   `ycenter` as returned by [stats::cancor()].
#' @param matrix Character specifying which list element should be tidied,
#'   matched to the following options (defaults to `"cor"`):
#'
#'   - `"xcoef"`: returns information about the estimated coefficients in the
#'   `x` variables.
#'
#'   - `"ycoef"`: returns information about the estimated coefficients in the
#'   `y` variables
#'
#'   - `"cor"`: returns information about the canonical correlations.
#' @template param-unused-dots

#' @return A [tibble::tibble] with columns depending on the element of the CCA
#'   being tidied.
#'
#'   If `matrix` is `"xcoef"` (respectively, `"ycoef"`), each row in the tidied
#'   output corresponds to the estimated coefficients of the `x` (respectively,
#'   `y`) variables. The columns are:
#'   

#'   \item{`CC`}{Integer indicating a canonical dimension.}
#'   \item{`variable`}{Column name (or integer position) in the original data.}
#'   \item{`value`}{The coordinate of this variable in the canonical dimension.}
#'

#'   If `matrix` is `"cor"`, each row contains information about one canonical
#'   dimension. The columns are:
#'   

#'   \item{`CC`}{An integer indicating the canonical dimension.}
#'   \item{`cor`}{The canonical correlation in this dimension.}
#'   \item{`percent`}{Fraction of variation along this canonical dimension
#'                    (a numeric value between 0 and 1).}
#'   \item{`cumulative`}{Cumulative fraction of variation along canonical
#'                       dimensions up to this one
#'                       (a numeric value between 0 and 1).}
#' 
#' @examples
#' 
#' # savings data
#' class(LifeCycleSavings)
#' pop <- LifeCycleSavings[, 2:3]
#' oec <- LifeCycleSavings[, -(2:3)]
#' # canonical correlation analysis
#' savings_cca <- cancor(pop, oec)
#' 
#' # return the tidied canonical coefficients for the left variables
#' tidy(savings_cca)
#' # return the tidied canonical coefficients for the right variables
#' tidy(savings_cca, matrix = "ycoef")
#' # return the canonical coefficients, with summary statistics
#' tidy(savings_cca, matrix = "cor")
#' 
#' @examplesIf rlang::is_installed("ggplot2")
#' 
#' library(ggplot2)
#' 
#' # scree plot
#' savings_cca %>%
#'   tidy(matrix = "cor") %>%
#'   ggplot(aes(x = CC, y = percent)) +
#'   theme_bw() +
#'   geom_col() +
#'   labs(x = "Canonical dimension", y = "Percent of variance")
#' 
#' @family list tidiers
#' @seealso [generics::tidy()], [stats::cancor()]
tidy_cancor <- function(x, matrix = "cor", ...) {
  if (length(matrix) > 1) {
    stop("Must specify a single matrix to tidy.")
  }
  matrix <- match.arg(matrix, c("xcoef", "ycoef", "cor"))
  
  # following `broom:::tidy_svd()`
  if (matrix == "xcoef") {
    res <- tibble::rownames_to_column(as.data.frame(x$xcoef), "variable")
    res <- tidyr::pivot_longer(
      res,
      cols = c(dplyr::everything(), -tidyselect::all_of("variable")),
      names_to = "CC",
      values_to = "value"
    )
    res$CC <- as.integer(stringr::str_remove(res$CC, "V"))
    if (! is.null(rownames(x$xcoef)))
      res$variable <- factor(res$variable, levels = rownames(x$xcoef))
    res <- res[order(res$CC, res$variable), c("CC", "variable", "value")]
  } else if (matrix == "ycoef") {
    res <- tibble::rownames_to_column(as.data.frame(x$ycoef), "variable")
    res <- tidyr::pivot_longer(
      res,
      cols = c(dplyr::everything(), -tidyselect::all_of("variable")),
      names_to = "CC",
      values_to = "value"
    )
    res$CC <- as.integer(stringr::str_remove(res$CC, "V"))
    if (! is.null(rownames(x$ycoef)))
      res$variable <- factor(res$variable, levels = rownames(x$ycoef))
    res <- res[order(res$CC, res$variable), c("CC", "variable", "value")]
  } else if (matrix == "cor") {
    res <- data.frame(
      CC = seq_along(x$cor),
      cor = x$cor
    )
    res$percent <- res$cor^2 / sum(res$cor^2)
    res$cumulative <- cumsum(res$percent)
  }
  
  as_tibble(res)
}
