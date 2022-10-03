#' @templateVar class svd
#' @template title-desc-tidy-list
#'
#' @param x A list with components `u`, `d`, `v` returned by [base::svd()].
#' @param matrix Character specifying which component of the PCA should be
#'   tidied.
#'
#'   - `"u"`: returns information about the left singular vectors.
#'
#'   - `"v"`: returns information about the right singular vectors.
#'
#'   - `"d"`: returns information about the basis vectors and singular values.
#' @template param-unused-dots
#'
#' @return A [tibble::tibble] with columns depending on the component of the SVD
#'   being tidied.
#'
#'   If `matrix` is `"u"` (respectively, `"v"`), each row in the tidied output
#'   corresponds to the left (respectively, right) singular vectors. The columns
#'   are:
#'   

#'   \item{`row`}{Row (respectively, column) in the original data.}
#'   \item{`SV`}{Integer indicating a singular vector.}
#'   \item{`value`}{The coordinate of this row in the singular vector.}
#'

#'   If `matrix` is `"d"`, each row contains information for one basis vector.
#'   The columns are:
#'   

#'   \item{`SV`}{An integer vector indicating the basis vector.}
#'   \item{`std.dev`}{Singular value; the standard deviation along this basis
#'                    vector.}
#'   \item{`percent`}{Fraction of variation along this basis vector
#'                    (a numeric value between 0 and 1).}
#'   \item{`cumulative`}{Cumulative fraction of variation along basis vectors up
#'                       to this one (a numeric value between 0 and 1).}
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(modeldata)
#' data(hpc_data)
#'
#' mat <- scale(as.matrix(hpc_data[, 2:5]))
#' s <- svd(mat)
#'
#' tidy_u <- tidy(s, matrix = "u")
#' tidy_u
#'
#' tidy_d <- tidy(s, matrix = "d")
#' tidy_d
#'
#' tidy_v <- tidy(s, matrix = "v")
#' tidy_v
#' 
#' @examplesIf rlang::is_installed("ggplot2")
#' 
#' library(dplyr)
#' library(ggplot2)
#'
#' ggplot(tidy_d, aes(SV, percent)) +
#'   geom_point() +
#'   ylab("% of variance explained")
#'
#' tidy_u %>%
#'   mutate(class = hpc_data$class[row]) %>%
#'   ggplot(aes(class, value)) +
#'   geom_boxplot() +
#'   facet_wrap(~SV, scale = "free_y")
#' 
#' @seealso [generics::tidy()], [base::svd()]
#' @aliases svd_tidiers
#' @family svd tidiers
#' @family list tidiers
tidy_svd <- function(x, matrix = "u", ...) {
  if (length(matrix) > 1) {
    stop("Must specify a single matrix to tidy.")
  }
  
  if (matrix == "u") {
    ret <- x$u %>%
      as_tibble(.name_repair = "unique") %>%
      tibble::rowid_to_column("row") %>%
      pivot_longer(
        cols = c(dplyr::everything(), -row),
        names_to = "SV",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        SV = stringr::str_remove(SV, "...") %>% 
          as.numeric()
      ) %>%
      arrange(SV, row) %>%
      as.data.frame()
  } else if (matrix == "d") {
    ret <- tibble(SV = seq_along(x$d), std.dev = x$d) %>%
      mutate(
        percent = std.dev^2 / sum(std.dev^2),
        cumulative = cumsum(percent)
      )
  } else if (matrix == "v") {
    ret <- x$v %>%
      as_tibble(.name_repair = "unique") %>%
      tibble::rowid_to_column("column") %>%
      pivot_longer(
        cols = c(dplyr::everything(), -column),
        names_to = "SV",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        SV = stringr::str_remove(SV, "...") %>% 
          as.numeric()
      ) %>%
      arrange(SV, column) %>%
      as.data.frame()
  }
  as_tibble(ret)
}
