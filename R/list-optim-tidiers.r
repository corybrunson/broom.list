#' @templateVar class optim
#' @template title-desc-tidy-list
#'
#' @param x A list returned from [stats::optim()].
#' @template param-unused-dots
#' 
#' @return A [tibble::tibble] with the following columns:
#' 
#'   \item{parameter}{The parameter being modeled.}
#'   \item{std.error}{The standard error of the regression term.}
#'   \item{value}{The value/estimate of the component.
#'                Results from data reshaping.}
#' 
#' `std.error` is only provided as a column if the Hessian is calculated.
#' 
#' @examples
#'
#' f <- function(x) (x[1] - 2)^2 + (x[2] - 3)^2 + (x[3] - 8)^2
#' o <- optim(c(1, 1, 1), f)
#'
#' @note 
#' This function assumes that the provided objective function is a negative 
#' log-likelihood function. Results will be invalid if an incorrect
#' function is supplied.
#'
#' tidy(o)
#' glance(o)
#' @aliases optim_tidiers tidy.optim
#' @family list tidiers
#' @seealso [generics::tidy()], [stats::optim()]
tidy_optim <- function(x, ...) {
  if (is.null(names(x$par))) {
    names(x$par) <- paste0("parameter", seq_along(x$par))
  }
  ret <- tibble(parameter = names(x$par), value = unname(x$par))
  if ("hessian" %in% names(x)) {
    ret$std.error <- sqrt(diag(solve(x$hessian)))
  }
  ret
}

#' @templateVar class optim
#' @template title-desc-tidy-list
#'
#' @inherit tidy_optim params examples
#' 
#' @return A [tibble::tibble] with exactly one row and the following columns:
#' 
#'   \item{convergence}{Convergence code.}
#'   \item{function.count}{Number of calls to `fn`.}
#'   \item{gradient.count}{Number of calls to `gr`.}
#'   \item{value}{Minimized or maximized output value.}
#' 
#' @aliases glance.optim
#' @family list tidiers
#' @seealso [generics::glance()], [stats::optim()]
glance_optim <- function(x, ...) {
  as_glance_tibble(
    value = x$value,
    function.count = unname(x$counts["function"]),
    gradient.count = unname(x$counts["gradient"]),
    convergence = x$convergence,
    na_types = "riii"
  )
}
