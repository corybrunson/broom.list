#' @templateVar class irlba
#' @template title-desc-tidy-list
#'
#' @inherit tidy_svd return examples
#'
#' @param x A list returned from [irlba::irlba()].
#' @template param-unused-dots
#'
#' @details A very thin wrapper around [tidy_svd()].
#'
#' @aliases tidy.irlba irlba_tidiers
#' @family list tidiers
#' @family svd tidiers
#' @seealso [tidy()], [irlba::irlba()]
#' @export
tidy_irlba <- function(x, ...) {
  tidy_svd(x, ...)
}
