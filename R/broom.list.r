#' @title Convert Statistical List Output into Tidy Tibbles
#' @name broom
#' @description This package provides methods for the generic functions `tidy()`
#'   and `glance()` for lists, which are often returned by statistical analysis
#'   functions without S3 class attributes.
#' 
#' @docType package
#' @aliases broom.list broom.list-package
#' 
#' @importFrom purrr map_df set_names possibly
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' 
#' @importFrom utils head
#' 
#' @import dplyr
#' 
#' @keywords internal
"_PACKAGE"

#' @importFrom generics augment
#' @export
generics::augment

#' @importFrom generics tidy
#' @export
#' @seealso [tidy.list()]
generics::tidy

#' @importFrom generics glance
#' @export
#' @seealso [glance.list()]
generics::glance
