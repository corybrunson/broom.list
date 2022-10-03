
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# copied from *broom*; not sure whether one package should depend on the other

na_types_dict <- list(
  "r" = NA_real_,
  "i" = NA_integer_,
  "c" = NA_character_,
  "l" = NA
)

parse_na_types <- function(s) {
  
  positions <- purrr::map(
    stringr::str_split(s, pattern = ""), 
    match,
    table = names(na_types_dict)
  ) %>%
    unlist()
  
  na_types_dict[positions] %>%
    unlist() %>%
    unname()
}

as_glance_tibble <- function(..., na_types) {
  
  cols <- list(...)
  
  if (length(cols) != stringr::str_length(na_types)) {
    stop(
      "The number of columns provided does not match the number of ",
      "column types provided."
    )
  }
  
  na_types_long <- parse_na_types(na_types)
  
  entries <- purrr::map2(cols, 
                         na_types_long, 
                         function(.x, .y) {if (length(.x) == 0) .y else .x})
  
  tibble::as_tibble_row(entries)
  
}

#' @importFrom utils globalVariables
globalVariables(
  c(
    "SV", "std.dev", "percent", "column"
  )
)
