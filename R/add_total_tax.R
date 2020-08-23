#' add_total_tax
#'
#' @param df tibble to add column to
#'
#' @return tibble with extra column
#' @export
add_total_tax <- function(df) {
  df %>%
    dplyr::mutate(
      total_tax = income_tax + zvw_bijdrage + tariefsaanpassing_extra -
        alg_hef_korting - arb_korting
    )
}
