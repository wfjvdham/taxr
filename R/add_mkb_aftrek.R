#' add_mkb_aftrek
#'
#' @param df tibble to add mkb_aftrek to
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
add_mkb_aftrek <- function(df) {
  df %>%
    dplyr::mutate(
      mkb_aftrek = dplyr::if_else(
        bruto > 0,
        max(0, (bruto - starters_aftrek - zelf_aftrek - oudedag_reserve) * 0.14),
        bruto * 0.14
      )
    )
}
