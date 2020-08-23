#' add_winst
#'
#' @param df tibble to add column to
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
add_winst <- function(df) {
  df %>%
    dplyr::mutate(
      winst = dplyr::if_else(
        bruto > 0,
        max(
          0,
          bruto - starters_aftrek - zelf_aftrek - oudedag_reserve - mkb_aftrek -
            overige_aftrek
        ),
        bruto - mkb_aftrek
      )
    )
}
