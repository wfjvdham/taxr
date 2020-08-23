#' add_tariefsaanpassing_extra
#'
#' @param df tibble to add column to
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
add_tariefsaanpassing_extra <- function(df) {
  income_tax <- parse_yml("income_tax") %>%
    dplyr::mutate(
      lower_border_highest_disk = purrr::map_int(
        disks,
        ~ .x %>%
          head(-1) %>%
          tail(1) %>%
          purrr::pluck(1, "upper_border")
      )
    ) %>%
    dplyr::select(!disks)

  tariefsaanpassing <- parse_yml("tariefsaanpassing")

  df %>%
    dplyr::left_join(income_tax, by = "year") %>%
    dplyr::left_join(tariefsaanpassing, by = "year") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tariefsaanpassing_aftrek = zelf_aftrek + mkb_aftrek
    ) %>%
    dplyr::mutate(
      tariefsaanpassing_input = min(
        max(0, winst - lower_border_highest_disk),
        tariefsaanpassing_aftrek
      ),
      tariefsaanpassing_extra = calc_disks(tariefsaanpassing_input, disks)
    ) %>%
    dplyr::select(
      -tariefsaanpassing_aftrek, -tariefsaanpassing_input,
      -lower_border_highest_disk, -disks
    )
}
