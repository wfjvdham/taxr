#' add_tariefsaanpassing_extra
#'
#' @param df tibble to add column to
#'
#' @return tibble with extra column
#' @export
add_tariefsaanpassing_extra <- function(df) {

  data <- parse_yml("income_tax") %>%
    dplyr::mutate(
      lower_border_highest_disk = purrr::map_int(
        disks,
        ~.x %>%
          head(-1) %>%
          tail(1) %>%
          purrr::pluck(1, "upper_border")
      )
    )

  tariefsaanpassing <- parse_yml("tariefsaanpassing")

  df %>%
    dplyr::left_join(income_tax, by = "year") %>%
    dplyr::left_join(tariefsaanpassing, by = "year") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tariefsaanpassing_aftrek = zelf_aftrek + mkb_aftrek
    ) %>%
    dplyr::mutate(
      tariefsaanpassing_extra = min(
        max(0, winst - lower_border_highest_disk),
        tariefsaanpassing_aftrek
      ) * tariefsaanpassing
    ) %>%
    dplyr::select(
      -tariefsaanpassing_aftrek, -tariefsaanpassing, -lower_border_highest_disk,
      -disks
    )
}
