#' add_tax_variable
#'
#' Even testen hoor
#'
#' @param df tibble to add variable to
#' @param variable_name name of the variable to add
#' @param needs_true only add variable when this column in df is true
#' @param input_col calculate variable based on this column
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#'
#' library(taxr)
#'
#' df <- tibble::tibble(
#'   bruto = seq(from = -10000, to = 100000, length.out = 11),
#'   year = c(2018, 2019, 2020, 2018, 2019, 2020, 2018, 2019, 2020, 2018, 2020),
#'   ondernemer = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
#'   starter = c(rep(TRUE, 5), rep(FALSE, 6)),
#'   oudedag = c(rep(FALSE, 5), rep(TRUE, 6)),
#'   overige_aftrek = 1500
#' )
#'
#' df <- tibble::tibble(
#'   bruto = 65000,
#'   ondernemer = TRUE,
#'   starter = TRUE,
#'   oudedag = FALSE,
#'   overige_aftrek = 1575,
#'   year = 2020
#' )
#'
#' df <- df %>%
#'   add_tax_variable("starters_aftrek", "starter") %>%
#'   add_tax_variable("oudedag_reserve", "oudedag") %>%
#'   add_tax_variable("zelf_aftrek", "ondernemer") %>%
#'   add_mkb_aftrek() %>%
#'   add_winst() %>%
#'   add_tax_variable("alg_hef_korting", input_col = "box_1") %>%
#'   add_tax_variable("arb_korting", input_col = "bruto") %>%
#'   add_tax_variable("zvw_bijdrage", input_col = "winst") %>%
#'   add_tax_variable("income_tax", input_col = "box_1") %>%
#'   add_tariefsaanpassing_extra() %>%
#'   add_total_tax()
add_tax_variable <- function(df, variable_name = "starters_aftrek",
                             needs_true = NA, input_col = "bruto") {
  data <- parse_yml(variable_name)

  join_cols <- "year"
  if (needs_true %in% colnames(df)) {
    data <- data %>%
      dplyr::mutate(!!needs_true := TRUE)
    join_cols <- c(join_cols, needs_true)
  }

  df %>%
    dplyr::left_join(data, by = join_cols) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(!!variable_name := calc_disks(.data[[input_col]], disks)) %>%
    dplyr::select(-disks)
}
