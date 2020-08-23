#' parse_yml
#'
#' @param yml_name name of the yml file without extension
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
parse_yml <- function(yml_name) {
  data <- paste0(yml_name, ".yml") %>%
    system.file(package = "taxr") %>%
    yaml::read_yaml() %>%
    tibble::as_tibble()

  if (!("disks" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(
        disks = purrr::map(
          .data[[yml_name]],
          ~ list(list(upper_border = Inf, fixed = .x))
        )
      ) %>%
      dplyr::select(!(!!yml_name))
  }

  data
}
