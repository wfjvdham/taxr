#' Calculate total given some disks and a value
#'
#' @param values numeric vector containing values
#' @param disks list of disks. Every disk has a `max` and a `perc`
#'
#' @return numeric vector containing results per value
#' @export
#'
#' @examples
#' values <- seq(from = 0, to = 100000, length.out = 11)
#' disks <- list(
#'   list(upper_border = 30000, fixed = 3000),
#'   list(upper_border = 60000, perc = 0.2),
#'   list(upper_border = Inf, perc = 0.3)
#' )
#' calc_disks(values, disks)
calc_disks <- function(values, disks) {
  values %>%
    purrr::map_int(calc_disks_single_value, disks)
}

calc_disks_single_value <- function(value, disks) {

  # here we make a second list that contains the minimum borders of a disk
  # (which are the maximum borders of the previous disk)
  disks_prev <- append(list(list(upper_border = 0)), head(disks, -1))

  purrr::map2_int(disks, disks_prev, function(disk_n1, disk_n0) {
    amount_in_disk <- max(
      min(value, disk_n1$upper_border),
      disk_n0$upper_border
    ) - disk_n0$upper_border

    if (!is.null(disk_n1$perc)) {
      result <- amount_in_disk * disk_n1$perc
      if (!is.null(disk_n1$max_result)) {
        result <- min(result, disk_n1$max_result)
      }
    } else if (!is.null(disk_n1$fixed)) {
      result <- disk_n1$fixed
    } else {
      logger::log_error("{paste(disk_n1, collapse = ', ')} is not valid")
    }
    as.integer(floor(result))
  }) %>%
    sum() %>%
    max(0L)
}
