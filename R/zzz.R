#'
#'
#'
#'
#' @export
#'
#'

.onLoad <- function(libname = find.package("service"), pkgname = "service")

  {
    all_status <- system('service --status-all', intern = TRUE)

    all_staus_sp <- list()
    for (i in seq_along(all_status)) {
      sp <- stringr::str_split(all_status[[i]], '] ')[[1]]
      sp[[1]] <-
        trimws(stringr::str_replace(sp[[1]], ' \\[', ""), which = 'both')
      sp[[2]] <- trimws(sp[[2]], which = 'both')

      all_staus_sp[[i]] <-
        data.frame(status = sp[[1]], service = sp[[2]])
    }

    status_df <- do.call('rbind', all_staus_sp)

    status_df[, 'status'] <-
      stringr::str_replace_all(status_df[, 'status'], '\\+', 'active')
    status_df[, 'status'] <-
      stringr::str_replace_all(status_df[, 'status'], '\\-', 'inactive')

    options(SERVICES = status_df)
    options(SERVICE_TIME = Sys.time())

  }
