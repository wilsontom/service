#' Service Status
#'
#' @param service_name name of service to query
#' @export

status <- function(service_name)
{
  if (as.double(difftime(Sys.time(), getOption('SERVICE_TIME'), units = 'mins')) > 5) {
    service_status_all()
  }

  SERVICES <- getOption('SERVICES')

  idx <- which(SERVICES[, 'service'] == service_name)

  if (length(idx) == 0) {
    cat(crayon::white(
      paste0(
        clisymbols::symbol$cross,
        ' ',
        service_name,
        ' - service not found'
      )
    ), '\n')

    return(invisible(NULL))
  } else{
    service_ext <- SERVICES[idx, ]

    if (service_ext$status == 'active') {
      cat(crayon::green(
        paste0(
          clisymbols::symbol$bullet,
          ' ',
          service_ext$service,
          ' - active'
        )
      ), '\n')
    }


    if (service_ext$status == 'inactive') {
      cat(crayon::red(
        paste0(
          clisymbols::symbol$bullet,
          ' ',
          service_ext$service,
          ' - inactive'
        )
      ), '\n')
    }
    return(invisible(NULL))
  }
}
