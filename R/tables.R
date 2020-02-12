#' List tables in a database
#'
#' @param database The name of the database
list_tables <- function(database = "fox_integrated") {
  api_key <- get_api_key()
  response <- foxGET(paste0("/sync/v1/", database, "/tables"),
                     query = list(key = api_key, format = "json"))
  if (response$status_code == 200) {
    content <- httr::content(response)
    if (length(content) == 0) {
      stop(paste("There is no database called", database))
    }
    return(content)
  } else {
    stop()
  }
}

#' List columns and column destinations of a table
#'
#' @param table The table name
#' @inheritParams list_tables
list_columns <- function(table, database = "fox_integrated") {
  api_key <- get_api_key()
  response <- foxGET(paste0("/sync/v1/", database, "/", table, "/columns"),
                     query = list(key = api_key, format = "json"))
  if (response$status_code == 200) {
    content <- httr::content(response)
    return(content)
  } else {
    stop()
  }
}

#' Download a table
#'
#' @param table The table name
#' @param download_to Local folder to download the table to
#' @param inheritParams list_tables
download_table <- function(table, download_to = NULL, database = "fox_integrated") {
  api_key <- get_api_key()
  response <- foxGET(paste0("/sync/v1/", database, "/download"),
                     query = list(key = api_key, table = table))
  return(response)
}
