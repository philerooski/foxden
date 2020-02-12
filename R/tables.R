#' List tables in a database
#'
#' @param database The name of the database.
#' @export
list_tables <- function(database = "fox_integrated") {
  api_key <- get_api_key()
  response <- foxGET(paste0("/sync/v1/", database, "/tables"),
                     query = list(key = api_key, format = "json"))
  content <- httr::content(response)
  if (length(content) == 0) {
    stop(paste("There is no database called", database))
  }
  return(content)
}

#' List columns and column destinations of a table
#'
#' @param table The table name.
#' @inheritParams list_tables
#' @export
list_columns <- function(table, database = "fox_integrated") {
  api_key <- get_api_key()
  response <- foxGET(paste0("/sync/v1/", database, "/", table, "/columns"),
                     query = list(key = api_key, format = "json"))
  content <- httr::content(response)
  return(content)
}

#' Download a table
#'
#' @param table The table name.
#' @param download_to Local folder to download the table to.
#' Defaults to the current working directory.
#' @param filters (Experimental. It's not clear the API supports this yet).
#' A list containing <column> = <value> pairs to filter the
#' table contents on.
#' @param inheritParams list_tables
#' @export
download_table <- function(table, download_to = ".", filters = NULL,
                           database = "fox_integrated") {
  api_key <- get_api_key()
  query <- list(key = api_key, table = table)
  if (!is.null(filters)) {
    query <- c(query, filters)
  }
  response <- foxGET(paste0("/sync/v1/", database, "/download"), query = query)
  f <- httr::content(response, "text")
  output_path <- file.path(download_to, paste0(table, ".csv"))
  write(f, output_path)
  return(normalizePath(output_path))
}
