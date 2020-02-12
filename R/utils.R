#' Make a GET call to Fox Den
#'
#' @param endpoint Server endpoint.
#' @param ... named arguments to pass to \code{httr::modify_url}
foxGET <- function(endpoint, ...) {
  results <- fox(endpoint = endpoint, method = httr::GET, ...)
  return(results)
}

#' Make a POST call to Fox Den
#'
#' @param endpoint Server endpoint.
#' @param ... named arguments to pass to \code{httr::modify_url}
foxPOST <- function(endpoint, ...) {
  results <- fox(endpoint = endpoint, method = httr::POST, ...)
  return(results)
}

#' Log in to Fox Den
#'
#' @param email Your email
#' @param password Your Fox Den password
#' @export
fox_login <- function(email = NULL, password = NULL) {
  if (is.null(email) || is.null(password)) {
    credentials <- get_credentials()
  } else {
    credentials <- list(email = email, password = password)
  }
  response <- foxPOST("/sync/v1/auth",
                      query = list(email = credentials$email),
                      body = credentials$password)
  api_key <- content(response)$key
  Sys.setenv(FOX_DEN_API_KEY = api_key)
  return(TRUE)
}

#' Make a REST call to Fox Den
#'
#' @param endpoint Server endpoint.
#' @param method function which accepts the API endpoint as its first argument
#' @param body The body of the request
#' @param ... named arguments to pass to \code{httr::modify_url}
fox <- function(endpoint, method, body = NULL, ...) {
  base_url <- "https://foxden.michaeljfox.org"
  args <- list(...)
  url <- httr::modify_url(base_url, path = endpoint, scheme = args$scheme,
                          hostname = args$hostname, port = args$port,
                          query = args$query, params = args$params,
                          fragment = args$fragment, username = args$username,
                          password = args$password)
  print(url)
  results <- method(url, body = body)
  return(results)
}

#' Fetch credentials from Fox Den credentials file
#'
#' @return A list with elements \code{email} and \code{password}
get_credentials <- function(path = "~/.foxdenCredentials") {
  credentials <- yaml::read_yaml(path)
  return(credentials)
}

get_api_key <- function(env_var = "FOX_DEN_API_KEY") {
  api_key <- Sys.getenv(env_var)
  if (nchar(api_key) == 0) {
    stop("You are not logged in to Fox Den.")
  }
  return(api_key)
}
