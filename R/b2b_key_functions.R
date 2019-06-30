# B2B set and get secret key for API connection


# B2B enviroment ----------------------------------------------------------
b2b_env <- new.env(parent = emptyenv())
b2b_env$export_keys <- list() # keys for export API
b2b_env$user_keys <- list() #keys for user API


# Export keys set and get -------------------------------------------------

#' Установка ключа для API Export
#'
#'@param b2b_site B2Bmotion site adress
#'@param b2b_key secret export-API key
#' @export 
set_api_export_key <- function(b2b_site, b2b_key) {
    b2b_env$export_keys[[b2b_site]] <- b2b_key
    invisible(b2b_site)
}

# Get export API key 
get_api_export_key <- function(b2b_site) {
    b2b_env$export_keys[[b2b_site]]
}


# User keys set and get ---------------------------------------------------

#' Set user API key
#'
#'@param b2b_site B2Bmotion site adress
#'@param b2b_user_key secret user-API key
#' @export 

set_user_api_key <- function(b2b_site, b2b_user_key) {
  b2b_env$user_keys[[b2b_site]] <- b2b_user_key
  invisible(b2b_site)
}

# Get export API key 

get_user_api_key <- function(b2b_site) {
  b2b_env$user_keys[[b2b_site]]
}
