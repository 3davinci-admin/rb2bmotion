# B2B set and get secret key for API connection


# B2B enviroment ----------------------------------------------------------
b2b_env <- new.env(parent = emptyenv())
b2b_env$keys <- list()


#' Set export API key
#'
#'@param b2b_site B2Bmotion site adress
#'@param b2b_key secret export-API key

b2b_set_key <- function(b2b_site, b2b_key) {
    b2b_env$keys[[b2b_site]] <- b2b_key
    invisible(b2b_site)
}

# Get export API key -----------------

get_b2b_key <- function(b2b_site) {
    b2b_env$keys[[b2b_site]]
}


