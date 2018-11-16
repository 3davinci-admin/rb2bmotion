# API url functions
# https://dataexport.docs.apiary.io/

# list of available API methods ---------------------------------------
api_all_methods <- function() {

  c("user",
    "company",
    "order",
    "order-items",
    "specification",
    "user-cart",
    "document",
    "commercial-offer",
    "product")
}



# new URL for API response  -------------------------------------------------
api_new_url <- function(b2b_site, method, limit = 1000, offset = 0, filters = NULL) {

  if (!length(method) == 1) stop()
  if (!method %in% api_all_methods()) stop(paste(method, "method is not available"))
  if (limit > 1000) stop("limit can not be more than 1000")

  url_base <-
    paste0(
      "https://",
      b2b_site, "/",
      "api/v1/export-data/", method, "?",
      "limit=", limit,
      "&offset=", offset
    )

  # filters list to string
  filters_list_to_str <- function(filters_list) {

    if (is.null(filters_list)) return(NULL)
    # check is list
    stopifnot(is.list(filters_list))
    # chek all element lenght == 1
    stopifnot(all(map_lgl(filters_list, ~ length(.x) == 1)))

    filters_str <-
      map2_chr(
        names(filters_list),
        filters_list,
        ~paste0("&filters%5B", .x, "%5D=", .y)
      ) %>% reduce(paste0)

    filters_str
  }


  # ! to do add filters string
  url_filters <- filters_list_to_str(filters)

  url <- paste0(url_base, url_filters)

  return(url)
}



# get API method from url ---------------------------------------------
api_url_method <- function(url) {

  patern_method <- reduce(api_all_methods(), paste, sep = "|")

  method <-
    url %>%
    str_extract(paste0("(?<=/export-data/)(", patern_method, ")(?=\\?)"))

  if (!method %in% api_all_methods()) stop(paste("no method", method))

  method
}

# set API method to url ---------------------------------------------
`api_url_method<-` <- function(url, value) {

  stopifnot(value %in% api_all_methods())

  patern_method <- reduce(api_all_methods(), paste, sep = "|")

  url %>%
    str_replace(paste0("(?<=/export-data/)(", patern_method, ")(?=\\?)"), value)

}

api_url_set_method <- function(url, method) {

  stopifnot(method %in% api_all_methods())

  patern_method <- reduce(api_all_methods(), paste, sep = "|")

  url %>%
    str_replace(paste0("(?<=/export-data/)(", patern_method, ")(?=\\?)"), method)

}


# get site from url ------------------------------------------------
api_url_site <- function(url) {

   url %>%
    str_extract("(?<=https://)[a-z, 0-9, \\-, \\.]*(?=/)")

}


# set site to url ------------------------------------------------
`api_url_site<-` <- function(url, value) {

  url %>%
    str_replace("(?<=https://)[a-z, 0-9, \\-, \\.]*(?=/)", value)

}


api_url_set_site <- function(url, site) {

  url %>%
    str_replace("(?<=https://)[a-z, 0-9, \\-, \\.]*(?=/)", site)

}


