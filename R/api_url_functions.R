#' All API methods
#' 
#' Get all API methods from \url{https://dataexport.docs.apiary.io/}
#' @export 
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


#' Create new URL for API request
#' 
#' Returns the character link from parameters
#' 
#' @param b2b_site B2Bmotion site adress ("avs.express", "b2b.el-com.ru" etc)
#' @param method method from \code{\link{api_all_methods}}
#' @param limit maximum count of elements in export API response 
#' @param offset how much element to skip
#' @param filters character string to filter for current method
#' @import lubridate
#' @import rlang
#' @export 

api_new_url <- function(b2b_site, method, limit = 100, offset = 0, ...) {
  
  
  if (!length(method) == 1 &!method %in% api_all_methods()) 
    stop(paste(method, "method is not available"))
  if (limit > 1000) 
    stop("limit can not be more than 1000")
  
  url_base <- paste0("https://", b2b_site, "/", "api/v1/export-data/", method, "?", 
                     "limit=", limit, "&offset=", offset)
  
  
  # Добавляем фильтр и выводим результат
  api_url_add_filter(url_base, ...)
}

#' Get API method from url 
#' 
#' Returns the method from url
#' @export 
api_url_method <- function(url) {
    
    patern_method <- reduce(api_all_methods(), paste, sep = "|")
    
    method <- url %>% stringr::str_extract(paste0("(?<=/export-data/)(", patern_method, ")(?=\\?)"))
    
    if (!method %in% api_all_methods()) 
        stop(paste("no method", method))
    
    method
}


#' Set API method to url 
#' 
#' Set the method to url
#' @param url URL to change
#' @param method new method from \code{\link{api_all_methods}}

api_url_set_method <- function(url, method) {
  
  stopifnot(method %in% api_all_methods())
  
  patern_method <- reduce(api_all_methods(), paste, sep = "|")
  
  url %>% stringr::str_replace(paste0("(?<=/export-data/)(", patern_method, ")(?=\\?)"), method)
  
}


#' @rdname api_url_set_method
#' @param value character new method
#' @export 
#' @export 
`api_url_method<-` <- function(url, value) {
    
    stopifnot(value %in% api_all_methods())
    
    patern_method <- reduce(api_all_methods(), paste, sep = "|")
    
    url %>% stringr::str_replace(paste0("(?<=/export-data/)(", patern_method, ")(?=\\?)"), new_method)
    
}


#' Get site from url
#' 
#' Get B2Bmotion site adress from url

# Эта функция используется в USER-API также
#' @export 
api_url_site <- function(url) {
    
    url %>% stringr::str_extract("(?<=https://)[a-z, 0-9, \\-, \\.]*(?=/)")
    
}


#' Set site to url
#' 
#' Set B2Bmotion site adress to url
#' @param url URT to change site
#' @param site new site 
#' @export 
api_url_set_site <- function(url, site) {
  
  url %>% stringr::str_replace("(?<=https://)[a-z, 0-9, \\-, \\.]*(?=/)", site)
  
}

#' @param value new site
#' @export 
`api_url_site<-` <- function(url, value) {
    
    url %>% stringr::str_replace("(?<=https://)[a-z, 0-9, \\-, \\.]*(?=/)", value)
    
}


#' Управление фильтрами для запросов к Export API 
#'
#' @description 
#'
#' `api_create_url_filter` создаёт строку (потом заменим на объект S3) для
#' последующего присоединения к URL для запроса по API:
#'
#' - для начала мы делаем только в формате character
#' 
#' - TODO: потом нужно оформить объект S3 `api_url_filter`
#'
#' @details 
#' 
#' Стоит обратить внимание, что это просто character
#'
#' @import rlang
#' @import purrr
#' @export

api_url_add_filter <- function(url, ...) {
  # Список аргументов
  args <- list2(...)
  
  result <- ""
  
  for (name in names2(args)) {
    # Если это скаляр
    if (args[[name]] %>% is_scalar_atomic()) {
      result = paste0(result, "&filters[", name,"]=",args[[name]])
    }  else {
      for (x in args[[name]]) {
        result = paste0(result, "&filters[", name,"][]=", x)
      }
    }
  }
  
  paste0(url, result)
}





