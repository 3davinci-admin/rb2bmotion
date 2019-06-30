#' Получаем JSON из API URL
#' 
#' 
#' @export
get_json <- function(url) {
  UseMethod("get_json", url)
}

#' для получения всех страниц 
#' @export
get_all_json <- function(url) {
  UseMethod("get_all_json", url)
}

#' Собираем JSON из Export URL
#' @import httr 
#' @export

get_json.export_url <- function(url) {

    
}