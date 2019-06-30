#' Получаем JSON из API URL
#' @import httr
#' 
#' @export
get_json <- function(url) {
  UseMethod("get_json", url)
}


#' Собираем JSON из Export URL
#' @export
get_json.export_url <- function(url) {
  
  response <- GET(
    as.character(url),
    add_headers(`Secret-Key` = get_api_export_key(url$b2b_site))
  )
  
  # Check connection
  if (http_error(response)) {
    stop(
      "Ошибка при подключении к ", 
      url$b2b_site, 
      ". Код ошибки: ", 
      response$status_code
    )
  }
  
  # Выделяем JSON
  json <- content(response, type = "text", encoding = "UTF-8")
  
  # Назначаем класс
  class(json) <- "api_export_json"
  
  
  # Сохраняем изначальную ссылку в атрибутах
  attr(json, "url") <- url 
  
  return(json)
    
}


