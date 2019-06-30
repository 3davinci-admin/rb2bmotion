#' Создание Tible из JSON
#' @import jsonlite
#' @import tibble
#' @importFrom purrr modify_at
#' @export

as_tibble.api_export_json <- function(json, ...) {
  
  json_list <- jsonlite::fromJSON(json, simplifyVector = T, flatten = T)
  
  tbl <- as_tibble(json_list[["items"]], rownames = NULL)
  
  # Добавляем информацию по URL источника JSON
  attributes(tbl) <- c(attributes(tbl), attributes(unclass(json)))
  # Добавляем общее количество элементов в ответе
  attr(tbl, "totalCount") <- json_list[["totalCount"]]
  
  # Проставляем форматдаты
  tbl <- modify_at(tbl, c("createdAt", "updatedAt"), as.POSIXct, format = "%Y-%m-%d %H:%M:%OS")
  
  # Выводим результат
  tbl
}



