#' Список доступных методов API Export B2B Движение 
#' 
#' @description 
#' Подробное описание методов на  https://dataexport.docs.apiary.io/. 
#' Функция выводить список достпных методов в формате вектора character
#' 
#' @export
export_methods <- function() {
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


#' Созданием фильтра для URL API Export
#' 
#' @description 
#' Функция для создания объекта `export_filters`. Этот объект 
#' @param ... список параметров фильтра `id = c(13, 34), createdTo = "2019-01-01"`
#' 
#' нужен пример
#' @export
export_filters <- function(...) {
  filters <- list(...)
  structure(filters, class = "export_filters")
}


#' Создание ссылки для доступа API Export B2B Движение
#' 
#' @description 
#' Подробное описание Export API по ссылке: https://dataexport.docs.apiary.io/
#'  
#' @param b2b_site адрес сайта без "http://" ("b2b.sanergy.ru", "avs.express" и т.д.)
#' @param method метод Export API, список всех методов можно получить 
#' через \code{\link{export_methods}}
#' @param limit количество запрашиваемых объектов на одной странице (не может 
#' быть больше 1000), рекомендуется не более 500   
#' @param ... набор фильтров
#' @export
export_url <- function(b2b_site, method, limit = 100, offset = 0, ...) {
  # Необходимые проверки
  if (!length(method) == 1 &!method %in% api_export_methods()){
    stop(paste(method, "– недопустимый методв в Export API URL"))
  }
  if (limit > 1000) {
    stop("limit не может быть больше 1000")
  } 
  # TODO добавить проверку на наличие ключа для b2b_site
  # и выводит предупреждение, если ключа нет
  
  # Создаём объект S3
  url <- list(b2b_site =  b2b_site,
            method = method, 
            limit = limit,
            offset = offset, 
            filters = export_filters(...))
  
  class(url) <- "export_url"
  
  url
}


# TODO функция `add_export_filters(url, ...)` 
# - добавляем фильтры к URL

#' Функция для объекта со списком фильтров API Export в строку 
#' 
#' @description 
#' Функция необходима для создания ссылки API Export
#' 
#' @param filters объект S3 `export_filters` со списком фильтров
#' @export
as.character.export_filters <- function(filters) {
  # Создаем изначально пустую строку
  result <- ""
  
  # Проходим по всем элементам списка filters
  for (name in names2(filters)) {
    # Для скалярных элементов добавляем простую строчку с фильтром 
    if (filters[[name]] %>% rlang::is_scalar_character()) {
      result = paste0(result, "&filters[", name,"]=",filters[[name]])
    }  else {
      # Если это аргумент типа id = c(12, 31), то запускаем цикл для каждого значения в списке
      for (x in filters[[name]]) {
        result = paste0(result, "&filters[", name,"][]=", x)
      }
    }
  }
  
  result
}


#' Создание текстовой строки из объекта export_url
#' 
#' @param url объект типа `export_url`
#' 
#' @export
as.character.export_url <- function(url) {
  paste0("https://", url$b2b_site, "/", "api/v1/export-data/", url$method, "?", 
         "limit=", url$limit, "&offset=", url$offset, as.character(url$filters))
}