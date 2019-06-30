#' Функция экспорта всех объектов для конкретного метода 
#' и набора фильтров Export API 
#' 
#' @param b2b_site адрес сайта ("avs.express", "b2b.el-com.ru" etc)
#' @param method метод Export API \code{\link{all_export_methods}}
#' @param limit максимальное количество объектов на странице (при скачивании)
#' не может быть больше 1000, рекомендуемое значение 100. Большие значения создают
#' нагрузку на сервере особенно для методов  "product", "specification", "user-cart",
#' "document" и "commercial-offer"
#' @param ... набор фильтров, подробнее смотрите \code{\link{export_filters}}
#' 
#' 
#' 
#' 
#' @import magrittr
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom purrr reduce
#' 
#' @export 

b2b_export <- function(b2b_site, method, limit = 100,  ...) {
  
  
  
  # Узнаём общее колличество
  tbl0 <- export_url(b2b_site, method, limit = 0, offset = 0, ...) %>% 
    get_json() %>% 
    as_tibble()
  
  totalCount <- attr(tbl0, "totalCount")
  
  # Если нет ни одного объекта, передаём нулевой объект
  if (totalCount == 0) {
    return(NULL)
  }
  
  # Выводим информацию о начале процесса скачивания данных:
  print(paste("Скачивается", totalCount, method, "from ", b2b_site))
  
  # Количество страниц, которые будем скачивать
  page_num <- ceiling(totalCount/limit)
  
  # Создаём список в который будем сохранять результат
  tbl_list <- vector(mode = "list", length = page_num)
  
  # Запускаем прогрессбар
  progress_bar <- txtProgressBar(min = 0, max = page_num, style = 3)
  
  # Цикл по скачиванию всех страниц
  for (page in 1:page_num) {
    offset = limit * (page - 1)
    
    tbl_list[[page]] <- b2b_site %>% 
      export_url(method = method, 
                 limit = limit, 
                 offset = offset, 
                 ...) %>% 
      get_json() %>% 
      as_tibble()
    
    setTxtProgressBar(progress_bar, page)
  }
  
  # Закрываем прогрессбар
  close(progress_bar)
  
  # Объединяем список в один tibble, соединяя построчно
  tbl <- reduce(tbl_list, bind_rows)
  
  # Сохраняем атрибуты
  attr(tbl, "url") <- attr(tbl0, "url")
  attr(tbl, "download_time") <- Sys.time()
  
  # Выводим результат 
  tbl
}