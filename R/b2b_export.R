#' B2B export functions
#' 
#' @param b2b_site B2Bmotion site adress ("avs.express", "b2b.el-com.ru" etc)
#' @param method API method from \code{\link{api_all_methods}}
#' @param limit maximum elements in one page
#' @param ... filters for \code{\link{api_new_url}}
#' 
#' @import magrittr
#' @import tibble
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