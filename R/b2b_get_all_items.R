# Функция выдаёт список JSON с товарами
# TODO нужно добавить авт. распознование item_max_count
# Теперь сделаем функцию, которая сразу выдает DF

b2b_get_all_items <-
  function(b2b_site,
           items_max_count = 25000,
           limit = 1000) {
    # Определяем количество скролов для получения макс. кол-ва товаров
    scroll_number <- ceiling(items_max_count / limit)
    
    # Создаём пустой список для сохранения ответов JSON
    json_list <- vector(mode = "list", length = scroll_number)
    
    # Первый скрол пустой
    scrollId <- NULL
    
    # Запускаем цикл скролов
    for (i in 1:scroll_number) {
      # Генерируем ссылку с новым scrollId
      url <-
        paste0(
          "https://",
          b2b_site,
          "/api/v1/product-scroll",
          "?scrollId=",
          scrollId,
          "&filters%5Blimit%5D=",
          limit
        )
      
      #  Получаем ответ
      response <- user_api_get_response(url)
      
      # Проверяем, нет ли ошибки в ответе
      if (httr::http_error(response)) {
        stop(
          "Ошибка при подключении к ",
          api_url_site(url),
          ". Код ошибки: ",
          response$status_code
        )
      }
      
      # Сохраняем scrollId для следующей итерации
      scrollId <- content(response)$scrollId
      
      json_list[[i]] <- content(response)$items
    }
    
    # Выводим финальный результат в DF
    json_list %>%
      map_df(function(json) {
        tibble(
          # Далее следует список полей, который пока был нужен в работе
          # Его следует расширять
          id = json %>% map_chr("id"),
          type = json %>% map_chr("type"),
          article = json %>% map_chr("article"),
          brand = json %>% map("brand") %>% map_chr("name"),
          manufacturerCodeHtml = json %>% map_chr("manufacturerCodeHtml"),
          name = json %>% map_chr("name"),
          unitName = json %>% map_chr("unitName"),
          multiplicity = json %>% map_int("multiplicity"),
          image = json %>% map("image"),
          # ........
          # Необходимо вынести все другие поля
          # ....
        )
      })
    
  }
