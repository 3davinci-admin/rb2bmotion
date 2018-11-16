# B2B export functions

b2b_export <- function(b2b_site, method, filters = NULL, limit = 100) {

  # Выясняем сколько всего элементов  --------------------------
  totalCount <-
    b2b_site %>%
    api_new_url(method = method, limit = 1, filters = filters) %>%
    api_get_response() %>%
    content("text") %>%
    fromJSON() %>%
    .[["totalCount"]]

  print(paste("Load", totalCount, method))

  # Количество дополнительных страниц -----------------------
  page_num <- ceiling(totalCount/limit)

  # Создаём пустой список для загрузки результата --------------------------------
  tbl_list <- vector(mode = "list", length = page_num)

  # Иницилизируем полосу загрузки
  progress_bar <- txtProgressBar(min = 0, max = page_num, style = 3)

  # Проходим по всем страницам ---------------------
  for (page in 1:page_num) {
    offset = limit*(page-1)

    tbl_list[[page]] <-
      b2b_site %>%
      api_new_url(method = method, limit = limit, offset = offset, filters = filters) %>%
      api_get_response() %>%
      api_tidy_response()

    setTxtProgressBar(progress_bar, page)
  }

  # Закрываем полосу загрузки
  close(progress_bar)

  reduce(tbl_list, bind_rows)
}
