#' B2B export functions
#' 
#' @param b2b_site B2Bmotion site adress ("avs.express", "b2b.el-com.ru" etc)
#' @param method API method from \code{\link{api_all_methods}}
#' @param limit maximum elements in one page
#' @param ... filters 

b2b_export <- function(b2b_site, method, ...) {
  
   
  # Определяем лимит объектов на каждой странице
    limit <-  500
    
    # How much elements in export response  --------------------------
    totalCount <- b2b_site %>% 
      api_new_url(method = method, limit = 1, ...) %>% 
      api_get_response() %>% 
      content("text") %>% 
      fromJSON() %>% 
      .[["totalCount"]]
    
    # TODO! Если результат 0, то ошибка возникает
    # Error in txtProgressBar(min = 0, max = page_num, style = 3) : 
    #   must have 'max' > 'min' 
    # Ошибки наблюдатся на "b2b.hogart.ru" и "b2b.sanergy.ru"
    
     # пробуем исправить. 
   
    
    # Если нет ни одного объекта, передаём нулевой объект
    if (totalCount == 0) {
      return(NULL)
    }
    
    # ------
    print(paste("Load", method, "from ", b2b_site))
    
    
    # Page number  -----------------------
    page_num <- ceiling(totalCount/limit)
    
    # Tibble list for export results --------------------------------
    tbl_list <- vector(mode = "list", length = page_num)
    
    # Progress bar initialization
    progress_bar <- txtProgressBar(min = 0, max = page_num, style = 3)
    
   
    
    # Cicle for all page  ---------------------
    for (page in 1:page_num) {
        offset = limit * (page - 1)
        
        tbl_list[[page]] <- b2b_site %>% 
          api_new_url(method = method, 
                      limit = limit, 
                      offset = offset, 
                      ...) %>% 
          api_get_response() %>% 
          api_tidy_response()
        
        setTxtProgressBar(progress_bar, page)
    }
    
    # Close progress bar
    close(progress_bar)
    
    # Combine all results in one tibble by bind rows
    reduce(tbl_list, bind_rows)
}
