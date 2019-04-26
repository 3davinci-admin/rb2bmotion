# B2B export functions

b2b_export <- function(b2b_site, method, filters = NULL, limit = 100) {
    
    # How much elements in export response  --------------------------
    totalCount <- b2b_site %>% 
      api_new_url(method = method, limit = 1, filters = filters) %>% 
      api_get_response() %>% 
      content("text") %>% 
      fromJSON() %>% 
      .[["totalCount"]]
    
    print(paste("Load", totalCount, method, "from ", b2b_site))
    
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
                      filters = filters) %>% 
          api_get_response() %>% 
          api_tidy_response()
        
        setTxtProgressBar(progress_bar, page)
    }
    
    # Close progress bar
    close(progress_bar)
    
    # Combine all results in one tibble by bind rows
    reduce(tbl_list, bind_rows)
}
