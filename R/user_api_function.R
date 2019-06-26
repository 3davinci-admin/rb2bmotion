
# Универсальная функция для обращения к пользовательскому API
user_api_get_response <- function(url) {
  httr::GET(
    url,
    httr::add_headers(`Authorization` = paste0("Bearer ", get_b2b_user_api_key(api_url_site(url))))
  )
}


