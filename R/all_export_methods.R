#' Список доступных методов API Export B2B Движение 
#' 
#' @description 
#' Подробное описание методов на  https://dataexport.docs.apiary.io/. 
#' Функция выводить список достпных методов в формате вектора character
#' 
#' @export
all_export_methods <- function() {
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