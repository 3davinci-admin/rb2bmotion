#' Get http response from url
#' @param url URL from \code{\link{api_new_url}}
#' @import httr 
#' @import jsonlite
#' @import purrr
#' @import dplyr
#' @import tibble
#' @import magrittr
#' @export 

# TODO нужно добавить тесты (сохранить запросы из sanergy для тест --------
api_get_response <- function(url) {

  # TODO: проверить наличие Secret-Key и вывести предупреждение, 
  # TOTO: попробовать функцию tryCatch
  # tryCatch({
  #   !identical(attr(group_data(.tbl), ".drop"), FALSE)
  # }, error = function(e){
  #   TRUE
  # })
  # если его нет
  
  # load response
  response <- GET(
    url,
    add_headers(`Secret-Key` = get_b2b_key(api_url_site(url)))
  )

  # Check connection
  if (http_error(response)) {
    stop(
      "Ошибка при подключении к ", 
      api_url_site(url), 
      ". Код ошибки: ", 
      response$status_code
    )
  }
  
<<<<<<< HEAD
  # Добавляем класс по методу
  class_name <- api_url_method(url)
  # TODO нужно сделать приведение методов к правильному названию (тире заменить)
  # на подчеркивание
  if (class_name == "commercial-offer") class_name <- "commercial_offer"
  if (class_name == "user-cart") class_name <- "user_cart"
  if (class_name == "order-items") class_name <- "order_items"
  response <- append(class(response),paste0("response_", class_name))
=======
  # Добавляем класс к методу
  response <- append(class(response),paste0("response_", api_url_method(url)))
>>>>>>> 0341ed83275932fd34371801fd9f4b7f47cd6b42

  return(response)
}

# Функция для получения JSON-файла из ответа сервера
#' @export 
api_get_json <- function(response) {

  if (!class(response) == "response") stop("argument is not response class")
  if (http_error(response)) stop("http_error")
  
<<<<<<< HEAD
  json <- response %>%
    content() %>%
    .[["items"]]
  
  # TODO добавить различные классы для JSON
  return(json)
=======
  response %>%
    content() %>%
    .[["items"]]
>>>>>>> 0341ed83275932fd34371801fd9f4b7f47cd6b42
}

#' Tidy response 
#' 
#' Create tidy tibble from http-response 
#' 
#' @param response http-response from \code{\link{api_get_response}}
#' @export 
# Общий метод
api_tidy_response <- function(response){
  UseMethod("api_tidy_response", response)
}

# Список пользователей ----------
#' @export 
api_tidy_response.response_user <- function(response) {
  # retrive json content to list
  json <- api_get_json(response)
    
  # Выдаем результат
  tibble(
    customerId        = json %>% map("id") %>% as.character(),
    groupId           = json %>% map("group") %>% map("id") %>% as.character(),
    groupName         = json %>% map("group") %>% map("name") %>% as.character(),
    role              = json %>% map("role") %>% map("role")%>% as.character(),
    lastname          = json %>% map("lastname") %>% as.character(),
    firstname         = json %>% map("firstname") %>% as.character(),
    middlename        = json %>% map("middlename") %>% as.character(),
    gender            = json %>% map("gender") %>% as.character(),
    phone             = json %>% map("phone") %>% as.character(),
    email             = json %>% map("email")%>% as.character(),
    emailVerified     = json %>% map("emailVerified") %>% as.character(),
    currentCompanyId  = json %>% map("company") %>% map("id") %>% as.character(),
    currentCompanyName= json %>% map("company") %>% map("name") %>% as.character(),
    targetId          = json %>% map("target") %>% map("id") %>% as.character(),
    targetName        = json %>% map("target") %>% map("name") %>% as.character(),
    involvementId     = json %>% map("involvement") %>% map("id") %>% as.character(),
    involvementName   = json %>% map("involvement") %>% map("name") %>% as.character(),
    regionId          = json %>% map("region") %>% map("id") %>% as.character(),
    regionName        = json %>% map("region") %>% map("name") %>% as.character(),
    customerCreatedAt = json %>% map("createdAt") %>% as.character() %>% ymd_hms()
  )
}

# Список компаний ----------
#' @export 
api_tidy_response.response_company <- function(response){
  # retrive json content to list
  json <- api_get_json(response)
  
  # Список компаний
  tibble(
    companyId                = json %>% map("id") %>% as.character(),
    companyExternalId        = json %>% map("group") %>% as.character(),
    companyName              = json %>% map("name") %>% as.character(),
    companyDescription       = json %>% map("description") %>% as.character(),
    isIndividual             = json %>% map("isIndividual") %>% as.logical(),
    priceTypeId              = json %>% map("priceType") %>% map("id") %>% as.character(),
    priceTypeName            = json %>% map("priceType") %>% map("name") %>% as.character(),
    priceTypeCurrency        = json %>% map("priceType") %>% map("currency"),
    priceWithVat             = json %>% map("priceType") %>% map("withVat") %>% as.character(),
    legalEntities            = json %>% map("legalEntities"),
    addresses                = json %>% map("addresses"),
    payments                 = json %>% map("payments"),
    companyCreatedAt         = json %>% map("createdAt") %>% as.character() %>% ymd_hms()
  )
}

# Список заказов ----------
#' @export 
api_tidy_response.response_order <- function(response){
  # retrive json content to list
  json <- api_get_json(response)
  
  # Список заказов
  tibble(
    orderId                    = json %>% map("id") %>% as.character(),
    ip                         = json %>% map("ip") %>% as.character(),
    customerId                 = json %>% map("customer") %>% map("id") %>% as.character(),
    customerEmail              = json %>% map("customer") %>% map("email") %>% as.character(),
    customerPhone              = json %>% map("customer") %>% map("phone") %>% as.character(),
    customerRegionId           = json %>% map("customer") %>% map("region") %>% map("id") %>% as.character(),
    customerRegionName         = json %>% map("customer") %>% map("region") %>% map("name") %>% as.character(),
    
    chatChannelId              = json %>% map("chatChannelId") %>% as.character(),
    orderMerchant              = json %>% map("merchant") %>% as.character(),
    orderCompanyId             = json %>% map("companyId") %>% as.character(),
    orderManagers              = json %>% map("managers"),
    
    statusId                   = json %>% map("status") %>% map("id") %>% as.character(),
    statusName                 = json %>% map("status") %>% map("name") %>% as.character(),
    comment                    = json %>% map("comment") %>% as.character(),
    sourceId                   = json %>% map("source") %>% map("id") %>% as.character(),
    sourceName                 = json %>% map("source") %>% map("name") %>% as.character(),
    
    newDocumentsCount          = json %>% map("newDocumentsCount") %>% as.integer(),
    isNewStatus                = json %>% map("isNewStatus") %>% as.logical(),
    documents                  = json %>% map("documents"),
    
    orderItemsTotalPrice       = json %>% map("orderItemsTotalPrice") %>% as.double(),
    orderCustomItemsTotalPrice = json %>% map("orderCustomItemsTotalPrice") %>% as.double(),
    orderDocumentsCount        = json %>% map("orderDocumentsCount") %>% as.integer(),
    isTestOrder                = json %>% map("isTestOrder") %>% as.logical(),
    #TODO нужно заменить функцию lubridate на базовую --------
    # Warning messages:
    # 1: All formats failed to parse. No formats found. 
    # 2: All formats failed to parse. No formats found. 
    # 3: All formats failed to parse. No formats found. 
    # 4: All formats failed to parse. No formats found. 
    orderCreatedAt             = json %>% map_chr("createdAt") %>% as.character() %>% ymd_hms(), 
    orderUpdatedAt             = json %>% map_chr("updatedAt") %>% as.character() %>% ymd_hms(),
    currentPriceUpdatedAt      = json %>% map("currentPriceUpdatedAt") %>% as.character() %>% ymd_hms()
  )
}

# Содержание заказов ----------
#' @export 
<<<<<<< HEAD
api_tidy_response.response_order_items <- function(response){
=======
api_tidy_response.response_order <- function(response){
>>>>>>> 0341ed83275932fd34371801fd9f4b7f47cd6b42
  # retrive json content to list
  json <- api_get_json(response)
  
  # Выдаём список с содержанием заказов
  tibble(
    orderId                      = json %>% map("id") %>% as.character(),
    orderItemsTotalPrice         = json %>% map("orderItemsTotalPrice")  %>% as.double(),
    orderCustomItemsTotalPrice   = json %>% map("orderCustomItemsTotalPrice") %>% as.double(),
    items                        = json %>% map("items"),
    customItems                  = json %>% map("customItems")
  )
}


# Спецификации ----------
#' @export 
api_tidy_response.response_specification <- function(response){
  # retrive json content to list
  json <- api_get_json(response)
  
  # Спецификации
  tibble(
    specificationId             = json %>% map("id") %>% as.character(),
    userId                      = json %>% map("user") %>% map("id"),
    userGroupId                 = json %>% map("user") %>% map("group") %>% map("id"),
    userGroupName               = json %>% map("user") %>% map("group") %>% map("name"),
    userCompanyId               = json %>% map("companyId"),
    specificationName           = json %>% map("name") %>% as.character(),
    specificationDescription    = json %>% map("description") %>% as.character(),
    archive                     = json %>% map("archive") %>% as.logical(),
    orders                      = json %>% map("orders"),
    items                       = json %>% map("items"),
    customItems                 = json %>% map("customItems"),
    share                       = json %>% map("share"),
    commercialOffers            = json %>% map("commercialOffers"),
    specificationCreatedAt      = json %>% map("createdAt") %>% as.character() %>% ymd_hms(),
    specificationUpdatedAt      = json %>% map("updatedAt") %>% as.character() %>% ymd_hms(),
    currentPriceUpdatedAt       = json %>% map("currentPriceUpdatedAt") %>% as.character() %>% ymd_hms()
  )
}

# Корзины
<<<<<<< HEAD
#' @export
# TODO возникает проблема с именем 
api_tidy_response.response_user_cart <- function(response){
=======
#' @export 
api_tidy_response.response_user-cart <- function(response){
>>>>>>> 0341ed83275932fd34371801fd9f4b7f47cd6b42
  # retrive json content to list
  json <- api_get_json(response)
  
  # Корзины
  tibble(
    userId                      = json %>% map("user") %>% map("id"),
    userGroupId                 = json %>% map("user") %>% map("group") %>% map("id"),
    userGroupName               = json %>% map("user") %>% map("group") %>% map("name"),
    userCompanyId               = json %>% map("companyId"),
    items                       = json %>% map("items"),
    customItems                 = json %>% map("customItems"),
    totalPrice                  = json %>% map("totalPrice"),
    cartCreatedAt                   = json %>% map("createdAt") %>% as.character() %>% ymd_hms(),
    cartUpdatedAt                   = json %>% map("updatedAt") %>% as.character() %>% ymd_hms(),
    currentPriceUpdatedAt       = json %>% map("currentPriceUpdatedAt") %>% as.character() %>% ymd_hms()
  )
  
}


# Документы
#' @export 
api_tidy_response.response_document <- function(response){
  # retrive json content to list
  json <- api_get_json(response)
  
  # Документы
  tibble(
    documentId                  = json %>% map("id") %>% as.character(),
    externalId                  = json %>% map("externalId") %>% as.character(),
    documentNumber              = json %>% map("number") %>% as.character(),
    documentName                = json %>% map("name") %>% as.character(),
    documentTypeId              = json %>% map("type") %>% map("id") %>% as.character(),
    documentTypeName            = json %>% map("type") %>% map("name") %>% as.character(),
    totalAmount                 = json %>% map("totalAmount") %>% as.double(),
    paid                        = json %>% map("paid") %>% as.character() %>% as.logical(),
    orderId                     = json %>% map("orderId") %>% as.character(),
    documentCreatedAt           = json %>% map("createdAt") %>% as.character() %>% ymd_hms()
  )
}
  
# Коммерческие предложения
#' @export 
<<<<<<< HEAD
api_tidy_response.response_commercial_offer <- function(response){
=======
api_tidy_response.response_commercial-offer <- function(response){
>>>>>>> 0341ed83275932fd34371801fd9f4b7f47cd6b42
  # retrive json content to list
  json <- api_get_json(response)
  
  # КП
  tibble(
    commercialOfferId           = json %>% map("id") %>% as.character(),
    userId                       = json %>% map("user") %>% map("id"),
    userGroupId                  = json %>% map("user") %>% map("group") %>% map("id"),
    userGroupName                = json %>% map("user") %>% map("group") %>% map("name"),
    specificationId              = json %>% map("specificationId") %>% as.character(),
    commercialOfferName          = json %>% map("name") %>% as.character(),
    commercialOfferDescription   = json %>% map("description") %>% as.character(),
    logo                         = json %>% map("logo") %>% as.character(),
    contactsIntro                = json %>% map("contactsIntro") %>% as.character(),
    contactsFinal                = json %>% map("contactsFinal") %>% as.character(),
    title                        = json %>% map("title") %>% as.character(),
    textIntro                    = json %>% map("textIntro") %>% as.character(),
    textFinal                    = json %>% map("textFinal") %>% as.character(),
    vatRate                      = json %>% map("vatRate") %>% as.double(),
    archive                      = json %>% map("archive") %>% as.character() %>% as.logical(),
    items                        = json %>% map("items"),
    customItems                  = json %>% map("items"),
    commercialOfferCreatedAt     = json %>% map("createdAt") %>% as.character() %>% ymd_hms(),
    commercialOfferUpdatedAt     = json %>% map("updatedAt") %>% as.character() %>% ymd_hms()
  )
  
}
  

  