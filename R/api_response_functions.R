# Функции, которые реализуют методы API
# https://dataexport.docs.apiary.io/



# get API response --------------------------------------------------------

api_get_response <- function(url) {

  # !todo check secret_key for api_url_site(url)

  # load response
  response <- GET(url, add_headers(`Secret-Key` = get_b2b_key(api_url_site(url))))

  # Check connection
  if(http_error(response)) {
    stop("Ошибка при подключении к ", api_url_site(url),". Код ошибки: ", response$status_code)
  }

  return(response)
}


# tidy API response  ------------------------------------------------------------
api_tidy_response <- function(response) {

  if (!class(response) == "response") stop("argument is not response class")
  if (http_error(response)) stop("http_error")

  # retrive json content to list
  json <-
    response %>%
    content() %>%
    .[["items"]]

  # tidy user ------------------------
  if (api_url_method(response$url) == "user") {

    tbl <-
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
        companyId         = json %>% map("company") %>% map("id") %>% as.character(),
        companyName       = json %>% map("company") %>% map("name") %>% as.character(),
        targetId          = json %>% map("target") %>% map("id") %>% as.character(),
        targetName        = json %>% map("target") %>% map("name") %>% as.character(),
        involvementId     = json %>% map("involvement") %>% map("id") %>% as.character(),
        involvementName   = json %>% map("involvement") %>% map("name") %>% as.character(),
        regionId          = json %>% map("region") %>% map("id") %>% as.character(),
        regionName        = json %>% map("region") %>% map("name") %>% as.character(),
        createdAt         = json %>% map("createdAt") %>% as.character() %>% ymd_hms()
      )

  }

  # tidy company
  if (api_url_method(response$url) == "company") {

    tbl <-
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
        createdAt                = json %>% map("createdAt") %>% as.character() %>% ymd_hms()
      )

  }

  # tidy order ------------------------
  if (api_url_method(response$url) == "order") {

    tbl <-
      tibble(
        orderId                    = json %>% map("id") %>% as.character(),
        ip                         = json %>% map("ip") %>% as.character(),
        customerId                 = json %>% map("customer") %>% map("id") %>% as.character(),
        customerEmail              = json %>% map("customer") %>% map("email") %>% as.character(),
        customerPhone              = json %>% map("customer") %>% map("phone") %>% as.character(),
        customerRegionId           = json %>% map("customer") %>% map("region") %>% map("id") %>% as.character(),
        customerRegionName         = json %>% map("customer") %>% map("region") %>% map("name") %>% as.character(),

        chatChannelId              = json %>% map("chatChannelId") %>% as.character(),
        merchant                   = json %>% map("merchant") %>% as.character(),
        companyId                  = json %>% map("companyId") %>% as.character(),
        managers                   = json %>% map("managers"),

        statusId                   = json %>% map("status") %>% map("id") %>% as.character(),
        statusName                 = json %>% map("status") %>% map("name") %>% as.character(),
        comment                    = json %>% map("comment") %>% as.character(),
        sourceId                   = json %>% map("source") %>% map("id") %>% as.character(),
        sourceName                   = json %>% map("source") %>% map("name") %>% as.character(),

        newDocumentsCount          = json %>% map("newDocumentsCount") %>% as.integer(),
        isNewStatus                = json %>% map("isNewStatus") %>% as.logical(),
        documents                  = json %>% map("documents"),

        orderItemsTotalPrice       = json %>% map("orderItemsTotalPrice") %>% as.double(),
        orderCustomItemsTotalPrice = json %>% map("orderCustomItemsTotalPrice") %>% as.double(),
        orderDocumentsCount        = json %>% map("orderDocumentsCount") %>% as.integer(),
        isTestOrder                = json %>% map("isTestOrder") %>% as.logical(),
        createdAt                  = json %>% map_chr("createdAt") %>% as.character() %>% ymd_hms(),
        updatedAt                  = json %>% map_chr("updatedAt") %>% as.character() %>% ymd_hms(),
        currentPriceUpdatedAt      = json %>% map("currentPriceUpdatedAt") %>% as.character() %>% ymd_hms()
      )

  }

  # tidy order-items ------------------------
  if (api_url_method(response$url) == "order-items") {

    tbl <-
      tibble(
        orderId                      = json %>% map("id") %>% as.character(),
        orderItemsTotalPrice         = json %>% map("orderItemsTotalPrice")  %>% as.double(),
        orderCustomItemsTotalPrice   = json %>% map("orderCustomItemsTotalPrice") %>% as.double(),
        items                        = json %>% map("items"),
        customItems                  = json %>% map("customItems")
      )

  }

  # tidy specification ------------------------
  if (api_url_method(response$url) == "specification") {

    tbl <-
      tibble(
        specificationId              = json %>% map("id") %>% as.character(),
        userId                       = json %>% map("user") %>% map("id"),
        userGroupId                  = json %>% map("user") %>% map("group") %>% map("id"),
        userGroupName                = json %>% map("user") %>% map("group") %>% map("name"),
        userCompanyId                = json %>% map("companyId"),
        specificationName           = json %>% map("name") %>% as.character(),
        specificationDescription    = json %>% map("description") %>% as.character(),
        archive                     = json %>% map("archive") %>% as.logical(),
        orders                      = json %>% map("orders"),
        items                       = json %>% map("items"),
        customItems                 = json %>% map("customItems"),
        share                       = json %>% map("share"),
        commercialOffers            = json %>% map("commercialOffers"),
        createdAt                   = json %>% map("createdAt") %>% as.character() %>% ymd_hms(),
        updatedAt                   = json %>% map("updatedAt") %>% as.character() %>% ymd_hms(),
        currentPriceUpdatedAt       = json %>% map("currentPriceUpdatedAt") %>% as.character() %>% ymd_hms()
      )


  }

  # tidy user-cart ------------------------
  if (api_url_method(response$url) == "user-cart") {

    tbl <-
      tibble(
        userId                      = json %>% map("user") %>% map("id"),
        userGroupId                 = json %>% map("user") %>% map("group") %>% map("id"),
        userGroupName               = json %>% map("user") %>% map("group") %>% map("name"),
        userCompanyId               = json %>% map("companyId"),
        items                       = json %>% map("items"),
        customItems                 = json %>% map("customItems"),
        totalPrice                  = json %>% map("totalPrice"),
        createdAt                   = json %>% map("createdAt") %>% as.character() %>% ymd_hms(),
        updatedAt                   = json %>% map("updatedAt") %>% as.character() %>% ymd_hms(),
        currentPriceUpdatedAt       = json %>% map("currentPriceUpdatedAt") %>% as.character() %>% ymd_hms()
      )

  }


  # tidy document ------------------------
  if (api_url_method(response$url) == "document") {

    tbl <-
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
        createdAt                   = json %>% map("createdAt") %>% as.character() %>% ymd_hms()
      )

  }

  # tidy commercial-offer ------------------------
  if (api_url_method(response$url) == "commercial-offer") {

    tbl <-
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
        createdAt                    = json %>% map("createdAt") %>% as.character() %>% ymd_hms(),
        updatedAt                    = json %>% map("updatedAt") %>% as.character() %>% ymd_hms()
      )

  }

  # tidy product ------------------------
  if (api_url_method(response$url) == "product") {
    tbl <-
      tibble(
        productId                  = json %>% map("id") %>% as.character(),
        productExternalId          = json %>% map("externalId") %>% as.character(),
        article                    = json %>% map("article") %>% as.character(),
        manufacturerCode           = json %>% map("manufacturerCode")  %>% as.character(),
        productIdentifiers         = json %>% map("productIdentifiers"),
        manufacturer               = json %>% map("manufacturer"),
        brandId                    = json %>% map("brand") %>% map("id"),
        brandName                  = json %>% map("brand") %>% map("name"),
        brandSynonyms              = json %>% map("brand") %>% map("synonyms"),
        productName                = json %>% map("name") %>% as.character(),
        nameOfManufacturer         = json %>% map("nameOfManufacturer") %>% as.character(),
        multiplicity               = json %>% map("multiplicity") %>% as.double(),
        unitName                   = json %>% map("unitName") %>% as.character(),
        # ??? почему только одна цена??? (а не несколько прайс-листов)
        price                      = json %>% map("price"),
        series                     = json %>% map("series"),
        country                    = json %>% map("country"),
        hasImage                   = json %>% map("hasImage") %>% as.character() %>% as.logical(),
        hasFeatures                = json %>% map("hasFeatures") %>% as.character() %>% as.logical(),
        hasCertificates            = json %>% map("hasCertificates") %>% as.character() %>% as.logical(),
        stockStatus                = json %>% map("stockStatus"),
        section                    = json %>% map("section"),
        tokens                     = json %>% map("tokens"),
        productAnalog              = json %>% map("productAnalog"),
        productRelated             = json %>% map("productRelated")
      )
  }

  tbl
}
