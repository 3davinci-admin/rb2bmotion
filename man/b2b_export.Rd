% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/b2b_export.R
\name{b2b_export}
\alias{b2b_export}
\title{Функция экспорта всех объектов для конкретного метода 
и набора фильтров Export API}
\usage{
b2b_export(b2b_site, method, limit = 100, ...)
}
\arguments{
\item{b2b_site}{адрес сайта ("avs.express", "b2b.el-com.ru" etc)}

\item{method}{метод Export API \code{\link{all_export_methods}}}

\item{limit}{максимальное количество объектов на странице (при скачивании)
не может быть больше 1000, рекомендуемое значение 100. Большие значения создают
нагрузку на сервере особенно для методов  "product", "specification", "user-cart",
"document" и "commercial-offer"}

\item{...}{набор фильтров, подробнее смотрите \code{\link{export_filters}}}
}
\description{
Функция экспорта всех объектов для конкретного метода 
и набора фильтров Export API
}
