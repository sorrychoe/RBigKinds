#' RBigKinds: BigKinds Data Analysis Toolkit for R
#'
#' @docType package
#' @name RBigKinds
#' @description RBigKinds는 한국 언론의 빅데이터 저장소인 BigKinds에서 추출한 데이터를 low-Code로 분석할 수 있게 만든 툴입니다.
#'
NULL
#'> NULL

#' ### common
#' devtools::use_package("knitr","Suggests")
#' devtools::use_package("rmarkdown","Suggests")
#' devtools::use_package("testthat","Suggests")
#' devtools::use_package("dplyr","Imports")
#' devtools::use_package("tm","Imports")
#' devtools::use_package("tibble","Imports")
#' devtools::use_package("tidytext","Imports")
#'
#' ### visualization
#' devtools::use_package("ggplot2","Imports")
#' devtools::use_package("wordcloud2","Imports")
#'
#' ### topicmodel
#' devtools::use_package("topicmodels","Imports")
#'
#' ### association
#' devtools::use_package("arules","Imports")
#'
#' ### clustering
#' devtools::use_package("dbscan","Imports")
#'
#' ### network analysis
#' devtools::use_package("quanteda", "Imports")
