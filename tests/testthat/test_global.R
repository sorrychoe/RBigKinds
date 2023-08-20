library(readxl)
df <- read_excel("testdata/test.xlsx")

# header_remover 테스트
test_that("header_remover 함수가 헤더를 제거하는지 확인합니다.", {
  result <- header_remover(df)
  expect_equal(result$제목[1], " 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다")
})

# word_tokenizer 테스트
test_that("word_tokenizer 함수가 제목과 키워드를 분리하고 변환하는지 확인합니다.", {
  result <- word_tokenizer(df)
  expect_equal(result$키워드[1], "한반도")  
})

# keyword_dataframe 테스트
test_that("keyword_dataframe 함수가 키워드 빈도를 적절히 계산하는지 확인합니다.", {
  result <- keyword_dataframe(df)
  expect_equal(result$키워드[1], "대통령")
  expect_equal(result$n[1], 102) 
})
######

# keyword_dataframe_no_duplicated 테스트
test_that("keyword_dataframe_no_duplicated 함수가 중복되지 않은 키워드 개수를 적절히 계산하는지 확인합니다.", {
  result <- keyword_dataframe_no_duplicated(df)
  expect_equal(result$키워드[1], "윤석열")
  expect_equal(result$n[1], 31) 
})

# tfidf 테스트
test_that("tfidf 함수가 키워드의 tf-idf 스코어를 적절히 계산하는지 확인합니다.", {
  result <- tfidf(df)
  expect_equal(result$idf[1], 1.48807706)
  expect_equal(result$tf_idf[1], 0.0068260415) 
})

# tfidf_vector 테스트
test_that("tfidf_vector 함수가 tf-idf 벡터를 적절히 생성하는지 확인합니다.", {
  result <- tfidf_vector(df)
  expect_equal(is.matrix(result), TRUE)
})

# normalize_vector 테스트
test_that("normalize_vector 함수가 벡터를 정규화하는지 확인합니다.", {
  vec <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  result <- normalize_vector(vec)
  expect_equal(result[1, ], c(0, 0.6))
})