library(readxl)
df <- read_excel("testdata/test.xlsx")

# topic_models 테스트
test_that("lda 함수가 정상적으로 수행하는지 확인합니다.", {
  result <- topic_modeling(df, 3)
  expect_equal(typeof(result), "list")
  expect_equal(sum(unique(result$topic)), 6)
})
