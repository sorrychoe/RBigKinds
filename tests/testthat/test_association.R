library(readxl)
df <- read_xlsx("./test.xlsx")

# association 테스트
test_that(" association 함수가 연관 분석을 정확히 수행하는지 확인합니다.", {
  result <- association(df)
  expect_equal(colnames(result)[4], "confidence")
  expect_true(mean(result$confidence) > 0.5) 
})

