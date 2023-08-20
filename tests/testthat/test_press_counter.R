library(readxl)
df <- read_excel("testdata/test.xlsx")

# header_remover 테스트
test_that("press_counter가 언론사 갯수를 정확히 세는지 체크합니다.", {
  result <- press_counter(df)
  expect_equal(result$언론사[1], "경향신문")
  expect_equal(result$기사[1], 6)
})


