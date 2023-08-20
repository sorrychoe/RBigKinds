library(readxl)
df <- read_excel("testdata/test.xlsx")

# header_remover 테스트
test_that("day_range가 첫번째 날짜와 마지막 날짜를 정확히 출력하는지 확인합니다.", {
  expect_output(day_range(df), "first day:  20200101 \\nlast day:  20200131")
})

