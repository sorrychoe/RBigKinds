library(readxl)
df <- read_xlsx("./test.xlsx")

# dcm 테스트
test_that("dcm이 잘 형성되는지 확인합니다.", {
  result <- dcm(df)
  expect_true(is.data.frame(result))
})


