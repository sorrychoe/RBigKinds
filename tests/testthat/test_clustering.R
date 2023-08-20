library(readxl)
df <- read_excel("testdata/test.xlsx")
vec <- tfidf_vector(df)

# Kmeans 테스트
test_that("Kmeans 함수가 군집화를 잘 하는지 확인합니다.", {
  result <- Kmeans(vec, 3)
  expect_equal(sum(unique(result$cluster)), 6)
  expect_equal(sum(result$size), 6700)
})

# DBSCAN 테스트
test_that("DBSCAN 함수가 군집화를 잘 하는지 확인합니다.", {
  expect_output(DBSCAN(vec, 0.75, 50), "cluster 갯수: 10")
})

