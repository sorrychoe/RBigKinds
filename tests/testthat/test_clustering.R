library(readxl)
df <- read_excel("testdata/test.xlsx")
vec <- tfidf_vector(df)

# Kmeans 테스트
test_that("Kmeans 함수가 군집화를 잘 하는지 확인합니다.", {
  result <- Kmeans(vec, 3)
  expect_equal(sum(unique(result$cluster)), 6)
})

# DBSCAN 테스트
test_that("DBSCAN 함수가 군집화를 수행하고 결과를 출력하는지 확인합니다.", {
  expect_output(model <- DBSCAN(vec, eps = 0.75, min_samples = 2), "cluster 갯수:")
  expect_equal(length(model$cluster), nrow(vec))
  expect_true(all(model$cluster >= 0))
})

