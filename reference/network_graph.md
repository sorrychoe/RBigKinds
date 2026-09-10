# word network

단어 네트워크 분석 결과를 시각화합니다.

## Usage

``` r
network_graph(dcm, topwords = 50, min_freq = 0.5)
```

## Arguments

- dcm:

  단어 동시출현행렬

- topwords:

  feature 단어 갯수, 최대 50개까지 가능

- min_freq:

  그래프를 형성할 연결 최소 빈도 수
