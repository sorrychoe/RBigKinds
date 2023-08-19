# RBigKinds

RBigKinds는 한국 언론의 빅데이터 저장소인 BigKinds에서 추출한 데이터를 low-Code로 분석할 수 있게 만든 툴입니다.

## Requirements

- R >= 4.2

## Installation
본 패키지는 아직 CRAN에 배포되지 않았습니다. 
따라서, `devtools`을 활용해야만 다운로드가 가능합니다.

``` r
  devtools::install_github("sorrychoe/RBigKinds")
```

## Usage

```r
df <- read_xlsx("data.xlsx")

press_counter(df)

#    언론사 기사
#1 경향신문  938
#2 동아일보  614
#3 조선일보  611
#4 중앙일보  922
#5   한겨레  659

```

```r
data <- keyword_dataframe(df)

data[1:5,]

# A tibble: 5 × 2
#  키워드     n
#  <chr>  <int>
#1 대통령 16518
#2 정부    9723
#3 윤석열  6112
#4 일본    5882
#5 한국    5739
```

## License

[MIT](https://choosealicense.com/licenses/mit/)

## you have some issue?

사용 중 문제 발생 시, 해당 Repo issue에 등록해주세요.
