# top_words

언론사 별로 가장 많이 등장한 단어 순위를 시각화합니다. 최대 몇개의
단어를 추출할지는 직접 정할 수 있습니다. default는 25개입니다.

## Usage

``` r
top_words(df, press = NA, top_n = 25)
```

## Arguments

- df:

  BigKinds 원본 문서

- press:

  확인할 언론사 이름

- top_n:

  시각화할 단어 갯수
