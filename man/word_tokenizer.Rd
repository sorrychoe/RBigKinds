% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global.R
\name{word_tokenizer}
\alias{word_tokenizer}
\title{word_tokenizer}
\usage{
word_tokenizer(df)
}
\arguments{
\item{df}{BigKinds 원본 문서}
}
\description{
파일로부터 문서 별 키워드로 나열된 데이터 프레임으로 변환합니다.
}
\examples{
df <- data.frame(
  "일자" = c(20230101, 20230102, 20230103),
  "언론사" = c("조선일보", "한겨례", "경향신문"),
  "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
              "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
              "‘직권남용죄’ 남용 유감"),
  "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
                "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
                "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
data <- word_tokenizer(df)
head(data)

}
