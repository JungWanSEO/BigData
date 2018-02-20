# 클러스터링이란 복잡한 데이터의 구조를 이해하기 위한 방안을 제시한다. 각 데이터들의 유사도를 측정하여 여러개의 작은 부분으로 나눈다.
#비지도 학습의 하나로 독립변수와 종속변수의 구분이 없다.
#우리가 사용한 방법은 K-means 클러스터링 이다.
#K-means 알고리즘은 세가지 단계를 거치는데
#1단계 K개(우리 자료는 목표 충전소 개수)의 초기 점을 정하고
#2단계 각 데티어들을 가장 가까운 초기점에 할당한다.
#3단계 가장 가까운 초기점이 같은 데이터들 끼리 클러스터를 만들고, 각 클러스터의 중심을 찾는다. 이 중심이 다음 단계를 위한 초기점이 되고, 단계 2가 반복된다.
#위의 단계는 속한 클러스터가 변경되지 않을 때까지 계속 진행된다.


library(stats)
juyuso <- read.xlsx("가평군 위경도.xlsx", sheetName= 1, header=T)

#NA 값 제거
juyuso1 <- na.omit(juyuso)

#위도 경도의 데이터를 정규화 시켜준다.
#데이터 정규화는 변숫값의 분포를 표준화 하는것으로, 변수값의 평균이 0이 되고 퍼짐의 정도 를 일정하게 하기 위함이다. 클러스터링, 서포트벡터머신, 인공신경망 등  많은 '분류'알고리즘에 많이 사용된다.
juyuso1$위도 <- scale(juyuso1$위도)
juyuso1$경도 <- scale(juyuso1$경도)

#클러스터링 실행 centers가 k를 의미하며, 우리 자료에서는 예측된 충전소 갯수이다.
juyuso1.kmeans <- kmeans(juyuso1[,4:5],centers=11)
juyuso1.kmeans

juyuso2 <- data.frame(juyuso, juyuso1.kmeans$cluster)

write.csv(juyuso2, "위경도 클러스터링.csv")



