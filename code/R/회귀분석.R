#시군구 단위에서의 적합한 전기차 충전소의 수를 유추하기 위하여 회귀분석을 진행하였다.
# 회귀분석을 위한 가정
#1. 현재 존재하는 주유소는 승용자동차에 가장 적합한 위치에 세워져 있다.
# 데이터 탐색과정 중 주유소 시각화를 보면 각 주유소들은 주요 도로와 주요거점들에  적합하게 지어져 있음을 확인할 수 있었다. 
# 또한 시장 논리에 따라 적합한 위치가 아닌 곳에는 주유소를 세울리가 없고, 세워진다하더라도 그 주유소는 경영상 어려움을 겪은 후 폐업 수순을 밟을 것이기에 가장 적합하다고 판단하였다.

#2. 주유소 수에 영향을 끼치는 변수로 자동차 등록수, 아파트(1000세대 이상)수, 주요행정시설(동, 구, 시, 군, 경찰서, 법원)의 수, 관광지수, 면적 가정하였다.

#3. 전기차 충전소 수를 도출하기 위하여 자동차 등록 대수를 중요한 독립 변수로 삼았다. 이후 도출 값을 구하기 위하여 자동차 등록 대수의  2%만 전기차로 가정하여 설정하였다. 여기서 2%는 전기자동차 예상 점유율로 '서비스시설 입지와 용량 최적화(전기자동차 충전소 구축에의 적용, 저자 김정훈, 2011년)논문'을 인용한 값이다.

#4. 교통량의 값이 아닌 자동차 등록수를 가지고 변수를 한 이유는 교통량의 경우 전기차의 점유율을 예상할 수 있는 근거를 얻을 수 있는 자료를 구할 수 가 없었기 때문이다. 따라서 가장 근접한 충전소 수를 얻기 위한 일환으로 예측치를 부정확하게 인식할 수 있는 값은 배제 하였다.
### 주의! 위의 4번 내용은 누군가 물어보면 답하고, 굳이 묻지 않으면 넘어가야함. 교통량*아파트*관광지 로그값에 테클 들어올 수 있음.



g.station <- read.csv('변수 종합.csv', header = T)

g.station1 <- g.station

#자동차 등록대수와 면적, 주유소의 값이 다른 변수에 비해 값이 너무 커서 log값을 취하여 값을 줄였다.
g.station1$car <- log(g.station1$car)
g.station1$area <- log(g.station1$area)
g.station1$station <- log(g.station1$station)


#무작위 추출
g.ind <- sample(1:2,size=nrow(g.station1),replace=TRUE,prob=c(0.8,0.2))

#트레인 세트와 테스트 세트로 나눔(모델의 정확성 확인을 위해)
g.train <- g.station1[g.ind == 1,]
g.test <- g.station1[g.ind == 2,]

#다중 회귀분석에서 가장 좋은 독립변수를 선택할 수 있도록 해줌
#일반 다중 휘귀 방식 코드을 통해 계산한 결과 RMSE(오차값)이 크게 나와 라쏘 릿지 엘라스틱넷 회귀 분석을 이용하였다.

###RMSE '오차'(예측값 - 실제값)을 '제곱'해서 '평균'한 값의 '제곱근(루트값)'한것. 즉, 그냥 오차값으로 생각하면 됨.

library(leaps)
g.train.lm3 <- regsubsets(station ~ . , data = g.train)
g.train.lm3
summary(g.train.lm3)
summary(g.train.lm3)$bic
summary(g.train.lm3)$adjr2

#계산할 때 마다 달라지겠지만 최초 계산시 adj-R^2 값이 0.8281718인 것을 선택함.
#g.train.lm4 <- lm(station ~ car + factory + apt + gu_office +  si_office + gun_office + court + area , data = g.train) 

g.train.lm4 <- lm(station ~ car + factory + apt + dong_office + si_office + gun_office + court + area , data = g.train) 

g.train.lm4
summary(g.train.lm4)

#RMSE 값을 구해주는 식, 값이 작을 수록 좋다.
sqrt(mean((predict(g.train.lm4,newdata=g.test)-g.test$station)^2))


#라쏘(lasso), 릿지(ridge), 엘라스틱넷(elastic net) 회귀분석
# 일반적인 다중회귀 방식은 설명변수의 개수가 다수 존재하거나 변수간 상관성이 큰경우 모형이 매우 불안정해진다.
#이때 축소추정법에 의해 모형의 성능을 개선할 수 있는데 그 방법이 라쏘, 릿지, 엘라스틱넷이다. 
# 특히 라쏘 회귀식을 선택한 이유는 모델의 안전성과 해석력을 높이기 위해 사용되었다. 라쏘 회귀식은 회귀계수 축소를 통해 예측 정확도를 릿지의 장점과 영향력이 적은 회귀계수 값을 쉽게 0으로 만드는 변수선택 기능이 있어 예측정확도와 변수선택의 해석력을 모두 갖출 수 있는 분석 방법이다.
# 엘라스틱넷은 라쏘와 릿지의 회귀식을 조합하여 최상의 coef(회귀계수, 가중치값)을 찾아준다.하지만 여러번의 계산으 통해 적합한 값을 찾아야 한다.


#라쏘 회귀식을 위한 세팅
X <- as.matrix(g.train[,2:13])
Y <- g.train$station


library(glmnet)
#라쏘휘귀분석
#res.lasso <- cv.glmnet(X, Y, alpha = 1)
#res.lasso
#plot(res.lasso)

#최적의 값을 찾기 위해 fold(nfolds = 숫자)를 하는 데, 이를 교차 검증이라 한다. 교차검증은 변수들의 값중에서 Training set , Validation set , Test set을 미리 설정하여 계산하는것을 말한다. 이는 과적합을 방지하기 위하여 설정하는 것이다. 과적합이란 학습데이터를 과도하게 학습하여 생기는 형상으로 표본 집단에 너무 과도하게 적합하여 실제 모집단에 적용시 오히려 분석력이 떨어지는 현상이다. 

res.lasso <- cv.glmnet(X, Y, alpha = 1, nfolds = 3)
#res.lasso
plot(res.lasso)


#라쏘회귀 가중치값 뽑기. lambda.min은 교차검증에서 에러가 최소로 나타나는 람다 값을 뽑아준다.

coef.lasso <- coef(res.lasso, s = "lambda.min")[,1]
coef.lasso

#릿지회귀분석
res.ridge <- cv.glmnet(X, Y, alpha = 0, nfolds = 3)
#res.ridge 
plot(res.ridge)

#릿지회귀 가중치값 뽑기
coef.ridge <- coef(res.ridge, s = "lambda.min")[,1]
coef.ridge

#엘라스틱네 회귀
res.elastic <- cv.glmnet(X, Y, alpha =.5, nfolds = 3)
#res.elastic 
plot(res.elastic)
summary(res.elastic)

#엘라스틱넷 가중치값 뽑기
coef.elastic <- coef(res.elastic, s = "lambda.min")[,1]
coef.elastic


#오차율 계산 전처리
n.test <- as.matrix(g.test)

#라쏘오차율 계산(RMSE)
lasso.pred <- predict.cv.glmnet(res.lasso,lambda = res.lasso$lambda.min, newx = n.test[,2:13])
sqrt(mean((lasso.pred-n.test[,1])^2))

#릿지오차율 계산(RMSE)
ridge.pred <- predict.cv.glmnet(res.ridge,lambda = res.ridge$lambda.min, newx = n.test[,2:13])
sqrt(mean((ridge.pred-n.test[,1])^2))

#엘라스틱넷 오차율 계산(RMSE)
elastic.pred <- predict.cv.glmnet(res.elastic,lambda = res.elastic$lambda.min, newx = n.test[,2:13])
sqrt(mean((elastic.pred-n.test[,1])^2))

#LASSO 회귀식 적용했을 때의 RMSE값이 0.3712852 이고
#RIDGE의 RMSE값은 0.5692921이다.
