## sigmoid function : 0~1 사이연속된 실수값 반환
sigmoid <- function(x){
  1/(1+exp(-x))
}

sigmoid(0)
sigmoid(1)
sigmoid(17)
sigmoid(-1)

# graph
x <- seq(-5,5,0.01)
plot(x, sigmoid(x), col = "blue")


## step function : 0,1만 반환
step <- function(x){
  ifelse(x > 0, 1, 0)
}

step(3)
step(-3)
step(0)

# graph
x <- seq(-5,5,0.01)
plot(x, step(x), col = "red")


## Relu : 입력이 0을 넘으면 그 값을 리턴, 0 이하면 0을 리턴
Relu <- function(x){
  ifelse(x > 0, x, 0)
}

Relu(-3)
Relu(2)

# graph
x <- seq(-5,5,0.01)
plot(x, Relu(x), col = "orange")


## 콘크리트 데이터 분석

# cement : 콘크리트의 총량, 입방미터당 킬로미터
# slag : 시멘트
# ash : 회분
# water : 물
# superplastic : 고성능 감수재(콘크리트 강도를 높이는 첨가제)
# coarseagg : 굵은 자갈
# fineagg : 작은 자갈
# age : 숙성시간
# strength : 압축내구력

# step.1 : csv 파일 불러오기
concrete <- read.csv("/Users/hbk/Downloads/concrete.csv")  # data.frame
str(concrete)

# step.2 : 정규화 작업(단, 최대값-최소값 활용)
# 정규화 함수
normalize <- function(x){
  (x-min(x))/((max(x)-min(x)))
}

# 데이터프레임에 정규화 적용
concrete_norm <- as.data.frame(lapply(concrete, normalize))
concrete_norm

# 데이터 분석비교
summary(concrete$strength) # 원본
summary(concrete_norm$strength) # 정규화 : 0 ~ 1 사이의 범위값


# step.3 : 훈련데이터셋, 테스트데이터셋 생성
# 그냥 자르기 : 별로 좋지 않다.(데이터 분포를 고르게 해줄 필요가 있다)
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

# 랜덤시드 생성
set.seed(12345) # 난수발생시 같은 난수로 고정하기 위해 설정해 주는 것이고, 임의의 수를 시드값으로 부른다. 
concrete_ran <- concrete_norm[order(runif(1030)), ] # runif(num,min,max) : 난수발생(갯수 : num, 범위 : min~max), order : 크기순으로 나열했을 경우의 인덱스값 
concrete_train <- concrete_ran[1:826, ] # 트레이닝셋 80%
concrete_test <- concrete_ran[827:1030, ] # 테스트셋 20%

# 랜덤시드 생성
set.seed(12345)
#install.packages('ddalpha')
#install.packages("caret")
#install.packages("robustbase")
library(robustbase)
library(caret)

trainIndex = createDataPartition(concrete_norm$strength, p = 0.80,list=FALSE)
concrete_train <- concrete_norm[trainIndex, ] # 트레이닝셋 80%
concrete_test <- concrete_norm[-trainIndex, ] # 테스트셋 20%


# step.4 : 신경망 패키지 다운 및 훈련
install.packages("neuralnet")
library(neuralnet)

concrete_model <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)  # 학습
plot(concrete_model)

# step.5 : 테스트데이터셋 예측값 확인
# model 결과값
model_result <- compute(concrete_model, concrete_test[1:8])

# 강도값 예측
predicted_strength <- model_result$net.result

# 예측값과 실제값간의 상관관계를 확인
cor(predicted_strength, concrete_test$strength)  # 0.8064372857

# step.6 : model 개선(은닉층 추가)
concrete_model2 <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
                             data = concrete_train,
                             hidden = c(5,2))  # 은닉층 2개 생성(노드:5,2)
plot(concrete_model2)

model_result2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_result2$net.result
cor(predicted_strength2, concrete_test$strength)  # 0.9368218398







