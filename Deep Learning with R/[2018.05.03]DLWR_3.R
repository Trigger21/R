## 앙상블 연습 credit.csv

credit <- read.csv("/Users/hbk/Documents/빅데이터 과정/R/credit.csv")
str(credit)

# 기존의 알고있던 방법 : C5.0
install.packages("C50")
library(C50)

credit_model <- C5.0(credit[,-17], credit[,17], trials = 10)  # trials 설정 : 10번정도 다르게 트리 구성해봐(반복학습)
p <- predict(credit_model, credit)
table(p, credit$default)


# 앙상블
library(caret)

set.seed(12345)
credit_model <- train(default ~ ., data = credit, method = "C5.0")
credit_model
"C5.0 

1000 samples
16 predictor
2 classes: 'no', 'yes' 

No pre-processing
Resampling: Bootstrapped (25 reps) 
Summary of sample sizes: 1000, 1000, 1000, 1000, 1000, 1000, ... 
Resampling results across tuning parameters:
  
  model  winnow  trials  Accuracy   Kappa    <- ?? 
rules  FALSE    1      0.7018492  0.2929168
rules  FALSE   10      0.7216508  0.3332808
rules  FALSE   20      0.7340632  0.3532980
rules   TRUE    1      0.6990591  0.2847287
rules   TRUE   10      0.7164322  0.3166440
rules   TRUE   20      0.7265068  0.3342625
tree   FALSE    1      0.6956644  0.2685192
tree   FALSE   10      0.7342391  0.3233280
tree   FALSE   20      0.7404113  0.3340499
tree    TRUE    1      0.6973101  0.2656333
tree    TRUE   10      0.7271575  0.3014822
tree    TRUE   20      0.7369981  0.3244327

Accuracy was used to select the optimal model using the largest value.
The final values used for the model were trials = 20, model = tree and winnow = FALSE."

p <- predict(credit_model, credit)
table(p, credit$default)  # 결과가 너무 좋아짐


# bagging(bootstrap aggregating)
# : 트레이닝 데이터를 반복추출하여 표본을 여러개 만든 후에 
#   각 표본에 맞는 분류 모델을 표본 수 만큼 생성한 후에 
#   각각의 분류 모델을 앙상블 하는 방법
# : 분산(표준편차)이 큰 모형에 적합(예측 모형의 변동성이 큰 경우)

install.packages("iprd")
library(ipred)

set.seed(12345)
mybag <- bagging(default ~ ., data = credit, nbagg = 50)  # N개의 결정트리 모델 만듬
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# model1 ~ modelN 생성 


# boosting(부스팅)
# : 약한 학습기들의 성능을 올려서 강한 학습기의 성능을 얻는 방법
# : 전체 데이터에서 여러 샘플링 데이터를 추출하여 순차적으로 이전 학습 분류기의 결과를 
#   토대로 다음 학습 데이터의 샘플 가중치를 조중하면서 학습을 진행
# ex. 모델 1개로 시작해서 샘플 데이터를 거치면서 점점 진화되는 방식

install.packages("adabag")
library(adabag)

set.seed(12345)
model_ada <- boosting(default ~ ., data = credit)
predict_ada <- predict(model_ada, credit)
predict_ada


# randomForest
# : decesion tree, bagging을 결합한 알고리즘
# : 매 실행시 마다 무작위 관측치와 변수를 선택해서 실행 결과가 조금씩 달라지게 한다.

install.packages("randomForest")
library(randomForest)

rf <- randomForest(default ~ ., data = credit)
plot(rf)

tmp <- importance(rf) # 분류할 우선순위(entropy 수치별)
tmp
class(tmp)  # matrix

round(tmp[order(-tmp[,1]), 1, drop = F], 2)
varImpPlot(rf)

predict_rf <- predict(rf, credit)
table(predict_rf, credit$default)

help("randomForest")


############ Kappa란 ?? #############
"Cohen Kappa Coefficient(카파상관계수)
 : 카테고리 정보에 대한 2명의 평가자의 일치도 측정하는 통계적 지표를 의미한다.
   이 지표는 0(완전불일치)부터 1(완전일치) 사이의 값을 가지게 된다.
         Pr(a) - Pr(e)
    k = ---------------
           1 - Pr(e)
    Pr(a) : 데이터에서 관찰된 2명의 평가자들의 일치 확률
    Pr(e) : 2명 평가자들이 데이터로 부터 계산된 확률적으로 일치할 확률
    
시험을 응시한 학생이 10명이라 할 때 2명의 평가자가 합격, 불합격을 판정했다.
                         평가자 A
                   합격            불합격
         합격       4                1
평가자B  불합격     2                3

평가자 A와 B는 모두 4명을 합격을 3명은 불합격을 주었다. 
Pr(a) 는 2명의 평가자들의 일치 확률이므로, 
            4 + 3 
  Pr(a) = --------- = 7/10 = 0.7 이다.
             10
Pr(e)를 계산하기 위해서는 평가자 A와 평가자 B의 각각 합격, 불합격을 줄 확률을 구한다.

  - 평가자 A는 합격을 6명, 불합격을 4명을 주었다.
    평가자 A는 합격확률은 0.6(60%), 불합격은 0.4(40%)
  - 똑같은 논리로 평가자 B는 합격확률은 0.5(50%), 불합격은 0.5(50%)

평가자 A와 B 둘 모두가 확률적으로 합격을 줄 확률은 0.6 * 0.5 = 0.3(30%), 불합격을 줄 확률은 0.4 * 0.5 = 0.2(20%)
Pr(e)는 위 정의에 의해, 0.3 + 0.2 = 0.5(50%)

     0.7 - 0.5
k = ----------- = 0.4
      1 - 0.5
따라서 카파상관계수를 각각 값을 대입하여 계산하면 0.4가 된다.

전문가 한명이 시스템 성능 평가를 위해 만들어 놓은 정답이 신뢰성이 없다면 
이 정답을 사용하여 평가한 시스템 결과는 믿음직스럽지 못할 것이다.
만약 다른 전문가 한명이 이 시스템 성능 평가를 위해 만든 정답과
기존의 전문가의 평가가 유사하다면 즉, 카파상관계수가 높다면 이 시스템 평가 결과는 믿을만 하게 되는 것이다.
"

a <- c(1,0,0,1,1,1,0,0,0,1)
b <- c(1,0,0,1,1,0,1,1,0,1)

table(b,a)
xtabs(~b+a)

# Def of Pr(a) : 2명의 평가자들의 일치확률
Pr_a = 0.7

# Def of Pr(e) : 2명 평가자들이 데이터로 부터 계산된 확률적으로 일치할 확률
# a : 0
0.5
# a : 1
0.5

# b : 0
0.4
#b : 1
0.6

Pr_e = 0.5*0.4 + 0.5*0.6
Pr_e

k = (Pr_a - Pr_e) / (1 - Pr_e)
k # 0.4

table(a) # a vector 빈도수
table(b) # b vector 빈도수
table(c(a,b))


mat = prop.table(xtabs(~b+a)) # 비율값으로 리턴
mat[1,1]+mat[2,2]
mat
sum(a == b) / NROW(a)

sum(diag(mat))


library(caret)
confusionMatrix(ordered(a),ordered(b))
confusionMatrix(as.factor(a),as.factor(b)) # 빈도수, kappa 등 한번에 다보여줌


