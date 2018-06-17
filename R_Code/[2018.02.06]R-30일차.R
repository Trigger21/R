## R-30일차(2018.2.6)

  # 30-1. kNN
  # 30-2. 정규화
  # 30-3. 표준화[scale()]


## 30-1. kNN 최근접 이웃 알고리즘
# - 거리 유사도를 기준(유클리드 거리(Euclidean Distance))
# - 유클리드 거리
#  예) a 지점과 b 지점 사이의 거리
#  : 자로 두점을 연결해서 일직선 거리를 측정한다.

# 일직선  |a-b|
# 평면    sqrt((a-b)^2) 
"
재료  단맛  아삭한맛  음식종류
포도   8        5       과일
콩     3        7       채소
견과   3        6       단백질
오렌지 7        3       과일

토마토 6        4        ??
"

#거리
sqrt((6-8)^2 + (4-5)^2)
sqrt((6-3)^2 + (4-7)^2)
sqrt((6-3)^2 + (4-6)^2)
sqrt((6-7)^2 + (4-3)^2)
dist(rbind(c(6,4),c(8,5)))
dist(rbind(c(6,4),c(3,7)), method = "euclidean")
dist(rbind(c(6,4),c(3,6)), method = "euclidean")
dist(rbind(c(6,4),c(7,3)), method = "euclidean")

#case : k = 1 토마토는 오렌지와 제일 가깝기 때문에 과일로 분류

#case : k = 3 오렌지, 포도, 견과 다수결로 정한다. 과일:단백질 = 2:1 -> 과일로 분류
"
k값이 크면 노이지 데이터의 변화량은 줄어들기는 하지만 패턴을 무시하는 위험을 갖는 
학습기로 편향된 자료가 나올수 있다.
하나의 최근접 이웃을 사용한다면 노이지데이터나 이상치 영향을 받을 수 있다.
k 값은 3 이상을 하는게 보편적이다. training dataset 제곱근을 이용한다. 
sqrt(16) = 4 (짝수 보다는 홀수가 좋다)
"

#[문제213] 벡터에 있는 값을 0, 1사이의 범위 값으로 변환하세요.
"
x1 <- c(1,2,3,4,5) 
x2 <- c(10,20,30,40,50)
"

x1 <- c(1,2,3,4,5)
x2 <- c(10,20,30,40,50)
x1/100
x2/100


## 30-2. 정규화
# - 모든 속성의 값을 0과 1 사이의 범위값으로 변환한다
# - 다수 항목에 대해서 값이 상호다름으로 모든 값들에 대해서
#   동일한 범위로 표현하기 위해서 정규화 한다. 
# - (X-min(X))/(max(X)-min(X))  

(x1-min(x1)) / (max(x1)-min(x1))

normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}
normalize(x1)
normalize(x2)
x3 <- sample(100:999, 5)
normalize(x3)


## 30-3. 표준점수(standard score, 표준값, z값, z점수)
"z점수 표준화 : z점수는 범위가 정해지지 않은 양수, 음수가 된다. 
극단적으로 값이 중앙쪽으로 모이지 않기 때문에 악성조직을 가지고 있는 조직은 
매우 크게 자라기 때문에 일부 매우 극단적인 이상치를 볼 수 있다. 
거리 계산에서 이상치에 더 큰 가중치를 부여하여 계산된 값을 볼 수 있다."

# - Z = (X - mean(X)) / sd(X)
(x2-mean(x2))/sd(x2)
scale()
scale(x2)
class(scale(x2))


#[문제_214] 나이, 월수입, 상품구매여부 3개의 데이터를 갖는 데이터가 있다. 
#           이 데이터를 이용해서 나이가 44 이고 월급이 400 만원인 사람이 
#           상품을 구매할지 비구매할지를 knn 분류 알고리즘으로 분석하세요.

buy <- read.csv("c:/r/buy.csv", header = T, stringsAsFactors = F)
buy

test <- data.frame(나이 = 44, 월수입 = 400, 상품구매여부 = NA)
test

library(class)  # kNN() 

#그냥 kNN
knn(buy[,-3], test[-3], buy[,3], k = 5, prob = T)
buy1 <- rbind(buy, test)
buy1


#정규화
knn(normalize(buy1[,-3])[-22,], normalize(buy1[,-3])[22,], buy1[,3][-22], k = 5, prob = T)
#text 데이터 정규화 방법 (text - min(buy)) / (max(buy)-min(buy))
#정규화는 이상치 데이터가 그 성질이 사라질 수 도 있다.
(test[-3]-min(buy[-3]))/(max(buy[-3])-min(buy[-3]))

#표준화
knn(scale(buy1[,-3])[-22,], scale(buy1[,-3])[22,], buy1[,3][-22], k = 5, prob = T)
#text 데이터 표준화 방법 (text - mean(buy)) / sd(buy)

library(ggplot2)
ggplot(buy, aes(x = 나이, y = 월수입))+
  geom_point(aes(colour = 상품구매여부, pch = 상품구매여부), size = 3)+
  geom_point(data = test, aes(x = 나이, y = 월수입), pch = 0, size = 3)

# 정규화
buy_n <- cbind(normalize(buy[,-3]), 상품구매여부 = buy[,3])
buy_n

ggplot(buy_n, aes(x = 나이, y = 월수입))+
  geom_point(aes(pch = 상품구매여부, colour = 상품구매여부), size = 3)

# 표준화
buy_z <- cbind(as.data.frame(scale(buy[,-3])), 상품구매여부 = buy[,3])
buy_z
test_z <- test
test_z$나이 <- (test$나이-mean(buy$나이))/sd(buy$나이)
test_z$월수입 <- (test$월수입-mean(buy$월수입))/sd(buy$월수입)
test_z
ggplot(buy_z, aes(x = 나이, y = 월수입))+
  geom_point(aes(pch = 상품구매여부, colour = 상품구매여부), size = 3)+
  geom_point(data = test_z, aes(x = 나이, y = 월수입), size = 3, pch = 0)


#[문제215] zoo.csv 데이터 집합은 동물의 특징과 부류 정보가 있습니다. 
#          특정 데이터 동물 정보가 어느 부류에 속하는 지를 knn 알고리즘을 이용해서 분석하세요.
"
[변수 정보]

animal name: Unique for each instance
hair	Boolean
feathers	Boolean
eggs	Boolean
milk	Boolean
airborne	Boolean
aquatic	Boolean
predator	Boolean
toothed	Boolean
backbone	Boolean
breathes	Boolean
venomous	Boolean
fins	Boolean
legs	Numeric (set of values: {0,2,4,5,6,8})
tail	Boolean
domestic	Boolean
catsize	Boolean
type	Numeric (integer values in range [1,7])
[18. type] 1 : 포유류 2 : 조류 3 : 파충류 4 : 어류 5 : 양서류 6 : 곤충 7 : 갑각류
"

zoo <- read.csv("c:/r/zoo.csv", header = T, stringsAsFactors = F)
names(zoo) <- c("name","hair","feathers","eggs","milk","airborne",
                "aquatic","predator","toothed","backbone","breathes",
                "venomous","fins","legs","tail","domestic","catsize","type")
zoo

knn(zoo[-nrow(zoo),-c(1,ncol(zoo))], zoo[nrow(zoo),-c(1,ncol(zoo))], zoo$type[-nrow(zoo)], k = 9, prob = T)
zoo1 <- zoo
zoo1$legs <- normalize(zoo1$legs)
zoo1
train <- zoo1[-nrow(zoo1),-c(1,ncol(zoo1))]
test <- zoo1[nrow(zoo1),-c(1,ncol(zoo1))]
knn(train, test, zoo1$type[-nrow(zoo)], k = 9, prob = T)
ggplot(train, aes(x = legs, y = type))+
  geom_point(data = zoo1, aes(colour = type))


#[문제216] 유방암 데이터 악성과 양성분류입니다.

http://archive.ics.uci.edu/ml/datasets.html

#1 단계 : 데이터 수집
위스콘신대학의 연구원들의 자료
유방 종양의 미세침 흡인물 디지털 이미지에서 측정한 값 이며 이 값은 디지털 이미지에 나타난 세포 핵의 특징이다.
암조직 검사에 대한 관측값은 569개, 변수(속성) 32
식별숫자, 암진단 여부(악성(Malignant),양성(Benign)), 30개 수치 측정치 : 양성은 안 퍼진 것, 악성은 퍼진 것
세포핵의 모양과 크기관련된 10개 특성
radius(반지름)

texture(질감)

perimeter(둘레)

area(넓이)

smoothness(매끄러움)

compactness(조밀성)

concavity(오목함)

concave points(오목점)

symmetry(대칭성)

fractal dimension(프랙탈 차원)

log

#2 단계 : 데이터 준비
#2-1. wisc_bc_data.csv 파일을 wbcd 변수에 임포트하세요.
wbcd <- read.csv("c:/r/wisc_bc_data.csv", header = T, stringsAsFactors = F)
wbcd
library(doBy)
orderBy(~radius_mean, wbcd)

ggplot(wbcd, aes(x = diagnosis, y = radius_mean))+
  geom_boxplot(aes(fill = diagnosis), alpha = .6)+
  geom_jitter(width = .35)+
  text()
fivenum(wbcd$radius_mean)
IQR(wbcd$radius_mean)

#2-2. wbcd 데이터 프레임의 구조 확인하세요.
str(wbcd)

#2-3. id 속성 제거 하세요.
wbcd <- wbcd[-1];wbcd

#2-4. diagnosis 변수에 빈도수를 확인하세요.
table(wbcd$diagnosis)

#2-5. factor형으로 diagnosis값을 변환하세요. B -> Benign(양성) , M -> Malignant(악성)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
wbcd

#levels : 원래값, labels : 바꿀값 
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")) 
head(wbcd)

#2-6. diagnosis 변수 비율을 구하세요.
table(wbcd$diagnosis)/sum(table(wbcd$diagnosis))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#2-7. radius_mean, area_mean, smoothness_mean 변수에 대한 요약을 하세요.
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#2-8. wbcd 데이터 정규화하세요.
# normalize() : 정규화 함수 
wbcd_n <- wbcd

for(i in 2:ncol(wbcd_n)){
  wbcd_n[,i] <- normalize(wbcd_n[,i])
}

wbcd_n

head(wbcd)
head(wbcd_n)
summary(wbcd_n$area_mean)
summary(wbcd_n$concavity_worst)
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

normalize(c(1, 2, 3, 4, 5)) normalize(c(10, 20, 30, 40, 50))

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

head(wbcd) head(wbcd_n)

summary(wbcd_n$area_mean) summary(wbcd_n$concavity_worst)

#2-9. 훈련 데이터(1~469)와 테스트 데이터(470~569) 생성하세요.
train <- wbcd_n[1:469,]
test <- wbcd_n[470:569,]
wbcd_train <- wbcd_n[1:469, ] wbcd_test <- wbcd_n[470:569, ]

#2-10. 훈련 데이터와 테스트 데이터에 대한 라벨 생성
train_labels <- train[,1]
test_labels <- test[,1]
wbcd_train_labels <- wbcd[1:469, 1] wbcd_test_labels <- wbcd[470:569, 1]

#3단계 : 데이터로 모델 훈련
# kNN 알고리즘 결과 / 실제 결과 비교(98%)
knn(train[,-1], test[,-1], train_labels, k = 21) == test[,1]
test_pred <- knn(train[,-1], test[,-1], train_labels, k = 21)
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)

#4 단계 : 모델 성능 평가
# CrossTable() 
install.packages("gmodels")
library(gmodels)

#4-1 예측값과 실제값의 교차표 생성
# test_labels : 관찰값
# test_pred : 기댓값 
CrossTable(x = test_labels, y = test_pred, prop.chisq = T) # prop.chisq = F : 카이제곱 표시 X
"
카이제곱검정은 카이제곱 분포에 기초한 통계적 방법으로, 
관찰된 빈도가 기대되는 빈도와 의미있게 다른지의 여부를 
검증하기 위해 사용되는 검증방법이다. 
자료가 빈도로 주어졌을 때, 특히 명목척도 자료의 분석에 이용된다.

카이제곱 값 : χ2 = Σ (관측값 - 기댓값)2 / 기댓값
"
table(wbcd[470:569,1])

(63-61)^2/61

#5 단계 : 모델 성능 향상
wbcd_z <- as.data.frame(scale(wbcd[-1]));wbcd_z
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = train_labels, k=21)
CrossTable(x = test_labels, y = wbcd_test_pred, prop.chisq=FALSE)


#[문제217] 12번째 환자의 초음파 결과를 보고 종양이 양성인지 악성인지를 분석해 주세요.

p.12 <- read.csv("c:/r/patient12.csv", header = T, stringsAsFactors = F);p.12
knn(wbcd[,-1], p.12[,-c(1,2)], wbcd[,1], k = 23)
p.13 <- p.12[-c(1,2)]

for(i in 1:ncol(p.13)){
  p.13[,i] <- (p.13[,i] - mean(wbcd[,i+1]))/sd(wbcd[,i+1])
}

summary(p.13)
knn(wbcd_z[,-1], p.12[,-c(1,2)], wbcd[,1], k = 23)


## wine 분석

#1 단계 : 데이터 수집
wbcd <- read.csv("c:/r/wisc_bc_data.csv", header = T, stringsAsFactors = F)
wbcd <- wbcd[-1]

#2 단계 : 데이터 준비
str(wbcd)
wbcd_12 <- read.csv("c:/r/patient12.csv", header = T, stringsAsFactors = F)
wbcd_12 <- wbcd_12[-1]
wbcd_12

#2-4. diagnosis 변수에 빈도수를 확인하세요.
table(wbcd$diagnosis)

#2-5. factor형으로 diagnosis값을 변환하세요. B -> Benign(양성) , M -> Malignant(악성)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))
head(wbcd)
levels : 원래값, labels : 바꿀값 wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")) head(wbcd)

#2-6. diagnosis 변수 비율을 구하세요.
round(prop.table(table(wbcd$diagnosis))*100, 1)
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#2-7. radius_mean, area_mean, smoothness_mean 변수에 대한 요약을 하세요.
summary(wbcd[,c("radius_mean","area_mean","smoothness_mean")])

#2-8. wbcd 데이터 정규화하세요.(lapply는 컬럼별 함수처리 가능)
#정규화 : (X - min(X))/(max(X)-min(X))

normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

wbcd_n <- as.data.frame(lapply(wbcd[-1], normalize))
wbcd_n

wbcd_12_n <- wbcd_12[-1]
for(i in 2:ncol(wbcd_12)){
  wbcd_12_n[,i-1] <- (wbcd_12[,i]-min(wbcd[,i]))/(max(wbcd[,i])-min(wbcd[,i]))
}

wbcd_12_n

library(scales)
knn(wbcd_n, wbcd_12_n, wbcd[,1], k = sqrt(nrow(wbcd_n)), prob = T)

install.packages("shiny")
library(shiny)
knn(wbcd_z, p.13, wbcd[,1], k = 23)