## R-34일차(2018.2.12)
  # 34-1. 결정트리
  # 34-2. information.gain()
  # 34-3. C5.0()

"인생은 선택의 연속

ex) 회사 선택기준 출퇴근 거리?, 연봉?, 복지?, 복장규정?, 회사의 모토?, 근무시간?, 성비?, 구내식당 밥맛?
  
  데이터 마이닝"

## 34-1. 결정트리(분류)
# - 학습 데이터를 가지고 트리구조의 학습모델을 만들어 새로운 테스트 데이터에 라벨을 예측하는 알고리즘

"ex) 신용등급 모델
Q.직장 있나요?, 집(부동산) 있나요? 등등 기준들 

ex) 회사채용

ex) 변심이 심한 고객이나 고객만족을 관리하는 부서와 광고부서에서 공유
되어야하는 시장조사

ex) 연구측정, 증상, 매우 드문 질병 진행과정을 바탕으로 한 질병진찰"

# - 의사결정 나무 : 데이터 마이닝 분석의 대표적인 분석 방법 

# ★ 엔트로피(entropy)
# : 불확실성을 수치로 나타낸다.
# : 엔트로피의 결과값을 bit로 표현한다.(정보의 기대값)

# entropy(s) = - ∑ p log2(p) (p : 확률값)

"ex)
{빨강,파랑,빨강,파랑,빨강} <- 구슬들이 무질서하게 있다

entropy(s) [0,1] / 0 : 질서, 1 : 무질서
분활하지 않았을 때 엔트로피 : -(3/5)*log2(3/5)-(2/5)*log2(2/5)

{빨강 빨강 빨강} {파랑 파랑}
엔트로피 = 0 -> 무질서 사라짐 : -(3/3)*log2(3/3)-(2/2)*log2(2/2)"


#[문제227] 엔트로피를 구하는 함수를 만드세요

x <- c("red","blue","red","blue","red")

info_entropy <- function(x){
  v <- unique(x)
  e <- 0
  
  for(i in 1:length(v)){
    p <- sum(v[i] == x)/length(x)  # 이렇게 미리 계산하는게 더 빠르다
    e <- sum(e, -(p)*log2(p))
  }
  return(e)
}

info_entropy <- function(x){
  v <- unique(x)
  e <- 0
  
  for(i in v){
    p <- sum(i == x)/length(x) # 확률계산
    e <- sum(e, -(p)*log2(p))  # 누적합
  }
  return(e)
}

info_entropy(x)


#선생님 풀이
info_entropy <- function(x){
  factor_x <- factor(x)
  entropy <- 0
  
  for(str in levels(factor_x)){  # "red", "blue" 
    p <- sum(x == str)/length(x)
    entropy <- entropy - p*log2(p)
  }
  return(entropy)
}

info_entropy(x)


## 쿠폰반응 예측

#1 데이터 조회
skin <- read.csv("c:/r/skin.csv", header = T)
skin <- skin[-1]
str(skin$age)
head(skin)
sum(skin$age <= 35 & skin$marry == 'YES') / nrow(skin)


#2 러닝모델 생성
library(rpart) # 결정트리 만드는 패키지
case.1 : 쿠폰반응에 대한 예상 예측할 내용 ~ .(전체) / minsplit : 가지수

tree1 <- rpart(cupon_react~. , data = skin, control = rpart.control(minsplit = 2))
case.2 : 예측할 내용 ~ A + B(A, B 기준)

tree2 <- rpart(cupon_react~marry+age , data = skin, control = rpart.control(minsplit = 2))
plot(tree1, compress = T, uniform = T, margin = 0.1)
text(tree1, use.n = T, col = "blue")
plot(tree2, compress = T, uniform = T, margin = 0.1)
text(tree2, use.n = T, col = "blue")

* factor 각 level 순서대로 좌측에서 우측으로 분리
Q. 결정트리를 만들때 가장 먼저 해야할 일은 무엇인가?
  
  중요한 컬럼(변수)를 찾는 것이다.
정보획득량이 높은 변수
엔트로피 함수를 사용


## 34-2. information.gain() : 정보획득량 구하는 함수
# - 어떤 컬럼을 기준으로 찾아 나갈지를 결정하는데 도움을 준다
# - 정보획득량이 높은 순서대로 찾아나간다.
# - 0 ~ 1 사이가 보편적이나 넘어갈 수도 있다.

install.packages("FSelector")
library(FSelector)
information.gain(cupon_react~., skin)
#분할전 엔트로피를 계산하고 분할후 엔트로피를 계산한 것을 바탕으로 정보획득량 구함 age는 나이대별 분리

install.packages("rattle")
library(rattle)
library(rpart.plot)
fancyRpartPlot(tree1, cex = .7)

#http://www.dodomira.com/2016/07/19/r-%EC%9D%98%EC%82%AC%EA%B2%B0%EC%A0%95%EB%82%98%EB%AC%B4-%EA%B9%94%EB%81%94%ED%95%98%EA%B2%8C-plotting-%ED%95%98%EA%B8%B0-fancyrpartplot-r/
  
"  fancyrpartplot으로 작성된 node에는 아래 3가지 정보가 담겨 있습니다.

A. node의 성질 B. node의 순도 C. node가 전체에서 차지하는 비중

A. NODE의 성질 가장 위의 node 볼까요? 박스 가장 위의 “No”라는 구분자는 해당 node는 “no”(음성)라고 구분될 수 있다는 것을 의미합니다. 같은 원리로 가장 아랫줄의 제일 오른쪽 node(7번)를 보면 “yes”라고 적혀있죠? 이 node에 속하는 관측치는 양성그룹으로 분류되는 것이죠.

B. NODE의 순도 : 색상이 진할수록 순수하다. node의 순도는 node의 색상(진하기) 및 두번째 열의 숫자로 확인가능 ex) 1, 6을 비교해보자. 1은 (.60 .40) 6은 (.50 .50) -> 연두색 진하기 1 > 6

C. NODE가 전체에서 차지하는 비중 : 가장 아랫쪽의 숫자(%)가 의미하는 것은 해당 node가 전체 데이터 셋에서 차지하는 비중
"
rpart.plot(tree1, digits = 4, fallen.leaves = TRUE, type = 2, extra = 101)


## 34-3. C5.0()
# - 결정트리 종류 중 한가지
# - Quinlan의 가장 최근 알고리즘으로 저작권이 걸려있음 

install.packages("C50")
library(C50)

# skin data
skin
str(skin)

# 러닝모델(예측)을 만들자 * tials는 공부횟수로 생각하자
# skin_1 <- C5.0(skin[-6], skin$cupon_react)
skin_1 <- C5.0(skin[-6], skin$cupon_react, trials = 10)  # trials 10번정도 다르게 트리 구성해봐(반복학습)
skin_1
summary(skin_1)


{trials = 1}

Decision tree:
  
  marry = NO: NO (10) marry = YES: :...gender = MAN: :...job = NO: NO (3) : job = YES: YES (3/1) /O <- 에러율 gender = WOMAN: :...age > 20: YES (9/1) age <= 20: :...job = NO: YES (3/1) job = YES: NO (2)

Decision Tree   
----------------  
  Size      Errors  

6    3(10.0%)   <<  분류잘못 됨(원래 NO 18개임)


(a)   (b)    <-classified as
----  ----
  15     3    (a): class NO   
12    (b): class YES
sum(skin$cupon_react == 'NO')
plot(skin_1)
library(gmodels)
skin_pred1 <- predict(skin_1, skin)

# 실제값 예측값 비교 
CrossTable(skin$cupon_react, skin_pred1, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#정확도 100%

#test dataset 생성

test_skin <- data.frame(gender = c('WOMAN','MAN','MAN'),
                        age = c(30,40,40),
                        job = c('NO','YES','NO'),
                        marry = c('NO','YES','NO'),
                        car = c('NO','YES','YES'))
p <- predict(skin_1, test_skin)
p


## mushrooms 분류

#1 데이터 수집

# 독버섯을 구분하는 규칙을 식별하기 위해 카네기 멜로 대학교의 제프슈림머가 만든 버섯 데이터 셋

#1 데이터 준비

mushrooms <- read.csv("c:/r/mushrooms.csv")
str(mushrooms)

#veil_type은 단일값만 가져서 필요없다
mushrooms$veil_type <- NULL

#버섯의 타입 빈도수
table(mushrooms$type)

#2 데이터에 대한 모델 훈련
install.packages("RWeka")
library(RWeka) #java 기반

#OneR() : 하나의 사실만 가지고 간단하게 분류하는 알고리즘
mushroom_1R <- OneR(type ~. , data = mushrooms)
mushroom_1R
# ~. : 정보획득량이 가장높은 하나의 컬럼으로 선정해서 결과가 나옴
# -> 잘 못 될수도 있다

# 변수는 odor

#3 분류 정확도 : (8004/8124 instances correct)
8004/8124
summary(mushroom_1R)
#120개 독성인데 식용으로 분류됨 -> 1개의 비교결과의 문제가 있을 수 있다.

#4 모델 성능 개선

#RIPPER 규칙 학습 알고리즘 java기반 JRip() 함수
mushroom_JRip <- JRip(type~. , data = mushrooms)
mushroom_JRip  # 학습모델
#Number of Rules : 9 (9개의 규칙을 바탕으로 분류를 하였다)

#IF ELSE 기반 알고리즘이다

#8개까지 독성에 대한 조건, 마지막 식용에 대한 건수

res <- predict(mushroom_JRip, mushrooms[-1])
CrossTable(mushrooms[,1], res)

#{번외 연구}

head(mushrooms)
mush <- rpart(type~., data = mushrooms, control = rpart.control(minsplit = 2))
summary(mush)

library(doBy)
orderBy(~-attr_importance, information.gain(type~., mushrooms))
plot(mush, compress = T, uniform = T, margin = 0.1)
text(mush, use.n = T, col = "blue")
fancyRpartPlot(mush)

mush1 <- C5.0(mushrooms[-1], mushrooms$type, trials = 3)
summary(mush1)
plot(mush1)

library(rvest)
library(dplyr)
n_urll <- html_nodes(read_html('http://www.bandaimall.co.kr/premium/index.do'), 
                     xpath ="//*[@id='body']/div[4]/div/div[2]/div[2]/div/a/div/span[2]") %>% html_text()

ifelse(
  length(n_urll) >= 0, 
  paste0("현재", length(n_urll), "개의 클럽G가 있습니다."),
  "등록된 클럽G가 없습니다.")

class(n_urll)

ifelse(format(Sys.Date(),format='%Y/%m/%d')==substr(n_urll,6,15), "오늘 등록된 클럽G 입니다.", "오늘 등록된 클럽G가 아닙니다.")


#참고자료 링크

★ 의사결정나무(Decision Tree) 

https://ratsgo.github.io/machine%20learning/2017/03/26/tree/
  
http://www.dodomira.com/2016/05/29/564/
  
https://ko.wikipedia.org/wiki/%EC%9D%B8%EA%B3%B5%EC%8B%A0%EA%B2%BD%EB%A7%9D

https://ko.wikipedia.org/wiki/%EA%B2%B0%EC%A0%95_%ED%8A%B8%EB%A6%AC_%ED%95%99%EC%8A%B5%EB%B2%95

http://www.aistudy.co.kr/control/information_theory.htm

https://ko.wikipedia.org/wiki/%EC%A0%95%EB%B3%B4_%EC%97%94%ED%8A%B8%EB%A1%9C%ED%94%BC

https://ko.wikipedia.org/wiki/%EC%97%94%ED%8A%B8%EB%A1%9C%ED%94%BC

http://gentlej90.tistory.com/91