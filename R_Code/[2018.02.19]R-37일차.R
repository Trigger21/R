## R-37일차(2018.2.19)

  # 37-1. 상관분석
    # -1-1. 공분산
    # -1-2. 상관계수
  # 37-2. 회귀분석
    # -2-1. 최소제곱 추정법
    # -2-2. lm()

## 37-1. 상관분석
# - 두 변수 사이의 관련성을 파악하는 방법 

# 37-1-1. 공분산(Covariance)
# - 두 변수가 얼마나 함께 변화는지를 측정

# - 한 변수가 커질때 다른 변수가 함께 커지거나
#   한 변수가 작아질때 다른 변수가 함께 작아지는 경우는
#   변화의 방향이 같다. (공분산의 양의 값)
#   ex. 키~체중

# - 한 변수가 커질때 다른 변수는 작아지거나
#   한 변수가 작아질때 다른 변수가 커지는 경우는
#   변화의 방향이 다르다는 의미. (공분산의 음의 값)
#   ex. 흡연~기대수명, 수면시간~학업성취도

# - 두 변수의 값이 서로 상관없이 움직일 경우 공분산은 0


           ∑ (x - mean(x))*(y - mean(y))
cov(x,y) = -----------------------------
              N(조합을 이루는 개수)-1

x <- c(184,170, 180)
y <- c(85, 70, 82)
cov(x,y)
sum((x-mean(x))*(y-mean(y)))/2
#57은 너무 크고 상관정도를 파악하기 제한되면 상관계수(공분산 표준화)를 구한다

# 37-1-2. 상관계수
# - 공분산의 수치만 보아서는 상관의 정도를 알 수 없다.
#   키와 체중에서 cm, kg 단위를 변경하면 수치가 달라진다.
#   이래서 공분산을 표준화한 값

# - 계산방법 : 피언슨 상관계수, 스피어만 상관계수, 겐달 순위상관계수

# - 상관계수값이 크면 데이터간의 관계가 존재한다는 의미

# - 한쪽 값이 커질때 다른쪽 값이 커지는 정도가 크다

# - 상관계수는 -1 ≤ r ≤ 1

              cov(x,y)
cor(x,y) = ----------------
            sd(x) * sd(y)
?cor
cor(x,y)
#상관정도가 크다


## 37-2. 회귀분석
# - 회귀분석은 하나의 변수가 나머지 다른 변수들과의 선형적(1차방정식) 관계를
#   갖는가의 여부를 분석하는 방법으로 하너의 종속변수(반응변수, 기대값, 예상값)와
#   독립변수(설명변수, 입력값) 사이의 관계를 명시하는 것

# - 신경망 Python 하면서 할 예정(다음주부터 Python)

# - 독립변수 : 종속변수에 영향을 주는 변수

# - 종속변수 : 서로 관계를 가지고 있는 변수들 중에서 다른 변수에 영향을 받는 변수를 종속변수

# ex. 키가 클수록 몸무게 변화를 분석하는 목적
#      |             |
#    독립          종속

# 독립변수와 종속변수간의 관계를 직선식으로 나타내면 

# y = α + βx (회귀식), α : y절편, β : 기울기


# 37-2-1. 최소제곱 추정법
# - 최적의 α와 β를 결정하기 위해 정규최소제곱으로 알려진 추정기법을 사용한다.

# ∑ (관찰y-예측y)² <- 이 식의 최소값 구하자

# - 직선과 데이터와의 차이가 최소화되는 직선 방정식을 구하는게 목표

     ∑ (x-mean(x))*(y-mean(y))     cov(x,y)
β = -------------------------- = -------------
         ∑ (x-mean(x))²             var(x)


α = mean(y) - β*mean(x) 


#[문제230] 키 185일때 몸무게는?
  
height <- c(184,170,180,175,165,179,172,171,178,182)
weight <- c(85,70,82,73,67,75,70,68,71,80)

plot(height, weight)

#상관계수 판단
cor(height, weight)  # 0.8931362

#최소제곱법
b <- cov(height, weight)/var(height)
a <- mean(weight)-b*mean(height)

# 회귀직선 
y <- a+b*height
y

#키 185일때 몸무게는?
a+b*185  # 약 82.7kg 

#그래프로 확인
plot(height, weight)
par(new = TRUE)
lines(height, y, col = "blue")


# 37-2-2. lm() : linear models
m <- lm(weight~height)  # 종속 ~ 독립
m

" 절편            기울기
(Intercept)       height
 88.0502          0.9234"

#예측값 확인
predict(m, data.frame(height=185))  # predict(model, 인자값(data.frame))
lm


#[문제231] 사료의 탄닌 함유량과 애벌래의 성장추이가 상관관계가 있는지 확인하세요.

tannin <- read.csv("c:/r/tannin.csv", header = T)
tannin

#상관계수 확인
cor(tannin$tannin, tannin$growth)  # -0.9031408
#-> 반비례로 상관관계가 높다

m <- lm(tannin$growth~tannin$tannin)
n <- predict(m, as.data.frame(tannin[,2]))

#그래프 그리기
library(ggplot2)
ggplot(tannin, aes(x = tannin, y = growth))+
  geom_point(pch = 1, size = 2, col = "darkblue")+
  geom_line(aes(x = tannin, y = n), col = "red")+
  ggtitle("타닌 섭취량에 따른 애벌래 성장추이 관계")


#선생님 풀이
reg <- read.csv("c:/r/tannin.csv", header = T) 
attach(reg) 
# data.frame 열 사용 편리? 
cor(growth, tannin) 
model <- lm(growth ~ tannin) 
plot(tannin, growth, pch = 21, col = "blue", bg = "blue") 
abline(model, col = "red")

yhat <- predict(model)

for(i in 1:9){ lines(c(tannin[i], tannin[i]), c(growth[i], yhat[i]), col = "black", coef(model) 
# 회귀계수 fitted(model) 
# 적합된 값 predict(model, data.frame(tannin = 1)) }
                     
                     
coef(m)
fitted(m)
predict(m)
                     
#숲에서 비릿하는 냄새가 타닌의 냄새 사람에게 필요하다(오전 10~12시 나옴)
                     
#[문제232] 어느 실험실에서 10시간, 20시간, 30시간, 40시간 마다 물질의 방사능 수치를 측정한 자료가 있을 때, 
#          35시간에 물질의 방사능 수치는?
시간 | 10 20 30 40
방사능수치| 300 250 200 150
                     
radAct <- data.frame(시간 = c(10,20,30,40), 방사능수치 = c(300,250,200,150))
radAct
attach(radAct)
                     
cor(시간, 방사능수치)  # -1
model <- lm(방사능수치 ~ 시간)
                     
predict(model, data.frame(시간 = 35))  # 175
#-> 예상 방사능수치 : 175
                     
                     
#[문제233] 코스피 지수 수익율의 변동에 따라 삼성전자 주식의 수익율의 변동이 있는지 시각화해서 확인해보세요.
                     
#H_stock : 현대자동차 
#S_stock : 삼성전자 
#K_index : 코스피 지수

K_index <- read.csv("c:/r/K_index.csv", header = T)
S_stock <- read.csv("c:/r/S_stock.csv", header = T)
                     
K_index
S_stock
                     
KS <- merge(K_index, S_stock)
KS
                     
attach(KS)
cor(k_rate[-1], s_rate[-1])  # 0.7684845 만큼의 관련성 확인 
                     
s_model <- lm(s_rate ~ k_rate)
s_pre <- predict(s_model, as.data.frame(k_rate))

#ggplot
ggplot(KS, aes(x = k_rate, y = s_rate))+
  geom_point()+
  geom_line(aes(x = k_rate, y = s_pre), col = "red")+
  ggtitle("코스피 지수 수익율 증가에 따른 삼성전자 주식의 수익율 변화")
                     
#plot
plot(k_rate, s_rate)
abline(s_model, col = "red")
                     

#[문제234] 코스피 지수의 수익율 등락비율과 현대 자동차의 주식 수익율의 plot 그래프와 회귀직선식을 
#          그려서 삼성과 비교하세요. KS : 코스피 지수 + 삼성전자 H_stock : 현대자동차
                     
H_stock <- read.csv("c:/r/H_stock.csv", header = T)
KSH <- merge(KS, H_stock)  # 코스피 지수 + 삼성전자 + 현대자동차
attach(KSH)

cor(k_rate[-1], h_rate[-1])  # 0.3262777 만큼의 관련성
h_model <- lm(h_rate ~ k_rate)
h_pre <- predict(h_model, as.data.frame(k_rate))

#ggplot
ggplot(KSH)+
  geom_point(aes(x = k_rate, y = h_rate, col = "현대"))+
  geom_point(aes(x = k_rate, y = s_rate, col = "삼성"))+
  geom_line(aes(x = k_rate, y = h_pre, col = "현대"))+
  geom_line(aes(x = k_rate, y = s_pre, col = "삼성"))+
  labs(title = "코스피 지수 수익률에 따른 삼성과 현대 주식 수익률 비교", 
       x = "코스피 지수 수익률", y = "주식 수익률", colour = "범례")
                     
#plot
plot(k_rate, h_rate, ylim = c(-7,7), xlab = "코스피 지수 수익률", ylab = "주식수익률", col = "orange", pch = 20)
abline(h_model, col = "red", lwd = 2)
points(k_rate, s_rate, col = "blue", pch = 20)
abline(s_model, col = "purple", lwd = 2)
legend("topleft", legend = c("현대","삼성"), 
        fill = c("orange","blue"), lty = 1, col = c("red","purple"))
                     
#동시에 비교
graphics.off()
par(mfrow = c(1,2))
plot(k_rate, h_rate, col = "blue", main = "현대자동차", ylim = c(-7,7), pch = 20)
abline(h_model, col = "red", lwd = 2)
plot(k_rate, s_rate, col = "blue", main = "삼성전자", ylim = c(-7,7), pch = 20)
abline(s_model, col = "red", lwd = 2)
                     

#[문제235] 1986년 우주왕복선 챌린저호가 발사된지 73초만에 폭발하여 대서양에 추락하고 7명 승무원이 
#          전원 사망한 사고는 미국우주사의 최대 비극중 하나다. 폭발의 원인은 오른쪽 고체연료 부스터의 
#          부품인 O링이 망가졌기 때문이다. O링이 셔틀 출발시처럼 낮은 온도에서 작동하도록 설계되지 않았던것이다.
#          발사 온도에 대한 O링의 파손이 원인을 이용해서 만약 온도가 31도라고 추정하면 
#          O링의 파손수는 몇개가 될거라 예상되는가?
                       
#참고자료(오링 규격 O-Ring Standard Dimensions) : http://www.anyseal.co.kr/oring/oring_16_5.asp
                     
challenger <- read.csv("c:/r/challenger.csv", header = T)
challenger
                     
attach(challenger)
plot(temperature, distress_ct)
cor(temperature, distress_ct)  # -0.725671
                     
#온도
O_model <- lm(distress_ct ~ temperature)
predict(O_model, data.frame(temperature = 31))  # 2.520317 : 2~3개 망가짐을 예측
                     
#압력
O_model <- lm(distress_ct ~ pressure)
predict(O_model, data.frame(pressure = 200))  # 0.3906582 : 0개 망가짐을 예측
                     
#온도 + 압력
O_model <- lm(distress_ct ~ temperature+pressure)
predict(O_model, data.frame(temperature = 31, pressure = 200))  # 2.648453  : 2~3개 망가짐을 예측
                     
# -> 결론 : O-ring의 파손은 온도에 영향을 받는다