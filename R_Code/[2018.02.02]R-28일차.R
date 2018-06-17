## R-28일차(2018.2.2)

  # 28-1. 표준화
  # 28-2. cross-table
  # 28-3. 변동계수(Coefficient of variation)
  # 28-4. 백분위수(percentile)
  # 28-5. 사분위수(quartile)
    #: quantile(), summary(), fivenum()
    #: IQR() = Q3 - Q1


#[문제202] 세곳의 채소농원으로부터 10일 동안 측정한 pphm 단위의 오존 농도를 보고 받았다. 오존 농도를 분석하세요.

oz <- read.csv("c:/r/gardens.csv", header = T, stringsAsFactors = F)
rownames(oz) <- c(1:10)
oz

mean_A <- mean(oz$gardenA);mean_A
mean_B <- mean(oz$gardenB);mean_B
mean_C <- mean(oz$gardenC);mean_C

median(oz$gardenA)
median(oz$gardenB)
median(oz$gardenC)

sd_A <- sd(oz$gardenA);sd_A
sd_B <- sd(oz$gardenB);sd_B
sd_C <- sd(oz$gardenC);sd_C

library(ggplot2)
library(reshape2)

install.packages('plotly')
library(plotly)
library(colorspace)
ggplot(melt(oz), aes(x = variable, y = value))+
  geom_boxplot(fill = heat_hcl(3), alpha = .4)+
  geom_jitter(width = .4, aes(colour = variable))
fivenum(oz$gardenC)
plot(oz)
plot(oz$gardenA, dnorm(oz$gardenA, mean_A, sd_A))
plot(oz$gardenB, dnorm(oz$gardenB, mean_B, sd_B))
plot(oz$gardenC, dnorm(oz$gardenC, mean_C, sd_C))

#결론 : gardenA가 평균 오존농도가 제일 낮으며 분포가 밀집되어 있음 gardenB는 
#       gardenA보다 평균 오존농도는 높지만 분포가 밀집되어 있다 gardenC는 
#       gardenB와 평균 오존농도는 같지만 분포가 퍼져 있다(상대적) 
#       즉, 수확물의 품질은 A-B-C순으로 낮아진다고 판단된다.

#선생님 풀이
oz_tbl <- data.frame(Mean = colMeans(oz),
                     Var = c(var(oz$gardenA), var(oz$gardenB), var(oz$gardenC)),
                     Std = c(sd(oz$gardenA), sd(oz$gardenB), sd(oz$gardenC)))
oz_tbl

plot(oz[,1], pch = 16, col = "red", ylim = c(0,15))
abline(h = oz_tbl[1,1], col = "blue")
points(oz[,2], pch = 16, col = "green")
abline(h = oz_tbl[2,1], col = "yellow")
points(oz[,3], pch = 16, col = "black")
abline(h = oz_tbl[3,1], col = "black", lty = 3)
legend(x=1, y=15, legend = names(oz), fill = c("red","green","black"), border = FALSE)
text(3, oz_tbl[1,1], "gardenA 평균", pos = 3)
text(5, oz_tbl[2,1], "gardenB(C) 평균", pos = 3)


#[문제203]A반 B 반의 수학점수를 비교하세요.
"
A반 35,55,60,70,100 
B반 55,60,65,65,75
"

math <- data.frame(A = c(35,55,60,70,100), B = c(55,60,65,65,75))
math
mean(math$A);median(math$A)
mean(math$B);median(math$B)

# A는 절삭평균으로
mean(math[2:4,1])
sd(math$A)
sd(math$B)
plot(math$A, col = "red", pch = 65)
points(math$B, col = "blue", pch = 66)
abline(h = 64, lty = 3, col = "green")
ggplot(melt(math), aes(x = variable, y = value))+
  geom_boxplot(fill = heat_hcl(2), alpha = .3, width = .4)+
  geom_jitter(aes(colour = variable), width = .2)+
  geom_hline(yintercept = 64, lty = 2, col = "red")

#결론 : B반의 수학성적이 더 높다

#[문제204] 성적.csv 파일에 과목별 평균값, 분산값, 행을 추가하시오.

grade <- read.csv("c:/r/성적.csv", header = T, stringsAsFactors = F)
grade

M_df <- data.frame(name = 'Mean', 
                   sql = as.integer(colMeans(grade[,-1])[1]), 
                   r = as.integer(colMeans(grade[,-1])[2]))

V_df <- data.frame(name = 'Var',
                   sql = apply(grade[,-1], 2, var)[1],
                   r = apply(grade[,-1], 2, var)[2])

S_df <- data.frame(name = 'Sd',
                   sql = round(apply(grade[,-1], 2, sd),2)[1],
                   r = round(apply(grade[,-1], 2, sd),2)[2])

grade <- rbind(rbind(rbind(grade, M_df), V_df), S_df);grade

## 28-1. 표준화 작업
# - 표준값, 편차값
#   : 비교해야할 데이터의 기준이 서로 다르므로 같은 기준을 말들어서 비교

#[문제205] carrick의 sql = 90점과 pogba의 r = 90 둘 중 어느 사람이 더 잘한 것인가?

# - 표준값 = (X-mean(X))/sd(X)  

# carrick
carrick_z <- (90-72)/20

# pogba
pogba_z <- (90-72)/12

carrick_z
pogba_z

# - 편차값 = 표준값 * 10 + 50(평균 50, 표준편차 10)

carrick_t <- ((90-72)/20) * 10 + 50 ;carrick_t
pogba_t <- ((90-72)/12) * 10 + 50 ;pogba_t

#결론 : pogba가 더 잘했음


#[문제206] 지난달을 기준으로 a 사원은 영업 매출이 월 평균 1000만원, 
#          표준편차가 200만원인 대리점을 담당하고있고, b 사원은 영업 매출이 월 평균 100만원, 
#          표준편차가 10만원인 대리점을 담당하고 있다. 이번달 a 사원이 담당하는 대리점의 매출이 
#          1200만원으로 늘었고, b 사원이 담당하는 대리점은 120만원 늘었다. 평균 매출 대비 20% 상승했다. 
#          누가 잘한거죠?
  
a : mean(a) = 1000, sd(a) = 200
b : mean(b) = 100, sd(b) = 10

(1200-1000)/200
(120-100)/10
#결론 : b사원이 더 잘했음

#[문제207] 혈액형_성격_설문 파일입니다. 분석하세요.(crose-table)

blood_type <- read.csv("c:/r/설문.csv", header = T, stringsAsFactors = F)
blood_type

## 28-2. cross-table
table(blood_type$혈액형, blood_type$성격)
x <- xtabs(~혈액형+성격, blood_type);x
y <- xtabs(~혈액형+노래장르, blood_type);y
z <- xtabs(~혈액형+취미, blood_type);z

#혈액형 ~ 음악장르

#성격 -> 1번 : 내성적 2번 : 낙천적 3번 : 활동적 4번 : 합리적
#전체 비율값 : 1(행), 2(열)

prop.table(x,1)
melt(x)
ggplot(melt(x), aes(x = 혈액형, y = 성격))+
  geom_jitter(aes(colour = 혈액형), width = .3)
barplot(z, beside = T,
        legend.text = c("A", "AB", "B", "O"),
        col = rainbow_hcl(4),
        args.legend = list(x = "topleft", fill = rainbow_hcl(4)))


## 28-3. 변동계수(Coefficient of variation)
# - CV = (표준편차/표본평균) * 100 


#[문제208] A, B 두 회사의 주가를 한달동안 조사하여 평균주가, 표준편차를 조사한 결과다. 분석하세요.

# A회사의 평균 = 45,000원, 표준편차 = 3,000원 B회사의 평균 = 5,000원, 표준편차 = 2,000원
# 평균주가가 동일한 수준이라면 주가의 표준편차가 클수록 주가변동이 심하다고 할 수 있다. 
# 그러나 위와 같이 A회사 주가 표준편차는 3000원으로 B회사의 표준 편차 2000원 보다 크지만
# A 회사 주가의 표준편차는 평균주가 45000원을 기준으로한 3000원인데 비해 B회사 주가의 표준편차는 
# 평균주가 5000원을 기준으로한 2000원이므로 표준편차만을 가지고 두 회사 주가 변동성을 파악할 수는 없다.

# 이런 경우 상대적인 측면에서 변동정도를 파악한 것이 변동계수 이다.

(3000/45000)*100
(2000/5000)*100
# 결론 : A에 투자

## 28-4. 백분위수(percentile)
# - 자료를 크기 순서대로 나열해 놓고 100등분한 후에 위치에 있는 값을 말한다.
# - 백분위수 = ((x보다 작은 값의 수 + 0.5) / 전체자료 수) * 100

#[문제209] 10명 학생의 시험점수 입니다.(20점 만점) 12점에 대한 백분위수를 구하세요.

18 15 12 6 8 2 3 5 20 10

# x <- scan()
((sum(x < 12)+.5)/length(x))*100

# 결론 : 12점을 받은 학생은 반 학생의 65%보다 잘한다.

((sum(x<=12))/length(x))*100


## 28-5. 사분위수(quartile)
# - 자료를 순서대로 늘어놓고
"
관측치 25%     다음 25%      다음 25%    마지막 25%
  -------------------------------------------------------
  |             |             |            |            |
  0%(최소값)   25%(Q1)      50%(Q2)        75%(Q3)     100%(Q4)
"

# - quantile(), summary(), fivenum()
h <- c(160, 166, 170, 172, 172, 173, 175, 178, 180, 181, 182, 185, 200)
h

quantile(h)
summary(h)

#이상치 표현해주기
bp <- boxplot(h, border = "blue", ylim = c(150,200))
abline(h = c(fivenum(h)[2]-IQR(h)*1.5, fivenum(h)[4] + IQR(h)*1.5), lty = 3, col = "red")
text(bp$out, lables = bp$out, pos = c(1,1,3,1))
fivenum(h)

bp <- boxplot(h, border = "blue", horizontal = TRUE)
text(bp$out, rep(1,NROW(bp$out)),
     labels = bp$out, pos = c(1,1,3,1))

#사분위수 범위 : Q3 - Q1 ( IQR() )
IQR(h) * 1.5

#이상치값 : Q1 - IQR(h)*1.5
108.2-IQR(h)*1.5

#이상치값 : Q3 + IQR(h)*1.5
151.8 + IQR(h)*1.5