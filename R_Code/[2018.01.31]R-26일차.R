## R-26일차(2018.1.31)

  # 26-1. cut()
  # 26-2. Histogram
  # 26-3. Stem and Leaf Diagram(줄기잎 그림)


#[문제186] 조사된 데이터 값들이 있습니다. 도수분포표를 만드세요.

90 88 78 65 80 94 69 72 83 64 95 68 87 69 82 91 63 70 81 67

#계급 도수
#90점이상          4
#80점이상 90점미만 6 
#70점이상 80점미만 3 
#60점이상 70점미만 7

#양적자료도 도수분포표를 만들수 있다 계급의 갯수를 정하는 방법이 있다?
  
df <- data.frame(계급 = c("90점이상","80점이상~90점미만","70점이상~80점미만","60점이상~70점미만"),
                   도수 = c(0,0,0,0), stringsAsFactors = FALSE)
df

d <- c(90, 88, 78, 65, 80, 94, 69, 72, 83, 64, 95, 68, 87, 69, 82, 91, 63, 70, 81, 67)
d

table('90~' = d >= 90, 
      '80~90' = d >= 80 & d < 90, 
      '70~80'= d >= 70 & d < 80, 
      '60~70' = d >= 60 & d < 70)
#  90~ : 4, 80~90 : 6, 70~80 : 3, 60~70 : 7

df$도수 <- c(4,6,3,7)
df
df$도수 <- NULL
for(i in 1:length(d)){
  ifelse(d >= 90, df$도수[1] <- df$도수[1] + 1, 
         ifelse(d >= 80 & d < 90, df$도수[2] <- df$도수[2] + 1, 
                ifelse(d >= 70 & d < 80, df$도수[3] <- df$도수[3] + 1, df$도수[4] <- df$도수[4] + 1)))
}

df

#선생님 풀이

#도수분포표(Frequency Distribution)
"""
- 처음 조사된 원자료는 그 자료의 특징 및 분포를 파악하기 어렵다.

- 처음 조사된 원자료를 구간을 나누거나 도수를 세거나 해서 정리하여 자료의
  구조적 특징을 파악하는 표 

- 미리 구간을 설정해 놓고 각 구간의 범위안에 조사된 데이터 값들이 몇개씩
  속하는가를 표시한다

- 계급(class) : 각 구간

- 도수(frequency) : 각 구간(계급)에 속한 데이터 값들의 개수 

- 계급의 수 결정
  계급의 수 = (자료의 측정값들의 갯수)^(1/3)

- 구간 = (자료 측정값의 최대값 - 자료 측정값의 최소값) / 계급의수    
"""
score <- scan()

# 계급의수
round(length(score)^(1/2))
round(length(score)^(1/3)) 

# 구간
(max(score)-min(score)) / 3

for(i in score){
  if(i >= 90){
    df[1,2] <- df[1,2] + 1
  }else if(i >= 80 & i < 90){
    df[2,2] <- df[2,2] + 1
  }else if(i >= 70 & i < 80){
    df[3,2] <- df[3,2] + 1
  }else if(i >= 60 & i < 70){
    df[4,2] <- df[4,2] + 1
  }
}
df

#총계 입력
df[5,] <- list(c("전체"), sum(df$도수))
df

#선생님 풀이
df <- rbind(df, data.frame(계급 = "전체", 도수 = sum(df[,2])))

#상대도수 : 계급에 속한 도수가 전체도수에 차지하는 비율 
#누적도수 : 계급 도수와 그 위에 계급에 속하는 도수를 모두 합한다.

df

#상대도수
cbind(df, data.frame(상대도수 = c(df[1:4,2]/df[5,2],NA)))
for(i in 1:nrow(df)-1){
  df[i,3] <- df[i,2]/df[5,2]
}
names(df)[3] <- "상대도수"
df
df$도수/df$도수[5]

#누적도수
sgm <- NULL
for(i in 1:4){
  sgm <- c(sgm,sum(df[1:i,2]))
}
sgm

df <- cbind(df, data.frame(누적도수 = c(sgm,NA)))
df
for(i in 1:NROW(df)){
  if(i==1){
    df$누적[i] <- df$도수[i]
  }else if(i == NROW(df)){
    df$누적[i] <- NA
  }else{
    df$누적[i] <- sum(df$도수[1:i])
  }
}
df

# 누적도수 구하는 법
for(i in 1:length(df$누적도수)){
  if(sum(df$cnt[df$score[1:i]]) <= df$cnt[df$score=="전체"]){
    df$누적도수[i]<- sum(df$cnt[df$score[1:i]])
  }
  else if(sum(df$cnt[df$score[1:i]]) > df$cnt[df$score=="전체"]){
    df$누적도수[i]<- NA
  }
}


## 26-1. cut(score, breaks, right = FALSE, labels = c(""))

# 이산형 -> 명목형
ft_cut <- cut(score, breaks = c(60,70,80,90,101), right = FALSE, 
              labels = c("60점이상~70점미만","70점이상~80점미만","80점이상~90점미만","90점이상"))
ft_cut


# right = FALSE

60 <= score < 70
70 <= score < 80
80 <= score < 90
90 <= score < 101


# right = TRUE

60 < score <= 70
70 < score <= 80
80 < score <= 90
90 < score <= 101

ft_cut_table <- table(ft_cut)
ft_cut_table

# prop.table : 상대도수
## 상대도수 만드는 법
prop.table(ft_cut_table) 
cbind(ft_cut_table, prop.table(ft_cut_table))
## 상대도수 만드는 법
prop.table(ft_cut_table) 


#[문제187] 학생들의 시험 성적입니다. grade값을 만들어 주세요.

id      score   grade
100         63
101         93
102         72
103         80
104         54
105         88
106         68
107         87
108         66
109         98
110         70

score < 60 : F 
60<= score <70 : D 
70<= score <80 : C 
80<= score <90 : B 
90<= score : A

scr <- c(63,93,72,80,54,88,68,87,66,98,70)
df_187 <- data.frame(id = 100:110, score = scr, grade = NA)
df_187

#sol.1 : cut()
df_cut <- cut(scr, breaks = c(0,60,70,80,90,101), labels = c("F","D","C","B","A"), right = FALSE)
ordered(df_cut)
df_187$grade <- df_cut
df_187


#sol.2 : if()
  for(i in 1:NROW(scr)){
    if(scr[i] >= 90){
      df_187$grade1[i] <- "A"
    }else if(scr[i] < 90 & scr[i] >= 80){
      df_187$grade1[i] <- "B"
    }else if(scr[i] < 80 & scr[i] >= 70){
      df_187$grade1[i] <- "C"
    }else if(scr[i] < 70 & scr[i] >= 60){
      df_187$grade1[i] <- "D"
    }else{
      df_187$grade1[i] <- "F"
    }
  }
df_187


#sol.3 : ifesle()
df_187$grade2 <- ifelse(scr < 60, "F",
                        ifelse(scr >= 60 & scr < 70, "D",
                               ifelse(scr >= 70 & scr < 80, "C",
                                      ifelse(scr >= 80 & scr < 90, "B", "A"))))
df_187


#번외연구
pt <- prop.table(df_cut_tbl)  # df_cut_tbl <- table(df_cut)

df_pt <- as.data.frame(round(pt, 2))
df_pt

library(dplyr)
library(sqldf)

sqldf("select d1.*, d2.Freq 
      from df_187 d1, df_pt d2
      where d1.grade = d2.df_cut")
test <- transform(result, result = cut(grade, breaks = c(0, 60, 70, 80, 90, 100),
                                       include.lowest = TRUE,
                                       right = FALSE, 
                                       labels = c("F", "D", "C", "B", "A")))
pie(round(pt, 2), init.angle = 120)


## 26-2. Histogram
score <- scan() 
#1: 90 88 78 65 80 94 69 72 83 64 95 68 87 69 82 91 63 70 81 67

max(score)
min(score)

library(colorspace)
hist(x = score, breaks = seq(60, 101, by = 10), right = FALSE, col = heat_hcl(4))

# x : 히스토그램을 위한 벡터 데이터 
# breaks : 계급구간 

#[문제188] 학생들의 몸무게 자료를 이용해서 도수분포표,그래프를 생성하세요.

#step.1 : weight.txt 파일내용 벡터값으로 저장

# weight <- scan()
weight


#step.2 : 구간 정하기

# max : 93, min : 52

(max(weight)-min(weight))/length(weight)^(1/3) # 11.12911 : 그냥 10으로 하겠음

#step.3 : 도수분포표 작성

#3-1 : cut() 사용해서 분류
weight_cut <- cut(weight, 
                  breaks = seq(50,100,10), 
                  labels = c("50≤X<60","60≤X<70","70≤X<80","80≤X<90","90≤X<100"), 
                  right = FALSE)
weight_cut


#3-2 : data.fame으로 변환
weight_df <- as.data.frame(table(weight_cut))
names(weight_df) <- c("계급", "도수")
weight_df


#3-3 : 전체, 상대도수, 누적도수 반영

# 전체
weight_df <- rbind(weight_df, data.frame(계급 = "전체", 도수 = sum(weight_df$도수))) ; weight_df

# 상대도수
x <- as.data.frame(prop.table(table(weight_cut)))
weight_df <- cbind(weight_df, data.frame(상대도수 = c(x$Freq, sum(x$Freq)))) ; weight_df

# 누적도수
for(i in 1:NROW(weight_df)){
  weight_df$누적도수[i] <- ifelse(i == NROW(weight_df), NA, sum(weight_df$도수[1:i]))
} ; weight_df

#step.4 : 히스토그램

install.packages("colorspace")
library(colorspace)

weight_h <- hist(x = weight, breaks = seq(50, 100, by = 10), right = FALSE, col = heat_hcl(5), ylim = c(0,25))
text(x = weight_h$mids, y = weight_h$counts, labels = weight_h$counts, pos = 3, col = "blue")

library(reshape2)
library(ggplot2)
w_df <- as.data.frame(weight)

ggplot(data = w_df, aes(x = weight))+
  geom_histogram(binwidth = 10, aes(fill=..count..))
weight <- read.table("c:/r/weight.txt") 
str(weight) 
weight <- as.matrix(weight) 
dim(weight) <- c(50,1) 
weight <- as.data.frame(weight) 
max(weight) 
min(weight) 
w <- weight[,1] 
w_ct <- cut(w,breaks=c(50,60,70,80,90,101),right=FALSE,include.lowest = FALSE, labels=c("50이상60미만","60이상70미만","70이상80미만","80이상90미만","90이상")) 
w_ct 

w_t <- table(w_ct) 
prop.table(w_t) 
c_df <- cbind(w_t,prop.table(w_t))

rownames(c_df) 
colnames(c_df) <- c("도수","상대도수") 
c_df 
hist(x=weight[,1], main="학생들의 몸무게", xlab="몸무게", ylab="인원수", ylim=c(0,25), col=rainbow(5), breaks=seq(50,100,by=10))

library(ggplot2)

ggplot(data=weight,aes(x=freq))+ 
  geom_histogram(binwidth = 10, aes(fill=..count..))


## 26-3. Stem and Leaf Diagram(줄기잎 그림)
stem(weight)
5 | 2 52 
5 | 7 57 
6 | 2222 62(4) 
6 | 56778889999 65,66,67(2),68(3),69(4) 
7 | 11122344 71(3),72(2),73,74(2) 
7 | 5555567789999 75(5),76,77(2),78,79(4) 
8 | 00123 80(2),81,82,83 
8 | 55789 85(2),87,88,89 
9 | 03 90,93

x <- rchisq(100, df = 4)
qqplot(x, qchisq(ppoints(x), df = 4)); abline(0, 1, col = 2, lty = 2)
hist(x, freq = FALSE, ylim = c(0, 0.2))
curve(dchisq(x, df = 4), col = 2, lty = 2, lwd = 2, add = TRUE)
