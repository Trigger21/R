## R-11일차(2018.1.10)

  # 11-1. pie chart
  # 11-2. pie chart(3D)
  # 11-3. bar chart


#[문제144] mysentence 이름의 벡터 변수에 'Well begun is half done' 이 값을 입력한 후 
#          공백문자를 기준으로 분리해서 word 변수에 입력하세요. word 변수에 타입을 체크한 후 
#          word 변수에 입력된 값의 수를 출력하세요.

mysentence <- 'Well begun is half done'
mysentence
word <- str_split(mysentence, ' ')
word

class(word)    # list
mode(word)
str(word)

length(word[[1]])  # 5
NROW(word[[1]])
lapply(word, length)
do.call(NROW, word)

#선생님 풀이
word <- strsplit(mysentence, split = ' ')
word

class(word)
sapply(word, NROW)   # sum(sapply(word, NROW))
sapply(word, length)

word[[1]][1]


#[문제145] 문제 144번에 만든 word변수에 있는 값을 letters변수에 공백값을 기준으로 하나씩 저장하세요. 
#          (list 5개 방에 분리해서 저장)

#sol.1
letters <- NULL  # vector형 되버림

for(i in 1:NROW(word[[1]])){
  letters <- as.list(append(letters, word[[1]][i]))
}

letters

#sol.2
for(i in 1:NROW(word[[1]])){
  letters[i] <- word[[1]][i]
}
letters

#선생님 풀이
letters <- list(rep(NA, 5))   # 초기화(굳이 할 필요는 없지만)
letters

for(i in 1:sapply(word,length)){
  letters[[i]] <- word[[1]][i]
}

letters
sapply(letters, NROW)

#엄청 간단한 방법(이대곤)
word
as.list(unlist(word))


#[문제146] 문제 145에서 생성한 letters 변수에 있는 값을 myword변수에 하나로 합쳐서 넣어 주세요.

myword <- str_c(letters, collapse = ' ')  # paste(letters, collapse = ' ')
myword

#선생님 풀이
myword <- list(rep(NA,5))

for(i in 1:length(letters)){
  myword[[1]][i] <- paste(letters[[i]][1], collapse = '')
}
myword

paste(unlist(myword), collapse = ' ')
#NROW list 조작 고민(분리 - 합체)


#[문제147] 리스트 변수에 있는 값을 기준으로 홀수인지 짝수인지를 출력하세요.

[[1]] [1] 1 2 3 4 5 6 7 8 9 10

[[1]] [1] "홀수" "짝수" "홀수" "짝수" "홀수" "짝수" "홀수" "짝수" "홀수" "짝수"

x <- list(1:10) ; x
x <- list(1:10 %% 2)

list(ifelse(unlist(x) == 1, "홀수", "짝수"))

#또다른 방법(김승혁 형)
r<-list(1:10)
r
sapply(r, length)  # 리스트 길이
a<-list(rep(NA,10))  # NA만 10개 들어있는 리스트 생성
a
for (i in 1:sapply(r, length)){
  if (r[[1]][i] %% 2 > 0){
    a[[1]][i] <- '홀수'
  }else{a[[1]][i] <- '짝수'}
}
a 

#선생님 풀이
x <- list(1:10) ; x
lapply(x, function(x){ifelse(x%%2 == 0, 'even', 'odd')})
sapply(x, function(x){ifelse(x%%2 == 0, 'even', 'odd')})


#[문제148] 리스트 변수에 있는 값을 기준으로 홀수인지 짝수인지를 출력하세요. 
#          (단, 사용자 정의 함수를 생성해서 출력하세요.) 
x
[[1]] [1] 1 2 3 4 5 6 7 8 9 10

odd_even_check(x)

[1] "홀수" "짝수" "홀수" "짝수" "홀수" "짝수" "홀수" "짝수" "홀수" "짝수"

x <- list(1:10)
x

#sol.1 : vector
odd_even_check <- function(x){
  
  res <- ifelse(unlist(x)%%2 == 1, "홀수", "짝수")
  return(res)
  
}

odd_even_check(x)  # vector

#sol.2 : list
odd_even_check <- function(x){
  
  res <- ifelse(unlist(x)%%2 == 1, "홀수", "짝수")
  return(res)
  
}

lapply(x, odd_even_check)

#선생님 풀이
x <- list(1:10)
x

#sol.1 : vector형으로 반환하는 함수
odd_even_check <- function(x){
  e <- NULL
  
  for(i in 1:sapply(x,length)){
    
    if(x[[1]][i] %% 2 == 0){e <- c(e, "짝수")} else{e <- c(e,"홀수")}  # 누적해서 벡터에 넣는 방법
    
  }
  print(e)
}

odd_even_check(x)

#sol.2 : lapply(x, function(x){ifelse(x%%2 == 0, 'even', 'odd')})
odd_even_check <- function(x){
  e <- list(rep(NA,sapply(x,length)))
  
  for(i in 1:sapply(x,length)){
    
    if(x[[1]][i] %% 2 == 0){e[[1]][i] <- "짝수"} else{e[[1]][i] <- "홀수"}
    
  }
  print(e)
}

odd_even_check(x)

#sol.3 : 세로로 출력
odd_even_f <- function(x){
  for(i in 1:sapply(x,length)){
    
    if(x[[1]][i] %% 2 == 0){print("짝수")} else{print("홀수")}
    
  }
}

odd_even_f(x)

#별도 : sapply(x, function(x){ifelse(x%%2 == 0, 'even', 'odd')}) 처럼 행렬로 반환
odd_even_mat <- function(x){
  res <- NULL
  res <- matrix(rep(NA,lapply(x,length)))
  
  for(i in 1:do.call(NROW, x)){
    
    res[i,1] <- ifelse(x[[1]][i] %% 2 == 0, "짝수", "홀수")
    
  }
  return(res)
}

odd_even_mat(x)
class(sapply(x, function(x){ifelse(x%%2 == 0, "짝수", "홀수")})) 
#질적자료 : 혈액형, 성비(이산) 양적자료 : 키, 몸무게(연속) 그룹핑 여부에 따라 위 자료가 구분이 됨


## 11-1. pie chart
# ex) 회사별 연매출액
- A회사 : 100억
- B회사 : 50억
- C회사 : 30억
- D회사 : 10억

s <- c(100,50,30,10)
company <- c("A회사", "B회사", "C회사", "D회사")
pie() : pie chart 만드는 함수
library(graphics)
pie(s, company)
pie(s, labels = company, 
    main = "회사별 매출액", 
    col = c("red", "blue", "green", "yellow"))

# col 자동설정
rainbow(length(s))  # 무지개색 
heat.colors(12)     # 적색, 황색에 치우친 색
terrain.colors(12)  # 지구 지형색
topo.colors(12)     # 청색에 가까운 색
cm.colors(12)       # 핑크, 블루  

pie(s, labels = company, 
    main = "회사별 매출액", 
    col = rainbow(length(s)))

pie(s, labels = company, 
    main = "회사별 매출액", 
    col = heat.colors(4))

pie(s, labels = company, 
    main = "회사별 매출액", 
    col = terrain.colors(4))

pie(s, labels = company, 
    main = "회사별 매출액", 
    col = topo.colors(4))

pie(s, labels = company, 
    main = "회사별 매출액", 
    col = cm.colors(4))

# clockwise : 시계방향(TRUE), 반시계방향(FALSE)
pie(s, labels = company, 
    main = "회사별 매출액", 
    clockwise = F,
    col = rainbow(4))

# init.angle : 시작되는 지점의 각도를 지정
pie(s, labels = company, 
    main = "회사별 매출액", 
    clockwise = F,
    init.angle = 90,
    col = cm.colors(4))
p <- round((s/sum(s))*100) ; p
label <- paste(company, p)
label <- paste(label,'%', sep = '')  # sep = '' 빈 공간 붙이기
label
pie(s, labels = label, 
    main = "회사별 매출액", 
    clockwise = F,
    init.angle = 90,
    col = cm.colors(4))


## 11-2. pie chart(3D)
install.packages("plotrix")
library("plotrix")

# explore : 부채꼴들의 간격, labelcex : label의 문자 크기
pie3D(s, labels = label, explode = 0.1, labelcex = 2)


#[문제149] 부서별 급여의 총액을 pie chart로 그려보세요

agg <- aggregate(SALARY~DEPARTMENT_ID, emp, sum)
pie(agg$SALARY, agg$DEPARTMENT_ID, init.angle = 90, clockwise = F)
t <- tapply(emp$SALARY, emp$DEPARTMENT_ID, sum, na.rm = F) ; t
pie(t, main = "부서별 급여 총액", clockwise = F, col = terrain.colors(nrow(t)))
library("plyr")
ddply(emp, 'DEPARTMENT_ID', summarise, sum_sal = sum(SALARY))
library("dplyr")
emp_sal <- emp%>%
  group_by(DEPARTMENT_ID)%>%
  summarise(sum(SALARY))
emp_sal$DEPARTMENT_ID[is.na(emp_sal$DEPARTMENT_ID)] <- "NA"
dept_label <- paste(emp_sal$DEPARTMENT_ID, emp_sal$`sum(SALARY)`) 
dept_label <- paste(dept_label, "원", sep = '')
dept_label 

pie(emp_sal$`sum(SALARY)`, labels = emp_sal$DEPARTMENT_ID,
    main = "부서별 급여 총액", clockwise = F, col = rainbow(nrow(emp_sal)), border = NA)
pie3D(emp_sal$`sum(SALARY)`, 
      labels = emp_sal$DEPARTMENT_ID,
      main = "부서별 급여 총액", 
      clockwise = F, 
      col = rainbow(nrow(emp_sal)), border = NA, explode =  labelcex = 0.8)
barplot(t)


## 11-3. bar chart (!= 히스토그램)
sales <- c(150,100,70,30)
team <- c("영업 1팀", "영업 2팀", "영업 3팀", "영업 4팀")
# - height : 막대크기를 나타내는 벡터
# - width : 막대너비
# - names.arg : 막대아래 출력될 이름에 대한 벡터
# - col : 막대색상
# - main : 제목
# - sub : 부제목
# - horiz : TRUE(수평막대), FALSE(수직막대)
# - xlab : x축 label
# - ylab : y축 label
# - xlim : x축 크기
# - ylim : y축 크기

bp <- barplot(height = sales, 
              width = .5, 
              names.arg = team, 
              horiz = FALSE, 
              col = rainbow(length(sales)),
              main = "영업팀별 영업 실적",
              xlab = "영업팀", 
              ylab = "영업실적(억원)",
              ylim = c(0,200),
              density = 40)

text(x = bp, y = sales, labels = round(sales), pos = 3)

# pos = 1 : 막대 끝 선의 아래쪽
# pos = 2 : 막대 끝 선의 왼쪽
# pos = 3 : 막대 끝 선의 위쪽
# pos = 4 : 막대 끝 선의 오른쪽

VADeaths
t(VADeaths)
# help(VADeaths)

barplot(height = t(VADeaths * .1), ylim = c(0,40), col = rainbow(4), 
        legend = c("RM","RF","UM","UF"))

barplot(height = (VADeaths * .1), ylim = c(0,50), col = rainbow(4), 
        legend = rownames(VADeaths))