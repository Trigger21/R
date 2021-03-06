# R-4일차(2017-12-29)
  # 4-1. 정렬방법
  # 4-2. 제어문
  # 4-3. 반복문
  # 4-4. 문자함수 


## 4-1. 정렬방법

# 1) sort
  # 벡터정렬
x <- c(3,4,2,10,NA,20,5,11,NA,40)
x
sort(x)
sort(x, decreasing = FALSE)    # inc
sort(x, decreasing = TRUE)     # dec 1
sort(x, decreasing = TRUE, na.last = NA)     # NA X (default)
sort(x, decreasing = TRUE, na.last = TRUE)   # NA O (last)
sort(x, decreasing = TRUE, na.last = FALSE)  # NA O (first)
rev(sort(x))   # dec 2

# 2) order
  # 정렬한 색인을 반환
x <- c(3,4,2,10,NA,20,5,11,NA,40)
order(x)  
x[order(x)]
x[order(x, decreasing = TRUE)]
x[order(x, decreasing = TRUE, na.last = NA)]     # NA X
x[order(x, decreasing = TRUE, na.last = TRUE)]   # NA O (default)
x[order(x, decreasing = TRUE, na.last = FALSE)]  # NA O (first)

# 3) doBy
  # 데이터 프레임에 정렬
  # 기본내장 아님(별도설치 필요) 
install.packages("doBy")
library("doBy")

# doBY 내부 orderBy(~ ) 함수 사용 : - 는 내림차순
str(emp)
orderBy(~SALARY, emp[,c("LAST_NAME", "SALARY")])   # inc by salary
orderBy(~-SALARY, emp[,c("LAST_NAME", "SALARY")])  # dec by salary

orderBy(~DEPARTMENT_ID + SALARY, emp[,c("LAST_NAME", "SALARY", "DEPARTMENT_ID")])  # inc by dep_id salary
orderBy(~DEPARTMENT_ID - SALARY, emp[,c("LAST_NAME", "SALARY", "DEPARTMENT_ID")])  # inc by dep_id, dec by salary
orderBy(~SALARY, emp[emp$EMPLOYEE_ID <= 105, c("LAST_NAME", "SALARY")])


# [문제57] df이름의 data frame 변수를 선언합니다. id 컬럼의 값은 100,101,102,103,104,
#          weight 컬럼의 값은 60,90,75,95,65, size 컬럼의 값은 small, large, medium,large,small 값으로 생성하세요.

df <- data.frame(id = c(100:104), 
                 weight = c(60,90,75,95,65), 
                 size = factor(c('small','large','medium','large','small'), 
                               c('small','medium','large'), ordered = TRUE))
str(df)

# [문제58] df변수에 weight 컬럼을 기준으로 오름차순 정렬해서 df 변수에 값을 출력하세요.(order 함수를 이용하세요)

df[order(df$weight),]

# [문제59] df변수에 size, weight 컬럼을 기준으로 오름차순 정렬하세요.(order 함수를 이용하세요)

df[order(df$size, df$weight),]
str(df$size)
df[order(df$size, df$weight, decreasing = TRUE),]

# [문제60] df 변수에 있는 weight 컬럼을 기준으로 내림차순 정렬하세요.(order 함수를 이용하세요)

df[order(df$weight, decreasing = TRUE),]

# [문제61] job_id가 ST_CLERK 가 아닌 사원들의 last_name, salary, job_id를 출력하는데 
#          급여가 높은 사원부터 출력되게하세요.(orderBy 함수를 이용하세요)

nrow(emp[emp$JOB_ID != 'ST_CLERK',])  # 87rows

orderBy(~-SALARY, emp[emp$JOB_ID != 'ST_CLERK', c('LAST_NAME','SALARY','JOB_ID')])

# [문제62] 사원 last_name, salary, commission_pct를 출력하는데 
#          commission_pct를 기준으로 오름차순정렬하세요.(orderBy를 이용하세요)

orderBy(~COMMISSION_PCT, emp[,c('LAST_NAME','SALARY','COMMISSION_PCT')])

# [문제63] commission_pct를 받고 있는 사원들의 last_name, salary, commission_pct를 출력하는데 
#          commission_pct를 기준으로 오름차순정렬하세요.(orderBy를 이용하세요)

na.omit(orderBy(~COMMISSION_PCT, emp[, c('LAST_NAME','SALARY','COMMISSION_PCT')]))

# 번외 연구
a <- c("k","p")
b <- ordered(c("A","B"),c("C","B","A"))

str(a):str(b)

c <- data.frame(name = a, grade = b, stringsAsFactors = FALSE)
c
str(c)


## 4-2. 제어문
  # 조건의 흐름을 제어

# if문 : 조건에 따라 서로 다른 코드를 수행하도록 하는 문장
  # if(조건) { 조건이 참일때 수행문장 } else { 조건이 거짓일때 수행문장 }

ex)

if(1>2){
  print("1이 2보다 크다")
} else{
  print("1이 2보다 작다")
}

# ifelse 함수
  # ifelse(조건, 참, 거짓) : 조건이 참이면 참문장, 거짓이면 거짓문장 수행

ex)

x <- 2

ifelse(x%%2 == 0, 'even', 'odd')

if(x%%2 == 0){
  'even'
} else{
  'odd'
}

# 번외 연구
if(1>2){
  print(1)
} else if(1>2){
  print(2)
} else if(1>2){
  print(3)
} else if(1<2){
  print(4)
}


## 4-3. 반복문

# for문
  # for(변수 in 데이터 변수){ 반복 수행할 문장 }

# 1~10 출력
for(i in 1:10){
  print(i)
}

# 1~10 총합 출력
sum <- 0
x <- 1:10
for(i in x){
  sum <- sum + i
}
print(sum)

# 2의 배수 출력
sum <- 0
x <- c(2,4,6,8,10)
for(i in x){
  print(i)
  sum <- sum + i
}
print(sum)


# while문
  # while(조건){ 반복 수행할 문장(조건이 TRUE) }

i <- 1
while(i <= 10){
  print(i)
  i <- i + 1
}


# repeat문
  # repeat{ 반복 수행할 문장 break # 반복문 종료(infinite loop exit : ESC) }
  
i <- 1
repeat{
  print(i)
  if(i==10) {
      break
    } else{
      i <- i + 1
    }
}


i <- 1
repeat{
    print(i)
    ifelse(i==10, break, i<-i+1)
}


# [문제64] 1부터100까지 홀수만 x 변수에 입력해주세요.
  
  x <- 1:100
  for(i in x){
    if(i%%2==1){
      x <- seq(1,i,2)
    }
  }
  x

# 선생님 풀이
  x <- NULL
  z <- NULL
  y <- NULL
  r <- NULL
  for(i in 1:100)
  {
    if (i %% 2 != 0)
    {
      print(paste(i ,'홀수'))
      x<-append(x,i)
      z<-c(z,i)
      y<-cbind(y,i)
      r<-rbind(r,i)
    }
  }
  
  print(x)
  print(z)
  print(y)
  print(r)
  
  str(x)
  str(z)
  str(y)
  str(r)

# [문제65] 1부터100까지 전체합, 짝수합, 홀수합을 출력하세요.
  
  x <- 1:100
  sum <- 0
  sum_even <- 0
  sum_odd <- 0
  for(i in x){
    if(i%%2==0){
      sum_even <- sum_even + i
    } else{
      sum_odd <- sum_odd + i
    }
    sum <- sum + i
  }
  sum(x)
  paste('전체합 :',sum)
  paste('짝수합 :',sum_even)
  paste('홀수합 :',sum_odd)

# 선생님 풀이
  o <- 0
  e <- 0
  s <- 0
  
  for(i in 1:100)
  {
    s <- s+i
    if (i %% 2 == 1)
    {
      o <- o+i
    } else {
      e <- e+i
    }
  }
  
  print(o)
  print(e)
  print(s)
  
# [문제66] emp 변수에 있는 데이터를 가지고 새로운 df변수를 생성하세요.
#          last_name, salary, 급여가 10000 이상이면 A, 5000이상 10000보다 작으면 B 나머지는 C가 입력되어 있는 새로운 컬럼을 생성하세요.
#          컬럼이름은 name, sal, level 로 설정하세요.
  
  df <- data.frame(name = emp$LAST_NAME, sal = emp$SALARY)
  df
  nrow(df)
  level <- c(1:nrow(df))
  for(i in 1:nrow(df)){
    if(df[i,2] >= 10000){
      level[i] <- 'A'
    } else if(df[i,2] < 10000 & df[i,2] >= 5000){
      level[i] <- 'B'
    } else {
      level[i] <- 'C'
    }
  }
  df[,2]
  level
  
  df[,3] <- level
  names(df) <- c('name','sal','level')
  df
  df <- data.frame(name = emp$LAST_NAME, sal = emp$SALARY, level = NA, stringsAsFactors = FALSE)
  for(i in 1:nrow(df)){
    if(df[i,2] >= 10000){
      df[i,3] <- 'A'
    } else if(df[i,2] < 10000 & df[i,2] >= 5000){
      df[i,3] <- 'B'
    } else {
      df[i,3] <- 'C'
    }
  }
  df
  

  # 멋진풀이(곽상욱 형님)
  df1 <- data.frame(name = emp$EMPLOYEE_ID, sal = emp$SALARY, level = ifelse(emp$SALARY>=10000,'A',ifelse(emp$SALARY>=5000 & emp$SALARY<10000,'B','C')))
  df1

  # 선생님 풀이
  df <- data.frame(emp$LAST_NAME, emp$SALARY, ifelse(emp$SALARY >=10000, 'A',
                                                     ifelse(emp$SALARY >=5000 & emp$SALARY < 10000,'B', 'C')))
  
  
  names(df) <- c("name","sal","level")
  df
  
  # 또다른 접근(김승혁 형님)
  (dfA <- emp[emp$SALARY>=10000, c("LAST_NAME","SALARY")])
  (dfB <- emp[emp$SALARY>=5000&emp$SALARY<10000, c("LAST_NAME","SALARY")])
  (dfC <- emp[emp$SALARY<5000, c("LAST_NAME","SALARY")])
  (dfA<- cbind(dfA , 'A'))
  (dfB<- cbind(dfB , 'B'))
  (dfC<- cbind(dfC , 'C'))
  (names(dfA)<-c('name', 'sal', 'level'))
  (names(dfB)<-c('name', 'sal', 'level'))
  (names(dfC)<-c('name', 'sal', 'level'))
  (df <- rbind(dfA, dfB, dfC))
  df
  head(df, 10)
  
  
  ## 4-4. 문자함수
  
  # nchar 문자 수
  nchar('R Developer')
  nchar('R Developer',type = "chars")  # default 
  nchar('R Developer',type = "bytes")
  nchar('빅데이터')
  nchar('빅데이터',type="chars")
  nchar('빅데이터',type="bytes")
  
  # 번외 연구
  nchar(emp, type="bytes")
  
  
  # strsplit 부분문자로 분리
  strsplit('R Developer',split=character(0))
  strsplit('R Developer',split=" ")
  strsplit('R,Developer',split=",")
  str(strsplit('R,Developer',split=","))
  unlist(strsplit('R,Developer',split=","))
  str(unlist(strsplit('R,Developer',split=",")))
  
  # 번외 연구
  character(3)
  numeric(5)
  
  strsplit('R Developer',split=character(0))
  strsplit('123',split=character(0))
  
  
  # toupper 대문자
  toupper('r developer')
  
  
  # tolower 소문자
  tolower('R DEVELOPER')
  
  
  # substr 문자열 추출
  substr('R Developer',1 , 1)  # start, stop 
  substr('R Developer',1 , 4)
  substr('R Developer',4 , 4)
  
  
  # sub 첫번째 일치하는 문자만 바꾼다.
  sub('R','Python','R Programmer R Developer')
  sub('R','Python','R Programmer R Developer') : 찾을글자, 바꿀글자, 원본글자 [1] "Python Programmer R Developer"
  
  
  # gsub 문자열 안에 일치하는 모든 문자를 바꾼다.
  gsub('R','Python','R Programmer R Developer')
  gsub('[0-2]', '*', 120304)  # 120304에서 0,1,2 찾아서 *로 바꿔
  
  
  # which 찾은 값의 색인을 리턴
  name <- c('scott','king','smith','Smith','july','smith')
  name == 'smith'
  which(name=='smith')
  name[which(name == 'smith')]
  
  
  # [문제67] last_name의 글자의 수가 10이상인 사원의 employee_id, last_name 출력하세요.
  
  nrow(emp[nchar(emp$LAST_NAME) >= 10,])
  emp[nchar(emp$LAST_NAME) >= 10, c('EMPLOYEE_ID','LAST_NAME')]
  
  # [문제68] last_name, last_name의 첫번째 철자부터 세번째 철자까지 함께 출력하세요.
  
  # 1st sol
  l <- substr(emp$LAST_NAME,1,3)
  
  for(i in 1:nrow(emp)){
    print(emp$LAST_NAME[i])
    print(l[i])
  }
  
  # 2nd sol
  str(l)
  ex_68 <- matrix(c(emp$LAST_NAME, l), nrow = 2, byrow = TRUE)
  rownames(ex_68) <- c('full_name','3_name')
  ex_68
  str(ex_68)
  
  # 3rd sol
  data.frame(name = emp$LAST_NAME, nam = l)
  
  # 4th sol
  paste(emp$LAST_NAME, substr(emp$LAST_NAME,1,3))
  
  # [문제69] last_name의 두번째 철자가 m 인 사원들의 last_name, salary를 출력하세요.
  
  nrow(emp[substr(emp$LAST_NAME,2,2) == 'm',])
  emp[substr(emp$LAST_NAME,2,2) == 'm', c('LAST_NAME','SALARY')]
  
  # [문제70] last_name의 두번째 철자가 m 또는 g 인 사원들의 last_name, salary를 출력하세요.
  
  emp[substr(emp$LAST_NAME,2,2) %in% c('m','g') , c('LAST_NAME','SALARY')]
  
  # [문제71] last_name, salary값을 화면에 출력할때 0은 * 로 출력하세요.
  
  # 1st sol
  sal <- c(gsub('0','*',emp$SALARY))
  matrix(c(emp$LAST_NAME, sal), nrow = 2, byrow = TRUE)
  
  # 2nd sol
  data.frame(name = emp$LAST_NAME, sal = sal)
  
  # 3rd sol
  paste(name = emp$LAST_NAME, sal)