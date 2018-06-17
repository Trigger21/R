# R-3일차(2017-12-28)
  # 3-1. data type 형변환
  # 3-2. read.csv
  # 3-3. grep함수 
  # 3-4. 중복제거 
  # 3-5. 정렬

# [문제31] 아래와 같은 모양의 변수를 생성하세요. 변수 이름은 df로 하세요.

<화면출력>
  
  df name sql plsql 1 king 96 75 2 smith 82 91 3 jane 78 86

df <- data.frame(name = c("king","smith","jane"), 
                 sql = c(96,82,78),
                 plsql = c(75,91,86),
                 stringsAsFactors = FALSE)
df

# [문제32] df변수에 james, 90, 80 추가 해주세요.

<화면출력>
  
  df name sql plsql 1 king 96 75 2 smith 82 91 3 jane 78 86 4 james 90 80

df[4,] <- c("james",90,80)  # vector로 하면 안됨 
df

# 선생님 풀이(list or data frame 사용), data.frame
df[4,] <- list("james", 90, 80)
y <- data.frame(name = "james", sql = 90, plsql = 80)
rbind(df,y)
df[4,] <- y
str(df)
df

# [문제33] james에 대한 row 정보만 출력하세요.

<화면출력>
  
  name sql plsql r 4 james 90 80 60

df$r <- list(NA,NA,NA,60)
df[4,]

# 선생님 풀이
df[df$name == 'james',]
str(df)

# 번외 연구
df1 <- data.frame(name = c("king","smith","jane"), 
                  sql = c(96,82,78),
                  plsql = c(75,91,86))
df1$name <- as.vector(df1$name)
x <- list("james",90,80)
df1 <- rbind(df1,x)
str(df1)
df1$name <- as.data.frame(df1$name)

# 복습. R 데이터 구조
  # vector : 같은 데이터 타입을 갖는 1차원 배열구조
  # matrix : 같은 데이터 타입을 갖는 2차원 배열구조
  # array : 같은 데이터 타입을 갖는 3차원 배열구조
  # list : 서로 다른 데이터 구조 갖는다.(중첩가능)
  # data.frame : 서로 다른 데이터 타입을 갖는 컬럼으로 이루어진 2차원 배열구조
  # factor : 목록

ex)

a <- c(1,2)
b <- matrix(c(1,2))
c <- array(1:12, dim = c(2,2,3))
d <- list(c("king", 100))
e <- data.frame(x = c(1,2))
f <- factor(c('male', 'female'))

a;b;c;d;e;f
class(a);class(b);class(c);class(d);class(e);class(f)

is.numeric(a)     # 숫자벡터 이냐? 
is.character(a)   # 문자벡터 이냐?
is.factor(f)      # factor 이냐?
is.matrix(b)      # matrix 이냐?
is.array(c)       # array 이냐?
is.data.frame(e)  # data.frame 이냐?
is.list(d)        # list 이냐?


## 3-1. data type 형변환

[ex1]
# 1st : char type vector 생성
x <- c("a","b","c")
str(x)

# 2nd : char -> factor
x <- as.factor(x)
str(x)

# 3rd : factor -> char
x <- as.character(x)
str(x)

[ex2]
# 1st : matrix 생성
x <- matrix(1:4,ncol = 2)
x
str(x)

# 2nd : matrix -> data.frame
x <- as.data.frame(x)
str(x)

# 3rd : data.frame -> matrix
x <- as.matrix(x)
str(x)

# 4th : matrix -> array
x <- as.array(x)
x
str(x)

[ex 3]
# 1st : 문자형 숫자를 가진 벡터는 연산이 되지 않음
x <- c("1","2")
x + 100

# 2nd : char -> num
x <- as.numeric(x)
x + 100

[ex 4]
# 1st : vector로 행추가 해버린 참사
df <- data.frame(name = c("king","smith","jane"), 
                 sql = c(96,82,78),
                 plsql = c(75,91,86),
                 stringsAsFactors = FALSE)
df <- rbind(df, c("james", 90, 80))
str(df)

# 2nd : daty type 행변환 column별 실시
df$name <- as.character(df$name)
df$sql <- as.numeric(df$sql)
df$plsql <- as.numeric(df$plsql)

name <- as.character(df$name)
sql <- as.numeric(df$sql)
plsql <- as.numeric(df$plsql)

df <- data.frame(name=name, sql=sql, plsql=plsql, stringsAsFactors = FALSE)

str(df)
df

# 3rd : 앞으로 list를 사용하렴
df[5,]
df[5,] <- list('scott',90,70)
df


## 3-2. read.csv
  # csv파일을 데이터프레임으로 읽어들이는 함수
  # working directory 지정(R notebook에서는 안됨, console)

getwd()
setwd("/Users/hbk/data")

# emp <- read.csv() : console에서 실시
emp <- read.csv("emp.csv", header = T, stringsAsFactors = F)
str(emp)
names(emp)      
str(names(emp))  # char
View(emp)

# [문제34] emp 변수에 있는 데이터 중에 급여가 3000 인 사원들의 last_name, salary를 출력하세요. 
#          (단, emp 변수에 컬럼정보를 확인하시고 수행하세요.)

names(emp)      # column name 
nrow(emp)       # row count
length(emp)     # column count
str(emp)
c(emp$SALARY)
emp[emp$SALARY==3000, c("LAST_NAME","SALARY")]

# [문제35] 급여가 2000 이상인 사원들의 last_name, salary를 출력하세요.

emp[emp$SALARY >= 2000, c("LAST_NAME","SALARY")]

# [문제36] job이 ST_CLERK인 사원들의 이름과 월급과 직업을 출력하세요.

c(emp$JOB_ID)
emp[emp$JOB_ID == 'ST_CLERK', c("LAST_NAME","SALARY","JOB_ID")]

# [문제37] job이 ST_CLERK이 아닌 사원들의 이름과 월급과 직업을 출력하세요.

emp[emp$JOB_ID != 'ST_CLERK', c('LAST_NAME','SALARY','JOB_ID')]

# [문제38] 오라클의 in 연산자와 비슷한 R연산자는?
  
emp$JOB_ID %in% c('AD_ASST','MK_MAN')

# [문제39] job이 AD_ASST, MK_MAN 인 사원들의 employee_id,last_name,job_id를 출력하세요.

emp[emp$JOB_ID %in% c('AD_ASST','MK_MAN'), c('EMPLOYEE_ID','LAST_NAME','JOB_ID')]

# [문제40] job이 ST_CLERK, SH_CLERK, SA_REP 아닌 사원들의 employee_id,last_name,job_id를 출력하세요.

emp[emp$JOB_ID != 'ST_CLERK' & emp$JOB_ID != 'SH_CLERK' & emp$JOB_ID !='SA_REP',c('EMPLOYEE_ID','LAST_NAME','JOB_ID')]

# ★ !...%in% : not in
emp[!emp$JOB_ID %in% c('ST_CLERK', 'SH_CLERK', 'SA_REP'), c('EMPLOYEE_ID','LAST_NAME','JOB_ID')]

# [문제41] 부서번호가 10번,20번인 사원들의 last_name, salary, department_id를 출력하세요.

emp[emp$DEPARTMENT_ID %in% c(10,20), c('LAST_NAME','SALARY','DEPARTMENT_ID')]

# [문제42] 오라클의 연결 연산자 와 비슷한 R 연산자는? 

오라클	R 
||      paste(문자 붙이기, paste0도 있다)

# [문제43] emp 변수에 있는 아래결과와 같이 출력되도록하세요.
#          Grant 의 직업은 SH_CLERK 입니다.

Grant <- emp[emp$LAST_NAME == 'Grant', c('LAST_NAME','JOB_ID')]
Grant <- Grant[Grant$JOB_ID == 'SH_CLERK',]

paste(Grant$LAST_NAME, '의 직업은', Grant$JOB_ID, '입니다.')
paste(emp$LAST_NAME, '의 직업은', emp$JOB_ID, '입니다.')[emp$LAST_NAME == 'Grant' & emp$JOB_ID == 'SH_CLERK']
paste(emp$LAST_NAME, '의 직업은', emp$JOB_ID, '입니다.')[1-107]

# 번외 연구
str(emp$JOB_ID)
x <- matrix(1:12, nrow = 3, byrow = TRUE)
x
paste(x, "dksk")
str(x)
str(paste(x))
emp$COMMISSION_PCT

# [문제44] oracle에서 null(알수없는 값, 적용할수 없는, 할당할수 없는 값) R에서 NA(결측치)를 체크하는 함수는? 초기값 없는 변수 oracle : is null, is not null ★ R : is.na()

x <- c(1, NA, 2)
is.na(x)

# [문제45] commission_pct에 NA 인 사원들의 last_name, salary, commission_pct를 출력하세요.

nrow(emp[is.na(emp$COMMISSION_PCT),])  # 72
emp[is.na(emp$COMMISSION_PCT), c('LAST_NAME','SALARY','COMMISSION_PCT')]

# [문제46] department_id에 NA 인 사원들의 last_name, salary, department_id를 출력하세요.

nrow(emp[is.na(emp$DEPARTMENT_ID),])  # 1
emp[is.na(emp$DEPARTMENT_ID), c('LAST_NAME','SALARY','COMMISSION_PCT')]

# [문제47] commission_pct에 NA가 아닌 사원들의 last_name, salary, commission_pct를 출력하세요.

nrow(emp[!is.na(emp$COMMISSION_PCT),])  # 35
emp[!is.na(emp$COMMISSION_PCT), c('LAST_NAME','SALARY','COMMISSION_PCT')]

# [문제48] 30번 부서 사원들이면서 급여는 3000이상인 사원들의 employee_id, salary, department_id를 출력하세요.

nrow(emp[emp$SALARY >= 3000 & emp$DEPARTMENT_ID == 30,])

emp[emp$SALARY >= 3000 & emp$DEPARTMENT_ID == 30, c('EMPLOYEE_ID','SALARY','DEPARTMENT_ID')][1:2,]

# 선생님 풀이 : na.omit(), NA 없애고 출력
na.omit(emp[emp$SALARY >= 3000 & emp$DEPARTMENT_ID == 30, c('EMPLOYEE_ID','SALARY','DEPARTMENT_ID')])

# [문제49] 20번부서 사원이면서 급여는 10000를 초과한 사원 또는 급여가 2500 미만의 사원들의 employee_id, salary, department_id를 출력하세요.

nrow(emp[emp$DEPARTMENT_ID == 20 & (emp$SALARY > 10000 | emp$SALARY < 2500),])

emp[emp$DEPARTMENT_ID == 20 & (emp$SALARY > 10000 | emp$SALARY < 2500), c('EMPLOYEE_ID','SALARY','DEPARTMENT_ID')]


## 3-3. grep함수 : 문자 패턴을 찾을 때 사용되는 함수
  # ^ : 첫번째 
  # $ : 마지막
  # . : 한자리수
  # * : wild card(%)

# ex) ignore.case = TRUE : 대소문자 구분없이

emp[grep("aa", emp$LAST_NAME), c("LAST_NAME","SALARY")]

emp[grep("[x-z]", emp$LAST_NAME, ignore.case = TRUE), c("LAST_NAME","SALARY")]

emp[grep("[x-z]", emp$LAST_NAME, ignore.case = FALSE), c("LAST_NAME","SALARY")]

# [문제50]last_name의 첫번째 글자가 A 로 시작하는 사원들의 last_name, salary를 출력하세요.

emp[grep('^a', emp$LAST_NAME, ignore.case = TRUE), c("LAST_NAME","SALARY")]

# [문제51]last_name의 끝글자가 g 로 끝나는 사원들의 last_name, salary를 출력하세요.

emp[grep('g$', emp$LAST_NAME, ignore.case = TRUE), c("LAST_NAME","SALARY")]

# [문제52]last_name의 z 를 포함하고 있는 사원들의 last_name, salary를 출력하세요.

emp[grep('z', emp$LAST_NAME), c("LAST_NAME","SALARY")]

# [문제53]last_name의 두번째 철자가 u 인 사원들의 last_name, salary를 출력하세요.

emp[grep('^.u', emp$LAST_NAME), c("LAST_NAME","SALARY")]

# 번외 연구
emp[grep('u', emp$LAST_NAME), c("LAST_NAME","SALARY")]
emp[grep('^A', emp$LAST_NAME), c("LAST_NAME","SALARY")]
emp[grep('*g$', emp$LAST_NAME), c("LAST_NAME","SALARY")]


## 3-4. 중복제거 log
unique(변수$컬럼명)

# [문제54] 부서번호를 중복제거해서 출력하세요.

emp$DEPARTMENT_ID
unique(emp$DEPARTMENT_ID)


## 3-5. 정렬
  # data frame 에서 order 옵션을 사용, 기본값은 오름차순
  # order를 사용하면 행을 제한하는 조건을 사용불가 행을 조건절로 만든 변수를 다른 변수에 담아놓고 order 수행

ex)

emp[order(emp$SALARY), c("EMPLOYEE_ID","LAST_NAME","SALARY")]
emp[order(emp$SALARY, decreasing=T), c("EMPLOYEE_ID","LAST_NAME","SALARY")]

# [문제55] last_name, hire_date를 출력하는데 먼저 입사한 사원부터 출력하세요.

emp[order(emp$HIRE_DATE), c('DEPARTMENT_ID','LAST_NAME','HIRE_DATE')]
emp[emp$SALARY >= 10000, c('DEPARTMENT_ID','LAST_NAME','HIRE_DATE')][order(emp$HIRE_DATE),]

# [문제56] 30번 부서 사원들의 last_name, salary 를 출력하세요. 단, salary를 기준으로 내림차순정렬 하세요.

emp_30 <- emp[emp$DEPARTMENT_ID == 30, ]
emp_30
na.omit(emp_30[order(emp_30$SALARY, decreasing=T), c('DEPARTMENT_ID','LAST_NAME','SALARY')])
emp[emp$DEPARTMENT_ID == 30, c('DEPARTMENT_ID','LAST_NAME','SALARY')][,,order(emp$SALARY)]
