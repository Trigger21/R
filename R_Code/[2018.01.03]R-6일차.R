# R-6일차(2018.1.3)
  # 6-1. 그룹함수
  # 6-2. aggregate 함수
  # 6-3. apply
  # 6-4. lapply
  # 6-5. do.call
  # 6-6. sapply
  # 6-7. tapply


## 6-1. 그룹함수
x <- c(100,90,88,70)

# 합
sum(x)

# 평균
mean(x)

# 분산
var(x)

# 표준편차
sd(x)

# 최대값
max(x)

# 최소값
min(x)

# 갯수
length(x)
NROW(x)

x <- c(100,90,88,65,NA)

sum(x)
sum(x,na.rm = T)  # NA 제외한 합

mean(x,na.rm = T)  # NA를 0으로 대체한 결과와 다름

var(x,na.rm = T)

sd(x,na.rm = T)

max(x,na.rm = T)

min(x,na.rm = T)

length(x)

# [문제87] 최대월급을 출력하세요

max(emp$SALARY)
min(emp$SALARY)
mean(emp$SALARY)
sd(emp$SALARY)
sqrt(var(emp$SALARY))
var(emp$SALARY)
length(emp$SALARY)

# [문제88] 직업이 ST_CLERK 인 사원들중에 최대월급을 출력하세요

sal <- emp[emp$JOB_ID == 'ST_CLERK', c("SALARY")]
max(sal)
min(sal)
mean(sal)
var(sal)
sqrt(var(sal))
sd(sal)
sum(sal)
length(sal)

# [문제89] 커미션이 NA 가 아닌 사원들의 월급중에서 최대값을 출력하세요

max(emp[!is.na(emp$COMMISSION_PCT), c("SALARY")])
min(emp[!is.na(emp$COMMISSION_PCT), c("SALARY")])
mean(emp[!is.na(emp$COMMISSION_PCT), c("SALARY")])

# [문제90] total 컬럼을 생성해서 sql과 python 의 총합을 구하세요.

df <- data.frame(name=c('king', 'smith', 'jane', 'scott'), sql=c(96,NA,78,90), python=c(75,91,86,NA),stringsAsFactors=F)

df name sql python 1 king 96 75 2 smith NA 91 3 jane 78 86 4 scott 90 NA

df <- data.frame(name = c('king', 'smith', 'jane', 'scott'), 
                 sql = c(96,NA,78,90), 
                 python = c(75,91,86,NA), stringsAsFactors=F)
df

# sol.1
total_sum <- c(1:nrow(df))

for(i in 1:nrow(df)){
  
  if (is.na(df$sql[i])){
    df$sql[i] <- 0
  }else if(is.na(df$python[i])){
    df$python[i] <- 0
  }
  
  total_sum[i] <- df$sql[i] + df$python[i]
  
}
total_sum
df$total <- total_sum
df[df$name == 'smith', "sql"] <- NA
df[df$name == 'scott', "python"] <- NA
df

# sol.2
for(i in 1:nrow(df)){
  df$total[i] <- sum(df[i, c("sql","python")], na.rm = T)
}

df

# sol.3
total <- ifelse(is.na(df$sql),0,df$sql) + ifelse(is.na(df$python),0,df$python)
cbind(df,total)


## 6-2. aggregate 함수 (~sql group by)
  # 데이터를 분할하고 각 그룹으로 묶은후 그룹함수 적용
  # aggregate(계산될 컬럼~분할해야할 기준 컬럼, 데이터, 함수)

ex) JOB_ID별로 급여 총액 구하세요

aggregate(SALARY~JOB_ID, emp, sum)
aggregate(SALARY[1:50]~JOB_ID[1:50], emp[emp$EMPLOYEE_ID <= 200,], sum)

# [문제91] 부서번호별로 급여에 총액을 출력하세요.

aggregate(SALARY~DEPARTMENT_ID, emp, sum)
부서정보가 없는 사원(grant)의 급여정보도 나타내려고 할 때
rbind(aggregate(SALARY~DEPARTMENT_ID, emp, sum),
      c(NA, emp[is.na(emp$DEPARTMENT_ID), 'SALARY']))

# [문제92] 부서번호, 직업별로 급여에 총액을 출력하세요.

aggregate(SALARY~DEPARTMENT_ID+JOB_ID, emp, sum)
orderBy(~DEPARTMENT_ID + JOB_ID, aggregate(SALARY~DEPARTMENT_ID+JOB_ID, emp, sum))
x <- aggregate(SALARY~DEPARTMENT_ID+JOB_ID, emp, sum)
y <- emp[is.na(emp$EMPLOYEE_ID), c('JOB_ID', 'SALARY')]
x <- rbind(x, c(NA, y$JOB_ID, y$SALARY))
x

# [문제93] 부서번호별 최대월급을 출력하는데 최대월급이 높은것부터 출력하세요.

orderBy(~-SALARY, aggregate(SALARY~DEPARTMENT_ID, emp, max))

# [문제94] 직업별 인원수를 출력하세요.

aggregate(EMPLOYEE_ID~JOB_ID, emp, length)


## 6-3. apply
  # 행렬, 배열, 데이터프레임에 함수를 적용한 결과를 벡터, 리스트, 배열 형태로 리턴한다.
  # 행렬이나 행이나 열의 방향으로 함수를 적용
  # apply(x, MARGIN, FUN) 
    # x : 행렬, 배열, 데이터프레임 MARGIN : 함수를 적용할 때 방향을 지정 
    # (1 : 행방향, 2 : 열방향, c(1,2) : 행&열) 
    # FUN : 적용할 함수(sum, mean, var, sd, max, min)

ex.1)

m <- matrix(1:4, ncol=2)
m
dim(m)
apply(m, 1, sum)
apply(m, 2, sum)
apply(m, 1, mean)
apply(m, 2, mean)

ex.2)

ex_2 <- data.frame(name = c('king','smith','jane'),
                   sql = c(90,80,70),
                   python = c(75,90,86))
ex_2
apply(ex_2[,(2:3)], 1, sum)
apply(ex_2[,(2:3)], 1, mean)
apply(ex_2[,(2:3)], 1, var)
apply(ex_2[,(2:3)], 1, sd)
apply(ex_2[,(2:3)], 1, min)
apply(ex_2[,(2:3)], 1, max)
apply(ex_2[,(2:3)], 2, sum)
apply(ex_2[,(2:3)], 2, mean)
apply(ex_2[,(2:3)], 2, var)
apply(ex_2[,(2:3)], 2, sd)
apply(ex_2[,(2:3)], 2, min)
apply(ex_2[,(2:3)], 2, max)

ex.3)

df <- data.frame(name = c('king','smith','jane'),
                 sql = c(90,80,70),
                 python = c(75,90,NA))
df
apply(df[,(2:3)], 1, sum, na.rm = T)  # row sum
apply(df[,(2:3)], 2, sum, na.rm = T)  # column sum

# rowSums() : 배열, 행렬, 데이터 프레임의 행의 합
# rowMeans() : 배열, 행렬, 데이터 프레임의 행의 평
rowSums(df[,2:3], na.rm = T)
rowMeans(df[,2:3], na.rm = T) 

# colSums() : 배열, 행렬, 데이터 프레임의 열의 합
# colMeans() : 배열, 행렬, 데이터 프레임의 열의 평
colSums(df[,2:3], na.rm = T)
colMeans(df[,2:3], na.rm = T) 


# [문제95] total 컬럼을 생성해서 sql과 python 의 합을 구하세요. (단 apply함수를 이용하세요)

df <- data.frame(name=c('king', 'smith', 'jane', 'scott'), sql=c(96,NA,78,90), python=c(75,91,86,NA),stringsAsFactors=F)

df <- data.frame(name = c('king','smith','jane','scott'),
                 sql = c(96,NA,78,90),
                 python = c(75,91,86,NA),
                 stringsAsFactors = F)
df
total <- apply(df[,c(2,3)], 1, sum, na.rm = T)
df <- cbind(df,total)
df

# 선생님 풀이
apply 결과(행의 합)를 Total column 저장과 동시에 cbind해서 저장
apply 결과(열의 합)를 'Total'을 name column 넣어주면서 rbind해서 저장
df <- cbind(df, Total = apply(df[,c(2,3)], 1, sum, na.rm = TRUE))   
df <- rbind(df, c('Total', apply(df[,2:4], 2, sum, na.rm = TRUE)))  
df

# 번외 연구
avg <- apply(df[,c(2,3)], 1, mean, na.rm = T)
avg
cbind(df,avg)
list(1:3, c("a","b","c"))
rowSums(df[,c(2,3)], na.rm = T)
rowMeans(df[,c(2,3)], na.rm = T)
cbind(cbind(df, rowSums(df[,c(2,3)], na.rm = T)),rowMeans(df[,c(2,3)], na.rm = T))


## 6-4. lapply
  # 벡터, 리스트, 데이터 프레임에 함수를 적용하고 그 결과를 리스트로 리턴하는 함수
  # (list : 서로 다른 데이터타입에 값을 저장하는 기능)

x <- list(a = 1:3, b = 4:6)
x
str(x)
mean(x$a)
mean(x$b)

lapply(x, mean)  # x(list)에 mean() 적용후 list로 리턴받음

df <- data.frame(name = c('king','smith','jane','scott'),
                 sql = c(95,NA,70,90),
                 python = c(75,90,85,NA))
df

lapply(df[,2:3], mean, na.rm = T)    # list
# 비슷한 작동예
apply(df[,2:3], 2, mean, na.rm = T)  # data.frame
colMeans(df[,2:3], na.rm = T)        # data.frame

# ☆ data.frame -> list -> data.frame 변환 (중요)

# 1. unlist() : list -> vector
lapply(df[,2:3], mean, na.rm = T)              # list
unlist(lapply(df[,2:3], mean, na.rm = T))      # vector
str(unlist(lapply(df[,2:3], mean, na.rm = T)))

# 2. matrix() : vector -> matrix
matrix(unlist(lapply(df[,2:3], mean, na.rm = T)), nrow = 1)
matrix(unlist(lapply(df[,2:3], mean, na.rm = T)), ncol = 2,byrow = T)

# 3. as.data.frame() : matrix -> data.frame
d <- as.data.frame(matrix(unlist(lapply(df[,2:3], mean, na.rm = T)), ncol = 2,byrow = T))
str(d)
names(d) <- c("avg_sql", "avg_py")
d


## 6-5. do.call
  # list 함수에 계산함수를 적용해서 결과를 리턴가능하게 하는 함수

df
l <- lapply(df[,2:3], sum, na.rm = T)
l    # list

sum(l)  # error

sum(unlist(l))   # 1
do.call(sum, l)  # 2

# as.data.frame(lapply(df[,2:3], mean, na.rm = T))
do.call(cbind, lapply(df[,2:3], mean, na.rm = T))

ex)

x <- list(data.frame(name = 'scott', sal = 100),
          data.frame(name = 'king', sal = 200))
x
unlist(x)
do.call(rbind, x)
str(do.call(rbind,x))


## 6-6. sapply
  # 벡터, 리스트, 데이터 프레임에 함수를 적용하고 그 결과를 벡터로 리턴하는 함수

sapply(df[,2:3], mean, na.rm = T)
sum(sapply(df[,2:3], mean, na.rm = T))


## 6-7. tapply
  # 벡터, 데이터 프레임에 저장된 데이터를 주어진 기준에 따라 그룹으로 묶은 뒤
  # 그룹함수를 적용하고 그 결과를 리턴하는 함수
  # tapply(계산할 열, 그룹으로 묶어야 될 열, 계산할 함수)

tapply(emp$SALARY, emp$DEPARTMENT_ID, sum)
str(tapply(emp$SALARY, emp$DEPARTMENT_ID, sum))  # array, list


# [문제96] 부서, 직업별 급여 총액을 구하세요. (aggregate, tapply)

aggregate(SALARY~DEPARTMENT_ID+JOB_ID, emp, sum)
tapply(emp$SALARY, list(emp$DEPARTMENT_ID,emp$JOB_ID), sum)  # cross table, array


# [문제97] 96번문제의 결과를 x변수에 저장한 후 NA값들을 0으로 설정해주세요.

x <- aggregate(SALARY~DEPARTMENT_ID+JOB_ID, emp, sum)
x
rbind(x,c(NA,NA,0))
x <- tapply(emp$SALARY, list(emp$DEPARTMENT_ID,emp$JOB_ID), sum)
x
is.na(x)
ifelse(is.na(x),0,x)

# 선생님 풀이
tapply(emp$SALARY, list(emp$DEPARTMENT_ID,emp$JOB_ID), sum, default = 0)


# [문제98] job_id, hire_date(년도4자리) 총액 급여를 구하시고 NA 대신에 0 으로 출력하세요.(tapply함수사용)

ex_96 <- tapply(emp$SALARY, list(emp$JOB_ID, year(emp$HIRE_DATE)), sum)
ifelse(is.na(ex_96), 0, ex_96)
tapply(emp$SALARY, list(emp$JOB_ID, year(emp$HIRE_DATE)), sum, default = 0)


# [문제99] 부서별 인원수를 구하세요.(tapply 함수 사용)

tapply(emp$EMPLOYEE_ID, emp$DEPARTMENT_ID, length)
as.data.frame(tapply(emp$EMPLOYEE_ID, emp$DEPARTMENT_ID, length))


# [문제100] 1부터 100까지 짝수합, 홀수합을 tapply를 이용해서 구하세요.

s <- tapply(1:100, 1:100 %% 2 == 0, sum)
s
names(s) <- c("odd", "even")
s
tapply(1:100, 1:100 %% 2 == 1, sum)

# 번외 연구
df
as.integer(emp$LAST_NAME)
df <- data.frame(name = c('king','smith','jane','scott'),
                 sql = c(95,80,70,90),
                 python = c(75,90,85,84))
df
df[['sql']]
plot(df$sql, df$python, pch = as.integer(df$name), col = as.integer(df$name))
legend("topright", levels(df$name), pch = 1:length(levels(df$name)), col = as.integer(df$name))

# apply : matrix
mat <- matrix(1:9,3,3)
mat

apply(mat, 1, sd)
rowMeans(mat)

# apply : array
arr <- array(1:27, c(3,3,3))
arr

apply(arr, 1, var)
rowMeans(arr)
var(c(1,4,7,10,13,16,19,22,25))
