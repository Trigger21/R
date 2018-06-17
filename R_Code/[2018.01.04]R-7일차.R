## R-7일차(2018.1.4)
  # 7-1. merge
  # 7-2. split
  # 7-3. subset

#[문제101] 직업별 평균월급을 세로(aggregate), 가로(tapply)로 출력하세요.

aggregate(SALARY~JOB_ID, emp, mean)
tapply(emp$SALARY, emp$JOB_ID, mean)


#[문제102] 입사한 년도별 평균월급을 세로(aggregate), 가로(tapply)로 출력하세요.

library("doBy")
library("lubridate")
aggregate(SALARY~year(HIRE_DATE), emp, mean)
tapply(emp$SALARY, year(emp$HIRE_DATE), mean)
#번외 연구
plot(emp$DEPARTMENT_ID, emp$SALARY, col = 'red')
cor.test(emp$EMPLOYEE_ID, emp$SALARY)
abline(lm(emp$EMPLOYEE_ID~emp$SALARY))


#[문제103] 요일별 입사한 인원수를 아래와 같은 화면 출력으로 하세요.

#월 화 수 목 금 토 일 10 13 15 16 19 19 15

wday(as.Date(emp$HIRE_DATE), week_start = 1, label = T)

tapply(emp$EMPLOYEE_ID, wday(as.Date(emp$HIRE_DATE), week_start = 1, label = T), length)
tapply(emp$EMPLOYEE_ID, wday(as.Date(emp$HIRE_DATE), week_start = 1, label = T), NROW)
연습장(date function)
x <- tapply(emp$EMPLOYEE_ID, format(as.Date(emp$HIRE_DATE), '%u'), length)
names(x) <- c("월", "화", "수", "목", "금", "토", "일")
x
wday(as.Date(emp$HIRE_DATE), week_start = 1, label = T)
as.Date(emp$HIRE_DATE)
aggregate(emp$HIRE_DATE, list(wday(as.Date(emp$HIRE_DATE), week_start = 1, label = T)), length)

levels(as.factor(emp$HIRE_DATE))

length(emp$HIRE_DATE)

## 7-1. merge
# 두 데이터프레임의 공통된 값을 기준으로 병합한다.

x1 <- data.frame(id = c(100,200,300), sql = c(90,70,80))
x2 <- data.frame(id = c(100,200,500), python = c(80,60,70))

# 컬럼이름이 다르면 rbind 되지 않는다.
rbind(x1,x2)  # error

# cbind : 동일한 컬럼이름까지 중복된다.(error)
cbind(x1,x2)

# case1.
merge(x1, x2)

merge(x1, x2, all = T)      # full outer join

merge(x1, x2, all.x = T)    # left outer join
merge(x1, x2, all.y = T)    # right outer join

# case2
x1
x3 <- data.frame(no = c(100,200,500), python = c(80,60,70))
x3
merge(x1, x3)   # cartesian product

merge(x1, x3, by.x = 'id', by.y = 'no')

# case3
x1
x2
merge(x1, x2, by = 'id')
merge(x1, x2, by = 'no')
merge(x2, x3, by.x = "id", by.y = "no")

#[문제104] last_name, salary, department_name을 출력하세요.

merge(emp, dept, by = "DEPARTMENT_ID", all.x = T)[, c("LAST_NAME", "SALARY", "DEPARTMENT_NAME")]


#[문제105] 20번 부서에 소속되어 있는 사원의 last_name, salary, job_id, department_name을 출력해주세요.

#sol.1
m <- merge(emp, dept, by = "DEPARTMENT_ID", all.x = T)

m[m$DEPARTMENT_ID == 20, c("LAST_NAME", "SALARY", "JOB_ID", "DEPARTMENT_NAME")]

#sol.2
m <- merge(emp[emp$DEPARTMENT_ID == 20,], dept[dept$DEPARTMENT_ID == 20, ], by = "DEPARTMENT_ID", all.x = T)

m[m$DEPARTMENT_ID == 20, c("LAST_NAME", "SALARY", "JOB_ID", "DEPARTMENT_NAME")]

#번외 연구
merge(emp[emp$DEPARTMENT_ID == c('20'), ], dept, by="DEPARTMENT_ID")[
  ,c("LAST_NAME","SALARY","JOB_ID","DEPARTMENT_NAME")]


#선생님 풀이.1(동일한 컬럼명이 2개 이상 있다면 by ="컬럼명" 기술하자)
merge(
  emp[emp$DEPARTMENT_ID == 20, c("LAST_NAME", "SALARY", "JOB_ID","DEPARTMENT_ID")],
  dept[dept$DEPARTMENT_ID == 20, ], by = "DEPARTMENT_ID"
)[, c("LAST_NAME", "SALARY", "JOB_ID", "DEPARTMENT_NAME")]


#선생님 풀이.2
emp_20 <- emp[emp$DEPARTMENT_ID == 20, c("LAST_NAME","SALARY","JOB_ID","DEPARTMENT_ID")]
dept_20 <- dept[dept$DEPARTMENT_ID == 20, ]

merge(emp_20, dept_20)[, c("LAST_NAME","SALARY","JOB_ID","DEPARTMENT_NAME")]
options(max.print = 100)
options(max.print = 99999999)


#[문제106] salary가 3000 이상이고 job_id는 ST_CLERK인 사원들의 employee_id, salary, job_id, department_id를 출력하세요.

v_emp <- emp[emp$SALARY >= 3000 & emp$JOB_ID == "ST_CLERK", 
             c("EMPLOYEE_ID", "SALARY", "JOB_ID", "DEPARTMENT_ID")]
v_emp


#[문제107] salary가 3000 이상이고 job_id는 ST_CLERK인 사원들의 employee_id, salary, job_id, department_id, department_name을 출력하세요.

merge(v_emp, dept, by = "DEPARTMENT_ID")[, c("EMPLOYEE_ID", "SALARY", "JOB_ID", "DEPARTMENT_ID", "DEPARTMENT_NAME")]


#[문제108] 부서이름별 총액 급여를 출력하세요.

dept_sal <- aggregate(SALARY~DEPARTMENT_ID, emp, sum)  # merge 일량 감소
dept_sal

merge(dept, dept_sal)[,c("DEPARTMENT_NAME", "SALARY")]
t <- tapply(emp$SALARY, emp$DEPARTMENT_ID, sum)
t

s <- merge()


#[문제109] 부서이름,직업별 급여의 총액을 구하세요.

sal_DJ <- aggregate(SALARY~DEPARTMENT_ID + JOB_ID, emp, sum)
mer_DJ <- merge(sal_DJ, dept[,c("DEPARTMENT_NAME", "DEPARTMENT_ID")])[,c("DEPARTMENT_NAME", "JOB_ID", "SALARY")]
names(mer_DJ) <- c("DEPT_NAME", "JOB", "SUM_SAL")
mer_DJ
sal_DJ <- aggregate(emp$SALARY~DEPARTMENT_ID+emp$JOB_ID, emp, sum)
names(sal_DJ)[2:3] <- c("JOB", "SUM_SAL")
sal_DJ

mer_DJ <- merge(sal_DJ, dept[,c("DEPARTMENT_NAME", "DEPARTMENT_ID")])[,c("DEPARTMENT_NAME", "JOB", "SUM_SAL")]
names(mer_DJ)[1] <- "DEPT_NAME"
mer_DJ


#[문제110] 커미션이 NA 인 사원들의 last_name, commission_pct, department_id, department_name을 출력하세요.

comm_NA <- emp[is.na(emp$COMMISSION_PCT), c("LAST_NAME", "COMMISSION_PCT", "DEPARTMENT_ID")]

merge(comm_NA, dept[,c("DEPARTMENT_ID","DEPARTMENT_NAME")])[,c("LAST_NAME", "COMMISSION_PCT", "DEPARTMENT_ID", "DEPARTMENT_NAME")]


#[문제111] 커미션이 NA가 아닌 사원들의 last_name, commission_pct,department_id, department_name을 출력하세요.

comm_NNA <- emp[!is.na(emp$COMMISSION_PCT), c("LAST_NAME", "COMMISSION_PCT", "DEPARTMENT_ID")]

merge(comm_NNA, dept[,c("DEPARTMENT_ID","DEPARTMENT_NAME")])[,c("LAST_NAME", "COMMISSION_PCT", "DEPARTMENT_ID", "DEPARTMENT_NAME")]


#[문제112] 부서번호가 10,20번인 사원들의 last_name,salary, department_id, department_name을 출력하세요.

dept_10_20 <- emp[emp$DEPARTMENT_ID %in% c(10,20), c("LAST_NAME", "SALARY", "DEPARTMENT_ID")]

merge(dept_10_20, dept[dept$DEPARTMENT_ID %in% c(10,20),c("DEPARTMENT_ID","DEPARTMENT_NAME")])[,c("LAST_NAME", 'SALARY', "DEPARTMENT_ID", "DEPARTMENT_NAME")]


#[문제113]커미션이 NA가 아닌 사원들의 last_name, commission_pct, department_id, department_name을 출력하세요. 단 department_id가 NA인 사원도 출력해주세요.

comm_NNA <- emp[!is.na(emp$COMMISSION_PCT), c("LAST_NAME", "COMMISSION_PCT", "DEPARTMENT_ID")]

merge(comm_NNA, dept[,c("DEPARTMENT_ID","DEPARTMENT_NAME")], all.x = T)[,c("LAST_NAME", "COMMISSION_PCT", "DEPARTMENT_ID", "DEPARTMENT_NAME")]


#[문제114] loc.csv 파일을 loc 변수로 로드하세요. Toronto 지역에 근무하는 사원들의 'LAST_NAME','SALARY','DEPARTMENT_ID','DEPARTMENT_NAME','STREET_ADDRESS' 정보를 출력하세요.

# loc <- read.csv("C:/R/loc.csv", header = T, stringsAsFactors = F)

loc$CITY

emp_LSD <- emp[,c("LAST_NAME","SALARY","DEPARTMENT_ID")]
dept_INL <- dept[,c("DEPARTMENT_ID", "DEPARTMENT_NAME", "LOCATION_ID")]
loc_IS <- loc[,c("LOCATION_ID","STREET_ADDRESS")]

mer_ED <- merge(emp_LSD, dept_INL)

merge(mer_ED, loc_IS)[,c('LAST_NAME','SALARY','DEPARTMENT_ID','DEPARTMENT_NAME','STREET_ADDRESS')]


#[문제115] 사원의 last_name, 관리자 last_name을 출력해주세요. 관리자가 없는 사원도 출력해주세요.

#sol.1
e_id <- emp[,c("LAST_NAME", "MANAGER_ID")]
e_mgr <- emp[,c("LAST_NAME", "EMPLOYEE_ID")]
merge(e_id, e_mgr, by.x = "MANAGER_ID", by.y = "EMPLOYEE_ID", all.x = T)[,c("LAST_NAME.x","LAST_NAME.y")]
id_mgr <- aggregate(emp$MANAGER_ID~emp$EMPLOYEE_ID, emp, print)

names(id_mgr) <- c("EMPLOYEE_ID", "MANAGER_ID")

id_mgr

emp[,c("EMPLOYEE_ID", "MANAGER_ID")]
merge(emp[,c("LAST_NAME","EMPLOYEE_ID","MANAGER_ID")], id_mgr)


## 7-2. split()
# 조건에 따라 데이터를 분리

# split(데이터, 조건)
split(emp, emp$DEPARTMENT_ID)
split(emp$SALARY, emp$DEPARTMENT_ID)

lapply(split(emp$SALARY, emp$DEPARTMENT_ID), mean)


## 7-3. subset()
# - 조건에 만족하는 데이터를 선택
# - NA행 안 나옴

# ex 1)

emp[emp$DEPARTMENT_ID == 30, ]  # select * from employees where department_id = 30;
subset(emp, DEPARTMENT_ID == 30)

# ex 2)

emp[emp$DEPARTMENT_ID == 30 & emp$SALARY >= 3000, c("LAST_NAME", "SALARY", "DEPARTMENT_ID")]
subset(emp, DEPARTMENT_ID == 30 & SALARY >= 3000, select = c(LAST_NAME, SALARY, DEPARTMENT_ID))
subset(emp, DEPARTMENT_ID == 30 & SALARY >= 3000, select = c('LAST_NAME', 'SALARY', 'DEPARTMENT_ID'))
subset(emp, DEPARTMENT_ID == 30 & SALARY >= 3000, c(LAST_NAME, SALARY, DEPARTMENT_ID))

# ex 3) 전체중 제외할 컬럼만 지정해서 나머지를 보는 것

emp[,!names(emp) %in% c('LAST_NAME', 'SALARY', 'DEPARTMENT_ID')]
subset(emp, select = -c(LAST_NAME, SALARY, DEPARTMENT_ID))


## 피보나치 수열
fibo <- function(n){
  if(n==1||n==2){
    return(1)
  }
  return(fibo(n-1) + fibo(n-2))   # f_n = f_(n-1) + f_(n-2)
}
for(i in 1:10){
  print(fibo(i))
}

fibo(10)
help(lowess)
