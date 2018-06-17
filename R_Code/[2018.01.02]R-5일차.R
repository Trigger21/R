# R-5일차(2018-1-2)
  # 5-1. round, trunc
  # 5-2. date calculus


# [문제72] x <- c(2,10,6,4,3,NA,7,9,1) x변수에 NA가 있는지를 검사하세요.

x <- c(2,10,6,4,3,NA,7,9,1)
is.na(x)

# NULL : R에서 초기화 되지 않았음

# [문제73] x 변수에 NA가 있는 인덱스 번호를 찾아 주세요.

x[is.na(x)]
for(i in 1:length(x)){
  ifelse(is.na(x[i]), print(i), 0) 
}
which() 함수 활용
which(is.na(x))

# [문제74] x 변수에 NA가 있으면 0으로 설정하세요

x[is.na(x)] <- 0
x
ifelse() 사용 : NA가 있을때 대체값 넣겠다면 이거 사용 (sql nvl2처럼 사용)
ifelse(is.na(x),0,x)

# [문제75] last_name, salary, commission_pct, commission_pct NA 면 salary * 12, 아니면 (salary * 12) + (salary * 12 * commission_pct)을 수행하세요.

head(df) name sal comm ann_sal 1 OConnell 2600 NA 31200 2 Grant 2600 NA 31200 3 Whalen 4400 NA 52800 4 Hartstein 13000 NA 156000 5 Fay 6000 NA 72000 6 Mavris 6500 NA 78000

emp[is.na(emp$COMMISSION_PCT), c("LAST_NAME", "SALARY", "COMMISSION_PCT")]
df <- data.frame(name=emp$LAST_NAME, sal=emp$SALARY, commission_pct=emp$COMMISSION_PCT,
                 ann_sal=ifelse(is.na(emp$COMMISSION_PCT), emp$SALARY * 12,
                                (emp$SALARY * 12) + (emp$SALARY * 12 * emp$COMMISSION_PCT))
)
head(df)


## 5-1-1. round() : 소숫점에서는 ~5 반올림 안함(6이상부터 함)
round(45.926, 2)  # 45.93
round(45.925, 2)  # 45.92
round(45.925, 1)  # 45.9
round(45.925, 0)  # 46
round(45.925, -1)  # 50 : 정수는 5에서 반올림 가능 
round(45.925, -2)


## 5-1-2. trunc() : 무조건 소숫점 버리는 함수
trunc(45.926,2)
trunc(45.926,1)
trunc(45.926,0)
trunc(45.926,-1)
trunc(45.926,-2)
trunc(45.926,-3)

# sql VS R
- mod     %%
  - power   **,^

# [문제76] 6의 9승을 출력하세요

6^9

# [문제77] 10을 3으로 나눈 나머지값을 출력하세요

10 %% 3

# [문제78] last_name, salary에 12를 곱해서 출력하고 컬럼명이 한글로 연봉으로 데이터 프레임으로 출력하세요

paste(emp$LAST_NAME,' ',emp$SALARY * 12)  # test

df <- data.frame(emp$LAST_NAME, emp$SALARY * 12)
names(df) <- c("last_name", "연봉")

df

# [문제79] last_name과 연봉을 출력하는데 연봉이 높은것부터 출력하세요

orderBy(~-연봉, df)

# [문제80] 위의 결과를 다시 출력하는데 round 함수를 이용해서 아래와 같이 백단위에서 반올림되게 하세요.

4: JONES 35700 -----> 36000

round(35700, -3)  # test

df[,"연봉"] <- round(emp$SALARY * 12, -3)
df[,"연봉"]

orderBy(~-연봉, df)
df$연봉 <- round(emp$SALARY * 12, -3)
df


## 5-2-1. 현재 날짜시간
Sys.Date()
Sys.time()
date()
Sys.Date()
Sys.time()
date()


## 5-2-2. as.Date : 문자날짜를 날짜형으로 변환하는 함수
as.Date('2018-01-02')
as.Date('2018-01-02')
as.Date('2018/01/02')
as.Date('20180102')  # error
as.Date('날짜', format = '%Y%m%d')
as.Date('20180102', format = '%Y%m%d')
as.Date('2018.01.02', format = '%Y.%m.%d')
as.Date('180102', format = '%y%m%d')
as.Date('18/01/02', format = '%D')   # %D = %y/%m/%d

# ☆ format

%Y : 세기를 포함한 년도(4자리) ex. 2018
%y : 세기를 생략한 년도(2자리) ex. 18

%m : 숫자달
%B : 문자달

%d : 일

%A : 요일
%u : 숫자요일 1 ~ 7 (월요일 1, 화요일 2, ... , 일요일 7)
%w : 숫자요일 0 ~ 6 (일요일 0, 월요일 1, ... , 토요일 6)

%H : 시
%M : 분
%S : 초
as.Date('2018년 1월 2일', format = '%Y년%m월%d일')
as.Date('2018년 1월 2일', format = '%Y년 %B %d일')


## 5-2-3. format 함수
format(Sys.time(), '%y%m%d')
format(Sys.time(), '%y %B %d')
format(Sys.time(), '%y%m%d %A')

format(Sys.time(), '%Y%m%d %u')
format(Sys.time(), '%H%M%S')

format(Sys.time(), '%w')


## 5-2-4. weekdays 함수 : 요일을 출력
weekdays(Sys.Date())  # 서버날짜의 요일
weekdays(as.Date('2017-12-31', format = '%Y-%m-%d'))


## 5-2-5. 날짜 계산
as.Date('2018-01-02') + 30                     # date + num = date 
as.Date('20180102', format = '%Y%m%d') - 30    # date - num = date
as.Date('2018-01-02') - as.Date('2017-12-31')  # date - date = Time difference of OO days
as.numeric(as.Date('2018-01-02') - as.Date('2017-12-31'))   # date - date = num


## 5-2-6. difftime 함수 : 두 날짜간의 일수
difftime('2018-01-02', '2017-12-31')
difftime('2017-12-31', '2018-01-02')
as.numeric(difftime('2018-01-02 14:00:00', '2017-12-31 23:59:59'))


## 5-2-7. as.POSIXct : 년월일시분초까지 사용하는 함수(as.Date 비슷)
as.POSIXct('2018-01-02 14:00:00') - as.POSIXct('2017-12-31 23:59:59')
as.numeric(as.POSIXct('2018-01-02 14:00:00') - as.POSIXct('2017-12-31 23:59:59'))


## 5-2-8. as.difftime : 시간의 차이를 계산하는 형변환
as.difftime('14:00:00') - as.difftime('09:00:00')
as.numeric(as.difftime('14:00:00') - as.difftime('09:00:00'))


## 5-2-9. lubridate
install.packages("lubridate")
library("lubridate")

date()
Sys.Date()

now()
date

# now() : 현재시간
date <- now()
date

# year() : 년도
format(Sys.time(), '%Y')
format(Sys.time(), '%y')

year(date)

# month() : 달(월)
format(Sys.time(), '%m')
format(Sys.time(), '%B')

month(date)   
month(date, label = T)   # label = F가 기본값 

# day() : 일
format(Sys.time(), '%d')

day(date)
day(now())

# wday() : 요일
format(Sys.time(), '%A')
format(Sys.time(), '%u')
format(Sys.time(), '%w')

wday(date)
wday(now())
wday(Sys.Date())
wday(now(), week_start = 1)  # 월요일 기준
wday(now(), week_start = 7)  # 일요일 기준 
wday(now(), label = T)
wday(now(), label = F)


## 5-2-10. 날짜계산
oracle sql : to_yminterval('10-00')

years(10)
now() + years(10)

months(4)
now() + months(4)
now() + months(13)

days(100)
now() + days(100)
now() + days(30)

hours(3)
now() + hours(3)

minutes(100)
now() + minutes(100)
now() - minutes(100)

seconds(100)
now() + seconds(100)
now() + years(1) + months(1) + days(1) + hours(10) + minutes(20) + seconds(60)

hm('08:00')
now() + hm('08:00')

hms('08:30:59')
now() + hms('08:30:59')

date <- now()

year(date) <- 2017
date

month(date) <- 12
date

day(date) <- 31
date

hour(date) <- 00
date

minute(date) <- 00
date

second(date) <- 00
date


# [문제81] 2002-06-07에 입사한 사원들의 last_name, hire_date를 출력하세요.

# sol.1
str(as.Date('2002-06-07'))
str(emp$HIRE_DATE)
as.Date(emp$HIRE_DATE, format = '%Y-%m-%d')

emp[as.Date('2002-06-07') == as.Date(emp$HIRE_DATE, format = '%Y-%m-%d'), c("LAST_NAME", "HIRE_DATE")]

# sol.2
emp[as.Date('2002-06-07') == as.Date(emp$HIRE_DATE), c("LAST_NAME", "HIRE_DATE")]

# sol.3
difftime(as.Date('2002-06-07'), as.Date(emp$HIRE_DATE)) == 0

emp[difftime(as.Date('2002-06-07'),as.Date(emp$HIRE_DATE)) == 0, c("LAST_NAME", "HIRE_DATE")]

# sol.4
emp['2002-06-07' == emp$HIRE_DATE, c("LAST_NAME", "HIRE_DATE")]


# [문제82] 사원의 last_name, 근무일수를 출력하세요.

# sol.1
difftime(Sys.Date(), as.Date(emp$HIRE_DATE, format = '%Y-%m-%d'))
paste(emp$LAST_NAME, difftime(Sys.Date(), as.Date(emp$HIRE_DATE, format = '%Y-%m-%d')))

# sol.2
Sys.Date() - as.Date(emp$HIRE_DATE, format = '%Y-%m-%d')
data.frame(emp$LAST_NAME, diff=Sys.Date() - as.Date(emp$HIRE_DATE, format = '%Y-%m-%d'))


# [문제83] 사원의 last_name, 입사한 요일을 출력하세요.

weekdays(as.Date(emp$HIRE_DATE))
paste(emp$LAST_NAME, weekdays(as.Date(emp$HIRE_DATE)))


# [문제84] 오늘 날짜를 기준으로 100개월 되는 날짜의 요일을 출력하세요.

# sol.1
Sys.Date()
now()

weekdays(Sys.Date() + months(100))
weekdays(now() + months(100))

# sol.2
format(Sys.Date() + months(100), '%A')
format(now() + months(100), '%A')


# [문제85] 사원의 last_name, 입사한 요일을 출력하세요. 단 요일별로 정렬을 수행해서 아래와 같이 출력하세요.

  name    day
1 Ernst 월요일 
2 Vollman 월요일 
3 Mallin 월요일 
4 Ladwig 월요일 
5 Cambrault 월요일 
6 Greene 월요일 
7 Ande 월요일 
8 Banda 월요일 
9 Kumar 월요일 
10 Walsh 월요일 
11 Hartstein 화요일 
12 King 화요일 
13 Hunold 화요일

  name    day
1 Grant 일요일 
2 Pataballa 일요일 
3 Khoo 일요일 
4 Tobias 일요일 
5 Weiss 일요일 
6 Fripp 일요일 
7 Landry 일요일 
8 Atkinson 일요일 
9 Seo 일요일 
10 Vargas 일요일 
11 Tucker 일요일 
12 McEwen 일요일 
13 Livingston 일요일 
14 Geoni 일요일 
15 Bull 일요일 
16 Ernst 월요일

# sol.1 : factor
hire_days <- ordered(weekdays(as.Date(emp$HIRE_DATE)), c('일요일','월요일','화요일','수요일','목요일','금요일','토요일'))

orderBy(~day, data.frame(name=emp$LAST_NAME, day=hire_days))

# sol.2 : 요일별 정렬을 통해 만들어진 컬럼생성 -> 데이터 프레임 작성
day <- weekdays(as.Date(
  emp[order(wday(as.Date(emp$HIRE_DATE), label = T)), c('HIRE_DATE')]
))

data.frame(name = emp$LAST_NAME, day)

# 선생님 풀이.1 : 요일별 순서숫자를 기준(월요일)으로 입사일 정렬 -> 요일로 변환
x <- emp[order(format(as.Date(emp$HIRE_DATE), '%u')), c('LAST_NAME', 'HIRE_DATE')]
data.frame(name = x$LAST_NAME, day = format(as.Date(x$HIRE_DATE), '%A'))

# 선생님 풀이.2 : 요일별 순서숫자를 기준으로 입사일 정렬 -> 요일로 변환
x <- emp[order(wday(as.Date(emp$HIRE_DATE))), c('LAST_NAME','HIRE_DATE')]

data.frame(name = x$LAST_NAME, day = format(as.Date(x$HIRE_DATE),'%A'))

# 선생님 풀이.3 : 요일별 순서숫자를 기준(일요일)으로 입사일 정렬 -> 요일로 변환
x <- emp[order(format(as.Date(emp$HIRE_DATE),'%w')), c('LAST_NAME','HIRE_DATE')]

data.frame(name = x$LAST_NAME, day = format(as.Date(x$HIRE_DATE),'%A'))


# 번외 연구(feat. 김동일)
#1.
orderBy(~day, data.frame(
  last_name = emp$LAST_NAME, 
  day = format(as.Date(emp$HIRE_DATE, format = '%Y-%m-%d'),'%u %A')
)
)

#2. 
d <- data.frame(
  name = emp$LAST_NAME, 
  day = weekdays(as.Date(emp$HIRE_DATE, format = '%Y-%m-%d'))
)

d[order(format(as.Date(emp$HIRE_DATE, format = '%Y-%m-%d'),'%u')), ]
d[order(format(as.Date(emp$HIRE_DATE, format = '%Y-%m-%d'),'%w')), ]


# [문제86] 새로운 변수에 last_name, salary,hire_date, 2001 년도에 입사했으면 level를 A 2002 년도에 입사했으면 level를 B
#          2003 년도에 입사했으면 level를 C 2004 년도에 입사했으면 level를 D 나머지 년도는 E 가 입력해주세요.

hire_year <- year(emp$HIRE_DATE)
new <- data.frame(emp$LAST_NAME, emp$SALARY, emp$HIRE_DATE, ifelse(hire_year == 2001, 'A', 
                                                                   ifelse(hire_year == 2002, 'B',
                                                                          ifelse(hire_year == 2003, 'C',
                                                                                 ifelse(hire_year == 2004, 'D','E')
                                                                          )
                                                                   )
)
)
names(new) <- c('name','sal','date','level')
new