## R-10일차(2018.1.9)
  # 10-1. melt
  # 10-2. dcast
  # 10-3. grep
  # 10-4. stringr()
    # -4-1. str_detect()
    # -4-2. str_count()
    # -4-3. str_c()
    # -4-4. str_dup()
    # -4-5. str_length()
    # -4-6. str_locate()
    # -4-7. str_replace()
    # -4-8. str_split()
    # -4-9. str_sub()
    # -4-10. str_trim()


#[문제133] 결측값이 포함되어 있는지 확인하세요.

x <- c(1, 2, NA, 4, NA, 6, 7, NA, 9, NA)

x <- c(1, 2, NA, 4, NA, 6, 7, NA, 9, NA)
is.na(x)


#[문제134] 결측값이 총 몇 개인지 계산하세요.

x <- c(1, 2, NA, 4, NA, 6, 7, NA, 9, NA)

sum(is.na(x))


#[문제135] 결측값이 있는 인덱스 번호를 확인하세요.

x <- c(1, 2, NA, 4, NA, 6, 7, NA, 9, NA)

which(is.na(x))


#[문제136] emp 데이터 프레임에 COMMISSION_PCT 컬럼에 결측값이 몇 개인지, 결측값이 아닌 개수를 확인하시고 결측값의 총 비율확인해주세요.

sum(is.na(emp$COMMISSION_PCT))   # 72(NA)
sum(!is.na(emp$COMMISSION_PCT))  # 35
sum(is.na(emp$COMMISSION_PCT)) * 100 / (sum(is.na(emp$COMMISSION_PCT))+sum(!is.na(emp$COMMISSION_PCT))) # 67%


#[문제137] emp 데이터 프레임에 변수(컬럼)별 결측값의 개수 합계를 구하세요.

apply(is.na(emp), 2, sum)
colSums(is.na(emp))


#[문제138] emp 데이터 프레임에 COMMISSION_PCT 합, 평균을 구하세요.

sum(emp$COMMISSION_PCT, na.rm = T)  # 7.8
mean(ifelse(is.na(emp$COMMISSION_PCT),0,emp$COMMISSION_PCT))  # 0.0728972


#[문제139] emp 데이터 프레임에 결측값이 들어있는 행 전체를 데이터 셋에서 제거 한 후 emp_omit 변수에 저장하세요.

emp_omit <- na.omit(emp)
emp_omit


#[문제140] emp 데이터 프레임에 COMMISSION_PCT에 결측값이 있는 행을 제거 한 후 emp_new 변수에 저장하세요.

emp_new <- na.omit(emp$COMMISSION_PCT)
emp_new

#선생님 풀이.1
emp_new <- emp[!is.na(emp$COMMISSION_PCT),]
emp_new

#선생님 풀이.2 complete.cases : NA 제외하는 함수
emp_new <- emp[complete.cases(emp[,"COMMISSION_PCT"]),]
emp_new


#[문제141] emp 데이터 프레임에 COMMISSION_PCT에 결측값을 0로 변경하세요.

ifelse(is.na(emp$COMMISSION_PCT), 0, emp$COMMISSION_PCT)

#선생님 풀이
emp_ex141 <- emp

length(emp_ex141$COMMISSION_PCT[is.na(emp_ex141$COMMISSION_PCT)])  # 72

emp_ex141$COMMISSION_PCT[is.na(emp_ex141$COMMISSION_PCT)] <- 0     # NA -> 0

length(emp_ex141$COMMISSION_PCT[is.na(emp_ex141$COMMISSION_PCT)])  # 0

#번외 연구(다른방법으로 풀어보자)
#sqldf : 문제의도에는 맞지 않는다
library(sqldf)
emp_ex141 <- sqldf("select coalesce(commission_pct,0) from emp_ex141")
emp_ex141

#data.frame : 선생님 풀이랑 비슷비슷
emp_ex141[is.na(emp_ex141$COMMISSION_PCT), c("COMMISSION_PCT")] <- 0
emp_ex141


#[문제142] emp 데이터 프레임에 모든 결측값을 0으로 대체한 후 emp_zero변수에 저장하세요.

#1st step : NA 들어있는 column 확인
colSums(is.na(emp))

#2nd step : 해당 column에 대한 수정작업
comm_zero <- ifelse(is.na(emp$COMMISSION_PCT), 0, emp$COMMISSION_PCT)
man_zero <- ifelse(is.na(emp$MANAGER_ID), 0, emp$MANAGER_ID)
dep_zero <- ifelse(is.na(emp$DEPARTMENT_ID), 0, emp$DEPARTMENT_ID)

emp_zero <- emp

emp_zero[,c("COMMISSION_PCT","MANAGER_ID","DEPARTMENT_ID")] <- data.frame(COMMISSION_PCT=comm_zero,
                                                                          MANAGER_ID=man_zero,
                                                                          DEPARTMENT_ID=dep_zero)
colSums(is.na(emp_zero)) # 검증 

#선생님 풀이
emp_zero <- emp
colSums(is.na(emp_zero))        # COMMISSION_PCT : 72, MANAGER_ID : 1, DEPARTMENT_ID : 1

emp_zero[is.na(emp_zero)] <- 0  # WARNING!! 

colSums(is.na(emp_zero))        # ALL : 0

#[문제 143] emp 데이터 프레임을 새로운 e 이름으로 복제하세요. 
#           e 데이터 프레임에 새로운 comm 컬럼을 생성하는데 COMMISSION_PCT 값을 기반으로 
#           값을 입력하시고 결측값은 기존 COMMISSION_PCT의 평균 값으로 입력해주세요.
#           (단, mutate함수를 이용하세요)

#sol.1
e <- emp
mutate(e, comm = ifelse(is.na(COMMISSION_PCT), 
                        mean(ifelse(is.na(COMMISSION_PCT), 0, COMMISSION_PCT)),
                        COMMISSION_PCT))
#sol.2
e <- emp
e$COMMISSION_PCT[is.na(e$COMMISSION_PCT)] <- 0
e

melt(e)

#선생님 풀이
library(dplyr)
e <- emp
e <- e%>% 
  mutate(comm_avg = ifelse(is.na(COMMISSION_PCT), 
                           mean(COMMISSION_PCT, na.rm = T), 
                           COMMISSION_PCT))
e

#번외연구 : 피보나치
F_1 <- matrix(c(1,1,1,0),2,2)
F_1
F_2 <- F_1 %*% F_1
F_2

(F_3 <- F_2 %*% F_1)

FB <- function(n){
  
  FB(n) <- FB(n-1) %*% F_1
  
}


## 10-1. melt

install.packages("reshape2")
library("reshape2")       

# 컬럼이 많은 형태(wide)를 세로로 긴(long) 형태로 변경
#before
str(fruits_sales)
fruits_sales

#after
library("reshape2") 
melt(fruits_sales)
melt(fruits_sales, id = 'year')
melt(fruits_sales, id = 'name')
melt로 wide -> long
m <- melt(fruits_sales, id = c('year', 'name'))
m


## 10-2. dcast (= melt^(-1))
# long(세로)을 wide(가로) 형태로 변경
dcast(m, year + name ~ variable)

# long -> wide 변형하면서 집계값 구하는 응용

# ex.1)
dcast(m, name ~ variable, sum)

# ex.2)
dcast(m, year ~ variable, sum)

# 사원번호별 emp 테이블 long으로 변형
emp_long_id <- melt(emp, id = 'EMPLOYEE_ID')
emp_long_id
dcast(emp_long_id, EMPLOYEE_ID ~ variable)


## 10-3. grep
# 동일한 문자열을 문자열 벡터에서 찾아서 인덱스 번호를 리턴
text <- c('a','ab','acb','accb','acccb','accccb')
grep('ab', text)
grep('ab', text, value = T)
grep('ac*', text, value = T)
grep('ac*b', text, value = T)      # c* : 적어도 0번 매칭하면 찾는다(c가 있든 없든 다나옴) 
grep('ac+b', text, value = T)      # c+ : 적어도 1번은 c가 있어야 함(1 <= )
grep('ac?b', text, value = T)      # c? : 많아야 1번 매칭하면 찾는다(0 or 1)
grep('ac{2}b', text, value = T)    # c{2} : 정확히 2번 매칭하면 찾는다
grep('ac{2,}b', text, value = T)   # c{2, } : 적어도 n번(여기선 2) 이상 매칭하면 찾는다
grep('ac{2,3}b', text, value = T)  # c{2,3} : 적어도 n번 이상 m 이하 매칭하면 찾는다(여기선 2~3)

test <- c('abcd', 'cdab', 'cabd', 'c adb')
grep('ab', text, value = T)
grep('^ab', text, value = T)
grep('ab$', text, value = T)

# ab 시작되는 문자를 찾는데 빈문자열 뒤에 시작된 ab도 찾는다.
grep('\\bab', text)
grep('\\bab', text, value = T)

# \ 원하는 것만 딱 뽑을 때
grep('\\*', text, value = T)   # * 이 들어간 것 뽑음

# ex.1
text <- c('^ab', 'ab', 'abc', 'abd', 'abe', 'ab 12')
text
grep('ab', text, value = T)
grep('ab.', text, value = T)      # . : 어떤 문자 하나 매칭
grep('ab[c,e]*', text, value = T)  # [,] : 리스트 안에 있는 문자 매칭 
grep('ab[c-e]', text, value = T)  # [-] : 리스트 범위 안에 있는 문자 매칭
grep('ab[^c]', text, value = T)   # [^] : 리스트 안에 있는 문자를 제외하고 매칭

# ex.2
text <- c('sql','SQL','Sql100', 'PLSQL', 'plsql', 'R', 'r', 'r0', 'python', 'PYTHON', 'Pyth0n', 'Python#','0')
NROW(text)
grep('sql', text) 

text2 <- c('sql', 'r')
grep(text2, text) # error

grep(paste(text2, collapse = '|'), text, value = T)

paste(text2,'maecu', sep = '#', collapse = '|')
grep('[0-9]', text, value = T)     
grep('[[:digit:]]', text, value = T)   # [[:digit:]] = [0-9]

setdiff(grep('0', text, value = T), grep('0*0', text, value = T))

# 번외 연구(질문자 장연철) : 숫자 1개만 포함한 것을 뽑아내라
text <- c('1p1','11p1','11p11','p1','0p','0p0','p000','0000p','01p','p011','0p111','101p1','10p10','01p01','00')

# ★★★ 숫자 1자리만 뽑아내는 방법(김승혁 형님 아이디어)
grep('^[^0-9]*[0-9][^0-9]*$', text, value = T)
grep('^[^0]*[0][^0]*$', text, value = T)

# ★★★ 최미리 (반장)
res <- NA; z<-1

for(i in 1:NROW(text)){
  
  count <- 0
  tmp <- unlist(strsplit(text[i], split=character(0)))  # strsplit
  
  for(j in 1:NROW(tmp)){
    
    if(tmp[j] %in% 0:9){ count <- count+1 }
    
  }
  
  if(count == 1) {       # 0 : 문자열만 있는 인자만 추출
    
    res[z] <- text[i]
    z <- z+1 
    
  }
}
res
grep('[[:upper:]]', text, value = T) 

grep('[[:lower:]]', text, value = T) 

grep('[[:alpha:]]', text, value = T)  # 대소문자 구분없이 다 찾는다(= upper + lower)

grep('[[:alnum:]]', text, value = T)  # 문자 + 숫자 다 찾는다 

grep('[[:punct:]]', text, value = T)  # 구두점 표현

grep('[[:upper:],[:lower:]]', text, value = T)  # alnum과 같다 

grep('[sql100]', text, value = T)

grep('[[:punct:], 0]', text, value = T)


## 10-4. stringr
install.packages("stringr")
library(stringr)

# 10-4-1. str_detect()
# - 특정 문자가 있는지를 검사해서 TRUE / FALSE를 출력

text <- c('sql','SQL','Sql100', 'PLSQL', 'plsql', 'R', 'r', 'r0', 'python', 'PYTHON', 'Pyth0n', 'Python#','0')
text

# 응용 : 0, 1개만 들어있는 문자 찾기
text[which(str_detect(text, '^[^0]*0[^0]*$'))]

# 대문자 SQL 단어 찾기
str_detect(text, 'SQL')    

# 소문자 s로 시작되는 단어 찾기
str_detect(text, '^s')

# 소문자 n으로 끝나는 단어 찾기
str_detect(text, 'n$')

# 시작되는 글자가 소문자 s, 대문자 S
str_detect(text, '^[sS]')
str_detect(text, 'qQ')

# 대소문자 구분 안하도록 설정
str_detect(text, ignore.case('s'))

# 10-4-2. str_count()
# - 주어진 단어에서 해당 글자가 몇번 나오는지 알려주는 함수
# - nchar(4-4) 비슷한 작동(그런데 한글자 단위 횟수 제공에서 차이) 
text vector
text
nchar(text)
str_count(text, ignore.case('s'))
str_count(text, ignore.case('l'))
str_count(text, fixed("S", ignore_case = T))

# 10-4-3. str_c()
# - 문자열 합쳐서 출력하는 함수(= paste())
str_c("R", "빅데이터 분석")
str_c("프로그램 언더 : ", text)
str_c(text, " 은 데이터 분석하기위해 좋은 언어는 ", text, " 이다.")
str_c(text)
str_c(text, collapse = "")
paste(text, collapse = "")

# 10-4-4. str_dup()
# - 주어진 문자열을 주어진 횟수만큼 반복해서 출력하는 함수
str_dup(text, 2)
str_c("프로그램 언어 : ", str_dup(text,2))

# 10-4-5. str_length()
# - 주어진 문자열의 길이를 출력하는 함수
str_length(text)

# 10-4-6. str_locate()
# - 주어진 문자열에서 특정 문자가 처음 나오는 위치와 마지막 위치를 출력하는 함수
str_locate('january', 'a')
str_locate('january', 'n..r')
str_locate('january', 'ry')

# str_locate_all : 무조건 다 찾음
str_locate_all('january', 'a')

# 10-4-7. str_replace()
# - 주어진 문자열에서 변경전 문자를 변경후 문자로 바꾸는 함수
str_replace('빅데이터분석', '빅데이터', '가치')

gsub('빅데이터', '가치', '빅데이터분석')
text <- c('sql','signal')
str_replace(text, 's', '**')
str_replace('banana', 'a', '*')
str_replace_all('banana', 'a', '*')

# 10-4-8. str_split()
# - 주어진 데이터셋에서 지정된 기호를 기준으로 분리하는 함수
str <- str_c('sql', '/', 'plsql', '/', 'r')
str

# / 를 기준으로 분리시킴
str_split(str, '/')   # list

# 10-4-9. str_sub()
# - 주어진 문자열에서 지정된 길이 만큼의 문자를 잘라내는 함수
text
str_sub(text, start = 1, end = 3)
str_sub(text, start = 5, end = 9)
str_sub(text, start = -2)   # 끝에서 2번째

# 10-4-10. str_trim()
# - 접두, 접미 부분에 공백문자 제거하는 함수
str_trim('    R     ')
str_trim('    R     A     P    ')

trimws('    R     ')
trimws('    R     A     P    ')
gsub(' ','','    R     A     P    ')
str_replace_all('    R     A     P    ',' ','')