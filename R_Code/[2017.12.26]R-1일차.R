# R-1일차(2017.12.26)
  # 1-1. 변수 
  # 1-2. 벡터
  # 1-3. list


Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8") #맥os사용자를 위한 한글 잘 사용하기 위한 명령어
par(family = "AppleGothic")


## 1-1. 변수
* 변수의 성질
- 변수이름은 알파벳, 숫자,_, .(마침표) 사용한다.
- 변수 첫글자는 알파벳, .(마침표)로 시작할 수 있다.
- .(마침표)로 시작시에는 바로 뒤에 숫자를 입력 못함.


ex) 변수가능 : a, i, x2, .y

ex) 불가능 : 1k, .2, k-j
변수값 할당(<-, <<-, =)

# 더하기 방법 1
x <- 1 
y <- 2
x+y    # 1+2 연산결과값 리턴

# 더하기 방법 2
x <- 1
y <- 2
z <- x+y  # z 변수에 3=1+2 을 저장
z         # 3 리턴

# 더하기 방법 3
sum(x1 <- c(1,2,3,4,5))  # x1에 1,2,3,4,5 저장후 총합:sum(x1) 리턴
sum(x2 = c(1,2,3,4,5))   # x2에 1,2,3,4,5 저장후 총합:sum(x2) 리턴
x1  # x1 : 1차원 배열
x2  # x2 : 배열로 나타나지 못함
# 결론 : <- 을 쓰자

# 숫자(정수, 부동소숫점)
a <- 2
print(a)

b <- 3.5
print(b)

a+b  # 부동소숫점이 더 세다
class(a) : 변수의 타입 확인
class(a)
문자열
s1 <- "hello"
s2 <- 'hello'
class(s2)
진리값(boolean)
TRUE & TRUE            # & : and 연산자
TRUE | FALSE
!TRUE                  # ! : not
T & T
T | F
!T

class(T)
class(F)
# 주의사항 T <- TRUE F <- FALSE

NA(Not Available) : 결측차(값), 데이터 입력중 실수로 값이 입력되지 않은 경우
a <- 100; b <- 90; c <- NA;

a+b+c
is.na(c)  # c에 NA 값이 있느냐?
NULL : 변수에 초기화 되지 않을때 사용
x <- NULL
is.null(x)

y <- 100

x + y 

z <- NA

x + z

# 산술연산
1 + 2

100-99

2*3

100/2

100/3

100 %/% 3   # 정수 나누기

100 %% 3    # 나머지

10^2

10**2
비교 연산자
5 > 3

5 >= 5

2 < 5

2 <= 5

5 == 5

3 != 4

# 숫자형 길이(지수표기법)
10000
100000  # 1e+05(0이 5개 부터 이렇게 나옴)


## 1-2. vector(벡터)
  # 정의: 같은 데이터 타입을 갖는 1차원 배열구조(R의 기본 데이터 구조)

- c() : combine value, seq() : sequence value 함수 사용
- 벡터는 중첩 불가능하다
- 벡터는 단일 데이터 타입만 사용(스칼라)
- 데이터 변환 규칙 : integer < double(부동소수점) < character

# 벡터 생성
x <- c(1,2,3)  # 배열변수 형태

x
class(x)
str(x)

x <- c(1,2,3.14)         # 정수 < 부동소숫값
x  

x <- c(1,2.1,"3.14")     # 부동소숫값 < 문자형
x  

x <- c(1,2,3,c(4,5,6))   # 벡터는 중첩 불가능
x  

x <- c(90,80,70)
names(x) <- c("국어", "영어", "수학")   # names() : 벡터의 각 셀에 이름을 설정

x

# 벡터의 요소 번호(1~)를 이용해서 접근
x[1]
x[2]
x[3]
x[1:3]
x[-1]      # 1번 요소 제외
x[c(1,3)]  # 1번 3번 요소만 조회
x['국어']
x['영어']
x['수학']
벡터의 길이
length(x)
NROW(x)

# 연속된 값 : c(시작값:종료값)

c(1:100)  # 1~100까지 정수를 나옴
x <- c(1:10000)
sequence : 자동일련번호를 생성 / seq(시작값,종료값,증가분)

seq(1,10,3)    # 1~10까지 3씩 증가(등차수열, 3n-2 n=1,...,4)
seq(0,100,10)  # 0~100까지 10씩 증가(등차수열, 10n n=0,...,10)
seq_along(x) : 길이 만큼의 연속된 값으로 벡터를 초기화
seq_along(5)

x <- c(2,5,8,15)

seq_along(x)

1:NROW(x)
rep() : 반복되는 값
rep(1:5, times = 2)  # 전체를 반복

rep(1:5, each = 2)   # 각각을 반복
벡터의 값 수정
x <- c(1:10)

x[2] <- 8   # 2번째 요소값을 8로 수정
x[3:5] <- c(5,3,9)   # 3,4,5번째의 요소값을 5,3,9로 수정

x
벡터의 값 추가
x[6] <- 60
x[8] <- 80

x
x <- append(x,90,after=10)   # 10번째 이후 90값 추가

x
벡터연산
x <- c(1,2,3,4,5)

x + 10

x * 10

x - 10

x / 2

x <- c(1,2,3)
y <- c(1,2,3)

x == y          # 각각 비교

identical(x,y)  # 전체비교(두 벡터 x,y의 값이 동일한지 판단)

z <- c(1,2,4)

identical(x,z)  # FALSE

w <- c(1,2,3,3)

identical(x,w)

setequal(x,w)  # 두 벡터가 같은 집합인지 판단(배열 모양과는 상관없이 값이 똑같으면 TRUE)

union(x,z)     # 합집합

intersect(x,z) # 교집합

setdiff(x,z)   # 차집합
x

1 %in% x   # x벡터 변수에 1이 있는지를 판단
2 %in% x
3 %in% x
4 %in% x

# 오름차순
sort_test <- c(sample(10,10))

sort_test
sort(sort_test)  # 오름차순(기본값)
sort(sort_test, decreasing = TRUE)  # 내림차순

# [문제1] x변수에 1,3,5,7,9 값을 입력, y 변수에 1,2,3,4,5 값을 입력하세요.

x <- seq(1,9,2)
y <- c(1:5)

x
y

# [문제2] x 변수와 y 변수를 중복성 없이 하나로 합친후에 u 변수에 넣어 주세요.

u <- union(x,y)
u
sort(u)

# [문제3] x 변수와 y 변수의 값들중에 중복성만 추출해서 i 변수에 넣어주세요.

i <- intersect(x,y)
i

# [문제4] x 변수의 값과 y 변수의 값중에 순수하게 x 변수에 들어 있는 값만 추출해서 m 변수에 넣어 주세요.

m <- setdiff(x,y)
m

# [문제5] x 변수의 값과 y 변수의 값이 일치가 되면 TRUE 아니면 FALSE를 출력해주세요.

setequal(x,y)

# [문제6] x 변수에 값들을 10을 곱한 결과를 x 변수에 적용하세요.

x <- x * 10
x

# [문제7] x 변수에 있는 50을 5로 수정하세요.

x[3] <- 5
x

# [문제8] x 변수에 있는 10 30 5 70 90을 원래의 값으로 1,3,5,7,9로 되돌려 주세요.단 union, 정수 나누기, sort 만 사용하세요

x <- sort(union(x[-3]%/%10,x[3]))
x

# [문제9] x변수에 11숫자를 제일 뒤에 입력하세요. 단 append와 length를 이용하세요.

x <- append(x,11,after=length(x))
x

# [문제10] x 변수에 제일 뒤에 있는 값을 NA로 수정하세요. 단 length를 이용하세요.

x[length(x)] <- NA
x
help(append)
?append


## 1-3. list
- 서로 다른 데이터 타입을 갖는 vector 들을 저장하거나 또는 다른 list 저장 가능
- list(키 = 값, 키 = 값) 형태로 사용
list 생성
x <- list(name = '홍길동', addr = '서울', pn = '010-0000-1111')
x
x$name
x$addr
x$pn
x[1]
x[[1]]
x[1:2]
list 요소 추가
x$sal <- 10000

x
list 요소 제거
x$sal <- NULL

x$sal
x
list 요소값 수정
x$pn <- '010-1234-5678'

x$pn
list 중첩
y <- list(a=list(vat=c(1,2,3)), b=list(val=c(1,2,3,4)))

y
y$a
y$b

# [문제11] lst 변수에 name = 'king' , height = 180, weight = 70 값을 넣어 주세요.

lst <- list(name='king', height=180, weight=70)
lst

class(lst$height)
mode(lst)
str(lst)

# [문제12] lst 변수에 blood = 'A' 추가하세요.

lst$blood <- 'A'
lst

# [문제13] lst 변수에 name의 값을 'scott'로 수정하세요.

lst$name <- 'scott'
lst$name

# [문제14] lst변수에 2번인덱스 값만 출력해주세요.

lst[2]
[문제15] lst변수에 blood 이름을 blood type 이름으로 수정하세요.

lst$blood_type <- lst$blood
lst$blood <- NULL

lst

# 선생님 풀이
names(lst)

names(lst)[4]

names(lst)[4] <- 'blood type'

names(lst)

lst
names(lst) <- NULL  # 키를 한번에 지울수도 있다.(값은 그대로 있음)