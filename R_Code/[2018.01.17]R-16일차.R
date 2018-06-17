## R-16일차(2018.1.17)


#[문제173] 아래와 같은 결과를 출력하도록 SQL문을 작성하세요.
"""
Job                     Dept 10    Dept 20    Dept 30    Dept 40    Dept 50    Dept 60    Dept 70    Dept 80    Dept 90   Dept 100
-------------------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
  AC_MGR
AC_ACCOUNT
IT_PROG                                                                          28800
ST_MAN                                                                36400
AD_ASST                       0
PU_MAN                                          11000
SH_CLERK                                                              64300
AD_VP                                                                                                     35700
FI_ACCOUNT                                                                                                           39600
MK_MAN                               13000
PR_REP                                                                                      10000
FI_MGR                                                                                                               12008
PU_CLERK                                        13900
SA_MAN                                                                                                 61000
MK_REP                                6000
AD_PRES                                                                                                   26400
SA_REP                                                                                                243500
HR_REP                                                      6500
ST_CLERK                                                              55700
"""

select * 
from (select job_id, department_id, salary from employees) 
pivot(sum(salary) for department_id in (10 "Dept 10", 20 "Dept 20", 30 "Dept 30", 40 "Dept 40", 50 "Dept 50", 60 "Dept 60", 70 "Dept 70", 80 "Dept 80", 90 "Dept 90", 100 "Dept 100"));

select * 
from (select job_id "Job", department_id, sum(salary) sumsal 
      from employees 
      group by job_id, department_id) 
pivot(max(sumsal) for department_id in (10 "Dept 10",20 "Dept 20",30 "Dept 30",40 "Dept 40",50 "Dept 50", 60 "Dept 60",70 "Dept 70",80 "Dept 80",90 "Dept 90",100 "Dept 100"));

select job_id, 
       nvl(max(decode(dep_id, 10, cnt)),0) "Dept 10", 
       nvl(max(decode(dep_id, 20, cnt)),0) "Dept 20", 
       nvl(max(decode(dep_id, 30, cnt)),0) "Dept 30", 
       nvl(max(decode(dep_id, 40, cnt)),0) "Dept 40", 
       nvl(max(decode(dep_id, 50, cnt)),0) "Dept 50", 
       nvl(max(decode(dep_id, 60, cnt)),0) "Dept 60", 
       nvl(max(decode(dep_id, 70, cnt)),0) "Dept 70", 
       nvl(max(decode(dep_id, 80, cnt)),0) "Dept 80", 
       nvl(max(decode(dep_id, 90, cnt)),0) "Dept 90", 
       nvl(max(decode(dep_id, 100, cnt)),0) "Dept 100" 
from (select job_id, sum(salary) cnt, department_id dep_id 
      from employees 
      group by department_id, job_id) 
group by grouping sets(job_id);

select job_id, decode(dep_id, 10, ss) "Dept 10", decode(dep_id, 20, ss) "Dept 20", decode(dep_id, 30, ss) "Dept 30", decode(dep_id, 40, ss) "Dept 40", decode(dep_id, 50, ss) "Dept 50", decode(dep_id, 60, ss) "Dept 60", decode(dep_id, 70, ss) "Dept 70", decode(dep_id, 80, ss) "Dept 80", decode(dep_id, 90, ss) "Dept 90", decode(dep_id, 100, ss) "Dept 100" from (select job_id, sum(salary) ss, department_id dep_id from employees group by department_id, job_id);

#선생님 풀이
select job_id "job", 
       sum(decode(department_id, 10, salary)) "Dept 10", 
       sum(decode(department_id, 20, salary)) "Dept 20", 
       sum(decode(department_id, 30, salary)) "Dept 30", 
       sum(decode(department_id, 40, salary)) "Dept 40", 
       sum(decode(department_id, 50, salary)) "Dept 50", 
       sum(decode(department_id, 60, salary)) "Dept 60",
       sum(decode(department_id, 70, salary)) "Dept 70", 
       sum(decode(department_id, 80, salary)) "Dept 80", 
       sum(decode(department_id, 90, salary)) "Dept 90", 
       sum(decode(department_id, 100, salary)) "Dept 100" 
from employees 
group by job_id;

#부서없는 사람도 출력시키려면
select * 
from (select job_id, 
      nvl(department_id,0) dp, 
      salary from employees) 
pivot(sum(salary) for dp in (10 "Dept 10", 20 "Dept 20", 30 "Dept 30", 40 "Dept 40", 50 "Dept 50", 60 "Dept 60", 70 "Dept 70", 80 "Dept 80", 90 "Dept 90", 100 "Dept 100", 0 "Dept null"));


#[문제174] 문제173을 R에서 오라클로 접속하셔서 수행하세요. SQL문을 수행해서 결과 집합을 만드세요.

install.packages("RJDBC")
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/r/ojdbc6.jar")
conn <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@localhost:1521/xe", "hr",  "hr")
dbGetQuery(conn, "select * from employees")
dbGetQuery(conn, "select *
           from (select job_id, department_id, salary
           from employees)
           pivot(sum(salary) for department_id 
           in (10 Dept_10, 20 Dept_20, 30 Dept_30, 40 Dept_40, 50 Dept_50, 
           60 Dept_60, 70 Dept_70, 80 Dept_80, 90 Dept_90, 100 Dept_100))")
dbGetQuery(conn, "select job_id,
           decode(dep_id, 10, ss) Dept_10,
           decode(dep_id, 20, ss) Dept_20,
           decode(dep_id, 30, ss) Dept_30,
           decode(dep_id, 40, ss) Dept_40,
           decode(dep_id, 50, ss) Dept_50,
           decode(dep_id, 60, ss) Dept_60,
           decode(dep_id, 70, ss) Dept_70,
           decode(dep_id, 80, ss) Dept_80,
           decode(dep_id, 90, ss) Dept_90,
           decode(dep_id, 100, ss) Dept_100
           from (select job_id, sum(salary) ss, department_id dep_id
           from employees
           group by department_id, job_id)")


#[문제175] R에서 오라클로 접속하셔서 EMPLOYEES 테이블을 읽어 들인 후 R함수를 이용하셔서 
#          문제173번 동일한 결과를 출력하세요.

tapply(emp$SALARY, list(emp$JOB_ID, emp$DEPARTMENT_ID), sum)
#apply 계열 함수 많이 쓰니까 잘 기억해라
nat_2013 <- read.csv("c:/r/2013nat.csv", header = T, stringsAsFactors = F)
te <- nat_2013
library(stringr)
for(i in 1:nrow(te)){
  for(n in 1:ncol(te)){
    if(te[i,n] == "-"){
      te[i,n] <- '0'
    }
  }
}
te
y <- as.matrix(nat2013[,c(3:ncol(nat2013))])
y[y == '-']<-0
y<-as.numeric(y)
nat2013[,c(3:ncol(nat2013))]<-y
nat2013


## 16-1. google map
# - ggmap : google map, stamen map을 정적으로 보여주는 기능의 패키

install.packages("ggmap")
library(ggmap)

#geocode("지역명(주소에 대한 벡터값)") : 위도 경도값을 반환하는 함수
library(ggmap)
gc <- geocode(enc2utf8('서울시')) ; gc

#enc2utf8 : 한글 지역명을 utf8 형식으로 변환함수
lon(longitude) : 경도, 세로선
lat(latitude) : 위도, 가로선
cen <- as.numeric(gc) ; cen
map1 <- get_googlemap(center = cen, zoom = 18, maptype = "satellite")
ggmap(map1)
get_googlemap

# - 정의 : 구글에서 제공하는 맵객체를 반환, 위도 경도를 중심으로 하는 지도 정보 반환
# - center : 지도중심 좌표값
# - zoom : 지도 크기 기본값(10, 도시), 3(대륙) ~ 21(빌딩)
# - size : 지도, 가로, 세로 픽셀 크기 기본값(640 X 640), size = c(640,640)
# - maptype : 지도유형
#   * terrain(지형정보기반지도), satellite(위성지도), roadmap(도로명 표시), hybrid(위성, 도로명)
# - marker : gc(위도 경도 위치에 마커 출력)
# - ggmap(map) : 지도를 출력하는 함수 

# ex.1)
map2 <- get_map(location = "seoul", zoom = 14, maptype = "roadmap")
ggmap(map2, size = c(600,600))
map_itwill <- get_map(location = enc2utf8("서울특별시 강남구 역삼1동 테헤란로 123"), 
                      zoom = 20, maptype = "hybrid", source = c("google"))
ggmap(map_itwill)

# ex.2)
gc <- geocode(enc2utf8("여래리")) ; gc
num <- as.numeric(gc)
map <- get_openstreetmap(bbox = c(left = num[1], bottom = num[2]))
ggmap(map)

# ex.3)
qmap(location = "baylor university", zoom = 14)
qmap(location = "baylor university", zoom = 14, source = "osm")
qmap(location = enc2utf8("국립중앙도서관"), zoom = 18, source = "google", maptype = "satellite")

# ex.4)
gc <- geocode(enc2utf8("제주도 서귀포"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen, maptype = "roadmap", marker = gc)
ggmap(map)

names <- c("1.협재해수욕장", "2.함덕해수욕장")
addr <- c("제주특별자치도 제주시 한림읍 협재리 2497-1",
          "제주특별자치도 제주시 조천읍 함덕리 1008")

gc <- geocode(enc2utf8(addr)) ; gc

df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
df

cen <- c(mean(df$lon), mean(df$lat))   # 중심좌표 
cen 

map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10, marker = gc)
ggmap(map)
qmap(location = "praha", zoom = 18, source = "google", maptype = "satellite", marker = )
qmap(location = 'City of Los Angeles Central Communications Facility', zoom = 18, 
     source = "google", maptype = "satellite")
qmap(location = "2800 E Observatory Rd, Los Angeles, CA 90027", zoom = 19, 
     source = "google", maptype = "satellite")
qmap(location = "222 Memorial Dr, Cambridge, MA 02142", zoom = 18,
     source = "google", maptype = "hybrid")


## 16-2. ggplot2
# - 그래픽 출력을 위한 기능을 제공하는 패키지 

install.packages("ggplot2")
library(ggplot2)

exam
x <- exam[exam$subject == 'SQL',]
x

library("colorspace")
barplot(x$grade, names.arg = x$name, las = 2, cex.names = .8, 
        ylim = c(0,100), col = heat_hcl(nrow(x)), border = NA)
help(ggplot)
ggplot(x, aes(x = name, y = grade))+
  geom_bar(stat = 'identity', fill = heat_hcl(nrow(x)), colour = NA)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,
                                   colour = "blue", size = 10))

#[문제176] 부서 인원수를 ggplot를 이용해서 막대그래프를 출력하세요.

df_agg <- aggregate(EMPLOYEE_ID ~ DEPARTMENT_ID, emp, length)
df_agg[,1] <- ordered(df_agg[,1], df_agg[,1])
df_agg
ggplot(df_agg, aes(x = DEPARTMENT_ID, y = EMPLOYEE_ID, colour = DEPARTMENT_ID))+
  geom_bar(stat = 'identity',fill = heat_hcl(nrow(df_agg)))+
  geom_text(aes(label = EMPLOYEE_ID), vjust = -0.5, size = 4, col = "red")+
  geom_abline(intercept = seq(5,50,5), slope = 0, lty = 3)+
  labs(title = '부서별 인원수', x = '부서번호', y = '인원수(명)')
ggplot(df_agg, aes(x = DEPARTMENT_ID, y = EMPLOYEE_ID))+
  geom_bar(stat = 'identity',fill = heat_hcl(nrow(df_agg)))+coord_flip()+
  geom_text(aes(label = EMPLOYEE_ID), hjust = -0.3, size = 4, col = "blue")

#선생님 풀이
ggplot(df_agg, aes(x = DEPARTMENT_ID, y = EMPLOYEE_ID))+
  geom_bar(stat = 'identity', fill = rainbow(11))+
  labs(title = "부서별 인원수", x = "부서번호", y = "인원수(명)")+
  theme(plot.title = element_text(face = 'bold', color = 'darkblue', hjust = .5))+
  theme(axis.title.x = element_text(face = 'bold.italic', color = 'brown', size = 6))+
  theme(axis.title.y = element_text(face = 'bold.italic', color = 'brown'))
a <-tapply(emp$EMPLOYEE_ID, emp$DEPARTMENT_ID, length)
a
names(a)
a <- as.list(a)
a2 <- data.frame(a)
a2<- data.frame(t(a2))
names(a2)<- names(a)
a2
a <- tapply(emp$EMPLOYEE_ID, emp$DEPARTMENT_ID, length)
a1 <- data.frame(t(a))
rownames(a1)<- c("cnt")
names(a1) <- names(a)
a1
emp[ , c("LAST_NAME", "SALARY", "DEPARTMENT_ID")]