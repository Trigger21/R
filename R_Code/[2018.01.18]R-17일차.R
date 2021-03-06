## R-17일차(2018.1.18)

# emp 테이블 불러오기
library(RJDBC)
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/r/ojdbc6.jar")
conn <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@localhost:1521/xe", "hr",  "hr")

emp <- dbGetQuery(conn, "select * from employees")
emp

# ggplot으로 부서별 인원수 그래프로 그리기
library(ggplot2)
factor

# 집계값을 자동적으로 구하고 색상적용 및 범례생성까지 가능
ggplot(emp, aes(x = factor(DEPARTMENT_ID), fill = factor(DEPARTMENT_ID)))+
  geom_bar(width = .4, show.legend = T)+
  labs(x = NULL, y = '인원수', title = '부서별 인원수', fill = '부서코드')+
  theme(legend.position = "bottom")  # legend.position : top, right, bottom, left, none
exam

library(plyr)
df <- arrange(exam, name, subject) ; df
ggplot(df, aes(x = name, y = grade, fill = subject))+
  geom_bar(stat = "identity")+
  geom_text(aes(y = grade, label = paste(grade, "점")),
            col = "black", size = 4, position = position_stack(vjust = .7))+
  geom_col(aes(fill = subject), position = "stack")+
  theme(legend.justification  = "top", #c(.93, .90),
        legend.text = element_text(size = 8, colour = rgb(.4,1,.2)),
        legend.title = element_text(colour = "white"),
        legend.background = element_rect(fill = "black", colour = "yellow", size = 2),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1, colour = "red", size = 10))
example("theme")

ggplot(df, aes(x = subject, y = grade, group = name))+
  geom_col(aes(fill = name), position = "stack")+
  geom_text(aes(label = grade), position = position_stack(vjust = .5))

ggplot(df, aes(x = name, y = grade, group = subject))+
  geom_col(aes(fill = subject), position = "dodge")+
  geom_text(aes(label = grade), position = position_dodge(.9), hjust = -.3) + coord_flip()

library(colorspace)

sql <- exam[exam$subject == 'SQL',]
ggplot(sql, aes(x = name, y = grade))+
  geom_bar(stat = "identity", fill = rainbow(9), width = .5)+
  geom_text(aes(label = grade), vjust = .3, hjust = -.2)+
  coord_flip()
hyundai <- mpg[mpg$manufacturer == 'hyundai',]
hyundai
x <- arrange(mpg, manufacturer, year)
ggplot(x, aes(x = manufacturer, y = hwy)
       seals
       base <- ggplot(mpg, aes(displ, hwy)) + geom_point()
       base + geom_smooth()
       
#[문제177] 년도별 입사한 수를 ggplot를 이용해서 막대그래프를 출력하세요.
       
library(lubridate)
year(emp$HIRE_DATE)
       
gg <- ggplot(emp, aes(x = factor(year(HIRE_DATE)), fill = factor(year(HIRE_DATE))))
       
gg + 
 geom_bar(width = .6, show.legend = T)+
 labs(title = "[문제177] 년도별 입사 현황", x = "입사년도", y = "입사수(명)", fill = "범례")+
 geom_hline(yintercept = seq(5,30,5), lty = 3)+
 theme(legend.title.align = .5, legend.box.background = element_rect(), 
       legend.box.margin = margin(t=.1, r=.1, b=.1, l = .1, unit = "cm"))
cnt_year
       
#선생님 풀이
library(lubridate)
library(plyr)
       
ggplot(emp,aes(x=year(HIRE_DATE)))+
  geom_bar()
       
ggplot(emp, aes(x=factor(year(HIRE_DATE)), fill=factor(year(HIRE_DATE))))+
  geom_bar()+
  labs(title = "년도별 입사현황", x="년도", y="인원수", fill="년도")+
  theme(plot.title=element_text(face='bold', color='darkblue', hjust=0.5))+
  theme(axis.title.x=element_text(face='bold', color='darkblue'))+
  theme(axis.title.y=element_text(face='bold', color='darkblue'))+
  theme(legend.title.align=0.5, legend.box.background = element_rect(),
        legend.box.margin = margin(t=0.1,r=0.1, b=0.1, l=0.1,unit='cm'))

e <- count(emp,"year(HIRE_DATE)")    # library(plyr)
colnames(e) <- c("year","cn")
e
       
ggplot(e, aes(x=year,y=cn,fill=factor(year)))+
  geom_bar(stat="identity")+
  geom_text(aes(label=cn))+
  ggtitle("년도별 입사현황",subtitle="신입사원")+  # main title, subtitle 
  theme(plot.title = element_text(lineheight=.8, face="bold",color="darkblue",hjust=0.5))+
  theme(plot.subtitle = element_text(lineheight=.8, face="bold",color="darkblue",hjust=0.5))+
  labs(caption = "2001년 ~ 2008년")+
  labs(x="년도",y="인원수")+
  guides(fill=guide_legend(title="년도"))+  # 범례 제목 지정 
  theme(legend.title.align=0.5, legend.box.background = element_rect(),
        legend.box.margin = margin(t=0.1,r=0.1, b=0.1, l=0.1,unit='cm'))

#번외 연구(guides)
dat <- data.frame(x = 1:5, y = 1:5, 
                  p = 1:5, q = factor(1:5),
                  r = factor(1:5))
dat
       
p <- ggplot(dat, aes(x, y, colour = p, size = q, shape = r))+geom_point()
       
p+
 guides(colour = "colorbar", 
        size = "legend", 
        shape = "legend")
       
p+
 guides(colour = guide_colorbar(),
        size = guide_legend(),
        shape = guide_legend())
       
p+
  scale_colour_continuous(guide = "colorbar")+
  scale_size_discrete(guide = "legend")+
  scale_shape(guide = "legend")
geom_point()
x <- exam[exam$subject == 'SQL',]
       
ggplot(x, aes(x = name, y = grade, size = grade))+
  geom_point(shape = 21, colour = "black", fill = "cyan")
  
ggplot(x, aes(x = name, y = grade, group = subject))+
  geom_line()+
  geom_point()+
  
geom_abline(intercept = mean(x$grade), slope = 0, col = "red")
facet_wrap()

ggplot(x, aes(x = name, y = grade))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_blank())+
  facet_wrap(~ name)

ggplot(exam, aes(x = name, y = grade))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  facet_wrap(~ name+subject, nrow = 3)+
  theme(axis.text.x = element_blank())

ggplot(exam, aes(x = name, y = grade))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  facet_grid(subject~name)+
  theme(axis.text.x = element_blank())

#[문제178] 학생들의 과목 점수를 각각으로 막대그래프를 그리세요.
       
ggplot(exam, aes(x = name, y = grade, fill = subject))+
  geom_bar(stat = "identity")+
  facet_grid(subject~name)+
  theme(axis.text.x = element_blank())+
         theme(legend.position = "left")
    
#[문제179] 화면과 같은 결과를 그래프로 생성하세요.
       
ggplot(exam, aes(x = subject, y = grade, group = name, colour = name))+   # group : line, colour : point
  geom_line()+
  geom_point()+
  geom_abline(intercept = mean())

grep(8, c(1:10000), value = T)
gregexpr(8, grep(8, c(1:10000), value = T))
       
length(unlist(gregexpr(8, grep(8, c(1:10000), value = T))))

ggplot(sql, aes(x = grade, y = reorder(name, grade)))+
  geom_point(size = 5, col = "red")+
  theme_bw()+
  theme(panel.grid.major.x = element_line(colour = rainbow_hcl(9), linetype = "dashed"), 
        panel.grid.minor.x = element_line(colour = rainbow_hcl(9), linetype = "dashed"),
        panel.grid.major.y = element_line(colour = rainbow_hcl(9), linetype = "dashed"))

#theme_bw() : 흑과 백만 가지고 있는 테마
#panel.grid.major.x : 배경화면 중에 간격을 나타내는 큰 선분
#element_black() : 선분이 안보이게 하는 기능
#panel.grid.minor.x : 큰 선분 사이에 있는 작은 선분
#panel.grid.major.y : 가로로 빨간색 점선을 출력
#reorder(name, grade) : name을 알파벳 순서가 아닌 grade 순으로 정렬

ggplot(sql, aes(x = grade, y = reorder(name, grade), colour = grade))+
  geom_point(size = 5)+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = rainbow_hcl(9), linetype = "dashed"),
        panel.grid.minor = element_line(colour = heat_hcl(100), linetype = "dashed"),
        plot.background = element_rect(fill = "grey80"))+
  labs(y = "name", title = "SQL 성적")+
  theme(plot.title = element_text(size = rel(2)))
      
ggplot(sql, aes(x = grade, y = reorder(name,grade), shape = factor(grade%/%10)))+
  geom_point()+
  labs(y = "name", fill = "등급별 구분")+
  theme(legend.title = element_text(color = "blue"))+
  guides(fill=guide_legend(title="등급"))
  
#숙제 : 문자열 관련 함수 공부
       
require(graphics)
bymedian <- with(InsectSprays, reorder(spray, count, median))
boxplot(count ~ bymedian, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE,
        col = "lightgray")