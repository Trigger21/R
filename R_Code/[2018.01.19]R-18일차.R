## R-18일차(2018.1.19)

#[문제180] 1군전염병발병현환_년도별.csv 에 데이터를 가지고 그래프를 생성하세요.

dis <- read.csv("c:/r/1군전염병발병현황_년도별.csv", header = T, stringsAsFactors = F)

dis
library("reshape2") 
m_dis <- melt(dis)
m_dis

#sol.1
library(ggplot2)
library(colorspace)

ggplot(m_dis, aes(x = 년도별, y = log(value), group = variable, colour = variable))+
  geom_line()+
  geom_point()+
  labs(title = "1군 전염병 발병현황(년도별)", y = "인원수(log)")+
  guides(fill = guide_legend(title = "범례"))+
  theme(legend.title.align=0.5, legend.box.background = element_rect(),
        legend.box.margin = margin(t=0.1,r=0.1, b=0.1, l=0.1,unit='cm'))+
  theme(panel.grid.major.x = element_line(colour = rainbow_hcl(9), linetype = "dashed"), 
        panel.grid.minor.x = element_line(colour = rainbow_hcl(9), linetype = "dashed"),
        panel.grid.major.y = element_line(colour = rainbow_hcl(9), linetype = "dashed"),
        panel.grid.minor.y = element_line(colour = rainbow_hcl(9), linetype = "dashed"))

#sol.2
library(ggplot2)
ggplot(m_dis, aes(x = 년도별, y = value, group = variable, colour = variable))+
  geom_line()+
  geom_point()+
  labs(title = "1군 전염병 발병현황(년도별)", y = "인원수")+
  guides(fill = guide_legend(title = "범례"))+
  facet_wrap(~variable, scales = "free")+
  theme(axis.text.x = element_text(angle = 25, size = 7),
        legend.title.align=0.5, legend.box.background = element_rect(),
        legend.box.margin = margin(t=0.1,r=0.1, b=0.1, l=0.1,unit='cm'))

#선생님 풀이
dis
ggplot(m_dis,aes(x=년도별,y=value,group = variable))+
  geom_col(aes(fill=variable),position="dodge")+
  geom_text(aes(label = value),position=position_dodge(0.9),vjust=0)+
  ggtitle("1군전염병발병현환",subtitle="년도별")+
  theme(plot.title = element_text(lineheight=.8, face="bold",color="darkblue",hjust=0.5))+
  theme(plot.subtitle = element_text(lineheight=.8, face="bold",color="darkblue",hjust=0.5))+
  labs(colour="종류")

I am a student

나는 학생입니다.

명사를 뽑아내? 누가 만들었다 그걸 쓰자고?
  
install.packages("KoNLP")
library("KoNLP")       # 사전에 jdk 설치해야 함 
useSejongDic()
text1 <- "R은 오픈소스로 통계, 기계학습, 금융, 생물정보학, 그래픽스에 이르는 다양한 통계 패키지를 갖추고 있는 좋은 프로그램이다."
str_split(text1, ' ')  # stringr package
strsplit(text1,'')

#extractNoun() : 한글의 명사를 추출하는 함수(한나눔 분석기)
#명사(名詞) : 사물, 사람, 장소의 이름을 나타내는 품사이다.
extractNoun(text1)

#SimplePos09() : 한글의 품사를 추출하는 함수
text2 <- SimplePos09(text1)

#str_match : 패턴 일치 되는 문자 찾기
text_noun <- str_match(text2, '([A-Z가-힣]+)/N')  # + : 적어도 한번은 있는다 
as.vector(na.omit(text_noun[,2]))
str_locate(grep('N',text2, value = T),'/N')
extractNoun(text1)
.libPaths()
#C:\Users\STU\Documents\R\win-library\3.4\KoNLP_dic\current : 사전(명사형) 등록

#사전 호출하는 순간 내가 저장한 단어는 초기화
useSejongDic()
dic_user.txt
#dic 추가
buildDictionary(ext_dic = "sejong", 
                user_dic = data.frame(c("기계학습", "생물정보학", "다양한"), c("ncn")),
                replace_usr_dic = T)
extractNoun(text1)

#연설문 단어 빈도수(wordclound 활용)
install.packages("wordcloud")
library("wordcloud")

#readLines() : line별 읽어드림
text <- readLines("c:/r/문재인대통령취임사.txt")
text
noun1 <- unlist(extractNoun(text))
noun1 <- setdiff(noun1, '')
noun1 <- str_replace(noun1, "대통령의", "대통령")
noun2 <- unlist(sapply(text, extractNoun, USE.NAMES = FALSE))
unlist(lapply(text, extractNoun))

#table : vector 건수 셈
word_cn1 <- table(noun1) 
word_cn2 <- table(noun2)
head(sort(word_cn1, decreasing = TRUE), 10)
pal <- brewer.pal(8, "Dark2") ; pal

wordcloud(names(word_cn1), freq = word_cn1, scale = c(5,.5),
          random.order = FALSE, rot.per = .1, colors = pal)

#names : 출력할 단어들
#freq : 빈도수
#scale : 글자의 크기 c(큰값, 작은값)
#min.freq : 최소 빈도수를 지정
#max.words : 이값 이상의 빈도수면 삭제
#random.order : 출력되는 순서를 임의로 지정  
#random.color : 글자의 색상을 임의로 지정 
#rot.per : 단어배치를 90도 각도로 출
#colors : 출력될 단어들의 색상 
word_df <- as.data.frame(word_cn1)
names(word_df) <- c("word","cnt")

orderBy(~-cnt,word_df)
word_df <- word_df[1 < str_length(word_df$word) & word_df$cnt >= 5,]

word_df$word<-gsub("대통령\\S","대통령",word_df$word)

barplot(word_df$cnt, names.arg = word_df$word, las = 2)

str_c("대한","민국")

install.packages("wordcloud2")
library(wordcloud2)
fl <- c("내가 그의 이름을 불러주기 전에는 
        그는 다만 
        하나의 몸짓에 지나지 않았다. 
        
        내가 그의 이름을 불러주었을 때 
        그는 나에게로 와서 
        꽃이 되었다. 
        
        내가 그의 이름을 불러준 것처럼 
        나의 이 빛깔과 향기에 알맞는 
        누가 나의 이름을 불러 다오. 
        그에게로 가서 나도 
        그의 꽃이 되고 싶다. 
        
        우리들은 모두 
        무엇이 되고 싶다. 
        나는 너에게 너는 나에게 
        잊혀지지 않는 하나의 의미가 되고 싶다." )
wordcloud2(table(extractNoun(fl)))

empty_house <- "사랑을 잃고 나는 쓰네
잘 있거라, 짧았던 밤들아
창밖을 떠돌던 겨울 안개들아
아무것도 모르던 촛불들아, 잘 있거라
공포를 기다리던 흰 종이들아
망설임을 대신하던 눈물들아
잘 있거라, 더 이상 내 것이 아닌 열망들아
장님처럼 나 이제 더듬거리며 문을 잠그네
가엾은 내 사랑 빈집에 갇혔네"

emp_house_cnt <- table(extractNoun(empty_house))

class(emp_house_cnt)

wordcloud2(emp_house_cnt)
eh <- melt(emp_house_cnt)
names(eh) <- c("word","cnt")
eh

ggplot(eh, aes(x = word, y = cnt, fill = word))+
  geom_bar(stat = "identity")
wordcloud2(word_cn2, shape = "triangle")
wordcloud2(table(c("")))
Map(extractNoun, text)
table(str_match(SimplePos09(text), '([A-Z가-�R]+)/N')[,2])
m <- melt(fruits_sales, id = c('name','year'))
m
dcast(m, name+year~variable)