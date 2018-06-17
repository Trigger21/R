## R-40일차(2018.2.22)


#[문제244]advice.csv 파일에 있는 데이터를 분석 하세요.

advice <- read.csv("c:/r/advice.csv", stringsAsFactors = F)
advice
attach(advice)
CONTENTS
library(KoNLP)
library(stringr)
useSejongDic()
buildDictionary(ext_dic = "sejong", 
                user_dic = data.frame(c("환불", "기스"), c("ncn")),
                replace_usr_dic = T)
t1 <- unlist(extractNoun(CONTENTS))
t2 <- gsub("환불해주세요", "환불", t1)
t3 <- t2[!t2 == "안들림"]
text <- head(sort(table(t3), decreasing = T),8)
text
#text1 <- str_match_all(SimplePos09(CONTENTS), "([가-�R]+)/N")
#text2 <- unlist(text1)
#text3 <- text2[-grep("/N$",text2)]
#text4 <- gsub("환불해주세요", "환불", text3)
#text4 <- gsub("지저분하게", "지저분", text4)
#sort(table(text4), decreasing = T)

library(wordcloud2)
wordcloud2(text)
# ※ rJava 문제시에 remove.packages -> install.packages 해보자 
#   선생님 팁 : remove.packages("rJava",lib=.libPaths()[1])

install.packages("KoNLP")
install.packages("rJava")
remove.packages('rJava')
library(rJava)
library(RJDBC)
library(KoNLP)
library(DBI)

Sys.getenv()
sessionInfo()
.jinit()
Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre1.8.0_161")


#[문제245] 문제244번의 결과 단어들을 가지고 아래처럼 만들어 보세요.

환불 기분 포장 선물 이어폰 정품 지저분 화장품 
 10   5    3    2     2     2     2      2

환불 기분 포장 선물 이어폰 정품 지저분 화장품
2017-01-10 0 0 1 0 0 0 1 0 
2017-01-11 1 0 0 0 0 0 0 1 
2017-01-12 1 0 0 0 1 1 0 0 
2017-01-13 1 0 0 0 0 1 0 0 
2017-01-14 1 1 0 0 0 0 0 0 
2017-01-15 1 1 0 0 0 0 0 0 
2017-01-16 1 0 0 0 1 0 0 0 
2017-01-17 1 1 1 1 0 0 0 0 
2017-01-18 1 1 0 0 0 0 0 0 
2017-01-19 1 0 0 0 0 0 0 0 
2017-01-20 1 1 1 1 0 0 1 0

# text <- head(sort(table(t3), decreasing = T),8)
text

t4 <- t3[t3 %in% names(text)]
length(t4) == sum(text)  # 28

ad <- advice
ad

v <- matrix(rep(0,nrow(ad)*length(text)), nrow(ad), length(text))

for(i in 1:length(text)){
  v[grep(names(text)[i], CONTENTS),i] <- 1
}

v <- as.data.frame(v)
v

names(v) <- names(text)

v1 <- as.matrix(v, "Transaction")
rownames(v1) <- ad[,1]
v1

#연관규칙 분석
library(arules)

rules1 <- apriori(v1, parameter = list(supp = 0.4, conf = 0.6, target = "rules", minlen = 2))
inspect(sort(rules1))

m <- t(v1) %*% v1
m1 <- m - diag(diag(m));m1
# -> '기분' 단어가 언급되면 '환불'을 요구할 가능성이 높다.

#그래프 그리기
library(sna)
library(rgl)

gplot(m1, displaylabels = T,
      label.col = "blue",
      vertex.cex = diag(m)/2,
      vertex.col = "purple",
      arrowhead.cex = .4,
      edge.col = "darkorange",
      edge.lwd = m1)


#[문제244]

library(KoNLP) 
library(wordcloud2)

useSejongDic()

#세종딕에 없는 단어 추가
buildDictionary(ext_dic="woorimalsam",user_dic=data.frame("큰맘먹고","ncn"),replace_usr_dic=F)
buildDictionary(ext_dic="woorimalsam",user_dic=data.frame("환불","ncn"),replace_usr_dic=F)
buildDictionary(ext_dic="woorimalsam",user_dic=data.frame("당장","ncn"),replace_usr_dic=F)
get_dictionary('woorimalsam') # 잡다구리
get_dictionary('sejong')      # 명사
get_dictionary('insighter')   # 동사

#게시판 글 불러와서 담기
advice <- read.csv("c:/r/advice.csv", header=T, stringsAsFactors=F) str(advice)

text <- advice[,2]
text

#정제작업
text<-gsub('기스가','기스',text)
text<-gsub('첨에왔을때도','첨에 왔을때도',text)
text<-gsub('함부로다룬것도아니여서','함부로 다룬 것도 아니여서',text)
text<-gsub('첨에왔을때도','첨에 왔을때도',text)
text<-gsub('환불해주세요','환불 해주세요',text)
text<-gsub('당장급해서','당장 급해서',text)
text<-gsub('[^가-�R]',' ',text)
word<-extractNoun(text)   
word

#word1<-unlist(strsplit(word1,' '))
word1<-unlist(word)
word1<-Filter(function(x){nchar(x)>=2},word1)  # Filter() 활용 : 문자길이 2개 이상만 추출
word1

word2<-c()
for (i in word1){
  if (!i %in% c('안들','하려','아닌가봐요','있는거','해주')){
    word2<-c(word2,i)
  }
}
word2

#빈도수 체크
word2
word_cn<-table(word2) 
word_cn

#wordcloud 그리기
library(wordcloud)
library(RColorBrewer)

wordcloud(names(word_cn),freq=word_cn,min.freq=1,scale=c(3,1),
          random.order=F,random.color =T,
          colors=brewer.pal(8,'Dark2'))

wordcloud2(word_cn, size = 1,shape='star')


#[문제245]

word_sort<-sort(word_cn,decreasing=T)   
word_sort<-word_sort[word_sort>=2]   # 빈도수 2개 이상만
word_sort

res_sort<-as.matrix(word_sort)
res_sort

keyword<-rownames(word_sort)
keyword  # 단어들

#matrix로 만들기
data<-c()
for (i in 1:length(word)){
  index<-intersect(word[[i]],keyword)
  data<-rbind(data,table(index)[keyword])
}
data
date <- advice[,1]
date

#NA제거(0으로 대체)
data<-ifelse(is.na(data),0,data)
data

colnames(data)<-keyword
rownames(data)<-date
data

res<-as.data.frame(data)
res
str(res) 

#연관분석
library(arules)
trans<-as.matrix(res,'Transaction')  # 'Transaction' : 0을 바라보지 않겠다.
res_rul<-apriori(trans, parameter = list(supp=0.4, conf = 0.9,  target = "rules"))
inspect(sort(res_rul,by='lift'))  # by='lift' : lift 값 보려면 

lhs rhs              support   confidence  lift 
[1] {기분} => {환불} 0.4545455 1.0000000   1.1 
[2] {} => {환불}     0.9090909 0.9090909   1.0

inspect(subset(res_rul,items %in% '기분'))  # subset() : 조건에 만족하는 데이터를 선택(NA행 X)


## 40-1. read.transactions()
# - 연관분석할 자료는 transactions matrix로 받자(훨씬 편하다)

#step.1 : 데이터 다운

library(arules)
#groceries <- read.transactions("http://www.sci.csueastbay.edu/~esuess/classes/Statistics_6620/Presentations/ml13/groceries.csv", sep = ",")
groceries <- read.transactions("c:/r/groceries.csv", sep = ",")
str(groceries)


#step.2 : 데이터 분석

summary(groceries) # 데이타 핵심요약

#장바구니 목록 확인
inspect(groceries[1:5])


## 40-2. itemFrequency() : 제품이 포함한 거래의 비율
itemFrequency(groceries[,1:3])


## 40-3. itemFrequencyPklot(data, support = N) : 제품이 포함한 거래의 비율을 그래프로 그리자
itemFrequencyPlot(groceries, support = 0.1)

# topN : 상위값 출력
itemFrequencyPlot(groceries, topN = 20)


## 40-4. image()
image(groceries[1:5])
image(sample(groceries, 100))


#step.3 : 연관분석

groceryrules <- apriori(groceries, parameter = list(supp = 0.006, conf = 0.25, minlen = 2))
inspect(sort(groceryrules, by = "lift")[1:5])
inspect(subset(groceryrules, items %in% 'berries'))  # berries만 추출
summary(groceryrules)

#정제한 파일 csv로 저장하기
write(groceryrules, file = "c:/r/groceryrules.csv", sep = ",", quote = T, row.names = F)

#data.frame 형식으로 변환해서 저장
groceryrules_df <- as(groceryrules, "data.frame")
groceryrules_df