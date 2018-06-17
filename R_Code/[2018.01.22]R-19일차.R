## R-19일차(2018.1.22)

# 서울시 게시판
library(KoNLP)
library(wordcloud)
library(wordcloud2)

seoul <- readLines("c:/r/seoul.txt")
seoul
noun_seoul <- unlist(extractNoun(seoul))
word_seoul <- table(noun_seoul)
df <- as.data.frame(word_seoul)
df
df <- df[!df$noun_seoul %in% grep("[0-9]", df$noun_seoul, value = T),]
df <- df[!df$noun_seoul %in% grep("O+$", df$noun_seoul, value = T),]

library(doBy)
df <- orderBy(~-Freq,df)
df
df1 <- df[-c(1:2),]
df1 <- df1[df1$Freq > 1,]
wordcloud2(df1)

#선생님 풀이
useSejongDic()
buildDictionary(ext_dic = )
data2 <- SimplePos09(seoul)
data2 <- str_match(data2,'([가-힣]+)/N')
data2 <- data2[,2]
head(unlist(data2),30)

data3 <- unlist(data2)
data3 <- Filter(function(x){nchar(x) >= 2}, data3)
data3

data3 <- gsub("서울","",data3)
data3 <- gsub("요청","",data3)
data3 <- gsub("제안","",data3)
data3 <- gsub(" ","",data3)
data3 <- gsub("-","",data3)
data3 <- gsub("O+","",data3)
data3

write(unlist(data3), "c:/r/seoul_1.txt")
seoul_1 <- read.table("c:/r/seoul_1.txt")
wordcloud2(table(seoul_1))

library(KoNLP)
library(wordcloud)
library(stringr)

data1 <- readLines("c:/r/seoul.txt")

useSejongDic()

buildDictionary(ext_dic="sejong", 
                user_dic=data.frame(c("AEμiCÐ±³","½AAa´O"),c("ncn")),
                replace_usr_dic=T)

##data2 <- extractNoun(data1) 
data2 <- SimplePos09(data1) 
data2 <- str_match(data2,'([°¡-ÆR]+)/N') 
data2 <- data2[,2] head(unlist(data2), 30) 
data3 <- unlist(data2) 
data3 <- Filter(function(x) {nchar(x) >= 2} ,data3) 
data3 
data3 <- gsub("\d+","", data3) 
data3 <- gsub("¼???¿i½A","", data3) 
data3 <- gsub("¼???¿i","", data3)
data3 <- gsub("¿aA≫","", data3)
data3 <- gsub("A|¾E","", data3) 
data3 <- gsub(" ","", data3)
data3 <- gsub("-","",data3) 
data3 <- gsub("O+","",data3) 
data3

write(unlist(data3),"c:/r/seoul_2.txt") 
data4 <- read.table("c:/r/seoul_2.txt")

wordcount <- table(data4) wordcount

head(sort(wordcount, decreasing=T),50)

library(RColorBrewer) 
palete <- brewer.pal(9,"Set3") 
wordcloud(names(wordcount), 
          freq=wordcount, 
          scale=c(2,0.5), 
          min.freq = 1, 
          random.order=F, 
          random.color=T, 
          colors=palete)

library(wordcloud2) 
wordcloud2(wordcount)

## jeju.txt
# - 제주도 지명을 뽑아내라 
jeju <- readLines("c:/r/jeju.txt")
jeju <- setdiff(jeju, '')
jeju <- setdiff(jeju, " ")
jeju
t1 <- SimplePos22(jeju)
str(t1)

t2 <- NULL
for(i in 1:length(t1)){
  t2 <- c(t2, str_match_all(t1, '([가-힣]+)/N')[[i]][,2])
}

t2

# 협재해변
grep('협재',t2,value = T)

t2 <- gsub("협재$", "협재해변", t2)
t2 <- gsub("협재해변욕장", "협재해변", t2)
t2 <- gsub("협재해수욕장", "협재해변", t2)

grep('협제',t2,value = T)
t2 <- gsub("협제해변", "협재해변", t2)
grep('서귀포.$',t2,value = T)

t2 <- gsub("서귀포.$", "서귀포", t2)
grep('우도', t2, value = T)

t2 <- gsub("우도박물", "우도박물관", t2)

grep('우도가시기', t2, value = T)
t2 <- gsub("우도가시기", "우도", t2)
t2 <- gsub("우도안", "우도", t2)
t2 <- gsub("우도관광", "우도", t2)
grep('휘닉스', t2, value = T)

t2 <- gsub('아일랜드가','',t2)
grep('천지연', t2, value = T)

t2 <- gsub('폭포폭','폭포',t2)
grep('용머리', t2, value = T)

t2 <- gsub('해안안','해안',t2)
buildDictionary(ext_dic="sejong",
                user_dic=data.frame(readLines("c:/r/tour.txt"),c("ncn")),replace_usr_dic=T)

tour <- readLines("c:/r/tour.txt")
grep("중문", t2, value = T)

t2 <- gsub('중문$','해안',t2)
t3 <- table(t2[t2 %in% tour])

t3 <- orderBy(~-Freq, as.data.frame(t3))

t3[1:10,]


wordcloud2(t3)
wordcloud(t3$Var1, freq = t3$Freq, scale = c(5,.5),
          random.order = FALSE, rot.per = .1, colors = rainbow(41))
t4 <- t2[nchar(t2) > 1]
t4 <- table(t4)
wordcloud2(t4[t4 > 1])


#숙제 : 상위 10개 맵으로 표현
# TOP 10
jeju_top10 <- t3[1:10,]

#ggmap package

library(ggmap)
gc <- geocode(enc2utf8("한라산"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen, maptype = "roadmap", marker = gc)

ggmap(map)
names <- as.character(jeju_top10$Var1)

gc <- geocode(enc2utf8(names)) ; gc

df <- data.frame(name = names, lon = gc$lon, lat = gc$lat)
df

cen <- c(mean(df$lon), mean(df$lat))   # 중심좌표 
cen 

map <- get_googlemap(center = cen, maptype = "roadmap", zoom = 10, marker = gc)
map1 <- get_googlemap(center = cen, maptype = "terrain", zoom = 10, marker = gc)
map2 <- get_googlemap(center = cen, maptype = "satellite", zoom = 10, marker = gc)
map3 <- get_googlemap(center = cen, maptype = "hybrid", zoom = 10, marker = gc)

ggmap(map3)

library(jpeg)
jpeg("c:/r/jeju_top10.jpg")

ggmap(map)

dev.off()
jpeg("c:/r/jeju_top10_1.jpg")

ggmap(map3)

dev.off()