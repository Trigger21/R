## R-23일차(2018.1.26)

  # 23-1. 크롤링 

install.packages("rvest")
library(rvest)
library(dplyr)  # %>%
library(httr)
ary()

html <- read_html("http://search.joins.com/news?keyword=%EB%B9%85%EB%8D%B0%EC%9D%B4%ED%84%B0&cloc=joongang|section|subsection")
html
url <- html_nodes(html, css = ".list_default .headline")%>%
  html_nodes('a')%>%
  html_nodes('href')
url
중앙일보
html <- read_html("http://search.joins.com/News?Keyword=%EB%B9%85%EB%8D%B0%EC%9D%B4%ED%84%B0&SortType=New&SearchCategoryType=News&PeriodType=All&ScopeType=All&ImageType=All&JplusType=All&BlogType=All&ImageSearchType=Image&TotalCount=0&StartCount=0&IsChosung=False&IssueCategoryType=All&IsDuplicate=True&Page=1&PageSize=3&IsNeedTotalCount=True")
html

url <- html_nodes(html,css=".list_default .headline") %>%
  html_nodes('a')%>%
  html_attr('href')
url  


html <- read_html("http://news.joins.com/article/22319631")

html_node(html, "#article_body")%>%    # 아이디 속성(#)
  html_text()
html <- read_html(url[12])
html_node(html, "#article_body")%>%    # 아이디 속성(#)
  html_text()
# http://search.chosun.com/search/total.search?query=%ED%97%88%EA%B2%BD%EC%98%81&pageconf=total

html <- read_html("http://search.chosun.com/search/news.search?query=%ED%97%88%EA%B2%BD%EC%98%81&pageno=0&orderby=&naviarraystr=&kind=&cont1=&cont2=&cont5=&categoryname=&categoryd2=&c_scope=news&sdate=&edate=&premium=")
html

url <- html_nodes(html, css = "dl.search_news dt")%>%
  html_nodes("a")%>%
  html_attr("href")
url


html.1 <- read_html(url[1])
html.1  

html_node(html.1, "#article_2011")%>%    # 아이디 속성(#)
  html_text()
html <- read_html("https://search.naver.com/search.naver?query=%EB%8B%88%EC%B2%B4&where=news&ie=utf8&sm=nws_hty")
html

url <- html_nodes(html, css = ".type01 dt a")%>%
  html_attr("href")
url

html <- read_html(url[2])

html_nodes(html, "#body")%>%
  html_text()

## 네이버 금융-쏠리드-종목토론실(2017.1.26 ~ 2018.1.26) 게시글 분석
# text 초기화(나중에 게시판 글 담을 벡터)
text <- NULL    

## 전체페이지 : 1~196
# 각 페이지별로 각 게시글에 대한 url을 생성 
for(i in 1:196){  
  
  html <- read_html(paste0("http://finance.naver.com/item/board.nhn?code=050890&page=", i), encoding = "euc-kr")
  
  url <- html_nodes(html, css = "table.type2 td.title")%>%
    html_nodes("a")%>%
    html_attr("href")
  
  ## 페이지별 게시글 : 1~20개  
  # 각 게시글 내용 text(벡터)에 저장 
  for(j in 1:20){   
    
    url1 <- paste0("http://finance.naver.com", url[j])
    html1 <- read_html(url1, encoding = "euc-kr")
    
    text <- c(text, html_nodes(html1, "#body")%>% html_text())
    
  }
}
# 위 결과 확인 
text
library(wordcloud2)  # wordcloud2() 사용
library(KoNLP)       # SimplePos() 사용
library(stringr)     # str_match_all(), str_length() 사용
txt <- gsub('\r',' ',text) 
t1 <- SimplePos09(txt)
t2 <- unlist(str_match_all(t1, '([A-Z가-�R]+)/N'))
t2 <- t2[!str_detect(t2, '/')]
head(t2)

t3 <- t2[str_length(t2) > 1]
t6 <- table(t3)

t6 <- t6[t6 > 3]
t7 <- sort(t6, decreasing = T)
head(t7, 100)
head(names(t7),100)

wordcloud2(t7, size = .6)  
library(jpeg) # 결과물 jpg 저장
jpeg("c:/r/solid.jpg")

wordcloud2(t7, size = .6)

dev.off()
html <- read_html(url[1], encoding = "euc-kr")

html_nodes(html, "#body")%>%
  html_text()
#은 아이디고 . 은 스타일

html_jj <- read_html("http://map.daum.net/?map_type=TYPE_MAP&q=%EC%A4%91%EA%B5%AD%EC%A7%91&currentBound=true&urlX=513780&urlY=1115332&urlLevel=3")
html_jj
url_jj <- html_nodes(html_jj, css = ".PlaceItem .para")%>%
  html_nodes("a")%>%
  html_attr("href")
url_jj
html_nodes(html_jj, "#name")%>%
  html_text()
# 숙제 : 관심있는 주제 스크롤링 해서 결과물 발표준비(작은 포트폴리오)
# 허경영, ppap,

html <- read_html("http://book.naver.com/bookdb/review.nhn?bid=1490613&page=2")
html
url <- html_nodes(html, css = ".review .basic dt")%>%
  html_nodes("a")%>%
  html_attr("href")
url
x<-read_html(url[2])
x
html_nodes(x, "#twocols")%>%
  html_text()
text <- NULL

for(i in 1:length(url)){
  
  html_nodes(url[i], )
  
}
Sys.setlocale(category = "LC_ALL", locale = "us")
Encoding(url) <- 'UTF-8'

## 네이버 책 - 시계태엽오렌지 리뷰 분석
# text 초기화(나중에 게시판 글 담을 벡터)
text <- NULL    

## 전체페이지 : 1~27
# 각 페이지별로 각 게시글에 대한 url을 생성 
for(i in 1:10){  
  
  html <- read_html(paste0("http://finance.naver.com/item/board.nhn?code=053110&page=", i), encoding = "euc-kr")
  
  url <- html_nodes(html, css = "table.type2 td.title")%>%
    html_nodes("a")%>%
    html_attr("href")
  
  ## 페이지별 게시글 : 1~20개  
  # 각 게시글 내용 text(벡터)에 저장 
  for(j in 1:length(url)){   
    
    url1 <- paste0("http://finance.naver.com", url[j])
    
    html1 <- read_html(url1, encoding = "euc-kr")
    
    text <- c(text, html_nodes(html1, "#body")%>% html_text())
    
  }
}
# 위 결과 확인 
text
install.packages("wordcloud2")
library(wordcloud2)  # wordcloud2() 사용

install.packages("KoNLP")
library(KoNLP)       # SimplePos() 사용
library(stringr)     # str_match_all(), str_length() 사용
txt <- gsub('\r',' ',text) 
t1 <- SimplePos09(txt)
t2 <- unlist(str_match_all(t1, '([A-Z가-�R]+)/N'))
t2 <- t2[!str_detect(t2, '/')]
head(t2)

t3 <- t2[str_length(t2) > 1]
t6 <- table(t3)

t6 <- t6[t6 > 2]
t7 <- sort(t6, decreasing = T)
head(t7, 100)
head(names(t7),100)

wordcloud2(t7, size = .6)  
library(jpeg) # 결과물 jpg 저장
jpeg("c:/r/solid.jpg")

wordcloud2(t7, size = .6)

dev.off()
html_gj <- read_html("http://tour.gwangju.go.kr/home/tour/mytour/open.cs?searchCondition=&searchKeyword=&&pageIndex=1")
html_gj
url_gj <- html_nodes(html_gj, css = ".mycourse_board li")%>%
  html_nodes("a")%>%
  html_attr("href")
url_gj
html1 <- read_html(paste0("http://tour.gwangju.go.kr/home/tour/mytour/open.cs",url_gj[1]))

html_nodes(html1, ".custom_view_body .desc_course")%>%
  html_text


## 오매광주 홈페이지(http://tour.gwangju.go.kr/home/main.cs)에서 네티즌 추천코스 크롤링
# text 초기화(나중에 추천코스 담을 벡터)
text <- NULL

# 총 페이지 : 1~6
for(i in 1:6){
  
  html_gj <- read_html(paste0("http://tour.gwangju.go.kr/home/tour/mytour/open.cs?searchCondition=&searchKeyword=&&pageIndex=", i))
  
  url_gj <- html_nodes(html_gj, css = ".mycourse_board li")%>%
    html_nodes("a")%>%
    html_attr("href")
  
  # 각 페이지당 게시글   
  for(j in 1:length(url_gj)){
    
    html_gj.1 <- read_html(paste0("http://tour.gwangju.go.kr/home/tour/mytour/open.cs",url_gj[j]))
    
    text <- c(text, 
              html_nodes(html_gj.1, ".custom_view_body .desc_course")%>%
                html_text())
    
  }
}
# 크롤링 결과 확인
text
library(wordcloud2)  # wordcloud2() 사용
library(KoNLP)
useSejongDic()
useNIADic()
정재작업
text1 <- unlist(strsplit(text, "\t"))
text1[grep("\n", text1)] <- ""
text2 <- grep("[가-�R]", text1, value = T)
text3 <- unlist(strsplit(text2, "^[[:digit:]$]"))
text4 <- unlist(strsplit(text3, "^[[:punct:]]"))
text4 <- grep("[가-�R]", text4, value = T)
text5 <- sort(table(text4), decreasing = T)

text5
wordcloud2(text5)
install.packages("ggmap")
library(ggmap)

# 추천 명소 TOP 10, 데이터프레임 형식으로 변환
head(text5, 10)
text6 <- as.data.frame(head(text5, 10))
names(text6) <- c("loc", "cnt")

text6
# repeat{
#   cen <- geocode(enc2utf8(text6$loc))
#   if(is.na(cen) == FALSE){break}
# }

# 장소별 경도, 위도(데이터프레임 형식으로 생성)
cen <- data.frame(lon = c(126.91931,126.91859,126.92462,126.92390,126.79194,
                          126.91990,126.91187,126.91787,126.79469,126.85700), 
                  lat = c(35.12882,35.14812,35.15667,35.13276,35.13643,
                          35.14700,35.14189,35.15195,35.13960,35.15503))
cen

# 경도, 위도 text6에 추가
text6$lon <- cen$lon
text6$lat <- cen$lat

text6
gj <- geocode(enc2utf8("광주광역시"))

map1 <- get_map(location = as.numeric(gj), zoom = 12, maptype = "hybrid", color = "bw")
map2 <- get_map(location = as.numeric(gj), zoom = 12, maptype = "roadmap", color = "bw")
library(ggplot2)
ggmap(map1)+
  geom_point(data = text6, aes(x = lon, y = lat, colour = loc, size = cnt), alpha = .7)
ggmap(map2)+
  geom_point(data = text6, aes(x = lon, y = lat), 
             colour = rainbow(nrow(text6)), size = 5, alpha = .6)+
  geom_text(data = text6, aes(x = lon, y = lat, label = loc), 
            check_overlap = TRUE, size = 3, nudge_x = .01, nudge_y = .002)