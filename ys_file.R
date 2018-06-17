#처음 R을 수행하기 전에 해야할일
#1. JAVA_HOME 환경변수 선언하기
#2. r selenium 설정만들기(http://hmtb.tistory.com/5) <- 사이트 보고 설정하기
#3. 2번까지 설정 후 콘솔에 가상 서버 실행 시키기. 위에 사이트에서 한것처럼(cmd setup)
#4. 서버 설정 후 아래 코드를 실행해 보자


install.packages("RSelenium")
library(RSelenium) #selenium 라이브러리 불러오기
remDr <- remoteDriver(remoteServerAddr = "localhost", port= 4445L, browserName ="chrome") #콘솔에 띄운 서버 접속 코드를 변수에 넣기
remDr$open() #크롬 브라우저 열기
remDr$navigate("http://race.kra.co.kr/dbdata/textData.do?Act=11&Sub=1&meet=1") # 원하는 사이트에 접속하기 주소에 원하는 주소를 넣으면 된다.
#나는 윤상이가 원하는 경마자료사이트로 넣었다.

#경주마 링크 객체 찾기▼
webElem0 <- remDr$findElement(using = "xpath", "//*[@id='contents']/ul/li[3]/ul/li[1]/a")
#찾은 객체 클릭 -> 해당 링크로 이동
webElem0$clickElement()


library(rvest)
#미리 변수 선언해 주기
kfile <- NULL
kfile1 <- NULL
kfile3 <- NULL
#10개씩 출력되는 페이지를 16번 넘기면서 각 페이지별로 나와있는 파일명을 가져온다.
for(j in 1:16){
  
  #이중 포문의 이유는 1~10페이지에 있는 것을 1개씩 가져온다.  i는 3부터 시작하는데 1번 페이지 객체시작이 3번부터이다.
  for(i in 3:13){
    
    # 페이지명을 프린트로 찍어준다. 로그 확인용
    ifelse(i == 13, print("1페이지"),print(paste(i-2,"페이지"))) 
    
    #1~10페이지의 객체를 하나씩 찾는다. 포문을 도는 i를 표시
    webElem11 <- remDr$findElement(using = "xpath", value= paste("//*[@id='inputVo']/div[2]/a[",i,"]", sep=""))
    #찾아서 클릭해준다.
    webElem11$clickElement()
    #로딩시간을 위해 2초 딜레이를 준다
    Sys.sleep(2)
    
    #해당 페이지에 있는 파일명들을 가져온다.
    source<-remDr$getPageSource()[[1]]
    #HTML을 읽어온다.
    html <- read_html(source)
    
    #중복되는 파일명을 없애기 위한 변수 선언
    kfile1 <- kfile 
    #kfile에 파일명들을 넣어준다.
    kfile <- html_nodes(html, css ="td >a")%>%html_text() 
    #로그용으로 아래 프린트문을 보여준다.
    print(paste("이전거:",kfile1[1]))
    print(paste("지금거:",kfile[1]))
    #현재 불러온 파일명과 이전에 불러왔던 파일명을 비교하여 같으면 저장하지 않고, 다르면 새로운 변수에 누적으로 저장한다.
    ifelse(kfile1[1] == kfile[1], print("저장하지마"), kfile3 <-rbind(kfile3, kfile))
  }
}
#전체 크롤링해온 파일명 모음보기
kfile3
#CSV파일로 저장
write.csv(kfile3, file = 'c:\\r\\race.csv', row.names=F)


#총 파일명 개수 포문 돌리기 위해 살펴보기
length(kfile3[,1]) #151개
length(kfile3[1,]) #10개

#첫번째 파일명 확인
kfile3[1,1]

#kfile3를 이용하여 경주마 정보 가져오기, kfile3 첫번쨰에 들어있는 파일명으로 가져와 보자!
grade <- readLines(paste('http://race.kra.co.kr/dbdata/fileDownLoad.do?fn=internet/seoul/horse/', kfile3[1,1], sep=""))

#잘 저장했는지 확인해보자
grade

#확인 완료 후 변수 재활용을 위해 널처리를 해주자
grade<- NULL

#오케이 이제는 저거를 몽땅 csv파일에 저장해 보자 근데 파일 하나하나 저장하는데 오래 걸리니까 중간중간 딜레이를 잘 걸어보자
#10개씩 151개가 있으니 2중포문을 또 돌려보자
#첫번째 포문은 전체 개수인 열단위 kfile3[,i]로 돌린다.
for(i in 1:length(kfile3[,1])){
  #두번째 포문은 각 열마다 10개의 행이 있다. 그래서 10개씩 돌려보자
  for(j in 1:10){
    #이중 포문을 돌면서 해당 주소의 텍스트를 grade로 가져오자
    grade <- readLines(paste('http://race.kra.co.kr/dbdata/fileDownLoad.do?fn=internet/seoul/horse/', kfile3[i,j], sep=""))
    #파일을 만들어 누적해서 저장해보자, 변수에 넣기에는 너무 많잖아?
    write.table(grade, file = 'c:\\r\\race_data.csv', sep=",", row.names=FALSE, col.names=FALSE, quote=FALSE, append = T)
    print(paste((152-i)*10-j, "번째 완료!", sep=""))
  }
  print("=================================================")
}

#race_data.csv 파일의 용량은 318메가 정도였음
#그 파일을 이용해서 분석하면 될듯

text <- read.table("http://race.kra.co.kr/dbdata/fileDownLoad.do?fn=internet/seoul/horse/20180204sdb1.txt")
head(text)
