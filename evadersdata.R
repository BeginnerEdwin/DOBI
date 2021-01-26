library(httr)
library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(leaflet)

request_url = 'https://www.nts.go.kr/nts/ad/openInfo/selectList.do'
referer_url <- 'https://www.nts.go.kr/nts/ad/openInfo/selectList.do' 

# 3페이지, 한 번에 20개씩.   

data <- POST(request_url, query = request_data)  
content(data, as = 'text') #페이지 정보를 받아온 뒤 content만 발라내고 

col <- data %>% read_html(encoding = "UTF-8") %>% 
  html_node('thead') %>% 
  html_nodes('tr') %>% 
  html_nodes('th') %>% 
  html_text() 
col <- gsub("[\r\n\t]", "", col)
#column data 먼저 뽑아내고 

for(i in 1:390){
    request_data <- list(
      currPage = i %>% as.character(),  
      sysld = 'nts',
      search_order = '1',
      tcd = '2',
      pageIndex = '100')
    
    data <- POST(request_url, query = request_data,
                 add_headers(referer = referer_url))  #여기에 add_header 추가해야 하나?
    content(data, as = 'text') #페이지 정보를 받아온 뒤 content만 발라내고 
    #user agent Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36
    
    page <- list()
    
    page[[i]] <- data %>% read_html(encoding = "UTF-8") %>% 
      html_nodes('tbody') %>% 
      html_nodes('tr') %>% 
      html_nodes('td') %>% 
      html_text()
    
     Sys.sleep(1)
}

head(page)
testpage <- page 
testpage <- testpage %>% unlist()
length(testpage)
str(testpage)
428472/11
testpage[428472]
df <- data.frame(matrix(ncol = 11, nrow = 38952))
colnames(df) <- col
#빈 데이터 프레임을 만들었당. 

for(i in 1:38952){
df[i,1] = testpage[11*i-10]
df[i,2] = testpage[11*i-9]
df[i,3] = testpage[11*i-8]
df[i,4] = testpage[11*i-7]
df[i,5] = testpage[11*i-6]
df[i,6] = testpage[11*i-5]
df[i,7] = testpage[11*i-4]
df[i,8] = testpage[11*i-3]
df[i,9] = testpage[11*i-2]
df[i,10] = testpage[11*i-1]
df[i,11] = testpage[11*i]
} #데이터 넣기 완료! 

head(df)
str(df)
df$상호 <- gsub("[\r\n\t]", "", df$상호)
evaderinfo <- df
str(evaderinfo)
evaderinfo$No <- as.numeric(evaderinfo$No)
evaderinfo$'공개년도' <- as.numeric(evaderinfo$'공개년도')
evaderinfo$'연령' <- as.numeric(evaderinfo$'연령')
evaderinfo$'총 체납액' <- as.numeric(gsub(pattern = "[^0-9]", replacement = "", evaderinfo$`총 체납액`))
evaderinfo$'납기' <- as.Date(evaderinfo$'납기')

evaderinfo <- arrange(evaderinfo, No) #순서대로 정렬
evaderinfo$No <- NULL #순서가 있는 칼럼과 행순서를 맞춰준 뒤 No칼럼을 
head(evaderinfo)
getwd()
write.csv(evaderinfo, 'Tax evaders map/evaderinfo.csv')
head(arrange(evaderinfo, 연령)) #가장 어린사람은 몇 살일까? 17살의 이예분 양도소득세 등 7억2900만원.  
head(arrange(evaderinfo, ))
colSums(is.na(evaderinfo))
evaderinfo %>% filter(is.na(연령))
col <- c('publication', 'name', 'age', 'businessname', 'job(category)', 'address', 'totalarrears', 'item', 'duedate', 'detail')
#column이름을 
colnames(evaderinfo) <- col
head(evaderinfo)

##도시별 구분해보자. 
evaderinfo_seoul <- evaderinfo %>% 
  filter(grepl("서울", address))
count(evaderinfo_seoul)
head(evaderinfo_seoul)
write.csv(evaderinfo_seoul, 'Tax evaders map/evaderinfo_seoul.csv')

head(evaderinfo_seoul)
nrow(evaderinfo_seoul) #8410개의 서울의 탈세자 정보.

for(i in 1:nrow(evaderinfo_seoul)){
  
  Sys.setlocale("LC_ALL", "English")
  kakaoapi <-'8effd551d68e1e695553e93d45e22bef'
  url=c("https://dapi.kakao.com/v2/local/search/address.json")
  query <- URLencode(paste0("?query=", evaderinfo_seoul$address[i]))
  #여기서 Bib2021은 데이터프레임이며, 1열 1행~60행까지 각종 위치키워드가 입력되어 있습니다. 
  
  res <- GET(str_c(url, query), 
             add_headers("Authorization" = str_c("KakaoAK ", kakaoapi)))
  result <- content(res)
  evaderinfo_seoul$long[i] <- result$documents[[1]]$address$x
  evaderinfo_seoul$lat[i] <- result$documents[[1]]$address$y
  Sys.setlocale("LC_ALL", "Korean")
}

head(evaderinfo_seoul)
evaderinfo_seoul$long <- as.numeric(evaderinfo_seoul$long)
evaderinfo_seoul$lat <- as.numeric(evaderinfo_seoul$lat)
#long, lat에다가 좌표데이터 넣고 데이터 형태 변환까지 완료! 


gu <- regexpr("[[:alpha:]]{1,}구", evaderinfo_seoul$address)
evaderinfo_seoul$address_gu <- regmatches(evaderinfo_seoul$address, gu)
#문제발생 
grepl("구", evaderinfo_seoul$address) 
str_detect(evaderinfo_seoul$address, "구") 
#위 둘이 같음
grep("구", evaderinfo_seoul$address)
str_which(evaderinfo_seoul$address, "구")
#위 둘이 같음. 
sum(!grepl("구", evaderinfo_seoul$address)) 
#"구"가 포함되어 있지 않은, FALSE값이 총 17개. 

evaderinfo_seoul[678, ]
evaderinfo_seoul[939, ] #'독서울길', '서울대학로'가가 들어왔다. 그런데 하나하나 찾기가 어려우니까 if문 사용해보자.
grep("경기", evaderinfo_seoul$address, value = TRUE) #여기에서 서울 데이터 네 개만 제하고 다 없애면 됨. 

grep("시흥시", evaderinfo_seoul$address)
grep("파주시", evaderinfo_seoul$address)
grep("양평군", evaderinfo_seoul$address)

head(evaderinfo_seoul)

