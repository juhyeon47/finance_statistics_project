##  금융통계 기말 프로젝트


<br/> 

***무역회사와 판매업체***의 업종을 위주로 ***외국인 비율이 높은 회사***의 특징을 살펴보도록 할 것이다.

<br/> 



## 1. 회사별 업종 탐색

<br/> 

`-` **포스코인터내셔널** : 에너지, 철강, 식량, 미래신사업

<br/> 

`-` **lx인터내셔널** : 자원개발, trading사업, 그린/신성장사업

<br/> 

`-`  **현대코퍼레이션** : 철강, 석유화학, 자동차, 상용차량, 선박, 플랜트, 전기전자, 신재생에너지, 건설장비, 자원개발, 해외투자

<br/> 

`-`  **gs global** : 철강, 석유화학, 산업재, e모빌리티, 리사이클링, 헬스케어, 태양광에너지, 바이오에너지, gs엔텍, pls

<br/> 

`-`  **stx** : 에너지사업, 원자재수출입, 기계/엔진, 해운/물류, 신규프로젝트

<br/> 

## 2. 종목코드와 종목이름 얻기
```
url <- "https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=334"

html <- read_html(url, encoding = "euc-kr")

종목코드    <- html %>%
  html_nodes("td") %>%
  html_nodes("a") %>%
  html_attr("href")

종목코드    <-
  종목코드[grep("code",    종목코드)] #앞의 종목코드에는 페이지 관련 내용도 포함되어 있기 때문에 코드만 포함된 것들을 추출해준다.

종목코드    <-
  unlist(strsplit(종목코드, "code=")) #코드를 기준으로 나눠줌으로써 코드만 추출해낼 수 있도록 한다.

종목코드    <-    종목코드[c(FALSE, TRUE)]  #코드는 짝수번째에만 존재한다.

종목코드    <-    종목코드[c(FALSE, TRUE)]

종목이름  <- html %>%
  html_nodes("td") %>%
  html_nodes("a") %>%
  html_text()

종목이름 <- 종목이름[c(TRUE, FALSE)]
```

### 3. 시가변동그래프 살펴보기
```r
chart_function <- function(code){

  base_url_1 <- "https://api.finance.naver.com/siseJson.naver?symbol="
  
  base_url_2 <- "&requestType=1&startTime=20130601&endTime=20230611&timeframe=month"
  
  url <- paste0(base_url_1, code, base_url_2)
  
  html <- read_html(url)
  
  info <- html %>%html_text()
  
  info <- gsub("\\n|\\t", "", info)
  
  info <- gsub("\\\"", "",info) #문자는 " "로 되어 있어야하고 숫자는 " "로 되어 있으면 안 되는데 " "로 연결된 숫자들에 대해 이들을 공백으로 대체해줄 것이다.
  
  info <- gsub("'", "\"", info) #마찬가지로 " "의 형식으로 맞춰주기 위해 ' '형태를 " "로 바꿔준다.
  
  info <- jsonlite::fromJSON(info)
  
  
  info_df <- data.frame(info[-1,], stringsAsFactors = FALSE) #첫번째 열은 제외하고 데이터프레임을 만들어 주는데, 열 이름으로 사용할 것이기 때문이다.
  colnames(info_df) <- info[1, ] #앞에서 제외한 열을 열 이름으로 넣어준다.
  
  info_df$날짜 <- as.Date(info_df$날짜, format = "%Y%m%d")
  
  print(info_df)
  
}

for (i in 1:length(종목코드)) {

    if (nrow(chart_function(종목코드[i])) != 0) {
      # 종목별 플롯 이름 설정
      plot_name <- paste(종목이름[i], "시가 변동 그래프", sep = " ")
      
      # 플롯 생성
      plot(chart_function(종목코드[i])$날짜, chart_function(종목코드[i])$시가, type = "l", main = plot_name)
      
      # 선형 회귀 직선 추가
      abline(lm(chart_function(종목코드[i])$시가 ~ chart_function(종목코드[i])$날짜), col = "red")
    }
}
```

