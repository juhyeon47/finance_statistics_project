##  금융통계 기말 프로젝트


<br/> 

***무역회사와 판매업체***의 업종을 위주로 ***외국인 투자 비율이 높은 회사***의 특징을 살펴보도록 할 것이다.

<br/>

**재무비율의 이론적 지식**을 기반으로 **실제 투자**에서도 이를 근거로 투자를 하는지 궁금증이 생겼다.

<br/>

특히, **포스코인터내셔널 VS LX인터내셔널**과 **GS글로벌 VS 현대코퍼레이션**의 비교를 통해 왜 LX인터내셔널과 현대코퍼레이션의 외국인 투자 비율이 높게 나타나는지 알아보고자 한다.


<br/> 



## 1. 회사별 사업 탐색

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
<br/> 

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
<br/> 

## 3. 시가변동그래프 살펴보기
<br/> 

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
<br/> 

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfNjgg/MDAxNjg2NDE2NzUyNzUy.HgVURp78-9sR9yISSLMBwA8wknu1uzxMBdxxcFyVTgQg.m77itb196VRvCAGaN6vQFqDCCFLWYQF4py-X985Nl0cg.PNG.juhyunss_/%EC%8B%9C%EA%B0%80%EB%B3%80%EB%8F%99%EA%B7%B8%EB%9E%98%ED%94%84.png?type=w773" data-lazy-src="" data-width="693" data-height="363" alt="" class="se-image-resource egjs-visible">

<br/> 

## 4. 종목 별 IFRS를 얻기 위한 함수
<br/> 

```r
ifrs_function <- function(code){
  base_url <- "https://finance.naver.com/item/main.naver?code="
  url <- paste0(base_url, code)
  
  html <- read_html(url, encoding = "euc-kr")
  
  table <- html %>%
    html_nodes("table.tb_type1.tb_num") %>%
    html_table(fill = TRUE)
  
  
  table <- as.data.frame(table[[1]])
  table <- table[, -1]
  table <- table[-c(1,2), ]
  
  
  # Replace commas with spaces and convert to numeric
  for (i in 1:ncol(table)) {
    table[, i] <- as.numeric(gsub(",", "", table[, i]))
  }
  
  rownames(table) <- c("매출액", "영업이익", "당기순이익", "영업이익률","순이익률","ROE(지배주주)","부채비율", "당좌비율","유보율", "EPS(원)", "PER(배)", "BPS(원)", "PBR(배)", "주당배당금(원)", "시가배당률", "배당성향")
  colnames(table) <- c("2020.12",	"2021.12",	"2022.12",	"2023.12(E)",	"2022.03",	"2022.06",
                       "2022.09",	"2022.12",	"2023.03",	"2023.06(E)")
  
  print(table)
  
  table <- as.data.frame(table)
  table <- table[-1, ]
  table <- table[, -1]
  
  # Replace commas with spaces and convert to numeric
  for (i in 1:ncol(table)) {
    table[, i] <- as.numeric(gsub(",", "", table[, i]))
  }
  
  print(table)
}



#ifrs_function(종목코드[5])
ifrs_names <- c()

for(i in 1:5){
  ifrs_names[i] <- 종목이름[i]
}


first_df<- ifrs_function(종목코드[1])
second_df <- ifrs_function(종목코드[2])
third_df <- ifrs_function(종목코드[3])
fourth_df <- ifrs_function(종목코드[4])
fifth_df <- ifrs_function(종목코드[5])
```
<br/> 

## 5. 무역회사와 판매업체 비교 정보 

<br/> 

```r
url <- "https://finance.naver.com/item/main.naver?code=047050"
html <- read_html(url, encoding = "euc-kr")

table <- html %>%
  html_nodes("table.tb_type1.tb_num") %>%
  html_table(fill = TRUE)


trade_compare<- as.data.frame(table[[2]])

trade_compare <- as.data.frame(trade_compare)
rownames(trade_compare) <- trade_compare[,1]
trade_compare <- trade_compare[, -1]

for (i in 1:ncol(trade_compare)) {
  trade_compare[, i] <- as.numeric(gsub(",", "", trade_compare[, i]))
}

trade_compare <- trade_compare[complete.cases(trade_compare), ]
```
<br/> 

## 6. 무역회사와 판매업체 비교의 시각화

<br/> 

```r
par(mfrow = c(3,4))

# 색상정의
cozy_colors <- c("#ECE6D8", "#D8CEBA", "#C4B69C", "#B0A07E", "#9C8860")


# 루핑을 통해 

for (i in 1:nrow(trade_compare)) {
  values <- as.numeric(as.character(trade_compare[i,]))
  title <- paste0(rownames(trade_compare)[i], " Bar Graph of Trade Compare")
  
  # Set cozy color for each category
  bar_colors <- cozy_colors[1:length(values)]
  
  # Create bar plot with specified colors
  barplot(values, names.arg =  colnames(trade_compare), col = bar_colors, xlab = "Category", ylab = "Value", main = title)
  
}

```
<br/> 

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjk1/MDAxNjg2NDE3MjIxMzIz.adqcpWVGD9Rcvm3_fl6DjhKbdi9ZMDbijdsN2ngslAAg.W7RvSV0Kn5DrMq4Vy92TW0JRsZv80rvPSI2izFJF1s4g.PNG.juhyunss_/%EB%B9%84%EA%B5%90.png?type=w773" data-lazy-src="" data-width="693" data-height="402" alt="" class="se-image-resource egjs-visible">


- 차례대로 포스코인터내셔널, lx인터내셔널, 현대코퍼레이션, gs글로벌, stx이다.


<br/> 

## 7. 회사 분기 별 IFRS 시각화

<br/> 

```r
first_df_quater <-  first_df [-c(13,14,15),-c(3,9)][,-c(1,2)]
second_df_quater <- lx_international_ifrs [-c(13,14,15),-c(3,9)][,-c(1,2)]
third_df_quater<- stx_ifrx [-c(13,14,15),-c(3,9)][,-c(1,2)]
fourth_df_quater<- gs_global [-c(13,14,15),-c(3,9)][,-c(1,2)]
fifth_df_quater <- hyundai_coporation [-c(13,14,15),-c(3,9)][,-c(1,2)]



# Iterate over each row and create a bar plot
par(mfrow = c(3,4))

for (i in 1:nrow(first_df_quater)) {
  values <- as.numeric(as.character(first_df_quater[i, ]))
  rowname <- rownames(first_df_quater)[i]
  title <- paste0(rowname, " Bar Graph")
  
  # Create the bar plot using cozy colors
  barplot(values, names.arg = colnames(first_df_quater),
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
  
  # Add numbers in the middle of the bars
  text(x = 1:length(values), y = values/2, labels = values, pos = 3)
  
  # Add a trend line
  trend_values <- 1:length(values)
  lines(trend_values, values, col = "black")
}


for (i in 1:nrow(second_df_quater)) {
  values <- as.numeric(as.character(second_df_quater[i, ]))
  rowname <- rownames(second_df_quater)[i]
  title <- paste0(rowname, " Bar Graph")
  
  # Create the bar plot using cozy colors
  barplot(values, names.arg = colnames(second_df_quater),
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
  
  # Add numbers in the middle of the bars
  text(x = 1:length(values), y = values/2, labels = values, pos = 3)
  
  # Add a trend line
  trend_values <- 1:length(values)
  lines(trend_values, values, col = "black")
}


for (i in 1:nrow(third_df_quater)) {
  values <- as.numeric(as.character(third_df_quater[i, ]))
  rowname <- rownames(third_df_quater)[i]
  title <- paste0(rowname, " Bar Graph")
  
  # Create the bar plot using cozy colors
  barplot(values, names.arg = colnames(third_df_quater),
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
  
  # Add numbers in the middle of the bars
  text(x = 1:length(values), y = values/2, labels = values, pos = 3)
  
  # Add a trend line
  trend_values <- 1:length(values)
  lines(trend_values, values, col = "black")
}

for (i in 1:nrow(fourth_df_quater)) {
  values <- as.numeric(as.character(fourth_df_quater[i, ]))
  rowname <- rownames(fourth_df_quater)[i]
  title <- paste0(rowname, " Bar Graph")
  
  # Create the bar plot using cozy colors
  barplot(values, names.arg = colnames(fourth_df_quater),
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
  
  # Add numbers in the middle of the bars
  text(x = 1:length(values), y = values/2, labels = values, pos = 3)
  
  # Add a trend line
  trend_values <- 1:length(values)
  lines(trend_values, values, col = "black")
}


for (i in 1:nrow(fifth_df_quater)) {
  values <- as.numeric(as.character(fifth_df_quater[i, ]))
  rowname <- rownames(fifth_df_quater)[i]
  title <- paste0(rowname, " Bar Graph")
  
  # Create the bar plot using cozy colors
  barplot(values, names.arg = colnames(fifth_df_quater),
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
  
  # Add numbers in the middle of the bars
  text(x = 1:length(values), y = values/2, labels = values, pos = 3)
  
  # Add a trend line
  trend_values <- 1:length(values)
  lines(trend_values, values, col = "black")
}
```
<br/> 

`-` STX

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjk1/MDAxNjg2NDE3MjIxMzIz.adqcpWVGD9Rcvm3_fl6DjhKbdi9ZMDbijdsN2ngslAAg.W7RvSV0Kn5DrMq4Vy92TW0JRsZv80rvPSI2izFJF1s4g.PNG.juhyunss_/%EB%B9%84%EA%B5%90.png?type=w773" data-lazy-src="" data-width="693" data-height="402" alt="" class="se-image-resource egjs-visible">

<br/> 

`-` 현대코퍼레이션

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMTc2/MDAxNjg2NDE3NTYzNTgz.TBPSG1_Tm7l6oPxxZ82YOZiq2QQVFtIs8ZGzNfd2ph8g._9Karo0cunF6L-C1mR0oTfuoVufcky26Fr14XVvM5UQg.PNG.juhyunss_/%EC%B2%AB%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="363" alt="" class="se-image-resource egjs-visible">

<br/> 

`-` LX인터내셔널

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMTY3/MDAxNjg2NDE3NTY2NjE0.cgW9fgdHCGQzi2um3aHt_HKvOfwr0kSGwY9a46H14P0g.ToZT57zdmQO78VRzTpF5_NOXaaMNwmd2kPlmMYDw9o8g.PNG.juhyunss_/%EC%84%B8%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/> 

`-` GS GLOBAL

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjMw/MDAxNjg2NDE3NTY3OTA3.Gwu5lzOmbT_UZp8N1GHeajyLrjRg-CpLvgvQTtRBpl8g.FQ87lC0Xd9HNk2q1RCMVGlQ8NrTL-mVodfjrSN859yAg.PNG.juhyunss_/%EB%84%A4%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/> 

`-` 포스코인터내셔널

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjEw/MDAxNjg2NDE3NTcwMzQy.tQZS0nVC1thTrTnT-kLti28WH5mPFr7e_6Akw0DRtpgg.R4XXAm0j5SLLDz7bloiXW7hevwyDwX4csykyB9p2jRwg.PNG.juhyunss_/%EB%8B%A4%EC%84%AF%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/> 

```r
for (i in 1:nrow(first_df_quater)) {
  values <- as.numeric(as.character(first_df_quater[i, ]))
  rowname <- rownames(first_df_quater)[i]
  title <- paste0(rowname, " Boxplot")
  
  # Create the boxplot using cozy colors
  boxplot(values, names = rowname,
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
}

for (i in 1:nrow(second_df_quater)) {
  values <- as.numeric(as.character(second_df_quater[i, ]))
  rowname <- rownames(second_df_quater)[i]
  title <- paste0(rowname, " Boxplot")
  
  # Create the boxplot using cozy colors
  boxplot(values, names = rowname,
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
}


for (i in 1:nrow(third_df_quater)) {
  values <- as.numeric(as.character(third_df_quater[i, ]))
  rowname <- rownames(third_df_quater)[i]
  title <- paste0(rowname, " Boxplot")
  
  # Create the boxplot using cozy colors
  boxplot(values, names = rowname,
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
}

for (i in 1:nrow(fourth_df_quater)) {
  values <- as.numeric(as.character(fourth_df_quater[i, ]))
  rowname <- rownames(fourth_df_quater)[i]
  title <- paste0(rowname, " Boxplot")
  
  # Create the boxplot using cozy colors
  boxplot(values, names = rowname,
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
}

for (i in 1:nrow(fifth_df_quater)) {
  values <- as.numeric(as.character(fifth_df_quater[i, ]))
  rowname <- rownames(fifth_df_quater)[i]
  title <- paste0(rowname, " Boxplot")
  
  # Create the boxplot using cozy colors
  boxplot(values, names = rowname,
          xlab = "Quarter", ylab = "Value", main = title,
          col = cozy_colors)
}
```
<br/> 

`-` STX

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfNzYg/MDAxNjg2NDkwNjcxMTE3._kWudFl_VV-u4jqRPS5rhTT5wI0M6wC1ZquWirTpPIgg.DEPU-wE5yz7psjkHcDPCWnb0rbjAW-myVMYC2KSAUIUg.PNG.juhyunss_/111.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/> 

`-` 현대코퍼레이션

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMTIw/MDAxNjg2NDkwNjc2ODA2.dW3sd_Bhl8G26bhvCkYmdySrx09Qe364JgdM6laYlvgg.HaaysOZpE55OrORSwUwccuj9qHMVOJhp-7tHZ0fFXMwg.PNG.juhyunss_/222.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">


<br/> 

`-` LX인터내셔널

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjc0/MDAxNjg2NDkwNjc5MTE0.IcJ6b4hUfBnDRXAOvibg_CIspXgafQVR3jsFzO2DHm8g.smvziFOlhqZjO551Rmfgo8HT5ls9aURVxgGlWPUUxyUg.PNG.juhyunss_/333.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/> 

`-` GS GLOBAL

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjYw/MDAxNjg2NDkwNjgxMDU1.ScjdLfo_5tFMlEIddsuspMrczwu9lr-NNZGsxmxW3xYg.Y6QUmAYO-uGLVZluWN7zE_nA552UaNdZh3RKQYlL8KMg.PNG.juhyunss_/444.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">


<br/> 

`-` 포스코인터내셔널

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfNzIg/MDAxNjg2NDkwNjgzNTI5.Fap42Zxh-wD_fgelB9sGVNocZjTLtVS9ssr3dyApQmAg.f5zV0fV03TufQxj2kz-zO_OSl0eeNkmunLxSgJEO63sg.PNG.juhyunss_/555.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/> 


## 8. 비교 및 해석
<br/> 

### (1) 사업보고서 분석

5개 회사 공통적으로 **ESG경영**으로의 흐름 변화를 기반으로 ***'친환경 에너지'*** 에 주력을 두고 있다. 친환경 에너지로의 패러다임 전환을 통해 세계 각국의 **협업과 신사업**을 기대할 수 있다.
<br/> 
과거에는 주주중심의 이익극대화가 기업의 목적이었지만 현재는 ***이해관계자중심의 이익극대화***로 변화되면서 ESG경영이 더욱 주목 받는 이유 중 하나이다. 그래야 기업은 **지속가능한성장**을 할 수 있다.

<br/> 

### (2) 시가 변동 그래프 분석
<br/>

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfNjgg/MDAxNjg2NDE2NzUyNzUy.HgVURp78-9sR9yISSLMBwA8wknu1uzxMBdxxcFyVTgQg.m77itb196VRvCAGaN6vQFqDCCFLWYQF4py-X985Nl0cg.PNG.juhyunss_/%EC%8B%9C%EA%B0%80%EB%B3%80%EB%8F%99%EA%B7%B8%EB%9E%98%ED%94%84.png?type=w773" data-lazy-src="" data-width="693" data-height="363" alt="" class="se-image-resource egjs-visible">

<br/>

전체적으로 주가가 떨어진 것을 확인할 수 있다. 2021년에는 COVID-19으로 인한 유가 하락, 미국과 중국의 무역 갈등, 글로벌 경기 둔화 등 불확실성에 관한 위험이 있었다. 또한 2022년에는 미국의 기준금리인상에 따른 글로벌 경기둔화, 미국과 중국의 패권 경쟁지속으로 이한 자국 중심의 보호무역주의 강화, 코로나 재확산 등 불확실성의 위험이 존재하였다. **예측불가능한 위험** 때문에 기업에서는 **친환경에너지사업**을 통해 더욱 **미래 지향적 성과 극대화**를 이루어내고자 한다.

<br/>

### (3) 동일업종비교(2023년 03월 분기 기준)

<br/>

- 현재가 : 포스코인터내셔널 > LX인터내셔널 > 현대코퍼레이션 > STX > GS글로벌
- 시가총액(억) : 포스코인터내셔널 > LX인터내셔널 > 현대코퍼레이션 > GS글로벌 > STX
- 외국인취득률(%) : LX인터내셔널 > 현대코퍼레이션 > STX > 포스코인터내셔널 > GS글로벌
- 매출액 (억) : 포스코인터내셔널 > LX인터내셔널 > 현대코퍼레이션 > GS글로벌 > STX
- 영업이익(억) : 포스코인터내셔널 > LX인터내셔널 > 현대코퍼레이션 > GS글로벌 > STX
- 조정영업이익 : 포스코인터내셔널 > LX인터내셔널 > 현대코퍼레이션 > GS글로벌 > STX
- 영업이익증가율(%) : 현대코퍼레이션 > 포스코인터내셔널 > GS글로벌 > LX인터내셔널 > STX
- 당기순이익(억) : 포스코인터내셔널 > LX인터내셔널 > 현대코퍼레이션 > GS글로벌 > STX
- 주당순이익(원) : LX인터내셔널 > 현대코퍼레이션 > 포스코인터내셔널 > GS글로벌 > STX
- ROE(%) : LX인터내셔널 > 현대코퍼레이션 > GS글로벌 > 포스코인터내셔널 > STX
- PER(배) : 포스코인터내셔널 > 현대코퍼레이션 > GS글로벌 > LX인터내셔널 > STX
- PBR(배) : STX > 포스코인터내셔널 > GS글로벌 > LX인터내셔널 =  현대코퍼레이션

<br/>


- 차례대로 포스코인터내셔널, lx인터내셔널, 현대코퍼레이션, gs글로벌, stx이다.

**알아보고자 하는 것**

<br/>

**1. 현재가가 비슷한 포스코인터내셔널과 LX인터내셔널은 어떤 차이가 있길래 LX인터내셔널의 외국인 취득률이 더 높은가?**
**2. ROE, PER, PBR 투자를 위한 지표가 비슷하게 나타나는 현대코퍼레이션과 GS글로벌은 어떤 차이가 있길래 현대코퍼레이션의 외국인 취득률이 더 높은가?**

<br/>

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjk1/MDAxNjg2NDE3MjIxMzIz.adqcpWVGD9Rcvm3_fl6DjhKbdi9ZMDbijdsN2ngslAAg.W7RvSV0Kn5DrMq4Vy92TW0JRsZv80rvPSI2izFJF1s4g.PNG.juhyunss_/%EB%B9%84%EA%B5%90.png?type=w773" data-lazy-src="" data-width="693" data-height="402" alt="" class="se-image-resource egjs-visible">

<br/>

#### (3)-1. 변수의 의미

<br/>

- **시가총액 = 주가 * 발행주식수**
  - 주가가 동일 할 때, 발행주식수가 많으면 시가총액이 커지고 발행주식수가 적으면 시가총액이 작아진다. **발행주식수가 많다는 의미는 주가 변동성이 낮다**는 의미이다. 반면, **발행주식수가 적다는 의미는 수요가 공급보다 많을 때 주가 상승폭이 높다**는 의미이다.
   <br/>
  
  - 현재가가 비슷한 포스코인터내셔널과 LX인터내셔널을 비교해보았을 때, **LX인터내셔널의 발행주식수가 포스코인터내셔널보다 적은 것**을 예측할 수 있다. 이는 ***LX인터내셔널의 주가상승폭이 크다.*** 즉, **주가하락폭도 크다는 의미이고 변동성과 리스크가 크다**는 것을 예측해 볼 수 있다.
  - 시가총액이 현대코퍼레이션과 GS글로벌은 비슷하지만 현재가는 현대코퍼레이션이 GS글로벌에 비해 훨씬 높다. 즉, **현대코퍼레이션의 발행주식수가 GS글로벌에  적다.** 이는 ***현대코퍼레이션 주가상승폭이 크다.*** 즉, **주가하락폭도 크다는 의미이고 변동성과 리스크가 크다**는 것을 예측해 볼 수 있다.
 
 <br/>

- **영업이익, 조정영업이익률, 당기순이익은 매출액에 의해 영향**을 받기 때문에 동일업종비교의 순서가 똑같은 것을 예측할 수 있다.
  - **영업이익(EBIT)** = 매출총이익 - 판관비
  - **조정영업이익** : 영업이익 - 비정상적항목/비용 => 영업성과 더 정확하게 평가 가능
  - **당기순이익** :  영업이익 + 영업외수익 - 영업외비용 - 법인세 => 영업이익이 적자임에도 영업외수익 때문에 당기순이익이 흑자일 가능성도 존재
    - 당기순이익이 중요한 이유? 배당의 원천 = 과거 쌓아놓았던 현금인 **이익잉여금** + 영업을 통해 흘러온 현금인 **당기순이익**
    - 배당의 원천이되는 당기순이익이 많을 수록 주주에게 돌아가는 배당이 많다. 
 
 <br/>
 
- **주당순이익(EPS)** : 당기순이익/보통주식수(재무비율 중 수익성 판단 지표)
  - 당기순이익이 동일 할 때, **보통주식수가 적으면 주당순이익 값이 높고**, **보통주식수가 많으면 주당순이익 값이 작다.** 
  - 당기순이익이 비슷한 포스코인터내셔널과 LX인터내셔널을 비교해보았을때, LX인터내셔널의 보통주식수가 더 적은 것을 예측해볼 수 있다. 
 
 <br/>

-  **ROE** : 당기순이익/자기자본(재무비율 중 수익성 판단지표) : 주주지분으로 돈을 얼마나 효율적으로 벌었는지 판단
 
 <br/>
 
- **PER** : 시가총액/EPS
 
 <br/>
 
- **PBR** : 시가총액/순자산 
 
 <br/>
 
- 보통 **ROE, EPS 값이 높고** **PER와 PBR이 낮은** 종목을 투자하라고 한다. 왜냐하면 ***저평가*** 되어있다고 판단하기 때문이다.

 <br/>

***알아보고자하는 것***

 <br/>

**포스코인터내셔널과 LX인터내셔널의 비교를 통해 왜 LX인터내셔널의 외국인 비율이 높은지 알아볼 것이고, GS글로벌과 현대코퍼레이션의 비교를 통해 왜 현대코퍼레이션의 외국인 비율이 높은지 알아볼 것이다.**
 
 <br/>
 
*TX는 다른 종목들에 비해 성과가 좋지 않기 때문에 비교 항목에서 제외하였다.*

<br/>

### (4) 포스코인터내셔널 VS LX인터내셔널

<br/>

**2023년 3월**

<br/>

- 영업이익 : 포스코인터내셔널 > LX인터내셔널
- 당기순이익 : 포스코인터내셔널 > LX인터내셔널
- 영업이익률 : 포스코인터내셔널 < LX인터내셔널
- 순이익률 : 포스코인터내셔널 < LX인터내셔널
- ROE : 포스코인터내셔널 < LX인터내셔널
- 부채비율 : 포스코인터내셔널 > LX인터내셔널
- 당좌비율 : 포스코인터내셔널 < LX인터내셔널
- 유보율 : 포스코인터내셔널 < LX인터내셔널
- EPS : 포스코인터내셔널 < LX인터내셔널
- PER : 포스코인터내셔널 > LX인터내셔널
- BPS : 포스코인터내셔널 < LX인터내셔널
- PBR : 포스코인터내셔널 > LX인터내셔널

<br/>

`-` 포스코인터내셔널

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjEw/MDAxNjg2NDE3NTcwMzQy.tQZS0nVC1thTrTnT-kLti28WH5mPFr7e_6Akw0DRtpgg.R4XXAm0j5SLLDz7bloiXW7hevwyDwX4csykyB9p2jRwg.PNG.juhyunss_/%EB%8B%A4%EC%84%AF%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/>

`-` LX인터내셔널

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMTY3/MDAxNjg2NDE3NTY2NjE0.cgW9fgdHCGQzi2um3aHt_HKvOfwr0kSGwY9a46H14P0g.ToZT57zdmQO78VRzTpF5_NOXaaMNwmd2kPlmMYDw9o8g.PNG.juhyunss_/%EC%84%B8%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">


<br/>

#### (4)-1. 변수의 의미

<br/>

- **영업이익** : 매출총이익(매출액 - 매출원가) - 판관비
  - 상대적으로 **기업의 규모가 크며 매출액이 더 높은 포스코인터내셔널**이 유리하기 때문에  포스코인터내셔널의 **영업이익이 높게** 나타난다.

- **당기순이익** : 매출총이익(매출액 - 매출원가) - 판관비 +/- 영업외 수익과 비용 +/- 특별이익과 손익 = 영업이익 +/- 영업외 수익과 비용 +/- 특별이익과 손익 
  - **배당의 원천**이 되기에 중요함
  - 영업이익에서 빠지는 값이 포스코인터내셔널은 약 800억원, LX인터내셔널은 약 100억원으로 **LX인터내셔널은 불필요한 비용을 포스코인터내셔널보다 적다**는 것을 알 수 있다.

- **영업이익률** (마진율) : 영업이익/매출액 * 100 = (매출액 - 매출원가 - 판관비)/매출액 * 100
  - 기업의 영업활동에 의한 성과판단
  - 영업이익률이 높다는의미는 경쟁사를 압도하는 원가 경쟁력이 있다는 의미이다. 
  - 활용 : 동종 산업 내 기업 간의 수익성 비교, 운영 효율성 평가, 재무 건전성, 추세분석
  - **LX인터내셔널의 영업이익률이 더 높음** => 동종 산업 내 기업 간의 수익성, 운영 효율성, 재무 건전성 측면에서 포스코인터내셔널보다 우위에 있다.

- **순이익률** : 당기순이익/매출액 * 100
  - **매출 1단위 당 얼마나 많은 이익**이 발생했는지 보여줌
  - 한계 : 다른 산업간 수익성 비교 X, 자본구조가 다른 기업간 비교 X, 유형자산 비중 높은 회사는 높은 감가상각비로 순이익률 낮아짐, 높은 순이익률 기록했더라도 현금창출로 이어지지 않는 경우 의미 X(재무제표를 기록하는 회계는 **발생주의**에 근거하기 때문)
  - **LX인터내셔널의 순이익률이 더 높음** => 수익성 측면에서 우위에 있다.

- **ROE** : 당기순이익/자기자본 * 100
  - 값이 높을 수록 적은 자본으로 많은 이익을 얻고있다는 의미 => 수익성 좋
  - 당기순이익은 비슷하지만 LX인터내셔널의 ROE는 약 23% 포스코인터내셔널의 ROE는 약 13%로 **LX인터내셔널이 적은 자본으로 많은 이익**을 얻고있다.

- **부채비율** : 부채총액/자기자본 * 100
  - 선진국에서의 부채비율은 200%이면 적정하다고 판단 => 포스코인터내셔널이 더 높지만 **두 기업 모두 200%이내**
  - 기업에서는 어느정도의 부채도 필요함 => **레버리지효과** : 적은 자기 자본에 타인의 자본을 빌려 투자를 하면 큰 수익을 만들수 있다는 의미

- **당좌비율** : 당좌자산/유동자산 * 100 = (유동자산 - 재고자산)/유동자산 * 100
  - 당좌비율이 높을 수록 당장 현금화 할 수 있는 자산 많다는 의미 => 어려움 생겼을 때 바로바로 해결할 수 있는 능력
  - EX) 당좌자산 : 현금 (O) 부동산 (X)
  - **LX인터내셔널이 더 높음** => 즉각적 어려움 해결 능력이 더 높음
 
 - **유보율** : 잉여금/납입자본금 * 100
    - 기업에 남은 돈 얼마인가?
    - 유보율이 높으면 위기대처, 사업확장, 무상증자(주식대금 받지 않고 주주에게 주식 나누어주는 것 => 자본금 상승) 가능성 측면에서 유리함
    - 성장성, 안정성 측면에서 중요
    - **LX인터내셔널이 더 높음**

- **EPS** : 당기순이익/발행주식수
  - 주식 1주당 순이익(당기순이익에 대한 주주의 몫)
  - **LX인터내셔널이 더 높음**

- **PER** : 현재주가/EPS
  - 주가수익비율 => 동종업계 기업대비 저평가인지 고평가 인지 판단
  - 저평가 : PER 낮음, 회사 이익에 비해 주가 낮음 / 고평가 : PER 높음, 회사 이익에 비해 주가 높음
  - 저평가 => 현재 상태로 단점 존재하지만 그 단점 해소될 가능성 크다는 의미
  - **LX인터내셔널이 더 낮음 (저평가)** => LX인터내셔널의 단점이 해소되면 더욱 성장할 수 있을 것이다.

- **BPS** : 순자산/주식수
  - 기업의 재무건전성 판단지표
  - BPS높다는 의미는 기업의 순자산이 높다고 판단 & 재무건전성도 좋다는 의미
  - **LX인터내셔널이 더 높음**

- **PBR** : 주가/주당순자산 
  - 현재 주가가 순자산에 비해 몇 배로 거래되었나?
  - 1이하 => 저평가
  - 둘 다 1이하지만 둘을 비교했을 때 **LX인터내셔널이 더 낮음 (저평가)** 

<br/>

### (5) GS글로벌 VS 현대코퍼레이션 

<br/>

`-` GS글로벌

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMjMw/MDAxNjg2NDE3NTY3OTA3.Gwu5lzOmbT_UZp8N1GHeajyLrjRg-CpLvgvQTtRBpl8g.FQ87lC0Xd9HNk2q1RCMVGlQ8NrTL-mVodfjrSN859yAg.PNG.juhyunss_/%EB%84%A4%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="330" alt="" class="se-image-resource egjs-visible">

<br/>

`-` 현대코퍼레이션

<img src="https://postfiles.pstatic.net/MjAyMzA2MTFfMTc2/MDAxNjg2NDE3NTYzNTgz.TBPSG1_Tm7l6oPxxZ82YOZiq2QQVFtIs8ZGzNfd2ph8g._9Karo0cunF6L-C1mR0oTfuoVufcky26Fr14XVvM5UQg.PNG.juhyunss_/%EC%B2%AB%EB%B2%88%EC%A7%B8.png?type=w773" data-lazy-src="" data-width="693" data-height="363" alt="" class="se-image-resource egjs-visible">

<br/>

#### (5)-1. 변수의 의미

<br/>

- **영업이익** : 매출총이익(매출액 - 매출원가) - 판관비
  - 상대적으로 **기업의 규모가 크며 매출액이 더 높은 현대코퍼레이션**이 유리하기 때문에  현대코퍼레이션의**영업이익이 높게** 나타난다.

- **당기순이익** : 매출총이익(매출액 - 매출원가) - 판관비 +/- 영업외 수익과 비용 +/- 특별이익과 손익 = 영업이익 +/- 영업외 수익과 비용 +/- 특별이익과 손익 
  - **배당의 원천**이 되기에 중요함
  - 영업이익에서 현대코퍼레이션은 약 14억원 추가되었고, GS글로벌은 약 134억원 줄어들었다. **현대코퍼레이션은 불필요한 비용을 GS글로벌보다 적으며 영업외수익을 창출한 것**을 알 수 있다.

- **영업이익률** (마진율) : 영업이익/매출액 * 100 = (매출액 - 매출원가 - 판관비)/매출액 * 100
  - 기업의 영업활동에 의한 성과판단
  - 영업이익률이 높다는의미는 경쟁사를 압도하는 원가 경쟁력이 있다는 의미이다. 
  - 활용 : 동종 산업 내 기업 간의 수익성 비교, 운영 효율성 평가, 재무 건전성, 추세분석
  - **GS글로벌의 영업이익률이 더 높음** => 동종 산업 내 기업 간의 수익성, 운영 효율성, 재무 건전성 측면에서 현대코퍼레이션보다 우위에 있다.

- **순이익률** : 당기순이익/매출액 * 100
  - **매출 1단위 당 얼마나 많은 이익**이 발생했는지 보여줌
  - 한계 : 다른 산업간 수익성 비교 X, 자본구조가 다른 기업간 비교 X, 유형자산 비중 높은 회사는 높은 감가상각비로 순이익률 낮아짐, 높은 순이익률 기록했더라도 현금창출로 이어지지 않는 경우 의미 X(재무제표를 기록하는 회계는 **발생주의**에 근거하기 때문)
  - **현대코퍼레이션의 순이익률이 더 높음** => 수익성 측면에서 우위에 있다.

- **ROE** : 당기순이익/자기자본 * 100
  - 값이 높을 수록 적은 자본으로 많은 이익을 얻고있다는 의미 => 수익성 좋음
  - 당기순이익은 현대코퍼레이션이 더 높지만 현대코퍼레이션의 ROE는 약 19.5% GS글로벌 ROE는 약 18%로 **GS글로벌이 적은 자본으로 많은 이익**을 얻고있다.

- **부채비율** : 부채총액/자기자본 * 100
  - 선진국에서의 부채비율은 200%이면 적정하다고 판단 => 현대코퍼레이션은 200%초과 => **부채측면에서 GS글로벌이 우위**에 있음
  - 기업에서는 어느정도의 부채도 필요함 => **레버리지효과** : 적은 자기 자본에 타인의 자본을 빌려 투자를 하면 큰 수익을 만들수 있다는 의미

- **당좌비율** : 당좌자산/유동자산 * 100 = (유동자산 - 재고자산)/유동자산 * 100
  - 당좌비율이 높을 수록 당장 현금화 할 수 있는 자산 많다는 의미 => 어려움 생겼을 때 바로바로 해결할 수 있는 능력
  - EX) 당좌자산 : 현금 (O) 부동산 (X)
  - **현대코퍼레이션이 더 높음** => 즉각적 어려움 해결 능력이 더 높음
 
 - **유보율** : 잉여금/납입자본금 * 100
    - 기업에 남은 돈 얼마인가?
    - 유보율이 높으면 위기대처, 사업확장, 무상증자(주식대금 받지 않고 주주에게 주식 나누어주는 것 => 자본금 상승) 가능성 측면에서 유리함
    - 성장성, 안정성 측면에서 중요
    - **현대코퍼레이션이 훨씬 더 높음**

- **EPS** : 당기순이익/발행주식수
  - 주식 1주당 순이익(당기순이익에 대한 주주의 몫)
  - **현대코퍼레이션이 더 높음**

- **PER** : 현재주가/EPS
  - 주가수익비율 => 동종업계 기업대비 저평가인지 고평가 인지 판단
  - 저평가 : PER 낮음, 회사 이익에 비해 주가 낮음 / 고평가 : PER 높음, 회사 이익에 비해 주가 높음
  - 저평가 => 현재 상태로 단점 존재하지만 그 단점 해소될 가능성 크다는 의미
  - **현대코퍼레이션이 더 낮음 (저평가)** => 현대코퍼레이션의 단점이 해소되면 더욱 성장할 수 있을 것이다.

- **BPS** : 순자산/주식수
  - 기업의 재무건전성 판단지표
  - BPS높다는 의미는 기업의 순자산이 높다고 판단 & 재무건전성도 좋다는 의미
  - **현대코퍼레이션이 더 높음**

- **PBR** : 주가/주당순자산 
  - 현재 주가가 순자산에 비해 몇 배로 거래되었나?
  - 1이하 => 저평가
  - 둘 다 1이하지만 둘을 비교했을 때 **현대코퍼레이션이 더 낮음 (저평가)** 
  
<br/>

## 9. 결론

<br/>

**외국인 취득률이 높게 나타나는 LX인터내셔널과 현대코퍼레이션에서 나타나는 특징을 살펴보도록 할 것이다.**

<br/>

- ESG경영을 통해 **이해관계자중심의 이익극대화**를 통한 지속가능적 성장 도모함(모든회사 공통적)
- **발행주식수가 적음**(수요가 많아질때 주가 상승폭이 큼)
- 배당의 원천이 되는 **당기순이익 측면에서 LX인터내셔널과 현대코퍼레이션이 불필요한 비용이 상대적으로 적었으며** 현대코퍼레이션은 추가적 수익도 창출
- 순이익률이 높음(**수익성** 측면에서 우위에 있음)
- **당좌비율과 유보율이 높음** => 현금이 필요할 때, 문제의 즉각적 해결 가능성이 더 높음
- **EPS가 높음** => 주주에게 돌아가는 몫이 높음
- **PER와 PBR 낮음** => 현재 저평가 되었음
- **BPS가 높음** => 재무건전성이 높다는 의미

<br/>

`-` ***실제 투자에서도 이론과 같이 수익성이 높고 저평가되어있으며 배당금이 많으며 재무건전성이 좋은 주식에 투자한다는 것을 관측할 수 있다.***

