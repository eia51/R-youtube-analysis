install.packages('rvest')       # 크롤링 패키지
install.packages('httr')        # html문서 획득위함
install.packages('stringr')     # 문자열 연산 관리 패키지
install.packages("devtools")    # 깃허브에서 패키지 다운받기 위함
install.packages("dplyr")       # 데이터프레임 조작관리 패키지
install.packages("googleVis")   # 구글파트 시각화 패키지
install.packages("shiny")       # 웹페이지로 퍼블리싱
install.packages("shinyjs")     # 샤이니에서 스크립트 함수 실행
install.packages("DT")          # 샤이니에서 데이터테이블 출력위한 라이브러리

devtools::install_github("ropensci/RSelenium")                      # 브라우저 제어를 위한 패키지

library(rvest)          # 크롤링 패키지
library(httr)           # html문서 획득위함
library(stringr)        # 문자열 연산 관리 패키지
library(dplyr)          # 데이터프레임 조작관리 패키지
library(googleVis)      # 구글파트 시각화 패키지
library(shiny)          # 웹페이지로 퍼블리싱
library(shinyjs)        # 샤이니에서 스크립트 함수 실행
library(DT)             # 샤이니에서 데이터테이블 출력위한 라이브러리
library(RSelenium)      # 브라우저 제어를 위한 패키지


#library(plotrix)


ui <- fluidPage(
    h1("키워드에 따른 채널 영향력 분석"),
    h4(em("Analyze the influence of channel on keyword with Youtube")),
    hr(),
    pageWithSidebar(
        headerPanel(""),
        sidebarPanel(
            textInput(
                inputId= "search",
                label = "검색 키워드를 입력하세요",
                value = ""
            ),

            actionButton("btnSearch","분석"),
            actionButton("btnPrint","출력"),
            br(),
            br(),

            p("Click the button to analyze the influence of channel on keyword.")
        ),
        mainPanel(
            h4(strong("사용 메뉴얼")),
            p("■ 왼쪽 검색란에 내가 원하는 키워드를 입력"),
            p("■ '분석하기' 버튼을 클릭"),
            p("■ 크롬브라우저 제어를 통해 유튜브에서 해당 키워드가 검색됨"),
            p("■ 페이지로부터 데이터 획득 "),
            p("■ 전처리 과정을 거쳐 데이터프레임 형식으로 가공"),
            p("■ 입력한 키워드에 대한 결과가 Table, Pie Chart 형식으로 출력")
        )
    ),
    br(),
    br(),

    textOutput("resultTitle1") %>% h2(),
    hr(),
    br(),

    DT::dataTableOutput("youtubeData"),
    br(),
    br(),
    br(),

    textOutput("resultTitle2") %>% h2(),
    hr(),
    br(),

    DT::dataTableOutput("analyzeData"),
    hr(),

    br(),
    br(),
    br(),

    textOutput("chartTitle") %>% h2(),
    hr(),
    br(),

    textOutput("resultLabel") %>% h4(),
    DT::dataTableOutput("resultTable"),
    br(),
    res5 <- textOutput("summary") %>% h4(),
    htmlOutput("chart3D", height=740),
    hr(),
    br(),
    br(),
    br()
    #plotOutput("chart",height=600)


)


server <- function(input, output, session){

    observeEvent(
        input$btnSearch,{
            if(input$search != ""){
                input.value <- input$search
                d.count <- 15

                ch <- wdman::chrome(port=4567L)                         # 포트 할당
                remDr <- remoteDriver(port=4567L, browserName='chrome') # 브라우저 크롬으로 하는     원격관리드라이버 생성
                remDr$open()                                            # 브라우저 원격관리를 위한 창 생성



                input.processing <- gsub(pattern=" ", replacement="+", x=input.value)
                myurl <- paste('https://www.youtube.com/results?search_query=', input.processing, sep="")
                remDr$navigate(myurl)



                delay(8000,NULL)
                #invalidateLater(6000, session)

                source <- remDr$getPageSource()[[1]]  # 페이지 소스 획득해옴
                main <- read_html(source)             # 페이지를 html형식으로 읽어들여옴
                main.info <- html_nodes(main, css='#video-title')  # 영상정보 크롤링을 위한 주 컨테이너박스 정보 획득
                v.title <- main.info %>% html_attr("title") %>% head(n=d.count) # 영상의 제목 획득


                video.access.url <- main.info %>% html_attr("href") %>% head(n=d.count) # 영상의 링크주소 획득 (쿼리+영상 고유id)
                #v.link <- paste("http://www.youtube.com/", video.access.url, sep="") # 실제 이동이 가능한 링크형태로 포맷팅

                # 문자열 분할을 통해 고유아이디를 획득하는 전처리과정
                video.info.splite <- c()
                video.info.splite <- strsplit(video.access.url, "?v=")
                video.id <- c()
                for(i in 1:d.count){
                    video.id[i] <- video.info.splite[[i]][2]
                }

                # 채널명 획득, 카테고리화
                ch.name <- html_nodes(main, css='#byline') %>% html_attr("title") %>% head(n=d.count) %>% factor()
                # 영상 조회수, 게시시간 획득을 위한 부가컨테이너 획득
                sub.info.container <- html_nodes(main, css='#metadata-line span') %>% html_text() %>% head(n=d.count*2)

                # 조회수 획득을 위한 전처리과정: '조회수 xxx회' 에서 숫자 관련정보만 추출
                visit.prev = gsub(pattern <- "\n", replacement = "", x = sub.info.container[seq(from=1, to=d.count*2, by=2)]) %>%     head(n=d.count)
                visit.mid <- c()
                visit.f.split <- strsplit(visit.prev, " ")
                for(i in 1:d.count){
                    visit.mid[i] <- visit.f.split[[i]][2]
                }
                visit.m.split <- strsplit(visit.mid, "회")
                visit <-c()
                for(i in 1:d.count){
                    visit[i] <- visit.m.split[[i]][1]
                }

                # 업데이트 일자 획득
                update <- sub.info.container[seq(from=2, to=d.count*2, by=2)] %>% head(n=d.count)

                # 데이터 프레임 생성
                youtube.dataset <- data.frame('영상제목'=v.title, '채널이름'=ch.name, '조회수'=visit, '업데이트'=update, '영상ID'=video.id)




                # 생성한 데이터프레임 디스플레이 및 디스플레이 된 영상목록 중, 어떤 영상의 댓글을 볼 건지 질의
                # SocialMediaLab 패키지 영역 (AuthenticateWithYoutubeAPI, CollectDataWithYoutubeAPI 사용)
                # AuthenticateWithYoutubeAPI()통해 유튜브에서 발급받은 인증키 인증을 받음
                output$youtubeData <- DT::renderDataTable({youtube.dataset})




                # 조회수 및 게시기간 수치 획득을 위한 정규식
                rexp.year <- '([1-9]년 전)|([1-9][0-9]년 전)|([1-9][0-9][0-9]년 전)'
                rexp.month <- '([1-9]개월 전)|(1[0-9]개월 전)'
                rexp.week <- '([1-9]주 전)|([1-9][0-9]주 전)'
                rexp.day <- '([1-9]일 전)|([1-3][0-9]일 전)'
                rexp.hour <- '([1-9]시간 전)|([1-9][0-9]시간 전)'
                rexp.min <- '([1-9]분 전)|([1-9][0-9]분 전)'

                rexp.th <- '([1-9]천)|([1-9].[0-9]천)|([1-9][0-9]천)|([1-9][0-9].[0-9]천)|([1-9][0-9][0-9]천)|([1-9][0-9][0-9].[0-9]천)|([1-9][0-9][0-9][0-9]천)|([1-9][0-9][0-9][0-9].[0-9]천)'
                rexp.10th <- '([1-9]만)|([1-9].[0-9]만)|([1-9][0-9]만)|([1-9][0-9].[0-9]만)|([1-9][0-9][0-9]만)|([1-9][0-9][0-9].[0-9]만)|([1-9][0-9][0-9][0-9]만)|([1-9][0-9][0-9][0-9].[0-9]만)'
                rexp.100mil <- '([1-9]억)|([1-9].[0-9]억)|([1-9][0-9]억)|([1-9][0-9].[0-9]억)|([1-9][0-9][0-9]억)|([1-9][0-9][0-9].[0-9]억)|([1-9][0-9][0-9][0-9]억)|([1-9][0-9][0-9][0-9].[0-9]억)'




                # 카테고리 레벨획득
                vj.factors.level <- levels(youtube.dataset$'채널이름')
                vj.factors.level

                # 카테고리의 카디널리티 획득
                len <- length(vj.factors.level) %>% print()

                # 정규식에 기반하여 검색어에 대해 채널별 영상노출 개수, 조회수 총합, 영상 게시시간 총합 산출을 위한 전처리
                video.count <- c()
                video.visit.sum <- c()
                video.publised.period <- c()
                visit.divide.period <- c()

                for(i in 1:len){
                    cnt <- 0   # 영상 등장 빈도수
                    video.count[i] <- 0
                    video.visit.sum[i] <- 0
                    video.publised.period[i] <- 0

                    for(j in 1:d.count){
                        each.visit <- 0
                        each.post.time <- 0

                        if(vj.factors.level[i] == youtube.dataset$'채널이름'[j]){
                            cnt <- cnt+1

                            # 채널의 게시영상 조회수 총합 구하기
                            if(length(matches(rexp.100mil, FALSE, visit[j])==1)){
                                each.visit <- (strsplit(visit[j], '억')[[1]][1] %>% as.numeric())*100000000
                            }else if(length(matches(rexp.10th, FALSE, visit[j])==1)){
                                each.visit <- (strsplit(visit[j], '만')[[1]][1] %>% as.numeric())*10000
                            }else if(length(matches(rexp.th, FALSE, visit[j])==1)){
                                each.visit <- (strsplit(visit[j], '천')[[1]][1] %>% as.numeric())*1000
                            }else{
                                each.visit <- as.numeric(visit[j])
                            }
                            video.visit.sum[i] <- video.visit.sum[i]+each.visit

                            # 채널의 게시영상 총 경과시간 구하기(단위 hours)
                            if(length(matches(rexp.year, FALSE, update[j])==1)){
                                each.post.time <- (strsplit(update[j], '년')[[1]][1] %>% as.numeric()*8760)
                            }else if(length(matches(rexp.month, FALSE, update[j])==1)){
                                each.post.time <- (strsplit(update[j], '개월')[[1]][1] %>% as.numeric()*720)
                            }else if(length(matches(rexp.week, FALSE, update[j])==1)){
                                each.post.time <- (strsplit(update[j], '주')[[1]][1] %>% as.numeric()*168)
                            }else if(length(matches(rexp.day, FALSE, update[j])==1)){
                                each.post.time <- (strsplit(update[j], '일')[[1]][1] %>% as.numeric()*24)
                            }else if(length(matches(rexp.hour, FALSE, update[j])==1)){
                                each.post.time <- (strsplit(update[j], '시간')[[1]][1] %>% as.numeric())
                            }
                            video.publised.period[i] <- video.publised.period[i]+each.post.time

                        }
                    }
                    video.count[i] <- cnt
                    visit.divide.period[i] <- (video.visit.sum[i]/video.publised.period[i]) %>% round()
                }





                # '조회/시간' 총합
                publised.all.sum <- sum(visit.divide.period) %>% print()

                # 채널 별 '조회/시간' 데이터 백분위 산출 > 이 백분위가 곧 채널의 영향력
                percent.visit.per.period <- (visit.divide.period/publised.all.sum*100) %>% round(digits = 1) %>% print()

                # 천단위 구분문자 설ㅈ
                f.video.visit.sum <- formatC(video.visit.sum, format="d", big.mark=",")
                f.video.published.period <- formatC(video.publised.period, format="d", big.mark=",")
                f.visit.divide.period <- formatC(visit.divide.period, format="d", big.mark = ",")

                # 전체 통계자료 데이터프레임화
                processing.data <- data.frame('채널'=vj.factors.level, '영상개수'=video.count, '총조회수'=f.video.visit.sum, '총게시시간'=f.video.published.period,  '시간당조회수'=visit.divide.period, '백분위'=percent.visit.per.period)

                # 최고 영향력을 갖는 채널 추추
                mindex <-which(max(processing.data[,5])==processing.data[,5]) %>% print()
                processing.data$'시간당조회수' <- f.visit.divide.period
                res <- subset(processing.data[mindex,])



                # 서버함수 출력값 지저
                output$analyzeData <- DT::renderDataTable({datatable(processing.data, options=list(columnDefs=list(list(className='dt-center', targets=2:6))))})
                output$resultTitle1 <- renderText("Youtube 영상정보")
                output$resultTitle2 <- renderText("시간당 조회수 통계")
                output$resultTable <- DT::renderDataTable(datatable(res, options = list(dom = 't', columnDefs=list(list(className='dt-center', targets='_all')))))
                output$chart3D <- renderGvis({

                    gvisPieChart(
                        data.frame(vj.factors.level, percent.visit.per.period),
                        options = list(
                            width=1050, height=830,
                            title="Analyze the influence of channel",
                            titleTextStyle="{color:'black', fontName:'Arial', fontSize:22}",
                            legend.alignment='end',
                            pieSliceText="value",
                            pieHole=0.5,
                            is3D=TRUE
                        )
                    )
                })
                output$chartTitle <- renderText({paste("'",input.value,"' 검색에 대한 채널 영향력 분석", sep="")})
                output$resultLabel <- renderText({
                    paste(
                        "키워드 '",
                        input.value,
                        "' 에 대한 검색결과 중, 상위 ",
                        len,
                        "개의 채널정보를 분석한 결과는 다음과 같습니다.",
                        sep="")
                })
                output$summary <- renderText({
                    paste(
                        "분석 결과, 채널 '",
                        res[,1],
                        "' 가  '",
                        input.value,
                        "' 와 관련 된 영상이 게시 된 채널 중, 시간 당 영상 조회수가 ",
                        res[,5],
                        "번으로 가장 영향력이 높았습니다.",
                        sep=""
                    )
                })
            }
        }
    )

}
shinyApp(ui, server)

