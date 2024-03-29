# YouTube 키워드 영향력 분석 및 통계 시각화

2018년에 한국클라우드 컴퓨팅 연구 조합에서 주관하는 '클라우드 기반 빅데이터 기초과정' 클래스를 들으면서 배웠던 R을 활용하여 만들었었던,

유튜브의 키워드에 대한 영향력을 분석, 통계 시각화 해주는 프로그램에 대해서 소개하겠습니다.

### 워크 플로우
> 1. 프로그램을 실행하면 R의 Shiny로 만들어진 간단한 웹페이지가 뜹니다.
>
>
> 2. 출력 된 페이지에 검색 할 키워드를 입력하고 '분석' 버튼을 누르면
    Selenium이 크롬 브라우저를 띄우고, 유튜브로 이동하여 입력 된
    키워드에 대한 검색을 수행합니다.
>
>
> 3. 검색 결과를 텍스트로 수집하고, 게시일과 조회수를 바탕으로 기간 당 
    조회수가 높은 채널 순으로 정렬하여 시각화 합니다.

<br/>

### 사용 된 기술
> 1. rvest : 크롤링을 위한 모듈
> 2. httr : html 문서를 get하기 위한 http 모듈
> 3. dplyr : R에서의 데이터 프레임 핸들링을 위한 모듈
> 4. googleVis : 구글통계 시각화 모듈
> 5. shiny : R에서 웹페이지를 만들기 위해 사용하는 모듈
> 6. RSelenium : R에서 사용하는 크롬 브라우저 제어 플러그인

<br/>

### 부족했던 점
> 1. Infinity Scroll 기반의 YouTube 페이지 특성 상, API를 사용하지 않고 크롤링해서 데이터를 가져오려면 RSelenium을 통해 지속적으로 스크롤 이벤트를 발생시키면서 데이터를 추가 수집해야했지만, 개발 당시 이러한 부분을 고려하지 못했습니다. 
(https://philosopher-chan.tistory.com/762)
> 
>
> 2. 구글 시각화에서의 언어팩 설정을 하지 않아 한글이 깨지는 문제가 있었습니다. 
(https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=ehdsnck&logNo=221334630789)

<br/>

### 시연 영상
[![youtube](https://img.youtube.com/vi/bbxOespGZbA/sddefault.jpg)](https://www.youtube.com/embed/bbxOespGZbA)



