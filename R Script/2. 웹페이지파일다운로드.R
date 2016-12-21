library(httr)
library(rvest)
#httr 라이브러리의 매뉴얼 PDF 파일을 가져옵니다
res = GET('http://cran.r-project.org/web/packages/httr/httr.pdf')

#가져온 파일을 httr.pdf 라는 이름으로 저장합니다
writeBin(content(res, 'raw'), 'httr.pdf')

#개9 사이트의 게시물을 엽니다
h = html('http://gae9.com/trend/1DRlnSN7k1nb#!hot')

#이미지만 가져옵니다
imgs = html_nodes(h, 'div.trend-post-content img')

#이미지의 주소를 나타내는 src 어트리뷰트를 뽑아냅니다
img.src = html_attr(imgs, 'src')

#주소에 ssproxy라는 표현이 들어간 이미지만 뽑아냅니다
img.src = img.src[grep('ssproxy', img.src)]

#각 주소의 이미지를 다운받아 001.jpg, 002.jpg와 같은 형식으로 저장합니다
for(i in 1:length(img.src)){
  res = GET(img.src[i])
  writeBin(content(res, 'raw'), sprintf('%03d.jpg', i))
}
