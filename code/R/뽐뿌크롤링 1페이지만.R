library(httr)
library(rvest)
library(stringr)

url1 <- 'http://www.ppomppu.co.kr/zboard/zboard.php?id=car&page='
url2 <- '&search_type=sub_memo&keyword=%C0%FC%B1%E2%C2%F7&divpage=106'

years <- c()
title <- c()
contents <- c()
reply <- c()
reply.years <- c()


pp.main.pas1 <- paste0(url1, 1)
pp.main.pas2 <- paste0(pp.main.pas1, url2)
pp.main.read <- read_html(pp.main.pas2, encoding = 'CP949')

pp.main.year1 <- html_nodes(pp.main.read, 'td.eng.list_vspace')
pp.main.year2 <- html_nodes(pp.main.year1, 'nobr.eng.list_vspace')

for(day in 3:22){
pp.year.text <- html_text(pp.main.year2[day])
years <- c(years, pp.year.text)

}

pp.main.node1 <- html_nodes(pp.main.read, 'table.title_bg')
pp.main.node2 <- html_nodes(pp.main.node1, 'td.list_vspace a')
pp.main.attr <- html_attr(pp.main.node2, 'href')

for(i in pp.main.attr[3:42]){
  if(i != '#'){
    pp.paste <- paste0('http://www.ppomppu.co.kr/zboard/', i)
    pp.a.read <- read_html(pp.paste, encoding = 'CP949')
    #제목
    pp.a.node.t1 <- html_nodes(pp.a.read,'td.han')
    pp.a.node.t2 <- html_nodes(pp.a.node.t1,'font.view_title2')
    pp.title <- str_trim(html_text(pp.a.node.t2))
    title <- c(title, pp.title)
    #본문
    pp.a.node.c1 <- html_nodes(pp.a.read,'table.pic_bg')
    pp.a.node.c2 <- html_nodes(pp.a.node.c1,'td#realArticleView')
    pp.contents <- str_trim(html_text(pp.a.node.c2))
    contents <- c(contents, pp.contents)
    
    #댓글
    pp.page <- html_nodes(pp.a.read, 'div#page')
    if(length(pp.page)==1){  #div#page가 없다면 length값이 0이 나오고, 있다면 1이 나옴
      pp.paste.r1 <- paste(pp.paste, '&c_page=', sep = '')
      counts <- html_nodes(pp.page,'font.pagelist_han a') #for문 숫자용
      for(k in 1:(length(counts)+1)){
        pp.paste.r2 <- paste0(pp.paste.r1, k)
        pp.a.read.r <- read_html(pp.paste.r2, encoding = 'CP949')
        pp.a.node.r1 <- html_nodes(pp.a.read.r, 'div#newbbs')
        pp.a.node.r2 <- html_nodes(pp.a.node.r1, 'div.han')
        pp.reply <- str_trim(html_text(pp.a.node.r2))
        reply <- c(reply, pp.reply)
      }
    }
    else {
      pp.a.node.r1 <- html_nodes(pp.a.read, 'div#newbbs')
      pp.a.node.r2 <- html_nodes(pp.a.node.r1, 'div.han')
      pp.reply <- str_trim(html_text(pp.a.node.r2))
      reply <- c(reply, pp.reply)
    
    }
  }
}



title.df <-data.frame(title)
contents.df <-data.frame(contents)
reply.df <- data.frame(reply)

write.csv(title.df, '뽐뿌제목1.csv')
write.csv(contents.df, '뽐뿌본문1.csv')
write.csv(reply.df, '뽐뿌댓글1.csv')