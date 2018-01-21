library(rvest)
library(dplyr)
library(ggplot2)
library(scales)

make_url = function(search,start,end){
  url = "https://search.naver.com/search.naver?where=news&query=search_word&ie=utf8&sm=tab_opt&sort=0&photo=0&field=0&reporter_article=&pd=3&ds=start_date&de=end_date&docid=&nso=so%3Ar%2Cp%3Afromstart_datetoend_date%2Ca%3Aall&mynews=0&mson=0&refresh_start=0&related=0"
  
  url_1 = gsub("search_word",search,url)
  url_1 = gsub("start_date",start,url_1)
  url_1 = gsub("end_date",end,url_1)
  
  return (url_1)
}

extract_number = function(url){
  html <- read_html(url)
  temp <- unique(html_nodes(html,"#wrap")%>%
                   html_nodes('#container')%>%
                   html_nodes('#content')%>%
                   html_nodes('#main_pack')%>%
                   html_nodes(css='.news ')%>%
                   html_nodes(css='.section_head')%>%
                   html_nodes(css='.title_desc.all_my')%>%
                   html_text())

  temp = gsub("건","",temp)
  temp = strsplit(temp, "/ ") 
  temp[[1]][2] = gsub(",","",temp[[1]][2])
  if(is.na(as.integer(temp[[1]][2]))){
    return (0)
  }
  return (as.integer(temp[[1]][2]))
}

news_count = function(keyword,start,end){
  start = as.Date(start)
  end = as.Date(end)
  
  arraydate = seq.Date(from=start,to=end,by=7)
  datebreaks <- seq.Date(from=start,to=end,by=21)
  j = length(arraydate)-1
  
  if(arraydate[length(arraydate)]!=end){
    arraydate[length(arraydate)+1] = end
    datebreaks[length(datebreaks)+1] = end
    j = length(arraydate)-1   
  }
  
  x = c(1:j)
  news_num = c(1:j)
  
  for(i in 1:j){
    start_date = 0
    end_date = 0
    start_date = format(arraydate[i],"%Y")
    start_date = paste(start_date,".")
    start_date = paste(start_date,format(arraydate[i],"%m"))
    start_date = paste(start_date,".")
    start_date = paste(start_date,format(arraydate[i],"%d"))
    start_date = gsub(" ","",start_date)
    
    end_date = format(arraydate[i+1],"%Y")
    end_date = paste(end_date,".")
    end_date = paste(end_date,format(arraydate[i+1],"%m"))
    end_date = paste(end_date,".")
    end_date = paste(end_date,format(arraydate[i+1],"%d"))
    end_date = gsub(" ","",end_date)
    
    x[i] = make_url(keyword,start_date,end_date)
    news_num[i] = extract_number(x[i])
  }
  temp = data.frame(date=arraydate[-(length(arraydate))],news_num =news_num)
  
  keyword=gsub("%20"," ",keyword)
  
  q <- ggplot(temp,aes(x=date,y=news_num))+
  geom_line()+
  ylim(0,max(temp$news_num))+
  scale_x_date(breaks=datebreaks)+
  ggtitle(paste(keyword,"시간에 따른 네이버뉴스 기사 수"))
  
  return (q)
}
news_count("인천%20어린이집%20아동%20학대%20사건","2015-01-08","2015-04-30")
