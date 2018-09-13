# R-爬虫

install.packages("RCurl")
install.packages("XML",destdir="E:/RStudio/packages")
install.packages("stringr",destdir="E:/RStudio/packages")
install.packages("RMySQL",destdir="E:/RStudio/packages")
install.packages("jiebaR",destdir="E:/RStudio/packages")
library(RCurl)
library(RMySQL)
library(XML)
library(stringr)
library(tcltk)
library(jiebaR)
#连接数据库
conn<-dbConnect(MySQL(),dbname="mysql",user="root",password="lee0305",host="127.0.0.1",port=3306)
#http请求头部
myheader <- c("User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:50.0) Gecko/20100101 Firefox/50.0",
                  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                  "Accept-Language"="en-us",
                  "Connection"="keep-alive",
                  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7")
#网址拆分
fpart<-"https://movie.douban.com/top250?start="
page_basen<-25
k=0
j=0:9
lpart<-"&filter="
#建立空白数据框
doubanmovie<-data.frame()
#爬取进度显示
pbar<-tkProgressBar(title="进度",label="主人，我已完成%",min=0,max=100,initial=0,width=500)

#调试信息收集
dg<-debugGatherer()
#数据获取
for(k in j){
  spider_url<-str_c(fpart,page_basen*k,lpart,sep="")
  #网址请求、解析网页
  analysis_url<-getURL(spider_url,httpheader=myheader,debugfunction=dg$update,verbose=TRUE)
  
  ana_doc<-htmlParse(analysis_url,encoding = "UTF-8")
  #电影中文名提取
  mname<-xpathSApply(ana_doc,"//*/span[@class='title'][1]",xmlValue)
  #oname<-xpathSApply(ana_doc,"//*/span[@class='title']",xmlValue)
  #电影别名提取
  oname<-xpathSApply(ana_doc,"//*/span[@class='other']",xmlValue)
  oname0<-str_split_fixed(str_trim(oname),"/",3)
  oname1<-str_trim(str_replace_all(oname0[,2],"/[:blank:]",""))
  oname2<-str_trim(oname0[,3])
  #导演、主演等信息
  mdirector<-xpathSApply(ana_doc,"//*/div[@class='bd']/p[1]",xmlValue)
  mdfixed1<-str_split_fixed(mdirector,"主演|主",2)
  #导演
  mdir<-str_trim(str_sub(mdfixed1[,1],start=str_locate(mdfixed1[,1],"导演")[1]+3))
  
  mdfixed2<-str_split_fixed(mdfixed1[,2],"\n",2)
  #主演
  lactor<-str_replace(mdfixed2[,1],":[:space:]","")
  mdfixed3<-str_split_fixed(mdfixed2[,2],"/",3)
  #上映年份
  myear<-str_trim(mdfixed3[,1])
  #制片国家/地区
  mc<-str_trim(mdfixed3[,2])
  #电影类型
  mtype<-str_trim(str_replace_all(mdfixed3[,3],"\n",""))
  #电影豆瓣评分
  ratenum<-xpathSApply(ana_doc,"//*/div[@class='star']/span[@class='rating_num']",xmlValue)
  #豆瓣评价人数  
  rnum<-xpathSApply(ana_doc,"//*/div[@class='star']/span[4]",xmlValue)
  ranum<-str_replace_all(rnum,"人评价","")
  #电影标签
  mtag<-xpathSApply(ana_doc,"//*/p[@class='quote']/span[@class='inq']",xmlValue)
  #电影豆瓣详情链接
  mlink<-xpathSApply(ana_doc,"//*/div[@class='item']/div[@class='pic']/a",xmlAttrs)
  mlinks<-str_replace_all(mlink,"href","")  
  #电影封面
  mpic<-xpathSApply(ana_doc,"//*/div[@class='pic']/a/img",xmlGetAttr,'src')
  doubaninfo<-data.frame(mname,oname1,oname2,mdir,lactor,myear,
                         mc,mtype,ratenum,ranum,mtag,mlinks,mpic)
  doubanmovie<-rbind.data.frame(doubanmovie,doubaninfo)
  
  info<- sprintf("已完成 %d%%", round((k+1)*100/length(j)))  
  setTkProgressBar(pbar, value =(k+1)*100/length(j), title = sprintf("进度 (%s)",info),label = info)
  
  Sys.sleep(5)
}
close(pbar)
#将数据写入数据库
dbWriteTable(conn, "doubantop", doubanmovie)
#设置下载图片需要放置的位置
setwd("F:/RSTUDIO")
for(m in 1:length(doubanmovie$mpic)){
  mop<-getBinaryURL(doubanmovie$mpic[m])
  picm<-file(paste("num",doubanmovie$mname[m],".jpg",sep=""),open="wb")
  writeBin(mop,picm)
  close(picm)
  Sys.sleep(3)
}
