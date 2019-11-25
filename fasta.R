fas<-function(x){
f<-readLines(x)

list<-data.frame(SEQ=0,A=0,T=0,C=0,G=0)#创建频率表
Acount=Ccount=Tcount=Gcount=0
k=1
for(i in 1:length(f))
{
  if(grepl('^>',f[i]))#识别注释行
  {
    note<-strsplit(f[i],'\\|')
    ID<-note[[1]][1]
    list[k,1]<-sub('>','',ID)#提取名字
    if(i>1)
    {
      n<-sum(Acount+Tcount+Ccount+Gcount)
      list[k-1,2]<-round(Acount/n*100,2) #计算频率
      list[k-1,3]<-round(Tcount/n*100,2)
      list[k-1,4]<-round(Ccount/n*100,2)
      list[k-1,5]<-round(Gcount/n*100,2)
      Acount=Ccount=Tcount=Gcount=0
      
    }
    k=k+1
  }
  else
  {
    letter<-strsplit(f[i],'')
    w<-as.vector(as.matrix(letter[[1]]))
    a<-as.data.frame(table(grepl(w,pattern='A')))#统计匹配频数并转换为数据框
    Acount=a[which(a$Var1==TRUE),2]+Acount
    t<-as.data.frame(table(grepl(w,pattern='T')))
    Tcount=t[which(t$Var1==TRUE),2]+Tcount
    c<-as.data.frame(table(grepl(w,pattern='C')))
    Ccount=c[which(c$Var1==TRUE),2]+Ccount
    g<-as.data.frame(table(grepl(w,pattern='G')))
    Gcount=g[which(g$Var1==TRUE),2]+Gcount
  }
  if(i==length(f))#最后一个序列需额外判断储存
  {
    n<-sum(Acount+Tcount+Ccount+Gcount)
    list[k-1,2]<-round(Acount/n*100,2)
    list[k-1,3]<-round(Tcount/n*100,2)
    list[k-1,4]<-round(Ccount/n*100,2)
    list[k-1,5]<-round(Gcount/n*100,2)
    Acount=Ccount=Tcount=Gcount=0
  }
}
return(list)
}




