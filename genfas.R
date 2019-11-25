genfas<-function(x,y){
f<-readLines(x)
#提取LOCUS
name<-strsplit(f[1],'\\s')
name<-as.vector(as.matrix(name[[1]]))
index = which(name=='')#去除空白元素
name=name[-index]
if(toupper(name[1])=='LOCUS')
{
  ID<-name[2]
}else{
  warning("the format is wrong,cannot recgonize the LOCUS.")
  ID=''
}
#提取DEFINITION
if(grepl("DEFINITION",f[2],ignore.case=TRUE))
{
  note<-gsub("DEFINITION","",f[2],ignore.case=TRUE)
  note<-gsub("^\\s+|\\s+$", "", note)
  note<-gsub("[.]$", "", note)
}else{
  warning("the format is wrong,cannot recgonize the DEFINITION.")
  note=''
}

cat(">",ID,"|", note,"\n",file=y )#输出注释行

#提取序列行
n<-grep(f,pattern="ORIGIN")#寻找序列行
for(i in (n+1):length(f))
{
  if(grepl("//",f[i]))
  {
    break
  }
  string<-gsub("\\d","",f[i])
  string<-toupper(gsub("\\s","",string))
  cat(string,"\n", file=y,append=TRUE )
}
}