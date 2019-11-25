alresult<-function(s1,s2,gap=-4,score_matrix=data.frame(A=c(4, -3, -3, -3), T=c(-3, 4, -3, -3), C=c(-3, -3,4, -3),G=c(-3, -3, -3, 4))){
  options(stringsAsFactors = F)
  score_matrix=score_matrix
  row.names(score_matrix)=c("A","T","C","G")
  score=0
  gap = gap
  
  #计算得分
  get_match_score<-function(s1,s2){
    score=score_matrix[s1,s2]
  }
  
  #判断输入是否合法
  s11=strsplit(s1,"")[[1]]
  for(i in 1:length(s11)){
    if(!grepl(pattern ="[ATCG]",s11[i],ignore.case = T)){
      s=paste(s1,'is wrong input')
      warning(s)
      break
      }
  }
  
  s22=strsplit(s2,"")[[1]]
  for(i in 1:length(s22)){
    if(!grepl(pattern ="[ATCG]",s22[i],ignore.case = T)){
      s=paste(s2,'is wrong input')
      warning(s)
      break
      }
  }
  
  #计算得分矩阵
  best_matrix=matrix(nrow=length(s22)+1,ncol=length(s11)+1)
  for(i in 1:(length(s22)+1)){
    for(j in 1:(length(s11)+1)){
      if(i == 1)
        best_matrix[i,j] = gap * j
      else if(j == 1)
        best_matrix[i,j] = gap *i
      else{
        match = get_match_score(s22[i-1],s11[j-1])
        gap1_score=best_matrix[i-1,j]+gap
        gap2_score=best_matrix[i,j-1]+gap
        match_score=best_matrix[i-1,j-1]+match
        best_matrix[i,j] = max(gap1_score,gap2_score,match_score)
  }}}
  #回溯
  i= length(s22)+1
  j= length(s11)+1
  s1=""
  s2=""
  while(i>1 | j>1){
  
  match = get_match_score(s22[i-1],s11[j-1])
  
  if(i>1 & j>1 & best_matrix[i,j] == best_matrix[i-1,j-1]+match){
    s1 = paste0(s11[j-1],s1,collapse="")
    s2 = paste0(s22[i-1],s2,collapse="")
    score =score+ best_matrix[i,j]
    i=i-1;j=j-1}else if(i>1 & best_matrix[i,j] == best_matrix[i-1, j] + gap){
    s1 =paste0("-",s1,collapse="")
    s2 =paste0(s22[i-1],s2,collapse="")
    score = score+best_matrix[i,j]
    i=i-1}else{
    s1=paste0(s11[j-1],s1,collapse="")
    s2=paste0("-",s2,collapse="")
    score =score+ best_matrix[i,j]
    j=j-1}
  }
  print("the score_matrix is:")
  print(score_matrix)
  print("the best_score_matrix is:")
  print(best_matrix)
  print("the align_result is:")
  print(s1)
  print(s2)
  sprintf("the score is %d",score)
}