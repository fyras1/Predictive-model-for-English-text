freq1<-read.table("data\\freq.csv",stringsAsFactors = FALSE)
freq2<-read.table("data\\twoGram.csv",stringsAsFactors = FALSE)
freq3<-read.table("data\\threeGram.csv",stringsAsFactors = FALSE)
freq4<-read.table("data\\fourGram.csv",stringsAsFactors = FALSE)

freq1$p<-freq1$percentage/100
freq1$rank<-c(1:nrow(freq1))


ls<-list(f1=freq1,
         f2=freq2,
         f3=freq3,
         f4=freq4)



suffix<-function(ch)
{
   ch<-unlist(strsplit(ch," "))
   if(length(ch)==0)
      return("")
   else{
      ch<-ch[length(ch)]
      return(ch)
   }
   
}


nextt<-function(xo="",n=4){
   x<-xo
   #repeat{
   vect<-ls[[n]][,1]
   perc<-ls[[n]][,5]
   
   if (n==1){
      # print(1)
      temp<-sapply( c("I","The","This"),suffix)
      return(data.frame(word=temp,prob=c(0.07694,0.06210,0.02062)))
      
   }
   
   strsp<-strsplit(x," ")
   strsp<-unlist(strsp)
   if (length(strsp)<n-1)
      return(nextt(xo,length(strsp)+1))
   
   nw<-""
   # print(strsp)
   for(j in (length(strsp)-n+2):length(strsp))
   {
      if(nchar(nw)==0)
         nw<-strsp[j]
      else
         nw<-paste(nw,strsp[j],sep=" ")
      
      #  print(nw)
   }
   x<-nw
   #print(x)
   #print("=")
   
   match<-grep(paste0("^",x," "),vect)
   a<-vect[match]
   b<-perc[match]
   
   #  print(a)
   # print(match)
   
   if(length(a)==0){
      
      return(nextt(xo,n-1))
   }
   else{
      #print(n)
      temp<-sapply(a,suffix)
      temp2<-b
      return(data.frame(word=temp,prob=temp2))
      
   }
   
   
   
   
   #   print(a)
   
}

generate<-function(x="the",n)
{
   for(i in 1:n)
   {
      ans<-nextt(x)
      ch<-sample(ans[,1],1,prob=ans[,2])
      x<-paste(x,ch)
      #print(x)
      
   }
   x
}


