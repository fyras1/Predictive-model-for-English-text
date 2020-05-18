
con <- file("data\\en_US.twitter.txt", "r")
twitter<-readLines(con)
close(con)
con <- file("data\\en_US.news.txt", "r")
news<-readLines(con)
close(con)
con <- file("data\\en_US.blogs.txt", "r")
blogs<-readLines(con)
close(con)

##------------------------- Write intial samples ------------------------
con<-file("data\\twitter_sample.txt","w")
writeLines(sample(twitter,50000),con)
close(con)

con<-file("data\\news_sample.txt","w")
writeLines(sample(news,50000),con)
close(con)

con<-file("data\\blogs_sample.txt","w")
sam<-sample(blogs,250000)
writeLines(sam,con) ## ******* blogs has 899288 lines******
close(con)

##------------------Processing and table samples save-----------------
con <- file("data\\twitter_sample.txt", "r")
twitter<-readLines(con)
close(con)
con <- file("data\\news_sample.txt", "r")
news<-readLines(con)
close(con)
con <- file("data\\blogs_sample.txt", "r")
blogs<-readLines(con)
close(con)

blogs<-gsub("â€™","'",blogs) ## blogs has an apostrophe problem. fixed.
#----------ALL GRAM---------

dat<-blogs[1:250000] ## CHOSES DATASET AND SIZE

oneGram<- character(0)
twoGram<-character(0)
threeGram<-character(0)
fourGram<-character(0)

idx1<-1
idx2<-1
idx3<-1
idx4<-1
len<-length(dat)
loading<-0
for (i in 2:len)
{  
    if(i/len*100>loading+0.5) #DELETE LATER ***********************
    {
        loading<-i/len*100
        print(loading)
    }
    
    a<-strsplit(dat[i]," ")
    a<-unlist(a)
    for(j in 1:(length(a))){
        
        oneGram[idx1]<-a[j] ; idx1<-idx1+1
        
        if(j<length(a))
            twoGram[idx2]<-paste(a[j],a[j+1]); idx2<-idx2+1
            
        if(j<length(a)-1)
            threeGram[idx3]<-paste(a[j],a[j+1],a[j+2]); idx3<-idx3+1
                
        if(j<length(a)-2)
            fourGram[idx4]<-paste(a[j],a[j+1],a[j+2],a[j+3]); idx4<-idx4+1
                    

                        
    }
    
}

freq1<-table(oneGram)
freq2<-table(twoGram)
freq3<-table(threeGram)
freq4<-table(fourGram)




#------ Data Frames ---------

freq1<-as.data.frame(freq1)
freq1$percentage<-freq1$Freq/sum(freq1$Freq)*100
freq1$cum<-cumsum(freq1$percentage)
names(freq1)[1]<-"words"
freq1$words<-as.character(freq1$words)

freq2<-as.data.frame(freq2)
freq2$percentage<-freq2$Freq/sum(freq2$Freq)*100
freq2$cum<-cumsum(freq2$percentage)
freq2$twoGram<-as.character(freq2$twoGram)

freq3<-as.data.frame(freq3)
freq3$percentage<-freq3$Freq/sum(freq3$Freq)*100
freq3$cum<-cumsum(freq3$percentage)
freq3$threeGram<-as.character(freq3$threeGram)

freq4<-as.data.frame(freq4)
freq4$percentage<-freq4$Freq/sum(freq4$Freq)*100
freq4$cum<-cumsum(freq4$percentage)
freq4$fourGram<-as.character(freq4$fourGram)





##===================== Remove rows with Freq=1 =================

prefix<-function(ch)
     {
       ch<-unlist(strsplit(ch," "))
       if(length(ch)==0)
           return("")
       else{
             ch<-ch[-length(ch)]
             ch<-paste(ch,collapse = " ")
             return(ch)
            }
           
     }


library(dplyr)
rem<-function(y)
{ 
    y<-y[y[[2]]>1,]
    y$pref<-sapply(y[[1]],prefix)

    a<-group_by(y,pref)
    aa<-summarize(a,sm=sum(Freq))
    y<-merge(y,aa)
    y$p<-y$Freq/y$sm
    y$sm<-NULL
    
    
    y<-y[order(y$pref,-y$Freq),]
    
    stay<-vector()
    stay[1]<-TRUE
    
    for(i in 4:nrow(y))
    {
        if(y[i,"pref"]==y[i-1,"pref"] &y[i,"pref"]==y[i-2,"pref"] & y[i,"pref"]==y[i-3,"pref"] )
            stay[i]<-FALSE
            
        else
            stay[i]<-TRUE
    }
    
    y<-y[stay,]
    y$pref<-NULL
    y
}
freq1$p<-1
freq2<-rem(freq2)
freq3<-rem(freq3)
freq4<-rem(freq4)


##----------------SORT-------------------


freq1<-freq1[order(-freq1[,3]),]
freq2<-freq2[order(-freq2[,3]),]
freq3<-freq3[order(-freq3[,3]),]
freq4<-freq4[order(-freq4[,3]),]

freq2<-freq2[complete.cases(freq2),]
freq3<-freq3[complete.cases(freq3),]
freq4<-freq4[complete.cases(freq4),]

write.table(freq1,"data\\freq.csv")
write.table(freq2,"data\\twoGram.csv")
write.table(freq3,"data\\threeGram.csv")
write.table(freq4,"data\\fourGram.csv")

