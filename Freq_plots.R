library(ggplot2)
freq1<-read.table("data\\freq.csv",stringsAsFactors = FALSE)
freq2<-read.table("data\\twoGram.csv",stringsAsFactors = FALSE)
freq3<-read.table("data\\threeGram.csv",stringsAsFactors = FALSE)
freq4<-read.table("data\\fourGram.csv",stringsAsFactors = FALSE)

#----------ONE GRAM PLOT---------
g1<-ggplot(freq1[1:20,],aes(x=reorder(words,-percentage),y=percentage,fill=percentage)) + 
    geom_bar(stat = "identity")+
    ggtitle(paste("Word frequency in",length(oneGram)," blog words"))+
    xlab("Words")+
    ylab("Frequency (%)")+
    labs(fill="Frequency")+
    theme(axis.text.x = element_text(angle = 90))
g1

ggsave("plots\\Words_freq.png",g1)

#----------TWO GRAM PLOT---------
g2<-ggplot(freq2[1:20,],aes(x=reorder(twoGram,-percentage),y=percentage,fill=percentage)) + 
    geom_bar(stat = "identity")+
    ggtitle(paste("2-Gram frequency in",length(twoGram)," blog 2-Grams"))+
    xlab("2-Gram")+
    ylab("Frequency (%)")+
    labs(fill="Frequency")+
    theme(axis.text.x = element_text(angle = 90))
g2
ggsave("plots\\2Gram_freq.jpg",g2)

#----------THREE GRAM PLOT---------
g3<-ggplot(freq3[1:20,],aes(x=reorder(threeGram,-percentage),y=percentage,fill=percentage)) + 
    geom_bar(stat = "identity")+
    ggtitle(paste("3-Gram frequency in",length(threeGram)," blog 3-Grams"))+
    xlab("3-Gram")+
    ylab("Frequency (%)")+
    labs(fill="Frequency")+
    theme(axis.text.x = element_text(angle = 90))
g3
ggsave("plots\\3Gram_freq.jpg",g3)

#----------FOUR GRAM PLOT---------
g4<-ggplot(freq4[1:20,],aes(x=reorder(fourGram,-percentage),y=percentage,fill=percentage)) + 
    geom_bar(stat = "identity")+
    ggtitle(paste("4-Gram frequency in",length(fourGram)," blog 4-Grams"))+
    xlab("4-Gram")+
    ylab("Frequency (%)")+
    labs(fill="Frequency")+
    theme(axis.text.x = element_text(angle = 90))
g4
ggsave("plots\\4Gram_freq.jpg",g4)




## The top 177 Words make 50% of English text
## The top 24468 words make 90% of English text 