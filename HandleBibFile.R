
library(bib2df)
bib_df<-bib2df(file="MyPub.bib")

library(stringi)

bib_df$TITLE<-stri_replace_all_regex(bib_df$TITLE, "[\\{\\}]", "")
bib_df$JOURNAL<-stri_replace_all_regex(bib_df$JOURNAL, "[\\{\\}]", "")

# sort bib_df by year
bib_df<-bib_df[order(bib_df$YEAR, decreasing=T),]

dims<-dim(bib_df)
p<-c("D:/Dropbox (INMACOSY)/Biblio/" )
p2<-c("C:/Users/u0099946/Documents/website/PubFile/")

for(i in 1:dims[1]){
  # create file name from DOI
  b<-bib_df[i,]$DOI
  b<-gsub("\\.", "_", b)
  b<-gsub("/", "-", b)
  
  # find original file
  a<-bib_df$FILE[i]
  split_a1<-unlist(strsplit(a,"/"))
  split_a<-unlist(strsplit(split_a1[length(split_a1)],":"))
  p3<-paste0(c(p,bib_df[i,]$YEAR,"/"),collapse="")
  bib_df$FILE[i]<-paste0(c(p3,split_a[1]),collapse="")
  
  if(file.exists(bib_df$FILE[i])){
    file.copy(from = file.path(p3,split_a[1]), to = file.path(p2, paste0(c(b,".pdf"),collapse="")),overwrite = TRUE)
  }else{
    print(paste0(p3,split_a[1]))
  }
}

