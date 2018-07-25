
library(bib2df)
bib_df<-bib2df(file="MyPub.bib")

library(stringi)

bib_df$TITLE<-stri_replace_all_regex(bib_df$TITLE, "[\\{\\}]", "")
bib_df$JOURNAL<-stri_replace_all_regex(bib_df$JOURNAL, "[\\{\\}]", "")
bib_df$FILE<-stri_replace_all_regex(bib_df$FILE,"\\{\\\\\`\\{e\\}\\}", "è")## replacing latex character by UTF character
bib_df$FILE<-stri_replace_all_regex(bib_df$FILE,"\\{\\\\\'\\{e\\}\\}", "é")## replacing latex character by UTF character
bib_df$FILE<-stri_replace_all_regex(bib_df$FILE,"\\{\\\\\"\\{u\\}\\}", "ü")## replacing latex character by UTF character
bib_df$FILE<-stri_replace_all_regex(bib_df$FILE,"\\{\\\\\\^\\{i\\}\\}", "î")## replacing latex character by UTF character
bib_df$FILE<-stri_replace_all_regex(bib_df$FILE,"â€\\“", "-")## replacing latex character by UTF character

# sort bib_df by year
bib_df<-bib_df[order(bib_df$YEAR, decreasing=T),]

dims<-dim(bib_df)
p<-c("D:/Dropbox (INMACOSY)/Biblio/" )
p2<-c("C:/Users/u0099946/Documents/website/PubFile/")
pg = c("https://github.com/jjodx/jjodx.github.io/raw/master/PubFile")

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
    #### if no match was found, try to find the filename that closely ressembles the given filename
    FileList <- list.files(p3);# list all files in teh directory where we look for the file.
    FileNumber <- agrep(split_a[1],FileList)
    FileName <-FileList[FileNumber]
    if(file.exists(paste0(c(p3,FileName),collapse=""))){
      file.copy(from = file.path(p3,FileName), to = file.path(p2, paste0(c(b,".pdf"),collapse="")),overwrite = TRUE)
    }
    else{
    print(i)
    print(FileName)}
  }
}

