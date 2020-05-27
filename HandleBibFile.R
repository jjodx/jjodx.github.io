library(stringi)
library(bib2df)

# bib_df<-bib2df(file="MyPub.bib")
bib_df<-bib2df(file="ExportedItems.bib")



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
p2<-c("C:/Users/u0099946/Dropbox (Personal)/website/PubFile")
pg = c("https://github.com/jjodx/jjodx.github.io/raw/master/PubFile")

for(i in 1:6){#dims[1]
  # create file name from DOI
  b<-bib_df[i,]$DOI
  b<-gsub("\\.", "_", b)
  b<-gsub("/", "-", b)
  
  # find original file
  a<-bib_df$FILE[i]
  babab<-gsub("\\\\\\\\","/",a)
  a<-gsub("\\\\","",babab)
  split_a1<-unlist(strsplit(a,"/"))
  split_a<-unlist(strsplit(split_a1[length(split_a1)],":"))
  p3<-paste(c(split_a1[1:length(split_a1)-1]),collapse="/")
  bib_df$FILE[i]<-paste0(c(p3,"/",split_a[1]),collapse="")
  
  if(file.exists(bib_df$FILE[i])){
    file.copy(from = file.path(p3,split_a[1]), to = file.path(p2, paste0(c(b,".pdf"),collapse="")),overwrite = TRUE)
  }else{
    print(i)
    print(FileName)
  }
}

