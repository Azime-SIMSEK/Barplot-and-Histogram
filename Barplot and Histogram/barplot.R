
  dosya <- read.table(file = "DatasetNA.txt",header = TRUE)

  gender_barplot<-function(variable, female_color="red" , male_color="red" ){
  
  #for calculating female and male numbers
  female_count<-sum(dosya$Gender=="Female")
  print(female_count)
  male_count<-sum(dosya$Gender=="Male")
  print(male_count)

  plot(female_count,type="n",main="Genders",xaxt="n",axes=FALSE,xlab="",ylab="",xlim=c(0,2),
       ylim = c(0, max(female_count, male_count)))
  axis(2)
  
  polygon(x=c(0.2, 0.2, 0.6, 0.6), y=c(0, female_count, female_count, 0), col=female_color , border=NA)
  polygon(x=c(1, 1, 1.4, 1.4), y=c(0, male_count, male_count, 0), col=male_color, border=NA)
  
  axis(1, at = c(0.4, 1.2),labels = c("Female" ,"Male"),tcl=0,col = "white")
}
  gender_barplot(dosya[[variable]], female_color = "bisque",male_color =  "cadetblue4")

  #for cleaning plot
  dev.off()

  group_plot<-function(variable,group1_color="" ,group2_color="",group3_color="",group4_color=""){
  
  group1_count<-sum(dosya$Group=="Group1")
  cat("group1 is: ",group1_count,"\n")
  group2_count<-sum(dosya$Group=="Group2")
  cat("group2 is: ",group2_count,"\n")
  group3_count<-sum(dosya$Group=="Group3")
  cat("group3 is: ",group3_count,"\n")
  group4_count<-sum(dosya$Group=="Group4")
  cat("group4 is: ",group4_count,"\n")
  
  plot(group1_count,type="n",main="Groups",axes=FALSE,xlab="",ylab="",
       xlim=c(1,14),ylim = c(0, max(group1_count, group2_count)))
  axis(2)
  polygon(x=c(1, 1, 2.5, 2.5), y=c(0, group1_count, group1_count, 0),
          col=group1_color , border=NA)
  polygon(x=c(4, 4, 6, 6), y=c(0, group2_count, group2_count, 0),
          col=group2_color , border=NA)
  polygon(x=c(8, 8, 10, 10), y=c(0, group3_count, group3_count, 0),
          col=group3_color , border=NA)
  polygon(x=c(12, 12, 14, 14), y=c(0, group4_count, group4_count, 0),
          col=group4_color , border=NA)
  axis(1, at = c(1.75,5,9,13),labels = c("Group1" ,"Group2" ,"Group3" ,"Group4" ),tcl=0,col = "white")
  
}
  group_plot(dosya[[variable]],group1_color="green" ,group2_color="pink" ,group3_color="red" 
           ,group4_color="yellow")


  #for cleaning plot 
  dev.off()

  two_plot<-function(variable,female_color="" , male_color="",group1_color="" ,group2_color="",
                   group3_color="",group4_color=""){
  
  #for setting up two plots in one output
  par(mfrow = c(1, 2))  
  
  female_count<-sum(dosya$Gender=="Female")
  cat("female count is",female_count,"\n")
  male_count<-sum(dosya$Gender=="Male")
  cat("male count is",male_count,"\n")
  
  plot(female_count,type="n",main="Genders",xaxt="n",axes=FALSE,xlab="",ylab="",
       xlim=c(0,2),ylim = c(0, max(female_count, male_count)))
  axis(2)
  
  polygon(x=c(0.2, 0.2, 0.6, 0.6), y=c(0, female_count, female_count, 0), col=female_color , border=NA)
  polygon(x=c(1, 1, 1.4, 1.4), y=c(0, male_count, male_count, 0), col=male_color, border=NA)
  
  axis(1, at = c(0.4, 1.2),labels = c("Female" ,"Male"),tcl=0,col = "white")
  
  group1_count<-sum(dosya$Group=="Group1")
  cat("group1 is: ",group1_count,"\n")
  group2_count<-sum(dosya$Group=="Group2")
  cat("group2 is: ",group2_count,"\n")
  group3_count<-sum(dosya$Group=="Group3")
  cat("group3 is: ",group3_count,"\n")
  group4_count<-sum(dosya$Group=="Group4")
  cat("group4 is: ",group4_count,"\n")
  plot(group1_count,type="n",main="Groups",axes=FALSE,xaxt="n",xlab="",ylab="",
       xlim=c(1,14),ylim = c(0, max(group1_count, group2_count)))
  axis(2)
  
  
  polygon(x=c(1, 1, 2.5, 2.5), y=c(0, group1_count, group1_count, 0),
          col=group1_color , border=NA)
  polygon(x=c(4, 4, 6, 6), y=c(0, group2_count, group2_count, 0),
          col=group2_color , border=NA)
  polygon(x=c(8, 8, 10, 10), y=c(0, group3_count, group3_count, 0),
          col=group3_color , border=NA)
  polygon(x=c(12, 12, 14, 14), y=c(0, group4_count, group4_count, 0),
          col=group4_color , border=NA)
  
  axis(1, at = c(1.75,5,9,13),labels = c("Group1" ,"Group2" ,"Group3" ,"Group4" ),tcl=0,col="white")
}
  two_plot(dosya[[variable]], female_color = "gray",male_color =  "blue"  ,group1_color="green"
         ,group2_color="pink" ,group3_color="red" ,group4_color="yellow")





