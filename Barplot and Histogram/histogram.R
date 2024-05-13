dev.off()
Dataset <- read.table("DatasetNA.txt", header = TRUE)
color<-"gray"
variables <- Dataset$Var1[!is.na(Dataset$Var1)]
main_title <- "Histogram"
my_x_label <- " "


myHistogram <- function(variable, color = "gray", variables, main_title, my_x_label) {
  
  max_value <- ceiling(max(variables))
  min_value <- round(min(variables))
  range <- max_value - min_value
  
  width <- range / 10
  
  #for limits
  bins <- seq(min_value, max_value, width)
  
  #for calculating frequency
  frequency <- tabulate(findInterval(variables, bins))
  
  print(bins)
  print(frequency)


  plot(frequency ,type = "n",axes=FALSE,xaxt="n",col = color,ylim = c(0, max(frequency)),
       main = main_title, xlab =my_x_label ,ylab = "Frequency")
  
  #for drawing columns 
  for (i in 1:length(frequency)) {
    polygon(x = c(i, i, i + 1, i + 1),
            y = c(0, frequency[i], frequency[i], 0), col = color, border = "black")
  }
  
  
  #for axes
  axis(2, at = seq(0, max(frequency), by = max(frequency) / 10))
  axis(1, at = seq_along(bins), labels = bins)
  
}

myHistogram(Dataset[[variable]], color = "pink",Dataset$Var7[!is.na(Dataset$Var7)]
            , main_title = "Histogram of Var", my_x_label = "Var5")
           

dev.off()
myHistogram <- function(variables, color = "gray", variables_list, main_titles, x_labels) {
  par(mfrow = c(1, length(variables_list)))  #for setting up multiple plots in one output
  
  for (i in seq_along(variables_list)) {
    variables <- variables_list[[i]]
    main_title <- main_titles[i]
    my_x_label <- x_labels[i]
    
    max_value <- ceiling(max(variables))
    min_value <- round(min(variables))
    range <- max_value - min_value
    
    width <- range / 8
    
    bins <- seq(min_value, max_value, width)
    frequency <- tabulate(findInterval(variables, bins))
    
    print(bins)
    print(frequency)
    length(variables)
    
   
    plot(frequency, type = "n", axes = FALSE, xaxt = "n", col = color, ylim = c(0, max(frequency)),
         main = main_title, xlab = my_x_label, ylab = "Frequency")
    
    for (j in 1:length(frequency)) {
      polygon(x = c(j, j, j + 1, j + 1),
              y = c(0, frequency[j], frequency[j], 0), col = color, border = "black")
    }
    
    axis(2, at = seq(0, max(frequency), by = max(frequency) / 10))
    axis(1, at = seq_along(bins), labels = bins)
  }
}

# multiple histograms for plotting in the output screen together 
variables_list <- list(Dataset$Var1[!is.na(Dataset$Var1)], 
                       Dataset$Var2[!is.na(Dataset$Var2)])

main_titles <- c("Histogram of Var1", "Histogram of Var2")
x_labels <- c("Var1", "Var2")

myHistogram(Dataset[[variables]], color = "pink",
            variables_list, main_titles, x_labels)

