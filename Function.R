library(ggplot2)

enter_data <- function(){

  number_of_rows <- as.numeric(readline(prompt="Enter number of rows in dataframe: "))
  
  color_for_bars <- readline(prompt="Enter the color for bar graph in hex: ")
  
  text_size <- as.numeric(readline(prompt="Enter the text size for values: "))
  
  text_color <- readline(prompt="Enter the text color: ")
  
  axis_color <- readline(prompt="Enter the axis color: ")
  
  axis_font <- readline(prompt="Enter the axis font: ")
  
  title_size <- as.numeric(readline(prompt="Enter the title font size: "))
  
  title_name <- readline(prompt="Enter the title for plot: ")
  
  return(list(number_of_rows, color_for_bars, text_size, text_color,
         axis_color, axis_font, title_size, title_name))
  
}

input_data <- enter_data()

number_of_rows <- unlist(input_data[1])
color_for_bars  <- unlist(input_data[2])
text_size <- unlist(input_data[3])
text_color <- unlist(input_data[4])
axis_color <- unlist(input_data[5])
axis_font <- unlist(input_data[6])
title_size <- unlist(input_data[7])
title_name <- unlist(input_data[8])

Data_for_bar_plot <- data.frame(Dimension = c("A", "B", "C"),
                                Value = c(20, 30, 40))

Data_for_bar_plot$Dimension <- as.character(Data_for_bar_plot$Dimension)

for(i in 1:number_of_rows) {
  
  Data_for_bar_plot[i, 1] <- readline(prompt="Enter the value for first column: ")
  
}

for(i in 1:number_of_rows) {
  
  Data_for_bar_plot[i, 2] <- readline(prompt="Enter the value for second column: ")
  
}

Data_for_bar_plot$Value <- as.numeric(Data_for_bar_plot$Value)


bar_plot <- function(Data_for_bar_plot, number_of_rows, color_for_bars,
                     text_size, text_color, axis_color, axis_font,
                     title_size, title_name){


  ggplot(Data_for_bar_plot, aes(x = reorder(Dimension, -Value), y = Value)) + 
    
    geom_bar(stat="identity", fill = color_for_bars )+
    
    geom_text(size = text_size, aes(label = Value, y = Value + 1), 
              color = text_color)+
    
    theme(text=element_text(family = axis_font), 
          axis.line.x = element_line(color=axis_color, size = 0.25), 
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="bottom",
          legend.direction="horizontal",
          legend.title = element_blank(),
          legend.spacing.x = unit(.4, 'cm'),
          axis.ticks.x= element_blank(), 
          axis.title.x= element_blank(), 
          axis.ticks.y= element_blank(), 
          axis.title.y= element_blank(), 
          axis.text.y= element_blank(),
          plot.title = element_text(hjust = 0.5, size = title_size, face="bold")) +
    
    ggtitle(title_name)
  
  #file_name <- paste(Data_for_bar_plot, "Bar plot.png", sep = "_")
  
  #ggsave(file_name, width=12, height=5)

}

