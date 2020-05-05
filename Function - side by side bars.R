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

populate_dataframe <- function(number_of_rows){
  
  
  Data_for_bar_plot <- data.frame(Dimension=character(),
                                  Value1=numeric(),
                                  Value2=character(),
                                  stringsAsFactors=FALSE) 
  
  for(i in 1:number_of_rows) {
    
    
    Data_for_bar_plot[i, 1] <- readline(prompt="Enter the value for first column: ")
    Data_for_bar_plot[i, 2] <- as.numeric(readline(prompt="Enter the value for second column: "))
    Data_for_bar_plot[i, 3] <- readline(prompt="Enter the value for third column: ")
    
  }
  
  return(Data_for_bar_plot)
  
}


bar_plot <- function(Data_for_bar_plot, number_of_rows, color_for_bars,
                     text_size, text_color, axis_color, axis_font,
                     title_size, title_name){
  
  
  ggplot(Data_for_bar_plot, aes(x = reorder(Dimension, -Value1), y = Value1, fill = Value2)) + 
    
    geom_bar(position = position_dodge(width = .9), stat="identity" )+
    #geom_bar(position = "dodge", stat="identity" )+
    
    
    #----------------------------dark grey, sky blue, dark blue, light grey
    #scale_fill_manual(values = c("#404041", "#00b3e7", "#003a79", "#939597"),
    scale_fill_manual(values = c("#939597", "#00b3e7", "#404041", "#003a79"),
                      breaks=c("Member Non-sig", "Prospects Non-sig"), 
                      labels=c("Member", "Prospects"))+
    
    geom_text(size = text_size, aes(label = Value1, y = Value1 + 1), 
              color = text_color,
              position = position_dodge(width = .9))+
    
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
  
}

{
  
  input_data <- enter_data()
  Data_for_bar_plot <- populate_dataframe(unlist(input_data[1]))
  bar_plot(Data_for_bar_plot, unlist(input_data[1]), unlist(input_data[2]), unlist(input_data[3]),
           unlist(input_data[4]), unlist(input_data[5]), unlist(input_data[6]),
           unlist(input_data[7]), unlist(input_data[8]))
}

