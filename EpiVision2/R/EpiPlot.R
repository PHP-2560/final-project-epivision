#' Create Explanatory and Descriptive Plots
#'
#'This function plots data according to given variables and parameters from a dataset.
#'EpiPlot includes the following plots: bar plots, grouped bar plots, stacked bar plots, box plots, box plots with correlating dots, histograms,
#'density plots, scatter plots, scatter plots with correlating line, and linear regression plots.
#'
#' Arguments:
#' data: dataset of choice formatted as specified
#' x: x variable
#' y: y variable
#' graph: specified graph of choice
#' fill: additional variable used for fill of the plot
#' title: Title of desired graph
#' xlab: Label for the x-axis
#' ylab: Label for the y-axis
#' Label: Label for graph legend
#'
#'@Example
#'epiplot(Fatalities,x=Fatalities$drinkage, y=Fatalities$income, graph="scatter", fill=Fatalities$population, title="Scatter Plot", xlab="Drinkage", ylab="Income", legend= "Population")
#'
#'@export

library(ggplot2)
library(RColorBrewer)
library(plotly)
library(AER)

epiplot<- function(data, x, y, graph, fill, title, xlab, ylab, legend){
  data[complete.cases(data), ]
  if(graph == "bar"){
    pic<-  ggplot(data=data, aes(x=x, fill=x)) +
      geom_bar( ) +
      scale_fill_brewer(palette = "Paired")+
      labs(title="title", x="xlab", y="ylab")
    #table1 = table(data$x)  ## get the cross tab
    #pic<-barplot(table1, beside = TRUE, legend = levels(data$x), col=c("lightblue","darkblue"),main="title", xlab="xlab", ylab = "ylab")
    return(pic)
    #barplot(table(data$x), col=c("lightblue","darkblue"),main="title", xlab="xlab", ylab = "ylab")
  } else if(graph=="bargroup"){
    pic<-ggplot(data=data, aes(x=x, y=y, fill=fill)) +
      geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Paired")+theme_bw()+facet_wrap(~"fill")
    return(pic)
  } else if(graph=="barstack"){
    pic<-ggplot(data=data, aes(fill=fill, y=y, x=x)) +
      geom_bar( stat="identity")
    return(pic)
  } else if(graph=="boxplot"){
    pic<-boxplot(y~x, data=data, notch=TRUE,
                 main="title", xlab="xlab", ylab="ylab")
    return(pic)
  } else if(graph=="dotboxplot"){
    pic<-plot_ly(y = ~y, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8)
    return(pic)
  } else if(graph=="hist"){
    pic<-ggplot(data=data, aes(x)) +
      geom_histogram(col="black", aes(fill=..count..)) +
      scale_fill_gradient("Count", low="light blue", high="navy")+
      labs(title="title", x="xlab", y="ylab")
    return(pic)
  } else if(graph=="densityhist"){
    pic<-ggplot(data=data, aes(x)) +
      geom_histogram(aes(y =..density..),col="blue", fill="light blue", alpha=.5) +
      geom_density(col=2) +
      labs(title="title", x="xlab", y="ylab")
    return(pic)
  } else if(graph=="scatter"){
    pic<-ggplot(data, aes(x, y, color = fill)) +
      geom_point(shape = 16, size = 5, show.legend = TRUE) +
      theme_minimal() +
      #scale_color_gradient(color = "Blues")+
      labs(title="title", x="xlab", y="ylab", color = "legend")
    return(pic)
  } else if(graph=="scatterline"){
    pic<-ggplot(data, aes(x, y, color = fill)) +
      geom_point(shape = 16, size = 5, show.legend = TRUE) +
      theme_minimal() +
      scale_color_gradient(low = "light blue", high = "dark blue")+
      labs(title="title", x="xlab", y="ylab", color = "legend")+geom_smooth()
    return(pic)
  } else if(graph=="linreg"){
    pic<-ggplot(data, aes(x, y, color = fill)) +
      geom_point(shape = 16, size = 5, show.legend = TRUE) +
      theme_minimal() +
      scale_color_gradient(low = "light blue", high = "dark blue")+
      labs(title="title", x="xlab", y="ylab", color = "legend")+ geom_smooth(method = 'lm', se = TRUE)
    return(pic)
  }
}
