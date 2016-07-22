####################################################
#      Segmentation Discriminant and Targeting     #
####################################################

library("shiny")
library("cluster")
library("ggbiplot")
library("mclust")
library("MASS")

shinyServer(function(input, output) {
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
      Dataset1 = scale(Dataset1, center = T, scale = T)
      return(Dataset1)
    }
  })
  
  Dataset2 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      return(Dataset1)
    }
  })
      
      discri_data <- reactive({
        if (is.null(input$file)) { return(NULL) }
        else{
          Dataset <- as.data.frame(read.csv(input$file1$datapath ,header=TRUE, sep = ","))
          rownames(Dataset) = Dataset[,1]
          Dataset1 = Dataset[,2:ncol(Dataset)]
          return(Dataset1)
        }
      })
    
      target_data <- reactive({
        if (is.null(input$file)) { return(NULL) }
        else{
          Dataset <- as.data.frame(read.csv(input$file2$datapath ,header=TRUE, sep = ","))
          rownames(Dataset) = Dataset[,1]
          Dataset1 = Dataset[,2:ncol(Dataset)]
          return(Dataset1)
        }
      })
      
  output$table <- renderTable({
    if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = kmeans(Dataset(),input$Clust)
        Segment.Membership =  fit$cluster
        d = data.frame(Segment.Membership,Dataset2())
        d
      }
    })
    
    else  if (input$select == "Model Based") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = Mclust(Dataset(),input$Clust)
        Segment.Membership =  fit$classification
        d = data.frame(Segment.Membership,Dataset2())
        d
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        distm <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(distm, method="ward") 
        Segment.Membership =  cutree(fit, k=input$Clust)
        d = data.frame(Segment.Membership,Dataset2())
        d
      }
    })  
  })
  
  output$caption1 <- renderText({
    if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
    else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
    else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
    else return (NULL)
  })
  
  output$caption2 <- renderText({
    if (input$select == "Model Based") 
    {
      fit0 = Mclust(Dataset())
      return(paste("Note - Optimal Segments Should be:",fit0$G,""))
    }
    else return(NULL)
  })
  
  output$summary <- renderPrint({
    if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        fit = kmeans(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$cluster)
        clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
        Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership) )
        Summary
      }
    })
    
    else  if (input$select == "Model Based") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = Mclust(Dataset(),input$Clust)
        Segment.Membership = as.character(fit$classification)
        clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
        Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans,ModelSumm = summary(fit) )
        Summary
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        d <- dist(Dataset(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
        Segment.Membership =  as.character(cutree(fit, k=input$Clust))
        clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
        Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership), ModelSumm = fit )
        Summary
      }
    })  
  })
  
  output$discriminat <- renderPrint({
    if (input$select == "K-Means") ({
      fit = kmeans(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$cluster)
    })
    else  if (input$select == "Model Based") ({
      fit = Mclust(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$classification)
    })
    else if (input$select == "Hierarchical") ({
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      Segment.Membership =  as.character(cutree(fit, k=input$Clust))
    })
    
    Classification = Segment.Membership
    
    data = discri_data()
    
    fit <- lda(Classification ~ . , data=data, na.action = "na.omit", CV=TRUE)
    fit0 <- lda(Classification ~ . , data=data)
    ct <- table(Classification, fit$class)
    Proportion = diag(prop.table(ct, 1))
    Percent.Correct = sum(diag(prop.table(ct)))*100
    discri = list(confusion.matrix = ct, 
                  Proportion= Proportion, Percent.Correct = Percent.Correct,modelout = fit0)
    discri
  })
  
  ############------------------------------------------------------------------------------------------#############
  output$targeting <- renderPrint({
    if (input$select == "K-Means") ({
      fit = kmeans(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$cluster)
    })
    else  if (input$select == "Model Based") ({
      fit = Mclust(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$classification)
    })
    else if (input$select == "Hierarchical") ({
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      Segment.Membership =  as.character(cutree(fit, k=input$Clust))
    })
    
    Classification = Segment.Membership
    data = discri_data()
    
    fit = lda(Classification ~ ., data=data)
    # , na.action = "na.omit", CV=F)
    prediction <- predict(fit, newdata=target_data())
    return(prediction)
      })
  ###############3-------------------------------------------------------------------3############################################
  
  
  output$table1 <- renderTable({
    
    if (input$select == "K-Means") ({
      fit = kmeans(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$cluster)
    })
    else  if (input$select == "Model Based") ({
      fit = Mclust(Dataset(),input$Clust)
      Segment.Membership = as.character(fit$classification)
    })
    else if (input$select == "Hierarchical") ({
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      Segment.Membership =  as.character(cutree(fit, k=input$Clust))
    })
    
    Classification = Segment.Membership
    data = discri_data()
    
    fit = lda(Classification ~ ., data=data, na.action = "na.omit", CV=F)
    prediction <- predict(fit, newdata=target_data())
    
    Targeted.segment = prediction$class
    data.frame(Targeted.segment, target_data())
  })
    
  
  output$plotpca = renderPlot({ 
    
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    else {
    data.pca <- prcomp(Dataset(),center = TRUE,scale. = TRUE)
    plot(data.pca, type = "l"); abline(h=1)    
    }
    })
    
  output$plot = renderPlot({  
    
    if (input$select == "K-Means") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      fit = kmeans(Dataset(),input$Clust)
      
      classif1 = as.character(fit$cluster)
      data.pca <- prcomp(Dataset(),
                         center = TRUE,
                         scale. = TRUE)
      
      # plot(data.pca, type = "l"); abline(h=1)    
      
      g <- ggbiplot(data.pca,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = classif1,
                    ellipse = TRUE,
                    circle = TRUE)
      
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'horizontal',
                     legend.position = 'top')
      print(g)
      
    })
    
    else if (input$select == "Model Based") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      fit = Mclust(Dataset(),input$Clust)
      classif1 = as.character(fit$classification)
      data.pca <- prcomp(Dataset(),
                         center = TRUE,
                         scale. = TRUE)
      
      # plot(data.pca, type = "l"); abline(h=1)    
      
      g <- ggbiplot(data.pca,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = classif1,
                    ellipse = TRUE,
                    circle = TRUE)
      
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'horizontal',
                     legend.position = 'top')
      print(g)
      
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      d <- dist(Dataset(), method = "euclidean") # distance matrix
      fit <- hclust(d, method="ward") 
      plot(fit) # display dendogram
      groups <- cutree(fit, k=input$Clust) # cut tree into 5 clusters
      # draw dendogram with red borders around the 5 clusters
      rect.hclust(fit, k=input$Clust, border="red") 
    })
  })
  
})