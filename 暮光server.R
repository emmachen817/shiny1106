rm(list=ls())
library(shiny)
library(shinyIncubator)
library(tm)
library(wordcloud)
library(cluster)
library(Rwordseg)
library(tmcn)
library(slam)
library(rCharts)
library(rHighcharts)
library(ape)
library(topicmodels)
library(igraph)





# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
   
  output$plot<-renderPlot({ 
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data<-read.table(inFile$datapath,colClasses="character")
    
    insertedwords <- c("愛德華","貝拉","停車場","泰勒","卡車","貨車","警車","救護車","庫倫",
                       "拉布席","海灘","雅各","布雷克","庫倫一家","保留區","冷血人","狩獵","卡萊爾","吸血鬼","故事",
                       "艾思蜜","艾利絲","賈斯伯","優雅","天賦","羅絲莉","艾密特","鋼琴",
                       "棒球","比賽","查理","閃電","密林","危險","山姆",
                       "羅倫特","維多利亞","詹姆斯","咆哮","恐懼","追蹤者","福克斯","鳳凰城",
                       "機場","家族","電話","毒液","房間","鏡子","電視","溫哥華","芭蕾舞教室","舞蹈","西雅圖","媽媽","預測","化妝間","獵物","短信","火焰","芮妮")
    insertWords(toTrad(iconv(insertedwords, "big5", "UTF-8"), TRUE))
    word<- segmentCN(data$V1)
    word<-Corpus(VectorSource(word))
    myStopWords <- c(stopwordsCN(), "你","我","他", "你們","我們","他們","因為","所以","如果","這樣","那樣","這麼","那麼","也","那","得")
    reuters <- tm_map(word, removeWords, myStopWords)
    d.corpus<- tm_map(reuters , segmentCN, nature = TRUE)
    d.corpu1 <- tm_map(d.corpus, function(sentence) {
      noun <- lapply(sentence, function(w) {
        w[names(w) =="userDefine"]
      })
      unlist(noun)
    })
    
    d.vec <- sapply(d.corpu1 , paste, collapse = " ")
    d.vec <- unique(d.vec)
    d.corpu3 <- Corpus(VectorSource(d.vec))
    
    dtm <- DocumentTermMatrix(d.corpu3, 
                              control = list(wordLengths=c(2,6),removeNumbers = TRUE,
                                             removePunctuation  = list(preserve_intra_word_dashes = FALSE),
                                             weighting = weightTf,encoding = "UTF-8"))
    
    table1 <- col_sums(dtm)
    wordsDf <- data.frame(WORD = names(table1), FREQ = as.vector(table1), stringsAsFactors = FALSE)
    
    wordcloud(wordsDf$WORD,wordsDf$FREQ,min.freq = input$freq, max.words=input$max,scale=c(10,.5),
              colors=rainbow(length(wordsDf$FREQ)))
    
        
    datasetInput <- reactive({table1})
    
    output$wf <- downloadHandler( filename = function() {paste("wordfreq", ".csv", sep='') },
                                  content = function(file) {write.csv(datasetInput(), file) } )
    
    output$baplot<-renderChart({
      a <- rHighcharts:::Chart$new()
      a$chart(type = "bar")
      a$plotOptions(column = list(stacking = "normal"))
      a$title(text = "詞頻長條圖") 
      a$yAxis(title = list(text = "頻率")) 
      x <- as.data.frame(wordsDf) 
      a$xAxis(categories = wordsDf$WORD) 
      a$data(x)
      return(a)})   
    
    
    output$table <- renderDataTable({
      
      moss<-data.frame( findAssocs(dtm, input$dd, 0.1) )
      moa<-rownames(moss)
      mob<-moss[,1]
      moc<-cbind(moa,mob)
      colnames(moc)=c("字詞", "相關性")
      finall<-moc
    })
    
    
    
    
    
    dtm = dtm[row_sums(dtm)>0, ]
    fold_num = 10
    kv_num =  c(5, 10*c(1:5, 10))
    seed_num = 2003
    try_num = 1
    
    smp<-function(cross=fold_num,n,seed)
    {
      set.seed(seed)
      dd=list()
      aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)
      for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]
      return(dd)
    }
    
    selectK<-function(dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp) # change 60 to 15
    {
      per_ctm=NULL
      log_ctm=NULL
      for (k in kv)
      {
        per=NULL
        loglik=NULL
        for (i in 1:try_num)  #only run for 3 replications# 
        {
          cat("R is running for", "topic", k, "fold", i,
              as.character(as.POSIXlt(Sys.time(), "Asia/Shanghai")),"\n")
          te=sp[[i]]
          tr=setdiff(1:dtm$nrow, te)
          Gibbs = LDA(dtm[tr,], k = k, method = "Gibbs",
                      control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000))
          
          per=c(per,perplexity(Gibbs,newdata=dtm[te,]))
          loglik=c(loglik,logLik(Gibbs,newdata=dtm[te,]))
        }
        per_ctm=rbind(per_ctm,per)
        log_ctm=rbind(log_ctm,loglik)
      }
      return(list(perplex=per_ctm,loglik=log_ctm))
    }
    
    sp=smp(n=dtm$nrow, seed=seed_num) # n = nrow(dtm)
    
    ctmK=selectK(dtm=dtm,kv=kv_num,SEED=seed_num,cross=fold_num,sp=sp)
    
    ## plot the perplexity
    
    m_per=apply(ctmK[[1]],1,mean)
    m_log=apply(ctmK[[2]],1,mean)
    
    k=c(kv_num)
    df = ctmK[[1]]  # perplexity matrix
    logLik = ctmK[[2]]  # perplexity matrix
    
    logLiktest<-data.frame(k, df, logLik)
    
    
    output$storyplot<-renderPlot({
      
      k= input$k
      SEED <- 2003
      
      jss_TM2 <- list(
        VEM = LDA(dtm, k , control = list(seed = SEED)),
        VEM_fixed = LDA(dtm, k , control = list(estimate.alpha = FALSE, seed = SEED)),
        Gibbs = LDA(dtm, k , method = "Gibbs", 
                    control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)),
        CTM = CTM(dtm, k , 
                  control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))) )  
      
      termsForSave4<- terms(jss_TM2[["VEM"]], 10) 
      termsForSave4<-iconv(termsForSave4, "UTF-8")
      
                                   
      output$topic <- downloadHandler( filename = function() {paste("topic", ".csv", sep='') },
                                    content = function(file) {write.csv((t(termsForSave4)), file) } )
      
      tfs4 = as.data.frame(termsForSave4, stringsAsFactors = F)
      adjacent_list = lapply(1:5, function(i) embed(tfs4[,i], 2)[, 2:1]) 
      edgelist = as.data.frame(do.call(rbind, adjacent_list), stringsAsFactors =F)
      topic = unlist(lapply(1:5, function(i) rep(i, 9)))
      edgelist$topic = topic
      g <-graph.data.frame(edgelist,directed=T )
      l<-layout.fruchterman.reingold(g)
      nodesize = centralization.degree(g)$res 
      V(g)$size = log( centralization.degree(g)$res )
      
      nodeLabel = V(g)$name
      E(g)$color =  unlist(lapply(sample(colors()[26:137], 5), function(i) rep(i, 9))); unique(E(g)$color)
      
      plot(g,vertex.label= nodeLabel,  edge.curved=TRUE,vertex.label.cex =2,  edge.arrow.size=1, layout=l  )
    },width=1000, height=1000)
    
    
    output$logLiktest <- downloadHandler( filename = function() {paste("logLiktest", ".csv", sep='')  },
                                  content = function(file) {write.csv(logLiktest, file)   }  )
    
    
    
    
    output$clplot<-renderPlot({ tdm <- TermDocumentMatrix(d.corpu3, control = list(wordLengths=c(2, 6),removeNumbers = TRUE,
                                                                                   removePunctuation = list(preserve_intra_word_dashes = FALSE),
                                                                                   weighting = weightTf,encoding = "UTF-8"))
                                data <- as.data.frame(inspect(tdm))
                                data.scale <- scale(data)
                                d <- dist(data.scale, method = "euclidean")
                                fit <- hclust(d,method="ward")
                                mypal = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")
                                clus5 = cutree(fit, k=input$cn)
                                op = par(bg = "#E8DDCB")
                                plot(as.phylo(fit), type = "fan", tip.color = mypal[clus5], label.offset = 1, 
                                     cex =input$pz, col = "red")
                                datasetInput <- reactive({cluster<-cutree(fit, k=input$cn) 
                                                          datacluster<-cbind(data$rownames, cbind(cluster))})
                                output$dc <- downloadHandler( filename = function() {paste("datacluster", ".csv", sep='') },
                                                              content = function(file) {write.csv(datasetInput(), file) } )
    
                                
                                  
                                 
    }) })
  output$ptplot <- renderPlot({
    
    par(mfrow=c(2,2))
    
    if (input$tp) {
      csv1 <- read.csv("C:\\Users\\11\\Desktop\\台北.csv",stringsAsFactors=F)
      wordsfreq1 <- csv1$l
      wordsname1 <- csv1$d.corpu3 
      wordcloud(wordsname1, wordsfreq1,  min.freq = 1,max.words=input$mmax,  colors = rainbow(length(wordsfreq1)))
      title(main = list( "台北", cex = 3,col= "gray22"　))
    }
    if (input$tc) {
      csv2 <- read.csv("C:\\Users\\11\\Desktop\\台中.csv",stringsAsFactors=F)
      
      wordsfreq2 <- csv2$l
      wordsname2 <- csv2$d.corpu3 
      wordcloud(wordsname2, wordsfreq2,  min.freq = 20,max.words=input$mmax,  colors = rainbow(length(wordsfreq2)))
      title(main = list( "台中", cex = 3,col= "gray22"　))
    }
    if (input$tn) {
      csv3 <- read.csv("C:\\Users\\11\\Desktop\\台南.csv",stringsAsFactors=F)
      
      wordsfreq3 <- csv3$l
      wordsname3 <- csv3$d.corpu3 
      wordcloud(wordsname3, wordsfreq3,  min.freq = 90,max.words=input$mmax,  colors = rainbow(length(wordsfreq3)))
      title(main = list( "台南", cex = 3,col= "gray22"　))
    }
    
    if (input$hs) {
      csv4 <- read.csv("C:\\Users\\11\\Desktop\\高雄.csv",stringsAsFactors=F)
      
      wordsfreq4 <- csv4$l
      wordsname4 <- csv4$d.corpu3 
      wordcloud(wordsname4, wordsfreq4,  min.freq = 10,max.words=input$mmax,  colors = rainbow(length(wordsfreq4)))
      title(main = list( "高雄", cex = 3,col= "gray22"　))
    }  
  })
  
  output$table2 <- renderDataTable({
    csv5 <- read.csv("C:\\Users\\11\\Desktop\\alldata.csv",stringsAsFactors=F)
    csv6<-csv5[,-(1:2)]
    csv6
    csv6[csv6$name==input$name,]
  
    })
    
  })