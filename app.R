#   *------------------------------------------------------
##################### Information ########################
#   *------------------------------------------------------
#   *       File-Name:    Google Scholar shiny app
#   *       Date:         05/02/2016
#   *       Author:       3AT
#   *       Purpose:      buil coauthorship network
#   *       Input File:   google scholar prifile link
#   *       Output File: coauthorship graph
#   *       Data Output:
#   *       Previous file:
#   *       Machine:    Ubuntu
#   *------------------------------------------------------
#   *------------------------------------------------------
### in order to see coauthorship networks graph, copy google scholar profile of author (example: "https://scholar.google.com/citations?user=zufgVroAAAAJ&hl=en") and put it in the text box provided in the application UI, you will see coauthorship network in seconds
# this application uses scholar package in R to exctract data from google scholar
# as always, I owe most parts of the code and what I wrote to stackoverflow and google searches

# load libraries
require(shiny)
require(shinyjs)
require(graphics)
require(methods)
require(scholar)
require(igraph)
# require(network)
require(stringdist)
require(networkD3)

# Shiny app
shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Who are researchers mostly co-authoring with?"),
    sidebarPanel(
      helpText("see how it works"),
      HTML('<iframe width="350" height="200" src="https://www.youtube.com/embed/NZ5WdBnZ-CE" frameborder="0" allowfullscreen></iframe>'),
      helpText("Copy the link to your person of interest Google Scholar profiles (example: 'https://scholar.google.com/citations?user=zufgVroAAAAJ&hl=en') below and see the co-authorships network", style = "color:green"),

    textInput("scholarid",'google scholar profile link',value = ""),
    actionButton("submit", "Submit"),
    helpText("Reference: data for co-authorships and publications are being extracted from google scholar with 'scholar' package for R"),
    helpText("**: It is the first and earliest version, some issues are knwon and in the process to be solved, some not, I will be happy to hear: akbaritabar[at] gmail.com")


  ),
  mainPanel(
    h4("author's data",align="top",align="center"),
    tableOutput("sctable"),
   h4("Raw co-authorships Graph",align="top",align="center"),
   helpText("use mouse scroll to zoom in and out; further details (including main component) below", style="color:green"),
   simpleNetworkOutput("evalPlot1"),
   h4("Main component",align="bottom",align="center"),

   plotOutput("evalPlot2"),
   h4("author name's inconsistencies",align="top",align="center"),
   helpText("if you see similar name with different spellings, means google scholar data has inconsistencies that need to be resolved", style="color:green"),
   textOutput("sctext")

    )
  ),
  server = function(input, output) {

    observeEvent(input$submit, {
      citid <- strsplit((strsplit(input$scholarid,"&",fixed = TRUE)[[1]][1]),"=",fixed = TRUE)[[1]][2]
      infosc <- get_profile(citid)
      pub <- get_publications(citid, flush=TRUE)
      authnum_articles <- get_num_articles(citid)
      authdistinc_journal <- get_num_distinct_journals(citid)
      autholdest_article <- get_oldest_article(citid)
      authnum_topjournals <- get_num_top_journals(citid)
      ## tolower does character conversion, and remove the trailing "..."
      coauthors <- sub('[ ,.]+$', '', tolower(pub$author))
      coauthors <- coauthors[nzchar(coauthors)]  # only keep entries that aren't blank
      ## Add self-loops for single-author entries
      adjlist <- strsplit(coauthors, '\\s*,\\s*')
      lens <- lengths(adjlist)
      adjlist[lens==1L] <- lapply(adjlist[lens==1L], rep, times=2)  # repeat single author entries

      edgelist <- cbind(
        unlist(lapply(adjlist, tail, -1L)),                        # col1
        rep(sapply(adjlist, `[`, 1L), times=lengths(adjlist)-1L)   # col2
      )

      charr.list <- list()
      charr.list2 <- list()
      for (j in seq_along(edgelist[,1])) {
        charr <- edgelist[,1][j]
        charr<-unlist(strsplit(charr, '\\s* \\s*'))
        charr <- charr[max(seq_along(charr))]
        charr.list[[j]] <- charr


        charr2 <- edgelist[,2][j]
        charr2<-unlist(strsplit(charr2, '\\s* \\s*'))
        charr2 <- charr2[max(seq_along(charr2))]
        charr.list2[[j]] <- charr2

      }
      edgelist.clean <- cbind(as.character(charr.list),as.character(charr.list2))

      coauthorgraph <- graph_from_edgelist(edgelist.clean,directed = TRUE)
      # report of author name inconsistencies
      name.author<-unlist(strsplit(infosc$name, '\\s* \\s*'))
      name.author <- tolower(name.author[max(seq_along(name.author))])
      comparisons <- stringdist(name.author,names(V(coauthorgraph)),method = "jw")
      node.comparisons <- V(coauthorgraph)[0<comparisons & comparisons<0.3]
      results <- list(node.comparisons=node.comparisons)

      # author information to be shown on app
      authordatadownloaded <- cbind.data.frame(
        "number of articles" = as.character(authnum_articles),
        "unique journals" = as.character(authdistinc_journal),
        "oldest article" = as.character(autholdest_article),
        "number of top journals" = as.character(authnum_topjournals),
        "h-index" = as.character(infosc$h_index)
      )
      networkData <- data.frame(edgelist.clean)

      shinyjs::disable("submit")
      shinyjs::disable("scholarid")
      shinyjs::disable("scholarid2")
      output$evalPlot1 <- renderSimpleNetwork({
        set.seed(333)
        # using networkD3 to have zoom possibility
        simpleNetwork(networkData,zoom = TRUE)
        })

        output$evalPlot2 <- renderPlot({
        set.seed(333)
        ord <- V(coauthorgraph)                                               # node order
        theta <- seq(0, 2*pi-2*pi/length(ord), 2*pi/length(ord))  # angle
        theta[theta>pi] <- -(2*pi - theta[theta>pi])              # convert to [0, pi]
        dists <- rep(c(1, 0.7), length.out=length(ord))           # alternate distance


        ## Plot
        plot(decompose.graph(coauthorgraph)[[order(clusters(coauthorgraph)$csize,decreasing = TRUE)[1]]], layout=layout.auto, vertex.label.degree=-theta, main = paste("main component of:",infosc$name),
             vertex.label.dist=dists, vertex.label.cex=1.1,
             vertex.size=5, vertex.color='#FFFFCC', edge.color='#E25822')
      })

      output$sctable <- renderTable(authordatadownloaded)
      output$sctext <- renderPrint(results)
      })
      }
)
