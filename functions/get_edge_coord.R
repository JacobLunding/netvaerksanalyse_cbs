get_edge_coord <- function(graph = comp1, layout = stable_lay, add_link = NULL) {
  e <- as_edgelist(graph) %>% data.frame 
  e <- e %>% left_join(., tibble(X1 = layout$name, x = layout$x, y = layout$y), by = "X1") %>% left_join(., tibble(X2 = layout$name, xend =layout$x,  yend = layout$y), by = "X2") 
  if(!is_null(add_link)){
    new_links <- lapply(add_link, function(x) {
      s1 <- which(layout$name == x[1])
      s2 <- which(layout$name == x[2])
      data.frame(X1 = layout$name[s1] %>% unique(), x = layout$x[s1] %>% unique(), y = layout$y[s1],
                 X2 = layout$name[s2] %>% unique(), xend = layout$x[s2] %>% unique(), yend = layout$y[s2])
    }) %>% bind_rows %>% mutate(new = 2)
    
    e <- bind_rows(e, new_links) %>% mutate(new = if_else(is.na(new), 1, new))
    e 
  }else{
    e
  }
}

generateRPointShapes<-function(){
  oldPar<-par()
  par(font=2, mar=c(0.5,0,0,0))
  y=rev(c(rep(1,6),rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
  x=c(rep(1:5,5),6)
  plot(x, y, pch = 0:25, cex=1.5, ylim=c(1,5.5), xlim=c(1,6.5), 
       axes=FALSE, xlab="", ylab="", bg="blue")
  text(x, y, labels=0:25, pos=3)
  par(mar=oldPar$mar,font=oldPar$font )
}