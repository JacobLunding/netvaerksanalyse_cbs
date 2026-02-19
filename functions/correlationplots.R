cor_plots <- function(metrics_table, plot.title = "Korrelationer mellem centralitetsmål", title.size = 12, name_var = c("name", "affiliation")) {
  require(ggplot2)
  require(tidyverse)
  require(ggpubr)
  metrics_table <- metrics_table %>% select(-any_of(name_var))
  deg <- lapply(colnames(metrics_table), function(t) { 
    lapply(colnames(metrics_table), function(z) {
      tmp <- cbind(metrics_table %>% select(matches(z)), 
                   metrics_table %>% select(matches(t))) 
      colnames(tmp) <- c("x", "y")
      ggplot(data = tmp, aes(x = x, y = y)) + 
        geom_point(alpha = .5) + #geom_smooth(method = smooth_fun, se = FALSE, color = "salmon") + 
        xlab(z) + ylab(t) + theme_minimal(base_family = "serif") + 
        xlim(min(metrics_table %>% pull(var = matches(z))),max(metrics_table %>% pull(var = matches(z)))) +
        ylim(min(metrics_table %>% pull(var = matches(t))),max(metrics_table %>% pull(var = matches(t)))) #+
      # annotate(geom="text", 
      #                 x=max(metrics_table %>% pull(var = matches(z)))*0.1, 
      #                 y=max(metrics_table %>% pull(var = matches(t)))*0.95, 
      #                 label=paste0("τ= ", cor(metrics_table %>%  select(matches(z)),  metrics_table %>%  select(matches(t)), method = method) %>% round(.,3)), color="salmon", hjust = 0) 
    })
  })
  
  names(deg) <- colnames(metrics_table)
  ggarrange(plotlist = do.call(what = base::c, deg), align = "hv") %>% annotate_figure(.,top = text_grob(plot.title, face = "bold", size = title.size, family = "serif"))
}

