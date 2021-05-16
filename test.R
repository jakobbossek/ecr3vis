library(tidyverse)

devtools::load_all()

tbl = read.table("inst/sampledf.csv", header = TRUE)

tbl2 = filter(tbl, repl == 1L, algorithm == "nsga2")[1:3, ]
tbl2 = tbl2[order(tbl2$y1), ]
tbl2$y3 = tbl2$y2


#print(plot_pcp(tbl2, obj.cols = obj.cols))
#print(plot_heatmap(tbl2, obj.cols = sample(obj.cols)))
tbl2 = data.frame(algorithm = rep("nsga2", 3L), y1 = c(1, 0.5, 2), y2 = c(1, 0.5, 1), y3 = c(2, 2, 0.5), y4 = c(2, 3, 2), y4 = c(0.5, 3, 2))
g = plot_radar(tbl2, obj.cols = paste0("y", 1:5))
g + facet_grid(nr ~ .)
