require("igraph")

gt <- graph.tree(24, children = 4, mode=c("out", "in", "undirected"))
V(gt)$name <- letters[1:24]
plot(gt)   # So 'name's get displayed if no label is present
V(gt)$label <- LETTERS[1:24]
plot(gt)    # Labels get displayed
V(gt)$name <- letters[1:24]  # see if then get overwritten 
plot(gt)    # Still plots with 'label's


V(gt)$label = 1:24
plot(gt)