d = iris

d$.err = rep(F, 150)
d[70, ".err"] = T


d$.err = !d$.err

d$col = paste(d$.err, d$Species, sep = ": ")

d$color = sample(1:8, 150, replace = T)
d$color = as.factor(d$color)

cols = c("red", "yellow", "blue")
cols = RColorBrewer::brewer.pal(nlevels(d$Species) + 1, "Greys")[-1]

cols
d$cols = as.character(factor(as.numeric(d$Species), labels = cols))

d[d$.err == T, "cols"] = "black"

d$.size = 10

d$.alpha = 0.4
d[d$.err == T, ".alpha"] = 1

plot_ly(data = d, x = Sepal.Length, y = Sepal.Width, z = Petal.Length,
        type = "scatter3d", mode = "markers", 
        symbol = Species, 
        marker = list(size = 8, color = cols, labels = Species))
