
#d = iris[1:6, ]
d = iris

#d1 = iris[1:5,]
#d2 = iris[6, ]

d$wrong = rep(FALSE, 150)
d[70:70, "wrong"] = TRUE
d$col = c("yellow", "green", "red")[d$Species]
d$col[d$wrong] = "black"

p = plot_ly(data = d, x = Sepal.Length, y = Sepal.Width, z = Petal.Length,
        type = "scatter3d", mode = "markers", symbol = d$Species,
        marker = list(size = 2))
p      
p = add_trace(p, data = subset(d, d$wrong), x = Sepal.Length, y = Sepal.Width, z = Petal.Length,
            type = "scatter3d", mode = "markers", symbol = d$Species,
            marker = list(color = "black"), showlegend = F)

print(p)


plot_ly(d, x = subset(d, d$wrong)[, "Sepal.Length"],
        y = subset(d, d$wrong)[, "Sepal.Width"],
        z = subset(d, d$wrong)[, "Petal.Length"],
        type = "scatter3d", mode = "markers", symbol = subset(d, d$wrong)[, "Species"])

plot_ly(d, x = "Sepal.Length", y = "Sepal.Width", z = "Petal.Length",
        type = "scatter3d", mode = "markers", symbol = "Species")

x1n = "Sepal.Length"
x2n = "Sepal.Width"
x3n = "Petal.Length"
target = "Species"

nx1n = noquote(x1n)
nx2n = noquote(x2n)
nx3n = noquote(x3n)
ntarget = noquote(target)
plot_ly(d, x = nx1n, y = nx2n, z = nx3n, 
        type = "scatter3d", mode = "markers", symbol = ntarget)

plot_ly(data = subset(d, d$wrong), x = Sepal.Length, y = Sepal.Width, z = Petal.Length,
          type = "scatter3d", mode = "markers", symbol = d$Species,
          marker = list(color = "black"), showlegend = F)

plot_ly(x = subset(d, d$wrong)[, x1n],
        y = subset(d, d$wrong)[, x2n],
        z = subset(d, d$wrong)[, x3n],
        type = "scatter3d", mode = "markers", symbol = d$Species)

plot_ly(x = subset(d, !d$wrong)[, x1n],
        y = subset(d, !d$wrong)[, x2n],
        z = subset(d, !d$wrong)[, x3n],
        type = "scatter3d", mode = "markers", symbol = d$Species)

q = plot_ly(data = d, x = get(x1n), y = get(x2n), z = get(x3n),
        type = "scatter3d", mode = "markers", symbol = get(target))

add_trace(q, data = subset(d, d$wrong), x = get(x1n), y = get(x2n), z = get(x3n),
          type = "scatter3d", mode = "markers", symbol = get(target))

