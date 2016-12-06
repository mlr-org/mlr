load_all()

x = c(1:10, 6:1)
x = matrix(x, nrow = 1)
cl = c(10, 6)
print(x)
print(cl)

f = extractMultiResFeatures(x, curve.lens = cl , res.level = 7, shift = 0.5)
print(f)


