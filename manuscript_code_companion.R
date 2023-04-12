nodes <- data.frame(name=paste0("s", 0:10), x=c(0,4,7,4,8,4,10,8,10,9,11), y=c(11,11,14,11,15,11,11,3,9,2,0))

plot(0, 0, type="n", xlab="x", ylab="y", xlim=c(0, 16), ylim=c(0, 16), xaxs="i", yaxs="i")
grid(nx = 16, ny = 16, lty = 1, col = "lightgray")

# Add the nodes as points and connect them with lines
points(nodes$x, nodes$y, type="b", pch=16, col="blue")
text(nodes$x, nodes$y+0.2, labels=nodes$name, pos=3)
abline(a=11, b=-1, col="red")

# Calculate the distances between each node and the diagonal line
distances <- abs((nodes$x + nodes$y - 11)/sqrt(2))
print(distances)
nodes$exploration <- distances
