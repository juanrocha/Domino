### Function to simplify CLS networks
## Juan Rocha
## juan.rocha@su.se
## J170131



## read a matrix to work with
net <- rs.net(dat, 25)

mat <- as.sociomatrix(net)
pol <- as.sociomatrix(net, attrname = "Polarity")
plotnet(net)
## The function should

z = 1

while (z > 0) {
    i <- colSums(mat) # in-degree
    o <- rowSums(mat) # out-degree

    # get indexes
    v <- (which(i == 1 & o == 1))
    z <- length(which(i == 1 & o == 1))

    # add a new link
    mat[which(mat[,v[z]] == 1 ), which(mat[v[z],] == 1 )] <- 1
    # add polairty # incoming link * outgoing link
    pol[[which(mat[,v[z]] == 1 ), which(mat[v[z],] == 1 )]] <- pol[ which(pol[,v[z]] != 0), v[z] ] *  pol[v[z] , which(pol[v[z],] != 0 )]

    # delete rows and columns in mat and pol
    mat <- mat[-c(v[z]), -c(v[z])]
    pol <- pol[-c(v[z]), -c(v[z])]

    ## reset z
    i <- colSums(mat) # in-degree
    o <- rowSums(mat) # out-degree

    # get indexes
    v <- (which(i == 1 & o == 1))
    z <- length(which(i == 1 & o == 1))
}

## visualizations

net1 <- network(mat, dir = T)
net1 %e% "Polarity" <- pol
plot.network(net1)



### Reproducible example of loops problems
                    # A,B,C,D,E,F,G
m <- matrix (data = c(0,1,0,0,1,0,0, #A
                      0,0,1,0,0,0,0, #B
                      0,0,0,1,0,0,0, #C
                      1,0,0,0,0,0,1, #D
                      0,0,0,1,0,0,0, #E
                      0,0,0,0,1,0,0, #F
                      0,0,0,0,0,1,0),#G
            nrow = 7, ncol=7, byrow =T)

n <- network(m, dir = T)

cycles <- kcycle.census(n, maxlen=network.size(n), mode='digraph',
    tabulate.by.vertex=T, cycle.comembership='bylength')

plot.network(n)
