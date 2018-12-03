
library(gRain)
yn <- c("yes","no")
a <- cptable(~asia, values=c(1,99), levels=yn)
t.a <- cptable(~tub+asia, values=c(5,95,1,99), levels=yn)
s <- cptable(~smoke, values=c(5,5), levels=yn)
l.s <- cptable(~lung+smoke, values=c(1,9,1,99), levels=yn)
b.s <- cptable(~bronc+smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e <- cptable(~xray+either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9),levels=yn)


cptable(~tub|asia, values=c(5,95,1,99), levels=yn)
cptable(c("tub","asia"), values=c(5,95,1,99), levels=yn)

e.lt <- ortable(~either+lung+tub, levels=yn)

plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
grn1 <- grain(plist)
summary(grn1)
plot(grn1)

grn1c <- compile(grn1)

summary(grn1c)
plot(grn1)

g <- grn1$dag
plot(g)
mg <- moralize(g)
plot(mg)
tmg <- triangulate(mg)
#plot triangulated moralized graph
plot(tmg)
rip(tmg)
#plot the junction tree
plot(grn1c,type="jt")

grn1c <- propagate(grn1c)
summary(grn1c)

grn1c.ev <-setFinding(grn1c,nodes=c("asia","xray"), states=c("yes","yes"))
#querying bronc=yes,lung=yes,tub=yes given evidence that asia=yes and xray=yes
querygrain(grn1c.ev,nodes=c("lung","bronc","tub"), type="joint")
#the output is 0.01063804
