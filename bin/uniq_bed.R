#!/usr/bin/Rscript --slave --vanilla

library("GenomicRanges")

input <- commandArgs(trailingOnly=TRUE)[1]
output <- commandArgs(trailingOnly=TRUE)[2]

df <- read.table(input, header=F)
gr <- GRanges(seqname = Rle(df[,1]), ranges = IRanges(start = df[,2], end = df[,3]))
red <- reduce(gr)
dis <- disjoin(gr)
res <- dis[countOverlaps(dis,gr)==1]

write.table(file=output, as.data.frame(red)[,1:3], quote=F, sep="\t", row.names=F, col.names=F)
#write.table(file=output, as.data.frame(dis)[,1:3], quote=F, sep="\t", row.names=F, col.names=F)
#write.table(file=output, as.data.frame(res)[,1:3], quote=F, sep="\t", row.names=F, col.names=F)
