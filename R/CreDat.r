#' @title a function to merge gff file and annotation file
#' @description  a function to merge gff file and protein domain annotation file
#' @author Hongwei Wang <\email{whweve@163.com}>
#' @export
#' @import shiny ggplot2 rintrojs dplyr data.table
#' @return a dataframe 
#' @examples
#' data(gff)
#' data(annofile)
#' mergedgff <- CreDat(gff,annofile)
CreDat <- function(gff,annofile) {
  gff <- gff
  annofile <- annofile
  gff <- gff[!is.na(gff$V9),]
  aa <- head(gff[gff$V3 == "gene",])
  trans_clean <- sub("ID=(.*).*?Name=\\1.*","\\1",aa$V9[1],perl = TRUE)
  trans_plus_version <- sub("ID=(.*?);.+","\\1",aa$V9[1],perl = TRUE)
  genome_version <- sub(trans_clean,"",trans_plus_version,perl = TRUE)
  genome_version <- sub("^[[:punct:]]","",genome_version)
  print(aa$V9[1])
  print(genome_version)
  gff$V9 <- sub(paste0("ID=(.*?)","\\.",genome_version, ".+","\\1",".*"), "\\1",gff$V9,perl=TRUE)
  #gff$V9 <- gsub("pacid=","PAC:",gff$V9,perl = TRUE)
  #gff$V9 <- sub("ID=(.*?)\\..*?(\\1).*?","\\1",gff$V9,perl = FALSE,ignore.case = TRUE)
  #gff$V9 <- sub("[[:punct:]]$","",gff$V9,perl = TRUE)
  print(head(gff))
  trans <- gff[gff$V3 %in% c("exon", "CDS", "three_prime_UTR"), ]
  trans <- trans[order(trans$V9,trans$V4),]
  transpos <- trans[trans$V7 == "+",]
  if(dim(transpos)[1] >= 1) {
    transposgroupmin <- aggregate(V4 ~ V9,data = transpos,FUN=min)
    names(transposgroupmin)[names(transposgroupmin) == "V4"] <- "V10"
    transpos <- left_join(transpos,transposgroupmin,by="V9")
    transpos$V4 <- transpos$V4-transpos$V10+1
    transpos$V5 <- transpos$V5-transpos$V10+1
  }
  transneg <- trans[trans$V7 == "-",]
  if(dim(transneg)[1] >= 1) {
    transneg <- transneg[order(transneg$V9,-transneg$V4),]
    transneggroupmax <- aggregate(V5 ~ V9,data = transneg,FUN=max)
    names(transneggroupmax)[names(transneggroupmax) == "V5"] <- "V10"
    transneg <- left_join(transneg,transneggroupmax,by="V9")
    transnegend<- abs(transneg$V4-transneg$V10+1)
    transnegstart <- abs(transneg$V5-transneg$V10+1)
    transneg$V4 <- transnegstart
    transneg$V5 <- transnegend
  }
  trans <- rbind.data.frame(transpos,transneg)
  trans$length <- abs(trans$V5-trans$V4+1)
  trans <- trans[order(trans$V9,trans$V4),]
  #trans$cumsum <- do.call(c, tapply(trans$length, trans$V9, FUN=cumsum))
  trans <- trans %>% group_by(V9) %>% mutate(cumsum=cumsum(length))
  trans$VV4 <- trans$cumsum - trans$length+1
  trans$VV5 <- trans$cumsum
  correspond <- data.frame("V2" = sub(".*?(Query_[0-9]+).*?transcript=(.*?) .*","\\1",annofile[grepl("^QUERY",annofile)]),
                           'V9' = sub(".*?(Query_[0-9]+).*?transcript=(.*?) .*","\\2",annofile[grepl("^QUERY",annofile)]))
  annofile <- annofile[grepl("^[0-9]+",annofile)]
  dat <- data.frame(V1 =  sub("(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*)","\\1",annofile),
                    V2 =  sub("(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*)","\\2",annofile),
                    from =  sub("(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*)","\\5",annofile),
                    to =  sub("(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*)","\\6",annofile),
                    domain =  sub("(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*?)\t(.*)","\\9",annofile))
  dat <- left_join(dat,correspond,by="V2")
  cds <- left_join(trans,dat,by="V9")
  cds <- cds[order(cds$V9,cds$V4),]
  cds$VVV4 <- NA
  cds$VVV5 <- NA
  cds$from <- as.numeric(as.character(cds$from))*3
  cds$to <- as.numeric(as.character(cds$to))*3
  cds$VV4 <- as.numeric(as.character(cds$VV4))
  cds$VV5 <- as.numeric(as.character(cds$VV5))
  cds <- as.data.frame(cds)
  cds$VVV4[cds$VV4 <= cds$to & cds$VV4 >= cds$from & cds$VV5 >= cds$to & !is.na(cds$from)] <- cds$VV4[cds$VV4 <= cds$to & cds$VV4 >= cds$from & cds$VV5 >= cds$to & !is.na(cds$from)]
  cds$VVV5[cds$VV4 <= cds$to & cds$VV4 >= cds$from & cds$VV5 >= cds$to & !is.na(cds$from)] <- cds$to[cds$VV4 <= cds$to & cds$VV4 >= cds$from & cds$VV5 >= cds$to & !is.na(cds$from)]
  cds$VVV4[cds$VV4 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)] <- cds$VV4[cds$VV4 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)]
  cds$VVV5[cds$VV4 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)] <- cds$VV5[cds$VV4 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)]
  cds$VVV4[cds$from >= cds$VV4 & cds$from <= cds$VV5 & cds$to >= cds$VV5 & !is.na(cds$from)] <- cds$from[cds$from >= cds$VV4 & cds$from <= cds$VV5 & cds$to >= cds$VV5 & !is.na(cds$from)]
  cds$VVV5[cds$from >= cds$VV4 & cds$from <= cds$VV5 & cds$to >= cds$VV5 & !is.na(cds$from)] <- cds$VV5[cds$from >= cds$VV4 & cds$from <= cds$VV5 & cds$to >= cds$VV5 & !is.na(cds$from)]
  cds$VVV4[cds$VV4 >= cds$from & cds$VV4 <= cds$to & cds$VV5 >= cds$to & !is.na(cds$from)] <- cds$VV4[cds$VV4 >= cds$from & cds$VV4 <= cds$to & cds$VV5 >= cds$to & !is.na(cds$from)]
  cds$VVV5[cds$VV4 >= cds$from & cds$VV4 <= cds$to & cds$VV5 >= cds$to & !is.na(cds$from)] <- cds$to[cds$VV4 >= cds$from & cds$VV4 <= cds$to & cds$VV5 >= cds$to & !is.na(cds$from)]
  cds$VVV4[cds$VV4 <= cds$from & cds$VV5 >= cds$to & !is.na(cds$from)] <- cds$from[cds$VV4 <= cds$from &cds$VV5 >= cds$to & !is.na(cds$from)]
  cds$VVV5[cds$VV4 <= cds$from & cds$VV5 >= cds$to & !is.na(cds$from)] <- cds$to[cds$VV4 <= cds$from &cds$VV5 >= cds$to & !is.na(cds$from)]
  cds$VVV4[cds$VV4 <= cds$from & cds$VV5 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)] <- cds$from[cds$VV4 <= cds$from & cds$VV5 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)]
  cds$VVV5[cds$VV4 <= cds$from & cds$VV5 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)] <- cds$VV5[cds$VV4 <= cds$from & cds$VV5 >= cds$from & cds$VV5 <= cds$to & !is.na(cds$from)]
  cds$VVVV4 <- cds$VVV4+cds$V4-cds$VV4
  cds$VVVV5 <- cds$VVV5+cds$V4-cds$VV4
  cds <- cds[,c("V9","V3","V4","V5","VV4","VV5","VVV4","VVV5","VVVV4","VVVV5","domain")]
  cds_fiveprime <- gff[gff$V3 == "five_prime_UTR",]
  if(dim(cds_fiveprime)[1]>=1) {
    cdsadd <- data.frame(V9 = cds_fiveprime$V9,
                         start = cds_fiveprime$V5 - cds_fiveprime$V4 + 1)
    cds <- left_join(cds,cdsadd,by= "V9")
    cds_fiveprime <- data.frame(V9 = cds_fiveprime$V9,
                                V3 = "five_prime_UTR",
                                V4 = 1,
                                V5 = cds_fiveprime$V5-cds_fiveprime$V4+1,
                                VV4 = 1,
                                VV5 = cds_fiveprime$V5-cds_fiveprime$V4+1,
                                VVV4 = 1,
                                VVV5 = cds_fiveprime$V5-cds_fiveprime$V4+1,
                                VVVV4 = 1,
                                VVVV5 = cds_fiveprime$V5-cds_fiveprime$V4+1)
    cds <- bind_rows(cds,cds_fiveprime)
    cds$V4[!is.na(cds$start)] <- cds$V4[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
    cds$V5[!is.na(cds$start)] <- cds$V5[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
    cds$VV4[!is.na(cds$start)] <- cds$VV4[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
    cds$VV5[!is.na(cds$start)] <- cds$VV5[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
    cds$VVV4[!is.na(cds$start)] <- cds$VVV4[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
    cds$VVV5[!is.na(cds$start)]<- cds$VVV5[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
    cds$VVVV4[!is.na(cds$start)] <- cds$VVVV4[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
    cds$VVVV5[!is.na(cds$start)] <- cds$VVVV5[!is.na(cds$start)] + cds$start[!is.na(cds$start)]
  }
  cds <- cds[,1:11]
  cds$domain[is.na(cds$VVVV5)] <- NA
  return(cds)
}
