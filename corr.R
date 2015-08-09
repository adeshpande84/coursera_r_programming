corr <- function(directory, threshold = 0) {

    specdataPath <- paste0(getwd(),"/",directory)
    x <- complete(directory,1:332)
    th <- x$nobs > threshold
    ids <- x$id[th]
    id_len <- length(ids)
    corarr <- rep(0, id_len)
    i <- 1
    for(id in ids) {
        
        if(id<10) {
            file = paste0("00",id,".csv")
        } else if (id>=10 & id<100) {
            file = paste0("0",id,".csv")
        } else {
            file = paste0(id,".csv")
        }
        
        x <- read.csv(paste0(specdataPath,"/",file))
        y <- complete.cases(x)
        cc <- x[y,]
        
        corarr[i] <- cor(cc$sulfate,cc$nitrate,use="complete.obs")
        i = i+1
    }
    
    corarr
    
}