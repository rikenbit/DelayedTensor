.sp_to_array <- function(hoge){
    out <- array(0, dim=dim(hoge))
    idx <- expand.grid(
    seq(dim(hoge)[1]),
    seq(dim(hoge)[2]),
    seq(dim(hoge)[3]))
    count <- 1
    for(i in seq_len(nrow(idx))){
        x <- idx[i, 1]
        y <- idx[i, 2]
        z <- idx[i, 3]
        out[x, y, z] <- as.vector(hoge)[count]
        count <- count + 1
    }
    if(length(which(dim(hoge) == 1)) >= 2){
        as.vector(out)
    }else{
        out
    }
}
