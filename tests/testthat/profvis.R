library("profvis")
library("einsum")
library("DelayedRandomArray")
library("DelayedArray")
library("DelayedTensor")

options(delayedtensor.verbose = TRUE)
setSparse(FALSE)
setAutoBlockSize(size=1E+7)
setHDF5DumpCompressionLevel(level=0L)
getAutoBlockSize()
getSparse()
getVerbose()

# data
darr_1E7 <- RandomNormArray(c(170,200,300))
darr_5E7 <- RandomNormArray(c(170,200,1500))
darr_1E8 <- RandomNormArray(c(400,500,500))
darr_5E8 <- RandomNormArray(c(400,500,2500))
darr_5E8_1 <- RandomNormArray(c(100,100))
darr_5E8_2 <- RandomNormArray(c(100,500))
darr_1E9_1 <- RandomNormArray(c(1000,100))
darr_1E9_2 <- RandomNormArray(c(100,100))

# unfold（1E8）
# .Callが遅いが、これが何をしているのか不明
profvis({
    out <- DelayedTensor::unfold(darr_1E8, row_idx=1, col_idx=c(2,3))
})

# fold（1E8）
# .Callが遅いが、これが何をしているのか不明
profvis({
    fold(out, row_idx=1, col_idx=c(2,3), modes=dim(darr_1E8))
})

# vec（1E8）
# .Callが遅いが、これが何をしているのか不明
profvis({
    DelayedTensor::vec(darr_1E8)
})

# innerProd（5E8）
# sum(darr*darr)を、あえて陽にBlock Processingで書いて早くなるかどうか...
profvis({
    DelayedTensor::innerProd(darr_5E8, darr_5E8)
})

# modeSum（5E7）
# 細かいピークがいっぱい出ているが...
profvis({
    DelayedTensor::modeSum(darr_5E7, m=1)
})

# kronecker（5E8）
# .Callが遅いが、これが何をしているのか不明
profvis({
    DelayedTensor::kronecker(darr_5E8_1, darr_5E8_2)
})

# einsum（5E8）
# .Callが遅いが、これが何をしているのか不明
profvis({
    DelayedTensor::einsum('ij,jk->ijk', darr_5E8_1, darr_5E8_2)
})
profvis({
    einsum::einsum('ij,jk->ijk', as.array(darr_5E8_1), as.array(darr_5E8_2))
})
