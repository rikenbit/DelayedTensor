# Setting
setVerbose(TRUE)
setAutoBlockSize(size=5E+8)
.Machine$integer.max = 10^12

load("../data/human_mid_brain.rda")
load("../data/mouse_mid_brain.rda")
darr_human_mini <- DelayedArray(as.array(human_mid_brain))
darr_mouse_mini <- DelayedArray(as.array(mouse_mid_brain))
# darr_human_mini <- DelayedArray(as.array(human_mid_brain[1:20, 1:20]))
# darr_mouse_mini <- DelayedArray(as.array(mouse_mid_brain[1:20, 1:20]))

#################################################################
# einsum
#################################################################
#                      total.time total.pct self.time self.pct
# "einsum"                   0.16     100.0      0.00      0.0
# "initialize"               0.12      75.0      0.00      0.0
# "new"                      0.12      75.0      0.00      0.0
# "validObject"              0.10      62.5      0.00      0.0
# "as"                       0.08      50.0      0.00      0.0
# "asMethod"                 0.08      50.0      0.00      0.0
# "DelayedArray"             0.08      50.0      0.00      0.0
# "HDF5ArraySeed"            0.08      50.0      0.00      0.0
# "new2"                     0.08      50.0      0.00      0.0
print("einsum")
setSparse(FALSE)
Rprof()
darr <- einsum('ij,ik->ijk', darr_human_mini, darr_mouse_mini)
Rprof(NULL)
prof_einsum <- summaryRprof()
save(prof_einsum, file="prof_einsum.RData")

#################################################################
# vec (Dense)
#################################################################
#                        total.time total.pct self.time self.pct
# ".vec"                       0.10     83.33      0.00     0.00
# "vec"                        0.10     83.33      0.00     0.00
# "initialize"                 0.06     50.00      0.02    16.67
# "new"                        0.06     50.00      0.00     0.00
print("vec (Dense)")
setSparse(FALSE)
Rprof()
vec(darr)
Rprof(NULL)
prof_vec_dense <- summaryRprof()
save(prof_vec_dense, file="prof_vec_dense.RData")

#################################################################
# vec (Sparse)
#################################################################
#                  total.time total.pct self.time self.pct
# ".vec"                 0.10       100      0.00        0
# "vec"                  0.10       100      0.00        0
# "as"                   0.06        60      0.02       20
# "initialize"           0.06        60      0.00        0
# "new"                  0.06        60      0.00        0
# "new2"                 0.06        60      0.00        0
print("vec (Sparse)")
setSparse(TRUE)
Rprof()
v <- vec(darr)
Rprof(NULL)
prof_vec_sparse <- summaryRprof()
save(prof_vec_sparse, file="prof_vec_sparse.RData")

#################################################################
# unfold (Dense)
#################################################################
#                                     total.time total.pct self.time self.pct
# ".unfold"                                 0.64    100.00      0.00     0.00
# "unfold"                                  0.64    100.00      0.00     0.00
# "as"                                      0.60     93.75      0.00     0.00
# ".class1"                                 0.58     90.62      0.00     0.00
# ".realize_and_return"                     0.58     90.62      0.00     0.00
# ".vec"                                    0.58     90.62      0.00     0.00
# "realize"                                 0.58     90.62      0.00     0.00
# "vec"                                     0.58     90.62      0.00     0.00
# "[.data.frame"                            0.52     81.25      0.00     0.00
# "["                                       0.52     81.25      0.00     0.00
# "setAutoRealizationBackend"               0.52     81.25      0.00     0.00
# ".subset"                                 0.50     78.12      0.50    78.12
# ".get_realization_sink_constructor"       0.50     78.12      0.00     0.00
# "match"                                   0.50     78.12      0.00     0.00
print("unfold (Dense)")
setSparse(FALSE)
Rprof()
dmat <- unfold(darr, row_idx=1:2, col_idx=3)
Rprof(NULL)
prof_unfold_dense <- summaryRprof()
save(prof_unfold_dense, file="prof_unfold_dense.RData")

#################################################################
# unfold (Sparse)
#################################################################
#                        total.time total.pct self.time self.pct
# ".unfold"                    0.16        80      0.00        0
# "unfold"                     0.16        80      0.00        0
# "as"                         0.12        60      0.00        0
# ".class1"                    0.10        50      0.00        0
# ".realize_and_return"        0.10        50      0.00        0
# "initialize"                 0.10        50      0.00        0
# "new"                        0.10        50      0.00        0
# "realize"                    0.10        50      0.00        0
# "validObject"                0.10        50      0.00        0
print("unfold (Sparse)")
setSparse(TRUE)
Rprof()
unfold(darr, row_idx=1:2, col_idx=3)
Rprof(NULL)
prof_unfold_sparse <- summaryRprof()
save(prof_unfold_sparse, file="prof_unfold_sparse.RData")

#################################################################
# modeSum
#################################################################
#                       total.time total.pct self.time self.pct
# ".modeSum"                  0.26     81.25      0.00     0.00
# "modeSum"                   0.26     81.25      0.00     0.00
# "as"                        0.22     68.75      0.00     0.00
# "asMethod"                  0.22     68.75      0.00     0.00
# ".local"                    0.18     56.25      0.00     0.00
# "initialize"                0.16     50.00      0.04    12.50
# ".class1"                   0.16     50.00      0.00     0.00
# ".realize_and_return"       0.16     50.00      0.00     0.00
# "new"                       0.16     50.00      0.00     0.00
# "new2"                      0.16     50.00      0.00     0.00
# "realize"                   0.16     50.00      0.00     0.00
# "writeHDF5Array"            0.16     50.00      0.00     0.00
print("modeSum")
setSparse(FALSE)
Rprof()
modeSum(darr, m=2)
Rprof(NULL)
prof_modesum <- summaryRprof()
save(prof_modesum, file="prof_modesum.RData")

#################################################################
# innerProd (Dense)
#################################################################
#                         total.time total.pct self.time self.pct
# ".BLOCK_Summary"              0.04       100      0.00        0
# ".innerProd"                  0.04       100      0.00        0
# "blockReduce"                 0.04       100      0.00        0
# "callNextMethod"              0.04       100      0.00        0
# "FUN"                         0.04       100      0.00        0
# "innerProd"                   0.04       100      0.00        0
# "sum"                         0.04       100      0.00        0
print("innerProd (Dense)")
setSparse(FALSE)
Rprof()
innerProd(darr, darr)
Rprof(NULL)
prof_innerprod_dense <- summaryRprof()
save(prof_innerprod_dense, file="prof_innerprod_dense.RData")

#################################################################
# innerProd (Sparse)
#################################################################
#                  total.time total.pct self.time self.pct
# ".Call2"               0.02       100      0.02      100
# ".BLOCK_Summary"       0.02       100      0.00        0
# ".h5mread2"            0.02       100      0.00        0
# ".innerProd"           0.02       100      0.00        0
# ".local"               0.02       100      0.00        0
# ".nextMethod"          0.02       100      0.00        0
# "blockReduce"          0.02       100      0.00        0
# "callNextMethod"       0.02       100      0.00        0
# "extract_array"        0.02       100      0.00        0
# "FUN"                  0.02       100      0.00        0
# "gridReduce"           0.02       100      0.00        0
# "h5mread"              0.02       100      0.00        0
# "innerProd"            0.02       100      0.00        0
# "lapply"               0.02       100      0.00        0
# "read_block"           0.02       100      0.00        0
# "sum"                  0.02       100      0.00        0
print("innerProd (Sparse)")
setSparse(TRUE)
Rprof()
innerProd(darr, darr)
Rprof(NULL)
prof_innerprod_sparse <- summaryRprof()
save(prof_innerprod_sparse, file="prof_innerprod_sparse.RData")

#################################################################
# hadamard (Dense)
#################################################################
#                       total.time total.pct self.time self.pct
# ".hadamard"                 0.10     55.56      0.00     0.00
# "<Anonymous>"               0.10     55.56      0.00     0.00
# "hadamard"                  0.10     55.56      0.00     0.00
# "new2"                      0.08     44.44      0.02    11.11
# "show_compact_array"        0.08     44.44      0.00     0.00
print("hadamard (Dense)")
setSparse(FALSE)
Rprof()
hadamard(darr, darr)
Rprof(NULL)
prof_hadamard_dense <- summaryRprof()
save(prof_hadamard_dense, file="prof_hadamard_dense.RData")

#################################################################
# hadamard (Sparse)
#################################################################
#                       total.time total.pct self.time self.pct
# ".hadamard"                 0.24     80.00      0.00     0.00
# "hadamard"                  0.24     80.00      0.00     0.00
# "FUN"                       0.22     73.33      0.00     0.00
# "gridReduce"                0.22     73.33      0.00     0.00
# ".block_hadamard"           0.20     66.67      0.00     0.00
print("hadamard (Sparse)")
setSparse(TRUE)
Rprof()
hadamard(darr, darr)
Rprof(NULL)
prof_hadamard_sparse <- summaryRprof()
save(prof_hadamard_sparse, file="prof_hadamard_sparse.RData")

#################################################################
# kronecker (Dense)
#################################################################
#                        total.time total.pct self.time self.pct
# ".kronecker"                12.96     98.33      0.00     0.00
# "kronecker"                 12.96     98.33      0.00     0.00
# "write_block"                7.70     58.42      0.00     0.00
# ".Call"                      6.86     52.05      6.86    52.05
# "doTryCatch"                 6.86     52.05      0.00     0.00
# "H5Dwrite"                   6.86     52.05      0.00     0.00
# "h5write.default"            6.86     52.05      0.00     0.00
# "h5write"                    6.86     52.05      0.00     0.00
# "h5writeDataset.array"       6.86     52.05      0.00     0.00
# "h5writeDataset"             6.86     52.05      0.00     0.00
# "h5writeDatasetHelper"       6.86     52.05      0.00     0.00
# "try"                        6.86     52.05      0.00     0.00
# "tryCatch"                   6.86     52.05      0.00     0.00
# "tryCatchList"               6.86     52.05      0.00     0.00
# "tryCatchOne"                6.86     52.05      0.00     0.00
print("kronecker (Dense)")
setSparse(FALSE)
Rprof()
kronecker(darr, darr)
Rprof(NULL)
prof_kronecker_dense <- summaryRprof()
save(prof_kronecker_dense, file="prof_kronecker_dense.RData")

#################################################################
# kronecker (Sparse)
#################################################################
#                        total.time total.pct self.time self.pct
# ".kronecker"                10.76     97.82      0.00     0.00
# "kronecker"                 10.76     97.82      0.00     0.00
# "write_block"                8.80     80.00      0.00     0.00
# "doTryCatch"                 6.86     62.36      0.02     0.18
# "try"                        6.86     62.36      0.00     0.00
# "tryCatch"                   6.86     62.36      0.00     0.00
# "tryCatchList"               6.86     62.36      0.00     0.00
# "tryCatchOne"                6.86     62.36      0.00     0.00
# ".Call"                      6.82     62.00      6.82    62.00
# "H5Dwrite"                   6.82     62.00      0.00     0.00
# "h5write.default"            6.82     62.00      0.00     0.00
# "h5write"                    6.82     62.00      0.00     0.00
# "h5writeDataset.array"       6.82     62.00      0.00     0.00
# "h5writeDataset"             6.82     62.00      0.00     0.00
# "h5writeDatasetHelper"       6.82     62.00      0.00     0.00
print("kronecker (Sparse)")
setSparse(TRUE)
Rprof()
kronecker(darr, darr)
Rprof(NULL)
prof_kronecker_sparse <- summaryRprof()
save(prof_kronecker_sparse, file="prof_kronecker_sparse.RData")

#################################################################
# khatri_rao (Dense)
#################################################################
#                        total.time total.pct self.time self.pct
# "khatri_rao"                 0.14      87.5      0.00      0.0
# ".khatri_rao"                0.12      75.0      0.00      0.0
# "[["                         0.06      37.5      0.00      0.0
# "getArrayElement"            0.06      37.5      0.00      0.0
print("khatri_rao (Dense)")
setSparse(FALSE)
Rprof()
khatri_rao(darr[,,1], darr[,,1])
Rprof(NULL)
prof_khatri_rao_dense <- summaryRprof()
save(prof_khatri_rao_dense, file="prof_khatri_rao_dense.RData")

#################################################################
# khatri_rao (Sparse)
#################################################################
#                  total.time total.pct self.time self.pct
# ".khatri_rao"          0.14      87.5      0.00        0
# "khatri_rao"           0.14      87.5      0.00        0
# "callNextMethod"       0.08      50.0      0.00        0
print("khatri_rao (Sparse)")
setSparse(TRUE)
Rprof()
khatri_rao(darr[,,1], darr[,,1])
Rprof(NULL)
prof_khatri_rao_sparse <- summaryRprof()
save(prof_khatri_rao_sparse, file="prof_khatri_rao_sparse.RData")

#################################################################
# fold (Dense)
#################################################################
#                        total.time total.pct self.time self.pct
# ".fold"                      0.18     75.00      0.00     0.00
# ".reshapeIncNumbers1D"       0.18     75.00      0.00     0.00
# "fold"                       0.18     75.00      0.00     0.00
# "new"                        0.12     50.00      0.00     0.00
print("fold (Dense)")
setSparse(FALSE)
Rprof()
fold(dmat, row_idx=1:2, col_idx=3, dim(darr))
Rprof(NULL)
prof_fold_dense <- summaryRprof()
save(prof_fold_dense, file="prof_fold_dense.RData")

#################################################################
# fold (Sparse)
#################################################################
#                        total.time total.pct self.time self.pct
# ".fold"                      0.18     75.00      0.00     0.00
# ".reshapeIncNumbers1D"       0.18     75.00      0.00     0.00
# "fold"                       0.18     75.00      0.00     0.00
# "initialize"                 0.14     58.33      0.04    16.67
# "new"                        0.14     58.33      0.00     0.00
print("fold (Sparse)")
setSparse(TRUE)
Rprof()
fold(dmat, row_idx=1:2, col_idx=3, dim(darr))
Rprof(NULL)
prof_fold_sparse <- summaryRprof()
save(prof_fold_sparse, file="prof_fold_sparse.RData")

#################################################################
# diag
#################################################################
#                        total.time total.pct self.time self.pct
# ".diag"                      0.18        90      0.00        0
# "["                          0.18        90      0.00        0
# "DelayedTensor::diag"        0.18        90      0.00        0
# "eval"                       0.18        90      0.00        0
# "drop"                       0.10        50      0.00        0
# "standardGeneric"            0.10        50      0.00        0
print("diag")
Rprof()
DelayedTensor::diag(darr)
Rprof(NULL)
prof_diag1 <- summaryRprof()
save(prof_diag1, file="prof_diag1.RData")

#################################################################
# "diag<-"
#################################################################
#                          total.time total.pct self.time self.pct
# "DelayedTensor::diag<-"        0.26    100.00      0.00     0.00
# "[<-"                          0.18     69.23      0.00     0.00
# "eval"                         0.18     69.23      0.00     0.00
print("diag<-")
Rprof()
DelayedTensor::diag(darr) <- seq(min(dim(darr)))
Rprof(NULL)
prof_diag2 <- summaryRprof()
save(prof_diag2, file="prof_diag2.RData")

#################################################################
# modebind_list (Dense)
#################################################################
#                   total.time total.pct self.time self.pct
# "modebind_list"         0.34        85      0.00        0
# "new2"                  0.22        55      0.02        5
# "FUN"                   0.22        55      0.00        0
# "initialize"            0.22        55      0.00        0
# "new"                   0.22        55      0.00        0
# "validObject"           0.22        55      0.00        0
print("modebind_list (Dense)")
setSparse(FALSE)
Rprof()
modebind_list(list(darr, darr), m=1)
Rprof(NULL)
prof_modebind_list_dense <- summaryRprof()
save(prof_modebind_list_dense, file="prof_modebind_list_dense.RData")

#################################################################
# modebind_list (Sparse)
#################################################################
#                  total.time total.pct self.time self.pct
# "modebind_list"        0.32     84.21      0.00     0.00
# "FUN"                  0.24     63.16      0.00     0.00
# "lapply"               0.22     57.89      0.00     0.00
# "initialize"           0.20     52.63      0.04    10.53
# "as"                   0.20     52.63      0.00     0.00
# "asMethod"             0.20     52.63      0.00     0.00
# "new"                  0.20     52.63      0.00     0.00
print("modebind_list (Sparse)")
setSparse(TRUE)
Rprof()
modebind_list(list(darr, darr), m=1)
Rprof(NULL)
prof_modebind_list_sparse <- summaryRprof()
save(prof_modebind_list_sparse, file="prof_modebind_list_sparse.RData")

#################################################################
# DelayedDiagonalArray
#################################################################
#                               total.time total.pct self.time self.pct
# ".print_2D_slices"                  0.04       100      0.00        0
# ".print_array_data"                 0.04       100      0.00        0
# ".print_nDarray_data"               0.04       100      0.00        0
# "<Anonymous>"                       0.04       100      0.00        0
# "show_compact_array"                0.04       100      0.00        0
print("DelayedDiagonalArray")
Rprof()
DelayedDiagonalArray(c(100,200,300), 1:100)
Rprof(NULL)
prof_delayeddiagonalarray <- summaryRprof()
save(prof_delayeddiagonalarray, file="prof_delayeddiagonalarray.RData")

#################################################################
# hosvd (Dense)
#################################################################
#                       total.time total.pct self.time self.pct
# ".hosvd"                   10.92     98.73      0.00     0.00
# "hosvd"                    10.92     98.73      0.00     0.00
# "DelayedArray"              9.66     87.34      0.00     0.00
# "<Anonymous>"               9.04     81.74      0.00     0.00
# "do.call"                   8.90     80.47      0.02     0.18
# "suppressWarnings"          8.62     77.94      0.02     0.18
# "withCallingHandlers"       8.62     77.94      0.02     0.18
# ".svd"                      8.62     77.94      0.00     0.00
# "runIrlbaSVD"               8.62     77.94      0.00     0.00
# ".super_BLOCK_mult"         8.60     77.76      0.00     0.00
# "mult"                      8.22     74.32      0.00     0.00
# "as"                        8.00     72.33      0.00     0.00
# "standardGeneric"           7.82     70.71      0.12     1.08
# "realize"                   7.66     69.26      0.00     0.00
# "drop"                      7.56     68.35      0.00     0.00
# "asMethod"                  7.40     66.91      0.00     0.00
# ".local"                    7.28     65.82      0.00     0.00
# "writeHDF5Array"            6.62     59.86      0.00     0.00
print("hosvd (Dense)")
setSparse(FALSE)
Rprof()
hosvd(darr, ranks=c(2,3,4))
Rprof(NULL)
prof_hosvd_dense <- summaryRprof()
save(prof_hosvd_dense, file="prof_hosvd_dense.RData")

#################################################################
# hosvd (Sparse)
#################################################################
#                        total.time total.pct self.time self.pct
# ".hosvd"                     2.28     94.21      0.00     0.00
# "hosvd"                      2.28     94.21      0.00     0.00
# "ttl"                        1.88     77.69      0.00     0.00
# "as"                         1.68     69.42      0.00     0.00
# "new"                        1.38     57.02      0.06     2.48
# "initialize"                 1.34     55.37      0.18     7.44
# "asMethod"                   1.26     52.07      0.00     0.00
print("hosvd (Sparse)")
setSparse(TRUE)
Rprof()
hosvd(darr, ranks=c(2,3,4))
Rprof(NULL)
prof_hosvd_sparse <- summaryRprof()
save(prof_hosvd_sparse, file="prof_hosvd_sparse.RData")

#################################################################
# cp (Dense)
#################################################################
#                        total.time total.pct self.time self.pct
# ".cp"                        5.52     97.87      0.00     0.00
# "cp"                         5.52     97.87      0.00     0.00
# "as"                         4.04     71.63      0.02     0.35
# "ttl"                        3.24     57.45      0.00     0.00
# "new"                        3.00     53.19      0.02     0.35
# "initialize"                 2.96     52.48      0.42     7.45
print("cp (Dense)")
setSparse(FALSE)
Rprof()
cp(darr, num_components=2, max_iter=2)
Rprof(NULL)
prof_cp_dense <- summaryRprof()
save(prof_cp_dense, file="prof_cp_dense.RData")

#################################################################
# cp (Sparse)
#################################################################
#                        total.time total.pct self.time self.pct
# ".cp"                        4.76     97.94      0.00     0.00
# "cp"                         4.76     97.94      0.00     0.00
# "as"                         3.28     67.49      0.00     0.00
# "ttl"                        2.86     58.85      0.00     0.00
setSparse(TRUE)
Rprof()
cp(darr, num_components=2, max_iter=2)
Rprof(NULL)
prof_cp_sparse <- summaryRprof()
save(prof_cp_sparse, file="prof_cp_sparse.RData")

#################################################################
# tucker (Dense)
#################################################################
#                       total.time total.pct self.time self.pct
# ".tucker"                  12.82     99.07      0.00     0.00
# "tucker"                   12.82     99.07      0.00     0.00
# "DelayedArray"             10.20     78.83      0.02     0.15
# "as"                        9.36     72.33      0.02     0.15
# "<Anonymous>"               8.54     66.00      0.04     0.31
# ".super_BLOCK_mult"         8.44     65.22      0.00     0.00
# "do.call"                   8.40     64.91      0.00     0.00
# ".svd"                      8.34     64.45      0.00     0.00
# "realize"                   8.32     64.30      0.00     0.00
# "asMethod"                  8.26     63.83      0.00     0.00
# "withCallingHandlers"       8.02     61.98      0.02     0.15
# "suppressWarnings"          8.02     61.98      0.00     0.00
# "runIrlbaSVD"               8.00     61.82      0.00     0.00
# "standardGeneric"           7.88     60.90      0.12     0.93
# "mult"                      7.68     59.35      0.00     0.00
# ".local"                    7.42     57.34      0.00     0.00
# "drop"                      7.38     57.03      0.00     0.00
# "writeHDF5Array"            6.78     52.40      0.00     0.00
print("tucker (Dense)")
setSparse(FALSE)
Rprof()
tucker(darr, ranks=c(2,2,2), max_iter=2)
Rprof(NULL)
prof_tucker_dense <- summaryRprof()
save(prof_tucker_dense, file="prof_tucker_dense.RData")

#################################################################
# tucker (Sparse)
#################################################################
#                        total.time total.pct self.time self.pct
# ".tucker"                    4.84     97.58      0.00     0.00
# "tucker"                     4.84     97.58      0.00     0.00
# "ttl"                        3.74     75.40      0.00     0.00
# "as"                         3.58     72.18      0.04     0.81
# "asMethod"                   2.60     52.42      0.00     0.00
# "new"                        2.56     51.61      0.12     2.42
# "initialize"                 2.50     50.40      0.32     6.45
print("tucker (Sparse)")
setSparse(TRUE)
Rprof()
tucker(darr, ranks=c(2,2,2), max_iter=2)
Rprof(NULL)
prof_tucker_sparse <- summaryRprof()
save(prof_tucker_sparse, file="prof_tucker_sparse.RData")

#################################################################
# mpca (Dense)
#################################################################
#                       total.time total.pct self.time self.pct
# ".mpca"                    13.64     98.70      0.00     0.00
# "mpca"                     13.64     98.70      0.00     0.00
# "DelayedArray"             11.40     82.49      0.00     0.00
# "<Anonymous>"              10.72     77.57      0.02     0.14
# "do.call"                  10.54     76.27      0.00     0.00
# ".super_BLOCK_mult"        10.14     73.37      0.02     0.14
# ".svd"                     10.04     72.65      0.00     0.00
# "as"                        9.96     72.07      0.06     0.43
# "suppressWarnings"          9.82     71.06      0.02     0.14
# "runIrlbaSVD"               9.82     71.06      0.00     0.00
# "withCallingHandlers"       9.82     71.06      0.00     0.00
# "standardGeneric"           9.48     68.60      0.18     1.30
# "realize"                   9.44     68.31      0.00     0.00
# "mult"                      9.40     68.02      0.02     0.14
# "asMethod"                  9.28     67.15      0.02     0.14
# "drop"                      9.10     65.85      0.00     0.00
# ".local"                    8.84     63.97      0.04     0.29
# "writeHDF5Array"            8.20     59.33      0.00     0.00
print("mpca (Dense)")
setSparse(FALSE)
Rprof()
mpca(darr, ranks=c(2,2), max_iter=2)
Rprof(NULL)
prof_mpca_dense <- summaryRprof()
save(prof_mpca_dense, file="prof_mpca_dense.RData")

#################################################################
# mpca (Sparse)
#################################################################
#                        total.time total.pct self.time self.pct
# ".mpca"                      3.70     95.36      0.00     0.00
# "mpca"                       3.70     95.36      0.00     0.00
# "as"                         2.60     67.01      0.00     0.00
# "ttl"                        2.58     66.49      0.00     0.00
# "new"                        2.12     54.64      0.00     0.00
# "initialize"                 2.10     54.12      0.42    10.82
# "asMethod"                   1.86     47.94      0.00     0.00
# ".reshapeIncNumbers1D"       1.74     44.85      0.00     0.00
# "DelayedArray"               1.74     44.85      0.00     0.00
# "realize"                    1.74     44.85      0.00     0.00
print("mpca (Sparse)")
setSparse(TRUE)
Rprof()
mpca(darr, ranks=c(2,2), max_iter=2)
Rprof(NULL)
prof_mpca_sparse <- summaryRprof()
save(prof_mpca_sparse, file="prof_mpca_sparse.RData")

#################################################################
# pvd (Dense)
#################################################################
print("pvd (Dense)")
setSparse(FALSE)
Rprof()
pvd(darr, uranks=rep(2, dim(darr)[3]), wranks=rep(3, dim(darr)[3]), a=2, b=3)
Rprof(NULL)
prof_pvd_dense <- summaryRprof()
save(prof_pvd_dense, file="prof_pvd_dense.RData")

#################################################################
# pvd (Sparse)
#################################################################
#                       total.time total.pct self.time self.pct
# ".pvd"                     15.64     98.24      0.00     0.00
# "pvd"                      15.64     98.24      0.00     0.00
# "FUN"                      13.30     83.54      0.08     0.50
# "lapply"                   13.28     83.42      0.08     0.50
# "as"                       13.10     82.29      0.12     0.75
# ".super_BLOCK_mult"         9.76     61.31      0.00     0.00
# "%*%"                       9.76     61.31      0.00     0.00
# ".class1"                   9.66     60.68      0.00     0.00
# "realize"                   9.50     59.67      0.00     0.00
# "asMethod"                  9.30     58.42      0.04     0.25
# "bplapply2"                 9.16     57.54      0.00     0.00
print("pvd (Sparse)")
setSparse(TRUE)
Rprof()
pvd(darr, uranks=rep(2, dim(darr)[3]), wranks=rep(3, dim(darr)[3]), a=2, b=3)
Rprof(NULL)
prof_pvd_sparse <- summaryRprof()
save(prof_pvd_sparse, file="prof_pvd_sparse.RData")

# Sparse mode is effective in the most cases
# > prof_vec_dense$sampling.time
# [1] 0.12
# > prof_vec_sparse$sampling.time
# [1] 0.1

# > prof_unfold_dense$sampling.time
# [1] 0.64
# > prof_unfold_sparse$sampling.time
# [1] 0.2

# > prof_innerprod_dense$sampling.time
# [1] 0.04
# > prof_innerprod_sparse$sampling.time
# [1] 0.02

# > prof_hadamard_dense$sampling.time
# [1] 0.18
# > prof_hadamard_sparse$sampling.time
# [1] 0.3

# > prof_kronecker_dense$sampling.time
# [1] 13.18
# > prof_kronecker_sparse$sampling.time
# [1] 11

# > prof_khatri_rao_dense$sampling.time
# [1] 0.16
# > prof_khatri_rao_sparse$sampling.time
# [1] 0.16

# > prof_fold_dense$sampling.time
# [1] 0.24
# > prof_fold_sparse$sampling.time
# [1] 0.24

# > prof_modebind_list_dense$sampling.time
# [1] 0.4
# > prof_modebind_list_sparse$sampling.time
# [1] 0.38

# > prof_hosvd_dense$sampling.time
# [1] 11.06
# > prof_hosvd_sparse$sampling.time
# [1] 2.42

# > prof_cp_dense$sampling.time
# [1] 5.64
# > prof_cp_sparse$sampling.time
# [1] 4.86

# > prof_tucker_dense$sampling.time
# [1] 12.94
# > prof_tucker_sparse$sampling.time
# [1] 4.96

# > prof_mpca_dense$sampling.time
# [1] 13.82
# > prof_mpca_sparse$sampling.time
# [1] 3.88

# > prof_pvd_dense$sampling.time
# [1] 0
# > prof_pvd_sparse$sampling.time
# [1] 15.92
