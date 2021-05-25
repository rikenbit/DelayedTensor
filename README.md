# DelayedTensor
Block-processed version of rTensor functions to perform tensor arithmetic and decomposition.
All the calculations are actually performed and not registered as delayed operartion.
DelayedTensor provides some generic function related to Tensor arithmetic/decompotision and dispatches it on the DelayedArray class.
DelayedTensor also suppors Tensor contraction by einsum function, which is inspired by numpy einsum.

コンセプト
- arrayやrTensorと使い心地を変えずに、DelayedArrayでテンソル演算・分解をする
- 中間生成物はHDF5Arrayに固定（writeHDF5Arrayやasでrealizeして書き込んでしまう、verboseでshowHDF5DumpLogmode()）
- ブロックプロセスで計算する、これはDelayed Operationではない（全てリセットされたDelayedArrayが返ってくるイメージ）
- 1つのオブジェクトにおいて、getAutoBlockSize()のブロックサイズを超えないようにする

TO DO list
======
**1. AllGenerics.R**
  - ✅/✅ unfold（reshape & realize系）
  - ✅/✅ k_unfold（unfoldベース、reshape & realize系）
  - ✅/✅ matvec（unfoldベース、reshape & realize系）
  - ✅/✅ rs_unfold（unfoldベース、reshape & realize系）
  - ✅/✅ cs_unfold（unfoldベース、reshape & realize系）
  - ✅/✅ modeSum（aperm系 & realize系）
  - ✅/✅ modeMean（modeSumベース、aperm & realize系）
  - ✅/✅ fnorm（innerProdベース、\* & realize系）
  - ✅/✅ innerProd（\* & realize系）
  - ✅/✅ vec（reshape & realize系）
  - ✅/✅ **hadamard（自力でBlock Processing）**
  - ✅/✅ **kronecker（自力でBlock Processing）**
  - ✅/✅ **khatri_rao（自力でBlock Processing）**
  - ✅/✅ **fold（自力でBlock Processing）**
  - ✅/✅ k_fold（foldベース）
  - ✅/✅ unmatvec（foldベース）
  - ✅/✅ rs_fold（foldベース）
  - ✅/✅ cs_fold（foldベース）
  - ✅/✅ ttm（foldベース）
  - ✅/✅ diag（普通に代入 & realize系）
  - ✅/✅ diag<-（普通に代入 & realize系）

**2. Misc.R**
  - ✅/✅ list_rep
  - ✅/✅ **modebind_list（自力でBlock Processing）**
  - ✅/✅ cbind_list（modebind_listベース）
  - ✅/✅ rbind_list（modebind_listベース）
  - ✅/✅ hadamard_list（hadamardベース）**
  - ✅/✅ kronecker_list（kronerckerベース）
  - ✅/✅ khatri_rao_list（khatri_raoベース）
  - ✅/✅ ttl（unfoldベース）
  - ✅/✅ DelayedDiagonalArray（普通に代入 & realize系）

**3. SVD.R**
  - ✅ .svd

**4. Decomp.R**
  - ✅/✅ hosvd
  - ✅/✅ cp
  - ✅/✅ tucker
  - ✅/✅ mpca
  - ✅/✅ pvd
  - ✅ .is_zero_tensor

**5. Einsum.R**
  - /✅ einsum
      - 左側がコピー、右側が係数（cf. kronecker）
      - ->は必須（オリジナルのeinsumと合わせる）
      - Delayed Operation -> .realize_and_return
      - ブロックごとに.block_einsumを適用

**その他**
  - man（最後にeinsumを追加、色々こけている）
  - README.md
  - ✅ DESCRIPTION
  - ✅ NAMESPACE（最後にeinsumを追加）
  - vignette（最後にtoc_float: trueを追加）
  - ✅ R CMD INSTALL DelayedTensor
  - ✅ R CMD build --keep-empty-dirs --no-resave-data DelayedTensor
  - R CMD check --no-vignettes --timings DelayedTensor
  - Rscript -e "BiocCheck::BiocCheck('DelayedTensor_0.99.0.tar.gz')"
  - Bioconductor/Contributions

Installation of Dependent Packages
======
```r
# CRAN
install.packages(c("XXXXX"),
    repos="http://cran.r-project.org")

# Bioconductor
library("BiocManager")
BiocManager::install(c("XXXXX"),
    suppressUpdates=TRUE)
```

Installation
======
```r
git clone https://github.com/rikenbit/DelayedTensor/
R CMD INSTALL DelayedTensor
```
or type the code below in the R console window
```r
install.packages("devtools", repos="http://cran.r-project.org")
library(devtools)
devtools::install_github("rikenbit/DelayedTensor")
```

## License
Copyright (c) 2021 Koki Tsuyuzaki and Laboratory for Bioinformatics Research, RIKEN Center for Biosystems Dynamics Reseach
Released under the [Artistic License 2.0](http://www.perlfoundation.org/artistic_license_2_0).

## Authors
- Koki Tsuyuzaki
- Itoshi Nikaido