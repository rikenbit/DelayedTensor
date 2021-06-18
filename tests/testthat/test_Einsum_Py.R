np <- import('numpy')

# DelayedArray arrects

context("### einsum (transpose) ###\n")
expect_equal(t(arrD),
    np$einsum('ij->ji', arrD),
    as.array(einsum('ij->ji', darrD)))

expect_equal(aperm(arrF, c(1,3,2)),
    np$einsum('ijk->ikj', arrF),
    as.array(einsum('ijk->ikj', darrF)))
expect_equal(aperm(arrF, c(2,1,3)),
    np$einsum('ijk->jik', arrF),
    as.array(einsum('ijk->jik', darrF)))
expect_equal(aperm(arrF, c(2,3,1)),
    np$einsum('ijk->jki', arrF),
    as.array(einsum('ijk->jki', darrF)))
expect_equal(aperm(arrF, c(3,1,2)),
    np$einsum('ijk->kij', arrF),
    as.array(einsum('ijk->kij', darrF)))
expect_equal(aperm(arrF, c(3,2,1)),
    np$einsum('ijk->kji', arrF),
    as.array(einsum('ijk->kji', darrF)))
expect_equal(aperm(arrG, c(1,2,4,3)),
    np$einsum('ijkl->ijlk', arrG),
    as.array(einsum('ijkl->ijlk', darrG)))
expect_equal(aperm(arrG, c(1,3,2,4)),
    np$einsum('ijkl->ikjl', arrG),
    as.array(einsum('ijkl->ikjl', darrG)))
expect_equal(aperm(arrG, c(1,3,4,2)),
    np$einsum('ijkl->iklj', arrG),
    as.array(einsum('ijkl->iklj', darrG)))
expect_equal(aperm(arrG, c(1,4,2,3)),
    np$einsum('ijkl->iljk', arrG),
    as.array(einsum('ijkl->iljk', darrG)))
expect_equal(aperm(arrG, c(1,4,3,2)),
    np$einsum('ijkl->ilkj', arrG),
    as.array(einsum('ijkl->ilkj', darrG)))
expect_equal(aperm(arrG, c(2,1,3,4)),
    np$einsum('ijkl->jikl', arrG),
    as.array(einsum('ijkl->jikl', darrG)))
expect_equal(aperm(arrG, c(2,1,4,3)),
    np$einsum('ijkl->jilk', arrG),
    as.array(einsum('ijkl->jilk', darrG)))
expect_equal(aperm(arrG, c(2,3,1,4)),
    np$einsum('ijkl->jkil', arrG),
    as.array(einsum('ijkl->jkil', darrG)))
expect_equal(aperm(arrG, c(2,3,4,1)),
    np$einsum('ijkl->jkli', arrG),
    as.array(einsum('ijkl->jkli', darrG)))
expect_equal(aperm(arrG, c(2,4,1,3)),
    np$einsum('ijkl->jlik', arrG),
    as.array(einsum('ijkl->jlik', darrG)))
expect_equal(aperm(arrG, c(2,4,3,1)),
    np$einsum('ijkl->jlki', arrG),
    as.array(einsum('ijkl->jlki', darrG)))
expect_equal(aperm(arrG, c(3,1,2,4)),
    np$einsum('ijkl->kijl', arrG),
    as.array(einsum('ijkl->kijl', darrG)))
expect_equal(aperm(arrG, c(3,1,4,2)),
    np$einsum('ijkl->kilj', arrG),
    as.array(einsum('ijkl->kilj', darrG)))
expect_equal(aperm(arrG, c(3,2,1,4)),
    np$einsum('ijkl->kjil', arrG),
    as.array(einsum('ijkl->kjil', darrG)))
expect_equal(aperm(arrG, c(3,2,4,1)),
    np$einsum('ijkl->kjli', arrG),
    as.array(einsum('ijkl->kjli', darrG)))
expect_equal(aperm(arrG, c(3,4,1,2)),
    np$einsum('ijkl->klij', arrG),
    as.array(einsum('ijkl->klij', darrG)))
expect_equal(aperm(arrG, c(3,4,2,1)),
    np$einsum('ijkl->klji', arrG),
    as.array(einsum('ijkl->klji', darrG)))
expect_equal(aperm(arrG, c(4,1,2,3)),
    np$einsum('ijkl->lijk', arrG),
    as.array(einsum('ijkl->lijk', darrG)))
expect_equal(aperm(arrG, c(4,1,3,2)),
    np$einsum('ijkl->likj', arrG),
    as.array(einsum('ijkl->likj', darrG)))
expect_equal(aperm(arrG, c(4,2,1,3)),
    np$einsum('ijkl->ljik', arrG),
    as.array(einsum('ijkl->ljik', darrG)))
expect_equal(aperm(arrG, c(4,2,3,1)),
    np$einsum('ijkl->ljki', arrG),
    as.array(einsum('ijkl->ljki', darrG)))
expect_equal(aperm(arrG, c(4,3,1,2)),
    np$einsum('ijkl->lkij', arrG),
    as.array(einsum('ijkl->lkij', darrG)))
expect_equal(aperm(arrG, c(4,3,2,1)),
    np$einsum('ijkl->lkji', arrG),
    as.array(einsum('ijkl->lkji', darrG)))

context("### einsum (sum + permutation) ###\n")
expect_equal(sum(arrA),
    as.vector(np$einsum('i->', arrA)),
    as.vector(einsum('i->', darrA)))
expect_equal(sum(arrD),
    as.vector(np$einsum('ij->', arrD)),
    as.vector(einsum('ij->', darrD)))

expect_equal(rowSums(arrD),
    as.vector(np$einsum('ij->i', arrD)),
    as.vector(einsum('ij->i', darrD)))
expect_equal(colSums(arrD),
    as.vector(np$einsum('ij->j', arrD)),
    as.vector(einsum('ij->j', darrD)))

expect_equal(sum(arrF),
    as.vector(np$einsum('ijk->', arrF)),
    as.vector(einsum('ijk->', darrF)))
expect_equal(tmp1,
    as.vector(np$einsum('ijk->i', arrF)),
    as.vector(einsum('ijk->i', darrF)))
expect_equal(tmp2,
    as.vector(np$einsum('ijk->j', arrF)),
    as.vector(einsum('ijk->j', darrF)))
expect_equal(tmp3,
    as.vector(np$einsum('ijk->k', arrF)),
    as.vector(einsum('ijk->k', darrF)))
expect_equal(tmp4,
    as.array(np$einsum('ijk->ij', arrF)),
    as.array(einsum('ijk->ij', darrF)))
expect_equal(tmp5,
    as.array(np$einsum('ijk->jk', arrF)),
    as.array(einsum('ijk->jk', darrF)))
expect_equal(tmp6,
    as.array(np$einsum('ijk->ik', arrF)),
    as.array(einsum('ijk->ik', darrF)))
expect_equal(sum(arrG),
    as.vector(np$einsum('ijkl->', arrG)),
    as.vector(einsum('ijkl->', darrG)))
expect_equal(tmp7,
    as.vector(np$einsum('ijkl->i', arrG)),
    as.vector(einsum('ijkl->i', darrG)))
expect_equal(tmp8,
    as.vector(np$einsum('ijkl->j', arrG)),
    as.vector(einsum('ijkl->j', darrG)))
expect_equal(tmp9,
    as.vector(np$einsum('ijkl->k', arrG)),
    as.vector(einsum('ijkl->k', darrG)))
expect_equal(tmp10,
    as.vector(np$einsum('ijkl->l', arrG)),
    as.vector(einsum('ijkl->l', darrG)))
expect_equal(tmp11,
    as.array(np$einsum('ijkl->ij', arrG)),
    as.array(einsum('ijkl->ij', darrG)))
expect_equal(tmp12,
    as.array(np$einsum('ijkl->ik', arrG)),
    as.array(einsum('ijkl->ik', darrG)))
expect_equal(tmp13,
    as.array(np$einsum('ijkl->il', arrG)),
    as.array(einsum('ijkl->il', darrG)))
expect_equal(tmp14,
    as.array(np$einsum('ijkl->ji', arrG)),
    as.array(einsum('ijkl->ji', darrG)))
expect_equal(tmp15,
    as.array(np$einsum('ijkl->jk', arrG)),
    as.array(einsum('ijkl->jk', darrG)))
expect_equal(tmp16,
    as.array(np$einsum('ijkl->jl', arrG)),
    as.array(einsum('ijkl->jl', darrG)))
expect_equal(tmp17,
    as.array(np$einsum('ijkl->ki', arrG)),
    as.array(einsum('ijkl->ki', darrG)))
expect_equal(tmp18,
    as.array(np$einsum('ijkl->kj', arrG)),
    as.array(einsum('ijkl->kj', darrG)))
expect_equal(tmp19,
    as.array(np$einsum('ijkl->kl', arrG)),
    as.array(einsum('ijkl->kl', darrG)))
expect_equal(tmp20,
    as.array(np$einsum('ijkl->li', arrG)),
    as.array(einsum('ijkl->li', darrG)))
expect_equal(tmp21,
    as.array(np$einsum('ijkl->lj', arrG)),
    as.array(einsum('ijkl->lj', darrG)))
expect_equal(tmp22,
    as.array(np$einsum('ijkl->lk', arrG)),
    as.array(einsum('ijkl->lk', darrG)))
expect_equal(tmp23,
    as.array(np$einsum('ijkl->ijk', arrG)),
    as.array(einsum('ijkl->ijk', darrG)))
expect_equal(tmp24,
    as.array(np$einsum('ijkl->ijl', arrG)),
    as.array(einsum('ijkl->ijl', darrG)))
expect_equal(tmp25,
    as.array(np$einsum('ijkl->ikj', arrG)),
    as.array(einsum('ijkl->ikj', darrG)))
expect_equal(tmp26,
    as.array(np$einsum('ijkl->ikl', arrG)),
    as.array(einsum('ijkl->ikl', darrG)))
expect_equal(tmp27,
    as.array(np$einsum('ijkl->ilj', arrG)),
    as.array(einsum('ijkl->ilj', darrG)))
expect_equal(tmp28,
    as.array(np$einsum('ijkl->ilk', arrG)),
    as.array(einsum('ijkl->ilk', darrG)))
expect_equal(tmp29,
    as.array(np$einsum('ijkl->jik', arrG)),
    as.array(einsum('ijkl->jik', darrG)))
expect_equal(tmp30,
    as.array(np$einsum('ijkl->jil', arrG)),
    as.array(einsum('ijkl->jil', darrG)))
expect_equal(tmp31,
    as.array(np$einsum('ijkl->jki', arrG)),
    as.array(einsum('ijkl->jki', darrG)))
expect_equal(tmp32,
    as.array(np$einsum('ijkl->jkl', arrG)),
    as.array(einsum('ijkl->jkl', darrG)))
expect_equal(tmp33,
    as.array(np$einsum('ijkl->jli', arrG)),
    as.array(einsum('ijkl->jli', darrG)))
expect_equal(tmp34,
    as.array(np$einsum('ijkl->jlk', arrG)),
    as.array(einsum('ijkl->jlk', darrG)))
expect_equal(tmp35,
    as.array(np$einsum('ijkl->kij', arrG)),
    as.array(einsum('ijkl->kij', darrG)))
expect_equal(tmp36,
    as.array(np$einsum('ijkl->kil', arrG)),
    as.array(einsum('ijkl->kil', darrG)))
expect_equal(tmp37,
    as.array(np$einsum('ijkl->kji', arrG)),
    as.array(einsum('ijkl->kji', darrG)))
expect_equal(tmp38,
    as.array(np$einsum('ijkl->kjl', arrG)),
    as.array(einsum('ijkl->kjl', darrG)))
expect_equal(tmp39,
    as.array(np$einsum('ijkl->kli', arrG)),
    as.array(einsum('ijkl->kli', darrG)))
expect_equal(tmp40,
    as.array(np$einsum('ijkl->klj', arrG)),
    as.array(einsum('ijkl->klj', darrG)))
expect_equal(tmp41,
    as.array(np$einsum('ijkl->lij', arrG)),
    as.array(einsum('ijkl->lij', darrG)))
expect_equal(tmp42,
    as.array(np$einsum('ijkl->lik', arrG)),
    as.array(einsum('ijkl->lik', darrG)))
expect_equal(tmp43,
    as.array(np$einsum('ijkl->lji', arrG)),
    as.array(einsum('ijkl->lji', darrG)))
expect_equal(tmp44,
    as.array(np$einsum('ijkl->ljk', arrG)),
    as.array(einsum('ijkl->ljk', darrG)))
expect_equal(tmp45,
    as.array(np$einsum('ijkl->lki', arrG)),
    as.array(einsum('ijkl->lki', darrG)))
expect_equal(tmp46,
    as.array(np$einsum('ijkl->lkj', arrG)),
    as.array(einsum('ijkl->lkj', darrG)))

context("### einsum (multiply, diagonal) ###\n")
expect_equal(base::diag(arrC),
    as.vector(np$einsum('ii->i', arrC)),
    as.vector(einsum('ii->i', darrC)))
expect_equal(tmp47,
    as.vector(np$einsum('iii->i', arrE)),
    as.vector(einsum('iii->i', darrE)))
expect_equal(tmp48,
    as.vector(np$einsum('iiii->i', arrG)),
    as.vector(einsum('iiii->i', darrG)))

context("### einsum (sum + multiply + transpose) ###\n")
# Hadamard matrix
expect_equal(arrA*arrA,
    as.array(np$einsum('i,i->i', arrA, arrA)),
    as.array(einsum('i,i->i', darrA, darrA)))
expect_equal(arrD*arrD,
    as.array(np$einsum('ij,ij->ij', arrD, arrD)),
    as.array(einsum('ij,ij->ij', darrD, darrD)))
expect_equal(arrF*arrF,
    as.array(np$einsum('ijk,ijk->ijk', arrF, arrF)),
    as.array(einsum('ijk,ijk->ijk', darrF, darrF)))
expect_equal(arrG*arrG,
    as.array(np$einsum('ijkl,ijkl->ijkl', arrG, arrG)),
    as.array(einsum('ijkl,ijkl->ijkl', darrG, darrG)))

# Frobenius norm^2
expect_equal(sum(arrA*arrA),
    as.vector(np$einsum('i,i->', arrA, arrA)),
    as.vector(einsum('i,i->', darrA, darrA)))
expect_equal(sum(arrD*arrD),
    as.vector(np$einsum('ij,ij->', arrD, arrD)),
    as.vector(einsum('ij,ij->', darrD, darrD)))
expect_equal(sum(arrF*arrF),
    as.vector(np$einsum('ijk,ijk->', arrF, arrF)),
    as.vector(einsum('ijk,ijk->', darrF, darrF)))
expect_equal(sum(arrG*arrG),
    as.vector(np$einsum('ijkl,ijkl->', arrG, arrG)),
    as.vector(einsum('ijkl,ijkl->', darrG, darrG)))

expect_equal(sum(arrC%*%arrD),
    as.vector(np$einsum('ij,jk->', arrC, arrD)),
    as.vector(einsum('ij,jk->', darrC, darrD)))

expect_equal(rowSums(arrC%*%arrD),
    as.vector(np$einsum('ij,jk->i', arrC, arrD)),
    as.vector(einsum('ij,jk->i', darrC, darrD)))
expect_equal(tmp49,
    as.vector(np$einsum('ij,jk->j', arrC, arrD)),
    as.vector(einsum('ij,jk->j', darrC, darrD)))
expect_equal(colSums(arrC%*%arrD),
    as.vector(np$einsum('ij,jk->k', arrC, arrD)),
    as.vector(einsum('ij,jk->k', darrC, darrD)))

expect_equal(tmp50,
    as.array(np$einsum('ij,jk->ij', arrC, arrD)),
    as.array(einsum('ij,jk->ij', darrC, darrD)))
expect_equal(tmp51,
    as.array(np$einsum('ij,jk->ji', arrC, arrD)),
    as.array(einsum('ij,jk->ji', darrC, darrD)))
expect_equal(tmp52,
    as.array(np$einsum('ij,jk->jk', arrC, arrD)),
    as.array(einsum('ij,jk->jk', darrC, darrD)))
expect_equal(tmp53,
    as.array(np$einsum('ij,jk->kj', arrC, arrD)),
    as.array(einsum('ij,jk->kj', darrC, darrD)))
expect_equal(tmp54,
    as.array(np$einsum('ij,jk->ik', arrC, arrD)),
    as.array(einsum('ij,jk->ik', darrC, darrD)))
expect_equal(tmp55,
    as.array(np$einsum('ij,jk->ki', arrC, arrD)),
    as.array(einsum('ij,jk->ki', darrC, darrD)))

expect_equal(tmp56,
    as.array(np$einsum('ij,jk->ijk', arrC, arrD)),
    as.array(einsum('ij,jk->ijk', darrC, darrD)))
expect_equal(tmp57,
    as.array(np$einsum('ij,jk->ikj', arrC, arrD)),
    as.array(einsum('ij,jk->ikj', darrC, darrD)))
expect_equal(tmp58,
    as.array(np$einsum('ij,jk->jik', arrC, arrD)),
    as.array(einsum('ij,jk->jik', darrC, darrD)))
expect_equal(tmp59,
    as.array(np$einsum('ij,jk->jki', arrC, arrD)),
    as.array(einsum('ij,jk->jki', darrC, darrD)))
expect_equal(tmp60,
    as.array(np$einsum('ij,jk->kij', arrC, arrD)),
    as.array(einsum('ij,jk->kij', darrC, darrD)))
expect_equal(tmp61,
    as.array(np$einsum('ij,jk->kji', arrC, arrD)),
    as.array(einsum('ij,jk->kji', darrC, darrD)))

expect_equal(tmp62,
    as.vector(np$einsum('ij,ijk->', arrC, arrE)),
    as.vector(einsum('ij,ijk->', darrC, darrE)))
expect_equal(tmp63,
    as.vector(np$einsum('ij,ijk->i', arrC, arrE)),
    as.vector(einsum('ij,ijk->i', darrC, darrE)))
expect_equal(tmp64,
    as.vector(np$einsum('ij,ijk->j', arrC, arrE)),
    as.vector(einsum('ij,ijk->j', darrC, darrE)))
expect_equal(tmp65,
    as.vector(np$einsum('ij,ijk->k', arrC, arrE)),
    as.vector(einsum('ij,ijk->k', darrC, darrE)))
expect_equal(tmp66,
    as.array(np$einsum('ij,ijk->ij', arrC, arrE)),
    as.array(einsum('ij,ijk->ij', darrC, darrE)))
expect_equal(tmp67,
    as.array(np$einsum('ij,ijk->ji', arrC, arrE)),
    as.array(einsum('ij,ijk->ji', darrC, darrE)))
expect_equal(tmp68,
    as.array(np$einsum('ij,ijk->jk', arrC, arrE)),
    as.array(einsum('ij,ijk->jk', darrC, darrE)))
expect_equal(tmp69,
    as.array(np$einsum('ij,ijk->kj', arrC, arrE)),
    as.array(einsum('ij,ijk->kj', darrC, darrE)))
expect_equal(tmp70,
    as.array(np$einsum('ij,ijk->ik', arrC, arrE)),
    as.array(einsum('ij,ijk->ik', darrC, darrE)))
expect_equal(tmp71,
    as.array(np$einsum('ij,ijk->ki', arrC, arrE)),
    as.array(einsum('ij,ijk->ki', darrC, darrE)))
expect_equal(tmp72,
    as.array(np$einsum('ij,ijk->ijk', arrC, arrE)),
    as.array(einsum('ij,ijk->ijk', darrC, darrE)))

expect_equal(tmp73,
    as.vector(np$einsum('ijk,ijkl->', arrE, arrG)),
    as.vector(einsum('ijk,ijkl->', darrE, darrG)))
expect_equal(tmp74,
    as.vector(np$einsum('ijk,ijkl->i', arrE, arrG)),
    as.vector(einsum('ijk,ijkl->i', darrE, darrG)))
expect_equal(tmp75,
    as.vector(np$einsum('ijk,ijkl->j', arrE, arrG)),
    as.vector(einsum('ijk,ijkl->j', darrE, darrG)))
expect_equal(tmp76,
    as.vector(np$einsum('ijk,ijkl->k', arrE, arrG)),
    as.vector(einsum('ijk,ijkl->k', darrE, darrG)))
expect_equal(tmp77,
    as.vector(np$einsum('ijk,ijkl->l', arrE, arrG)),
    as.vector(einsum('ijk,ijkl->l', darrE, darrG)))
expect_equal(tmp78,
    as.array(np$einsum('ijk,ijkl->ij', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ij', darrE, darrG)))
expect_equal(tmp79,
    as.array(np$einsum('ijk,ijkl->ji', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ji', darrE, darrG)))
expect_equal(tmp80,
    as.array(np$einsum('ijk,ijkl->ik', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ik', darrE, darrG)))
expect_equal(tmp81,
    as.array(np$einsum('ijk,ijkl->ki', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ki', darrE, darrG)))
expect_equal(tmp82,
    as.array(np$einsum('ijk,ijkl->il', arrE, arrG)),
    as.array(einsum('ijk,ijkl->il', darrE, darrG)))
expect_equal(tmp83,
    as.array(np$einsum('ijk,ijkl->li', arrE, arrG)),
    as.array(einsum('ijk,ijkl->li', darrE, darrG)))
expect_equal(tmp84,
    as.array(np$einsum('ijk,ijkl->jk', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jk', darrE, darrG)))
expect_equal(tmp85,
    as.array(np$einsum('ijk,ijkl->kj', arrE, arrG)),
    as.array(einsum('ijk,ijkl->kj', darrE, darrG)))
expect_equal(tmp86,
    as.array(np$einsum('ijk,ijkl->jl', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jl', darrE, darrG)))
expect_equal(tmp87,
    as.array(np$einsum('ijk,ijkl->lj', arrE, arrG)),
    as.array(einsum('ijk,ijkl->lj', darrE, darrG)))
expect_equal(tmp88,
    as.array(np$einsum('ijk,ijkl->kl', arrE, arrG)),
    as.array(einsum('ijk,ijkl->kl', darrE, darrG)))
expect_equal(tmp89,
    as.array(np$einsum('ijk,ijkl->lk', arrE, arrG)),
    as.array(einsum('ijk,ijkl->lk', darrE, darrG)))

expect_equal(tmp90,
    as.array(np$einsum('ijk,ijkl->ijk', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ijk', darrE, darrG)))
expect_equal(tmp91,
    as.array(np$einsum('ijk,ijkl->ikj', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ikj', darrE, darrG)))
expect_equal(tmp92,
    as.array(np$einsum('ijk,ijkl->jik', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jik', darrE, darrG)))
expect_equal(tmp93,
    as.array(np$einsum('ijk,ijkl->jki', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jki', darrE, darrG)))
expect_equal(tmp94,
    as.array(np$einsum('ijk,ijkl->kij', arrE, arrG)),
    as.array(einsum('ijk,ijkl->kij', darrE, darrG)))
expect_equal(tmp95,
    as.array(np$einsum('ijk,ijkl->kji', arrE, arrG)),
    as.array(einsum('ijk,ijkl->kji', darrE, darrG)))
expect_equal(tmp96,
    as.array(np$einsum('ijk,ijkl->ijl', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ijl', darrE, darrG)))
expect_equal(tmp97,
    as.array(np$einsum('ijk,ijkl->ilj', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ilj', darrE, darrG)))
expect_equal(tmp98,
    as.array(np$einsum('ijk,ijkl->jil', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jil', darrE, darrG)))
expect_equal(tmp99,
    as.array(np$einsum('ijk,ijkl->jli', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jli', darrE, darrG)))
expect_equal(tmp100,
    as.array(np$einsum('ijk,ijkl->lij', arrE, arrG)),
    as.array(einsum('ijk,ijkl->lij', darrE, darrG)))
expect_equal(tmp101,
    as.array(np$einsum('ijk,ijkl->lji', arrE, arrG)),
    as.array(einsum('ijk,ijkl->lji', darrE, darrG)))
expect_equal(tmp102,
    as.array(np$einsum('ijk,ijkl->ikl', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ikl', darrE, darrG)))
expect_equal(tmp103,
    as.array(np$einsum('ijk,ijkl->ilk', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ilk', darrE, darrG)))
expect_equal(tmp104,
    as.array(np$einsum('ijk,ijkl->kil', arrE, arrG)),
    as.array(einsum('ijk,ijkl->kil', darrE, darrG)))
expect_equal(tmp105,
    as.array(np$einsum('ijk,ijkl->kli', arrE, arrG)),
    as.array(einsum('ijk,ijkl->kli', darrE, darrG)))
expect_equal(tmp106,
    as.array(np$einsum('ijk,ijkl->lik', arrE, arrG)),
    as.array(einsum('ijk,ijkl->lik', darrE, darrG)))
expect_equal(tmp107,
    as.array(np$einsum('ijk,ijkl->lki', arrE, arrG)),
    as.array(einsum('ijk,ijkl->lki', darrE, darrG)))
expect_equal(tmp108,
    as.array(np$einsum('ijk,ijkl->jkl', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jkl', darrE, darrG)))
expect_equal(tmp109,
    as.array(np$einsum('ijk,ijkl->jlk', arrE, arrG)),
    as.array(einsum('ijk,ijkl->jlk', darrE, darrG)))
expect_equal(tmp110,
    as.array(np$einsum('ijk,ijkl->kjl', arrE, arrG)),
    as.array(einsum('ijk,ijkl->kjl', darrE, darrG)))
expect_equal(tmp111,
    as.array(np$einsum('ijk,ijkl->klj', arrE, arrG)),
    as.array(einsum('ijk,ijkl->klj', darrE, darrG)))
expect_equal(tmp112,
    as.array(np$einsum('ijk,ijkl->ljk', arrE, arrG)),
    as.array(einsum('ijk,ijkl->ljk', darrE, darrG)))
expect_equal(tmp113,
    as.array(np$einsum('ijk,ijkl->lkj', arrE, arrG)),
    as.array(einsum('ijk,ijkl->lkj', darrE, darrG)))

context("### einsum (sum + multiply + transpose, three tensor) ###\n")
expect_equal(tmp114,
    as.vector(np$einsum('i,ij,ijk->', arrA, arrD, arrF)),
    as.vector(einsum('i,ij,ijk->', darrA, darrD, darrF)))
expect_equal(tmp115,
    as.vector(np$einsum('i,ij,ijk->i', arrA, arrD, arrF)),
    as.vector(einsum('i,ij,ijk->i', darrA, darrD, darrF)))
expect_equal(tmp116,
    as.vector(np$einsum('i,ij,ijk->j', arrA, arrD, arrF)),
    as.vector(einsum('i,ij,ijk->j', darrA, darrD, darrF)))
expect_equal(tmp117,
    as.vector(np$einsum('i,ij,ijk->k', arrA, arrD, arrF)),
    as.vector(einsum('i,ij,ijk->k', darrA, darrD, darrF)))
expect_equal(tmp118,
    as.array(np$einsum('i,ij,ijk->ij', arrA, arrD, arrF)),
    as.array(einsum('i,ij,ijk->ij', darrA, darrD, darrF)))
expect_equal(tmp119,
    as.array(np$einsum('i,ij,ijk->ik', arrA, arrD, arrF)),
    as.array(einsum('i,ij,ijk->ik', darrA, darrD, darrF)))
expect_equal(tmp120,
    as.array(np$einsum('i,ij,ijk->ji', arrA, arrD, arrF)),
    as.array(einsum('i,ij,ijk->ji', darrA, darrD, darrF)))
expect_equal(tmp121,
    as.array(np$einsum('i,ij,ijk->jk', arrA, arrD, arrF)),
    as.array(einsum('i,ij,ijk->jk', darrA, darrD, darrF)))
expect_equal(tmp122,
    as.array(np$einsum('i,ij,ijk->ki', arrA, arrD, arrF)),
    as.array(einsum('i,ij,ijk->ki', darrA, darrD, darrF)))
expect_equal(tmp123,
    as.array(np$einsum('i,ij,ijk->kj', arrA, arrD, arrF)),
    as.array(einsum('i,ij,ijk->kj', darrA, darrD, darrF)))
expect_equal(tmp124,
    as.array(np$einsum('i,ij,ijk->ijk', arrA, arrD, arrF)),
    as.array(einsum('i,ij,ijk->ijk', darrA, darrD, darrF)))

expect_equal(
    tmp125,
    np$einsum('i,ij,ijk,ijkl->ijkl', arrA, arrD, arrF, arrH),
    as.array(einsum('i,ij,ijk,ijkl->ijkl', darrA, darrD, darrF, darrH)))

# cf.
# https://www.procrasist.com/entry/einsum
# https://github.com/numpy/numpy/blob/623bc1fae1d47df24e7f1e29321d0c0ba2771ce0/numpy/typing/tests/data/pass/einsumfunc.py
# https://ajcr.net/Basic-guide-to-einsum/
