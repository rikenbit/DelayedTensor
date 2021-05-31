arrA <- array(runif(3), dim=c(3))
arrB <- array(runif(3), dim=c(3))
arrC <- array(runif(3*3), dim=c(3,3))
arrD <- array(runif(3*4), dim=c(3,4))
arrE <- array(runif(3*3*3), dim=c(3,3,3))
arrF <- array(runif(3*4*5), dim=c(3,4,5))
arrG <- array(runif(3*3*3*3), dim=c(3,3,3,3))
arrH <- array(runif(3*4*5*6), dim=c(3,4,5,6))

darrA <- DelayedArray(arrA)
darrB <- DelayedArray(arrB)
darrC <- DelayedArray(arrC)
darrD <- DelayedArray(arrD)
darrE <- DelayedArray(arrE)
darrF <- DelayedArray(arrF)
darrG <- DelayedArray(arrG)
darrH <- DelayedArray(arrH)

#
# einsum (sum + permutation)
#
# tmp1: einsum('ijk->i', arrF)
tmp1 <- rep(0, dim(arrF)[1])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp1[i] <- tmp1[i] + arrF[i,j,k]
		}
	}
}

# tmp2: einsum('ijk->j', arrF)
tmp2 <- rep(0, dim(arrF)[2])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp2[j] <- tmp2[j] + arrF[i,j,k]
		}
	}
}

# tmp3: einsum('ijk->k', arrF)
tmp3 <- rep(0, dim(arrF)[3])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp3[k] <- tmp3[k] + arrF[i,j,k]
		}
	}
}

# tmp4: einsum('ijk->ij', arrF)
tmp4 <- array(0, dim=dim(arrF)[c(1,2)])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp4[i,j] <- tmp4[i,j] + arrF[i,j,k]
		}
	}
}

# tmp5: einsum('ijk->jk', arrF)
tmp5 <- array(0, dim=dim(arrF)[c(2,3)])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp5[j,k] <- tmp5[j,k] + arrF[i,j,k]
		}
	}
}

# tmp6: einsum('ijk->ik', arrF)
tmp6 <- array(0, dim=dim(arrF)[c(1,3)])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp6[i,k] <- tmp6[i,k] + arrF[i,j,k]
		}
	}
}

# tmp7: einsum('ijkl->i', arrG)
tmp7 <- rep(0, length=dim(arrG)[1])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp7[i] <- tmp7[i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp8: einsum('ijkl->j', arrG)
tmp8 <- rep(0, length=dim(arrG)[2])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp8[j] <- tmp8[j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp9: einsum('ijkl->k', arrG)
tmp9 <- rep(0, length=dim(arrG)[3])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp9[k] <- tmp9[k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp10: einsum('ijkl->l', arrG)
tmp10 <- rep(0, length=dim(arrG)[4])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp10[l] <- tmp10[l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp11: einsum('ijkl->ij', arrG)
tmp11 <- array(0, dim=dim(arrG)[c(1,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp11[i,j] <- tmp11[i,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp12: einsum('ijkl->ik', arrG)
tmp12 <- array(0, dim=dim(arrG)[c(1,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp12[i,k] <- tmp12[i,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp13: einsum('ijkl->il', arrG)
tmp13 <- array(0, dim=dim(arrG)[c(1,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp13[i,l] <- tmp13[i,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp14: einsum('ijkl->ji', arrG)
tmp14 <- array(0, dim=dim(arrG)[c(2,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp14[j,i] <- tmp14[j,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp15: einsum('ijkl->jk', arrG)
tmp15 <- array(0, dim=dim(arrG)[c(2,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp15[j,k] <- tmp15[j,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp16: einsum('ijkl->jl', arrG)
tmp16 <- array(0, dim=dim(arrG)[c(2,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp16[j,l] <- tmp16[j,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp17: einsum('ijkl->ki', arrG)
tmp17 <- array(0, dim=dim(arrG)[c(3,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp17[k,i] <- tmp17[k,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp18: einsum('ijkl->kj', arrG)
tmp18 <- array(0, dim=dim(arrG)[c(3,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp18[k,j] <- tmp18[k,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp19: einsum('ijkl->kl', arrG)
tmp19 <- array(0, dim=dim(arrG)[c(3,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp19[k,l] <- tmp19[k,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp20: einsum('ijkl->li', arrG)
tmp20 <- array(0, dim=dim(arrG)[c(4,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp20[l,i] <- tmp20[l,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp21: einsum('ijkl->lj', arrG)
tmp21 <- array(0, dim=dim(arrG)[c(4,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp21[l,j] <- tmp21[l,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp22: einsum('ijkl->lk', arrG)
tmp22 <- array(0, dim=dim(arrG)[c(4,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp22[l,k] <- tmp22[l,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp23: einsum('ijkl->ijk', arrG)
tmp23 <- array(0, dim=dim(arrG)[c(1,2,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp23[i,j,k] <- tmp23[i,j,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp24: einsum('ijkl->ijl', arrG)
tmp24 <- array(0, dim=dim(arrG)[c(1,2,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp24[i,j,l] <- tmp24[i,j,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp25: einsum('ijkl->ikj', arrG)
tmp25 <- array(0, dim=dim(arrG)[c(1,3,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp25[i,k,j] <- tmp25[i,k,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp26: einsum('ijkl->ikl', arrG)
tmp26 <- array(0, dim=dim(arrG)[c(1,3,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp26[i,k,l] <- tmp26[i,k,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp27: einsum('ijkl->ilj', arrG)
tmp27 <- array(0, dim=dim(arrG)[c(1,4,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp27[i,l,j] <- tmp27[i,l,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp28: einsum('ijkl->ilk', arrG)
tmp28 <- array(0, dim=dim(arrG)[c(1,4,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp28[i,l,k] <- tmp28[i,l,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp29: einsum('ijkl->jik', arrG)
tmp29 <- array(0, dim=dim(arrG)[c(2,1,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp29[j,i,k] <- tmp29[j,i,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp30: einsum('ijkl->jil', arrG)
tmp30 <- array(0, dim=dim(arrG)[c(2,1,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp30[j,i,l] <- tmp30[j,i,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp31: einsum('ijkl->jki', arrG)
tmp31 <- array(0, dim=dim(arrG)[c(2,3,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp31[j,k,i] <- tmp31[j,k,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp32: einsum('ijkl->jkl', arrG)
tmp32 <- array(0, dim=dim(arrG)[c(2,3,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp32[j,k,l] <- tmp32[j,k,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp33: einsum('ijkl->jli', arrG)
tmp33 <- array(0, dim=dim(arrG)[c(2,4,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp33[j,l,i] <- tmp33[j,l,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp34: einsum('ijkl->jlk', arrG)
tmp34 <- array(0, dim=dim(arrG)[c(2,4,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp34[j,l,k] <- tmp34[j,l,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp35: einsum('ijkl->kij', arrG)
tmp35 <- array(0, dim=dim(arrG)[c(3,1,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp35[k,i,j] <- tmp35[k,i,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp36: einsum('ijkl->kil', arrG)
tmp36 <- array(0, dim=dim(arrG)[c(3,1,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp36[k,i,l] <- tmp36[k,i,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp37: einsum('ijkl->kji', arrG)
tmp37 <- array(0, dim=dim(arrG)[c(3,2,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp37[k,j,i] <- tmp37[k,j,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp38: einsum('ijkl->kjl', arrG)
tmp38 <- array(0, dim=dim(arrG)[c(3,2,4)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp38[k,j,l] <- tmp38[k,j,l] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp39: einsum('ijkl->kli', arrG)
tmp39 <- array(0, dim=dim(arrG)[c(3,4,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp39[k,l,i] <- tmp39[k,l,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp40: einsum('ijkl->klj', arrG)
tmp40 <- array(0, dim=dim(arrG)[c(3,4,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp40[k,l,j] <- tmp40[k,l,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp41: einsum('ijkl->lij', arrG)
tmp41 <- array(0, dim=dim(arrG)[c(4,1,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp41[l,i,j] <- tmp41[l,i,j] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp42: einsum('ijkl->lik', arrG)
tmp42 <- array(0, dim=dim(arrG)[c(4,1,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp42[l,i,k] <- tmp42[l,i,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp43: einsum('ijkl->lji', arrG)
tmp43 <- array(0, dim=dim(arrG)[c(4,2,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp43[l,j,i] <- tmp43[l,j,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp44: einsum('ijkl->ljk', arrG)
tmp44 <- array(0, dim=dim(arrG)[c(4,2,3)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp44[l,j,k] <- tmp44[l,j,k] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp45: einsum('ijkl->lki', arrG)
tmp45 <- array(0, dim=dim(arrG)[c(4,3,1)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp45[l,k,i] <- tmp45[l,k,i] + arrG[i,j,k,l]
			}
		}
	}
}

# tmp46: einsum('ijkl->lkj', arrG)
tmp46 <- array(0, dim=dim(arrG)[c(4,3,2)])
for(i in 1:dim(arrG)[1]){
	for(j in 1:dim(arrG)[2]){
		for(k in 1:dim(arrG)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp46[l,k,j] <- tmp46[l,k,j] + arrG[i,j,k,l]
			}
		}
	}
}

#
# einsum (multiply, diagonal)
#
# tmp47: einsum('iii->i', arrE)
tmp47 <- rep(0, dim(arrE)[1])
for(i in 1:dim(arrE)[1]){
	tmp47[i] <- tmp47[i] + arrE[i,i,i]
}

# tmp48: einsum('iiii->i', arrG)
tmp48 <- rep(0, dim(arrG)[1])
for(i in 1:dim(arrG)[1]){
	tmp48[i] <- tmp48[i] + arrG[i,i,i,i]
}

#
# einsum (sum + multiply + transpose)
#
# tmp49: einsum('ij,jk->j', arrC, arrD)
tmp49 <- rep(0, dim(arrC)[2])
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp49[j] <- tmp49[j] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp50: einsum('ij,jk->ij', arrC, arrD))
tmp50 <- array(0, dim=c(dim(arrC)[1], dim(arrC)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp50[i,j] <- tmp50[i,j] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp51: einsum('ij,jk->ji', arrC, arrD))
tmp51 <- array(0, dim=c(dim(arrC)[2], dim(arrC)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp51[j,i] <- tmp51[j,i] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp52: einsum('ij,jk->jk', arrC, arrD))
tmp52 <- array(0, dim=c(dim(arrC)[2], dim(arrD)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp52[j,k] <- tmp52[j,k] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp53: einsum('ij,jk->kj', arrC, arrD))
tmp53 <- array(0, dim=c(dim(arrD)[2], dim(arrD)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp53[k,j] <- tmp53[k,j] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp54: einsum('ij,jk->ik', arrC, arrD))
tmp54 <- array(0, dim=c(dim(arrC)[1], dim(arrD)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp54[i,k] <- tmp54[i,k] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp55: einsum('ij,jk->ki', arrC, arrD))
tmp55 <- array(0, dim=c(dim(arrD)[2], dim(arrC)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp55[k,i] <- tmp55[k,i] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp56: einsum('ij,jk->ijk', arrC, arrD))
tmp56 <- array(0, dim=c(dim(arrC)[1], dim(arrC)[2], dim(arrD)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp56[i,j,k] <- tmp56[i,j,k] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp57: einsum('ij,jk->ikj', arrC, arrD))
tmp57 <- array(0, dim=c(dim(arrC)[1], dim(arrD)[2], dim(arrD)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp57[i,k,j] <- tmp57[i,k,j] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp58: einsum('ij,jk->jik', arrC, arrD))
tmp58 <- array(0, dim=c(dim(arrC)[2], dim(arrC)[1], dim(arrD)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp58[j,i,k] <- tmp58[j,i,k] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp59: einsum('ij,jk->jki', arrC, arrD))
tmp59 <- array(0, dim=c(dim(arrC)[2], dim(arrD)[2], dim(arrC)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp59[j,k,i] <- tmp59[j,k,i] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp60: einsum('ij,jk->kij', arrC, arrD))
tmp60 <- array(0, dim=c(dim(arrD)[2], dim(arrC)[1], dim(arrC)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp60[k,i,j] <- tmp60[k,i,j] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp61: einsum('ij,jk->kji', arrC, arrD))
tmp61 <- array(0, dim=c(dim(arrD)[2], dim(arrC)[2], dim(arrC)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrD)[2]){
			tmp61[k,j,i] <- tmp61[k,j,i] + arrC[i,j] * arrD[j,k]
		}
	}
}

# tmp62: einsum('ij,ijk->', arrC, arrE))
tmp62 <- 0
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp62 <- tmp62 + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp63: einsum('ij,ijk->i', arrC, arrE))
tmp63 <- rep(0, dim(arrC)[1])
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp63[i] <- tmp63[i] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp64: einsum('ij,ijk->j', arrC, arrE))
tmp64 <- rep(0, dim(arrC)[2])
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp64[j] <- tmp64[j] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp65: einsum('ij,ijk->k', arrC, arrE))
tmp65 <- rep(0, dim(arrE)[2])
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp65[k] <- tmp65[k] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp66: einsum('ij,ijk->ij', arrC, arrE))
tmp66 <- array(0, dim=c(dim(arrC)[1], dim(arrC)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp66[i,j] <- tmp66[i,j] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp67: einsum('ij,ijk->ji', arrC, arrE))
tmp67 <- array(0, dim=c(dim(arrC)[2], dim(arrC)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp67[j,i] <- tmp67[j,i] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp68: einsum('ij,ijk->jk', arrC, arrE))
tmp68 <- array(0, dim=c(dim(arrC)[2], dim(arrE)[3]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp68[j,k] <- tmp68[j,k] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp69: einsum('ij,ijk->kj', arrC, arrE))
tmp69 <- array(0, dim=c(dim(arrE)[3], dim(arrC)[2]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp69[k,j] <- tmp69[k,j] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp70: einsum('ij,ijk->ik', arrC, arrE))
tmp70 <- array(0, dim=c(dim(arrC)[1], dim(arrE)[3]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp70[i,k] <- tmp70[i,k] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp71: einsum('ij,ijk->ki', arrC, arrE))
tmp71 <- array(0, dim=c(dim(arrE)[3], dim(arrC)[1]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp71[k,i] <- tmp71[k,i] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp72: einsum('ij,ijk->ijk', arrC, arrE))
tmp72 <- array(0, dim=c(dim(arrC)[1], dim(arrC)[2], dim(arrE)[3]))
for(i in 1:dim(arrC)[1]){
	for(j in 1:dim(arrC)[2]){
		for(k in 1:dim(arrE)[2]){
			tmp72[i,j,k] <- tmp72[i,j,k] + arrC[i,j] * arrE[i,j,k]
		}
	}
}

# tmp73: einsum('ijk,ijkl->', arrE, arrG))
tmp73 <- 0
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp73 <- tmp73 + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp74: einsum('ijk,ijkl->i', arrE, arrG))
tmp74 <- rep(0, dim(arrE)[1])
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp74[i] <- tmp74[i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp75: einsum('ijk,ijkl->j', arrE, arrG))
tmp75 <- rep(0, dim(arrE)[2])
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp75[j] <- tmp75[j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp76: einsum('ijk,ijkl->k', arrE, arrG))
tmp76 <- rep(0, dim(arrE)[3])
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp76[k] <- tmp76[k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp77: einsum('ijk,ijkl->l', arrE, arrG))
tmp77 <- rep(0, dim(arrG)[4])
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp77[l] <- tmp77[l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp78: einsum('ijk,ijkl->ij', arrE, arrG))
tmp78 <- array(0, dim=c(dim(arrE)[1], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp78[i,j] <- tmp78[i,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp79: einsum('ijk,ijkl->ji', arrE, arrG))
tmp79 <- array(0, dim=c(dim(arrE)[2], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp79[j,i] <- tmp79[j,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp80: einsum('ijk,ijkl->ik', arrE, arrG))
tmp80 <- array(0, dim=c(dim(arrE)[1], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp80[i,k] <- tmp80[i,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp81: einsum('ijk,ijkl->ki', arrE, arrG))
tmp81 <- array(0, dim=c(dim(arrE)[3], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp81[k,i] <- tmp81[k,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp82: einsum('ijk,ijkl->il', arrE, arrG))
tmp82 <- array(0, dim=c(dim(arrE)[1], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp82[i,l] <- tmp82[i,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp83: einsum('ijk,ijkl->li', arrE, arrG))
tmp83 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp83[l,i] <- tmp83[l,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp84: einsum('ijk,ijkl->jk', arrE, arrG))
tmp84 <- array(0, dim=c(dim(arrE)[2], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp84[j,k] <- tmp84[j,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp85: einsum('ijk,ijkl->kj', arrE, arrG))
tmp85 <- array(0, dim=c(dim(arrE)[3], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp85[k,j] <- tmp85[k,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp86: einsum('ijk,ijkl->jl', arrE, arrG))
tmp86 <- array(0, dim=c(dim(arrE)[2], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp86[j,l] <- tmp86[j,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp87: einsum('ijk,ijkl->lj', arrE, arrG))
tmp87 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp87[l,j] <- tmp87[l,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp88: einsum('ijk,ijkl->kl', arrE, arrG))
tmp88 <- array(0, dim=c(dim(arrE)[3], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp88[k,l] <- tmp88[k,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp89: einsum('ijk,ijkl->lk', arrE, arrG))
tmp89 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp89[l,k] <- tmp89[l,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp90: einsum('ijk,ijkl->ijk', arrE, arrG))
tmp90 <- array(0, dim=c(dim(arrE)[1], dim(arrE)[2], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp90[i,j,k] <- tmp90[i,j,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp91: einsum('ijk,ijkl->ikj', arrE, arrG))
tmp91 <- array(0, dim=c(dim(arrE)[1], dim(arrE)[3], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp91[i,k,j] <- tmp91[i,k,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp92: einsum('ijk,ijkl->jik', arrE, arrG))
tmp92 <- array(0, dim=c(dim(arrE)[2], dim(arrE)[1], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp92[j,i,k] <- tmp92[j,i,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp93: einsum('ijk,ijkl->jki', arrE, arrG))
tmp93 <- array(0, dim=c(dim(arrE)[2], dim(arrE)[3], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp93[j,k,i] <- tmp93[j,k,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp94: einsum('ijk,ijkl->kij', arrE, arrG))
tmp94 <- array(0, dim=c(dim(arrE)[3], dim(arrE)[1], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp94[k,i,j] <- tmp94[k,i,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp95: einsum('ijk,ijkl->kji', arrE, arrG))
tmp95 <- array(0, dim=c(dim(arrE)[3], dim(arrE)[2], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp95[k,j,i] <- tmp95[k,j,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp96: einsum('ijk,ijkl->ijl', arrE, arrG))
tmp96 <- array(0, dim=c(dim(arrE)[1], dim(arrE)[2], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp96[i,j,l] <- tmp96[i,j,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp97: einsum('ijk,ijkl->ilj', arrE, arrG))
tmp97 <- array(0, dim=c(dim(arrE)[1], dim(arrG)[4], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp97[i,l,j] <- tmp97[i,l,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp98: einsum('ijk,ijkl->jil', arrE, arrG))
tmp98 <- array(0, dim=c(dim(arrE)[2], dim(arrE)[1], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp98[j,i,l] <- tmp98[j,i,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp99: einsum('ijk,ijkl->jli', arrE, arrG))
tmp99 <- array(0, dim=c(dim(arrE)[2], dim(arrG)[4], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp99[j,l,i] <- tmp99[j,l,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp100: einsum('ijk,ijkl->lij', arrE, arrG))
tmp100 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[1], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp100[l,i,j] <- tmp100[l,i,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp101: einsum('ijk,ijkl->lji', arrE, arrG))
tmp101 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[2], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp101[l,j,i] <- tmp101[l,j,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp102: einsum('ijk,ijkl->ikl', arrE, arrG))
tmp102 <- array(0, dim=c(dim(arrE)[1], dim(arrE)[3], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp102[i,k,l] <- tmp102[i,k,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp103: einsum('ijk,ijkl->ilk', arrE, arrG))
tmp103 <- array(0, dim=c(dim(arrE)[1], dim(arrG)[4], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp103[i,l,k] <- tmp103[i,l,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp104: einsum('ijk,ijkl->kil', arrE, arrG))
tmp104 <- array(0, dim=c(dim(arrE)[3], dim(arrE)[1], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp104[k,i,l] <- tmp104[k,i,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp105: einsum('ijk,ijkl->kli', arrE, arrG))
tmp105 <- array(0, dim=c(dim(arrE)[3], dim(arrG)[4], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp105[k,l,i] <- tmp105[k,l,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp106: einsum('ijk,ijkl->lik', arrE, arrG))
tmp106 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[1], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp106[l,i,k] <- tmp106[l,i,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp107: einsum('ijk,ijkl->lki', arrE, arrG))
tmp107 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[3], dim(arrE)[1]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp107[l,k,i] <- tmp107[l,k,i] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp108: einsum('ijk,ijkl->jkl', arrE, arrG))
tmp108 <- array(0, dim=c(dim(arrE)[2], dim(arrE)[3], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp108[j,k,l] <- tmp108[j,k,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp109: einsum('ijk,ijkl->jlk', arrE, arrG))
tmp109 <- array(0, dim=c(dim(arrE)[2], dim(arrG)[4], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp109[j,l,k] <- tmp109[j,l,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp110: einsum('ijk,ijkl->kjl', arrE, arrG))
tmp110 <- array(0, dim=c(dim(arrE)[3], dim(arrE)[2], dim(arrG)[4]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp110[k,j,l] <- tmp110[k,j,l] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp111: einsum('ijk,ijkl->klj', arrE, arrG))
tmp111 <- array(0, dim=c(dim(arrE)[3], dim(arrG)[4], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp111[k,l,j] <- tmp111[k,l,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp112: einsum('ijk,ijkl->ljk', arrE, arrG))
tmp112 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[2], dim(arrE)[3]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp112[l,j,k] <- tmp112[l,j,k] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

# tmp113: einsum('ijk,ijkl->lkj', arrE, arrG))
tmp113 <- array(0, dim=c(dim(arrG)[4], dim(arrE)[3], dim(arrE)[2]))
for(i in 1:dim(arrE)[1]){
	for(j in 1:dim(arrE)[2]){
		for(k in 1:dim(arrE)[3]){
			for(l in 1:dim(arrG)[4]){
				tmp113[l,k,j] <- tmp113[l,k,j] + arrE[i,j,k] * arrG[i,j,k,l]
			}
		}
	}
}

#
# einsum (sum + multiply + transpose, three tensor)
#
# tmp114: einsum('i,ij,ijk->', arrA, arrD, arrF)
tmp114 <- 0
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp114 <- tmp114 + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp115: einsum('i,ij,ijk->i', arrA, arrD, arrF)
tmp115 <- rep(0, dim(arrF)[1])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp115[i] <- tmp115[i] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp116: einsum('i,ij,ijk->j', arrA, arrD, arrF)
tmp116 <- rep(0, dim(arrF)[2])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp116[j] <- tmp116[j] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp117: einsum('i,ij,ijk->k', arrA, arrD, arrF)
tmp117 <- rep(0, dim(arrF)[3])
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp117[k] <- tmp117[k] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp118: einsum('i,ij,ijk->ij', arrA, arrD, arrF)
tmp118 <- array(0, dim=c(dim(arrF)[1], dim(arrF)[2]))
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp118[i,j] <- tmp118[i,j] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp119: einsum('i,ij,ijk->ik', arrA, arrD, arrF)
tmp119 <- array(0, dim=c(dim(arrF)[1], dim(arrF)[3]))
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp119[i,k] <- tmp119[i,k] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp120: einsum('i,ij,ijk->ji', arrA, arrD, arrF)
tmp120 <- array(0, dim=c(dim(arrF)[2], dim(arrF)[1]))
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp120[j,i] <- tmp120[j,i] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp121: einsum('i,ij,ijk->jk', arrA, arrD, arrF)
tmp121 <- array(0, dim=c(dim(arrF)[2], dim(arrF)[3]))
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp121[j,k] <- tmp121[j,k] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp122: einsum('i,ij,ijk->ki', arrA, arrD, arrF)
tmp122 <- array(0, dim=c(dim(arrF)[3], dim(arrF)[1]))
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp122[k,i] <- tmp122[k,i] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp123: einsum('i,ij,ijk->kj', arrA, arrD, arrF)
tmp123 <- array(0, dim=c(dim(arrF)[3], dim(arrF)[2]))
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp123[k,j] <- tmp123[k,j] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp124: einsum('i,ij,ijk->ijk', arrA, arrD, arrF)
tmp124 <- array(0, dim=c(dim(arrF)[1], dim(arrF)[2], dim(arrF)[3]))
for(i in 1:dim(arrF)[1]){
	for(j in 1:dim(arrF)[2]){
		for(k in 1:dim(arrF)[3]){
			tmp124[i,j,k] <- tmp124[i,j,k] + arrA[i] * arrD[i,j] * arrF[i,j,k]
		}
	}
}

# tmp125: einsum('i,ij,ijk,ijkl->ijkl', arrA, arrD, arrF, arrH)
tmp125 <- array(0, dim=dim(arrH))
for(i in 1:dim(arrH)[1]){
	for(j in 1:dim(arrH)[2]){
		for(k in 1:dim(arrH)[3]){
			for(l in 1:dim(arrH)[4]){
				tmp125[i,j,k,l] <- tmp125[i,j,k,l] + arrA[i] * arrD[i,j] * arrF[i,j,k] * arrH[i,j,k,l]
			}
		}
	}
}
