A <- array(runif(3), dim=c(3))
B <- array(runif(3), dim=c(3))
C <- array(runif(3*3), dim=c(3,3))
D <- array(runif(3*4), dim=c(3,4))
E <- array(runif(3*3*3), dim=c(3,3,3))
F <- array(runif(3*4*5), dim=c(3,4,5))
G <- array(runif(3*3*3*3), dim=c(3,3,3,3))
H <- array(runif(3*4*5*6), dim=c(3,4,5,6))

#
# einsum (sum + permutation)
#
# tmp1: einsum('ijk->i', F)
tmp1 <- rep(0, dim(F)[1])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp1[i] <- tmp1[i] + F[i,j,k]
		}
	}
}

# tmp2: einsum('ijk->j', F)
tmp2 <- rep(0, dim(F)[2])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp2[j] <- tmp2[j] + F[i,j,k]
		}
	}
}

# tmp3: einsum('ijk->k', F)
tmp3 <- rep(0, dim(F)[3])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp3[k] <- tmp3[k] + F[i,j,k]
		}
	}
}

# tmp4: einsum('ijk->ij', F)
tmp4 <- array(0, dim=dim(F)[c(1,2)])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp4[i,j] <- tmp4[i,j] + F[i,j,k]
		}
	}
}

# tmp5: einsum('ijk->jk', F)
tmp5 <- array(0, dim=dim(F)[c(2,3)])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp5[j,k] <- tmp5[j,k] + F[i,j,k]
		}
	}
}

# tmp6: einsum('ijk->ik', F)
tmp6 <- array(0, dim=dim(F)[c(1,3)])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp6[i,k] <- tmp6[i,k] + F[i,j,k]
		}
	}
}

# tmp7: einsum('ijkl->i', G)
tmp7 <- rep(0, length=dim(G)[1])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp7[i] <- tmp7[i] + G[i,j,k,l]
			}
		}
	}
}

# tmp8: einsum('ijkl->j', G)
tmp8 <- rep(0, length=dim(G)[2])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp8[j] <- tmp8[j] + G[i,j,k,l]
			}
		}
	}
}

# tmp9: einsum('ijkl->k', G)
tmp9 <- rep(0, length=dim(G)[3])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp9[k] <- tmp9[k] + G[i,j,k,l]
			}
		}
	}
}

# tmp10: einsum('ijkl->l', G)
tmp10 <- rep(0, length=dim(G)[4])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp10[l] <- tmp10[l] + G[i,j,k,l]
			}
		}
	}
}

# tmp11: einsum('ijkl->ij', G)
tmp11 <- array(0, dim=dim(G)[c(1,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp11[i,j] <- tmp11[i,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp12: einsum('ijkl->ik', G)
tmp12 <- array(0, dim=dim(G)[c(1,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp12[i,k] <- tmp12[i,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp13: einsum('ijkl->il', G)
tmp13 <- array(0, dim=dim(G)[c(1,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp13[i,l] <- tmp13[i,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp14: einsum('ijkl->ji', G)
tmp14 <- array(0, dim=dim(G)[c(2,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp14[j,i] <- tmp14[j,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp15: einsum('ijkl->jk', G)
tmp15 <- array(0, dim=dim(G)[c(2,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp15[j,k] <- tmp15[j,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp16: einsum('ijkl->jl', G)
tmp16 <- array(0, dim=dim(G)[c(2,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp16[j,l] <- tmp16[j,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp17: einsum('ijkl->ki', G)
tmp17 <- array(0, dim=dim(G)[c(3,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp17[k,i] <- tmp17[k,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp18: einsum('ijkl->kj', G)
tmp18 <- array(0, dim=dim(G)[c(3,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp18[k,j] <- tmp18[k,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp19: einsum('ijkl->kl', G)
tmp19 <- array(0, dim=dim(G)[c(3,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp19[k,l] <- tmp19[k,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp20: einsum('ijkl->li', G)
tmp20 <- array(0, dim=dim(G)[c(4,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp20[l,i] <- tmp20[l,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp21: einsum('ijkl->lj', G)
tmp21 <- array(0, dim=dim(G)[c(4,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp21[l,j] <- tmp21[l,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp22: einsum('ijkl->lk', G)
tmp22 <- array(0, dim=dim(G)[c(4,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp22[l,k] <- tmp22[l,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp23: einsum('ijkl->ijk', G)
tmp23 <- array(0, dim=dim(G)[c(1,2,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp23[i,j,k] <- tmp23[i,j,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp24: einsum('ijkl->ijl', G)
tmp24 <- array(0, dim=dim(G)[c(1,2,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp24[i,j,l] <- tmp24[i,j,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp25: einsum('ijkl->ikj', G)
tmp25 <- array(0, dim=dim(G)[c(1,3,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp25[i,k,j] <- tmp25[i,k,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp26: einsum('ijkl->ikl', G)
tmp26 <- array(0, dim=dim(G)[c(1,3,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp26[i,k,l] <- tmp26[i,k,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp27: einsum('ijkl->ilj', G)
tmp27 <- array(0, dim=dim(G)[c(1,4,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp27[i,l,j] <- tmp27[i,l,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp28: einsum('ijkl->ilk', G)
tmp28 <- array(0, dim=dim(G)[c(1,4,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp28[i,l,k] <- tmp28[i,l,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp29: einsum('ijkl->jik', G)
tmp29 <- array(0, dim=dim(G)[c(2,1,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp29[j,i,k] <- tmp29[j,i,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp30: einsum('ijkl->jil', G)
tmp30 <- array(0, dim=dim(G)[c(2,1,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp30[j,i,l] <- tmp30[j,i,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp31: einsum('ijkl->jki', G)
tmp31 <- array(0, dim=dim(G)[c(2,3,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp31[j,k,i] <- tmp31[j,k,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp32: einsum('ijkl->jkl', G)
tmp32 <- array(0, dim=dim(G)[c(2,3,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp32[j,k,l] <- tmp32[j,k,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp33: einsum('ijkl->jli', G)
tmp33 <- array(0, dim=dim(G)[c(2,4,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp33[j,l,i] <- tmp33[j,l,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp34: einsum('ijkl->jlk', G)
tmp34 <- array(0, dim=dim(G)[c(2,4,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp34[j,l,k] <- tmp34[j,l,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp35: einsum('ijkl->kij', G)
tmp35 <- array(0, dim=dim(G)[c(3,1,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp35[k,i,j] <- tmp35[k,i,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp36: einsum('ijkl->kil', G)
tmp36 <- array(0, dim=dim(G)[c(3,1,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp36[k,i,l] <- tmp36[k,i,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp37: einsum('ijkl->kji', G)
tmp37 <- array(0, dim=dim(G)[c(3,2,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp37[k,j,i] <- tmp37[k,j,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp38: einsum('ijkl->kjl', G)
tmp38 <- array(0, dim=dim(G)[c(3,2,4)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp38[k,j,l] <- tmp38[k,j,l] + G[i,j,k,l]
			}
		}
	}
}

# tmp39: einsum('ijkl->kli', G)
tmp39 <- array(0, dim=dim(G)[c(3,4,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp39[k,l,i] <- tmp39[k,l,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp40: einsum('ijkl->klj', G)
tmp40 <- array(0, dim=dim(G)[c(3,4,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp40[k,l,j] <- tmp40[k,l,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp41: einsum('ijkl->lij', G)
tmp41 <- array(0, dim=dim(G)[c(4,1,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp41[l,i,j] <- tmp41[l,i,j] + G[i,j,k,l]
			}
		}
	}
}

# tmp42: einsum('ijkl->lik', G)
tmp42 <- array(0, dim=dim(G)[c(4,1,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp42[l,i,k] <- tmp42[l,i,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp43: einsum('ijkl->lji', G)
tmp43 <- array(0, dim=dim(G)[c(4,2,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp43[l,j,i] <- tmp43[l,j,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp44: einsum('ijkl->ljk', G)
tmp44 <- array(0, dim=dim(G)[c(4,2,3)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp44[l,j,k] <- tmp44[l,j,k] + G[i,j,k,l]
			}
		}
	}
}

# tmp45: einsum('ijkl->lki', G)
tmp45 <- array(0, dim=dim(G)[c(4,3,1)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp45[l,k,i] <- tmp45[l,k,i] + G[i,j,k,l]
			}
		}
	}
}

# tmp46: einsum('ijkl->lkj', G)
tmp46 <- array(0, dim=dim(G)[c(4,3,2)])
for(i in 1:dim(G)[1]){
	for(j in 1:dim(G)[2]){
		for(k in 1:dim(G)[3]){
			for(l in 1:dim(G)[4]){
				tmp46[l,k,j] <- tmp46[l,k,j] + G[i,j,k,l]
			}
		}
	}
}

#
# einsum (multiply, diagonal)
#
# tmp47: einsum('iii->i', E)
tmp47 <- rep(0, dim(E)[1])
for(i in 1:dim(E)[1]){
	tmp47[i] <- tmp47[i] + E[i,i,i]
}

# tmp48: einsum('iiii->i', G)
tmp48 <- rep(0, dim(G)[1])
for(i in 1:dim(G)[1]){
	tmp48[i] <- tmp48[i] + G[i,i,i,i]
}

#
# einsum (sum + multiply + transpose)
#
# tmp49: einsum('ij,jk->j', C, D)
tmp49 <- rep(0, dim(C)[2])
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp49[j] <- tmp49[j] + C[i,j] * D[j,k]
		}
	}
}

# tmp50: einsum('ij,jk->ij', C, D))
tmp50 <- array(0, dim=c(dim(C)[1], dim(C)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp50[i,j] <- tmp50[i,j] + C[i,j] * D[j,k]
		}
	}
}

# tmp51: einsum('ij,jk->ji', C, D))
tmp51 <- array(0, dim=c(dim(C)[2], dim(C)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp51[j,i] <- tmp51[j,i] + C[i,j] * D[j,k]
		}
	}
}

# tmp52: einsum('ij,jk->jk', C, D))
tmp52 <- array(0, dim=c(dim(C)[2], dim(D)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp52[j,k] <- tmp52[j,k] + C[i,j] * D[j,k]
		}
	}
}

# tmp53: einsum('ij,jk->kj', C, D))
tmp53 <- array(0, dim=c(dim(D)[2], dim(D)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp53[k,j] <- tmp53[k,j] + C[i,j] * D[j,k]
		}
	}
}

# tmp54: einsum('ij,jk->ik', C, D))
tmp54 <- array(0, dim=c(dim(C)[1], dim(D)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp54[i,k] <- tmp54[i,k] + C[i,j] * D[j,k]
		}
	}
}

# tmp55: einsum('ij,jk->ki', C, D))
tmp55 <- array(0, dim=c(dim(D)[2], dim(C)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp55[k,i] <- tmp55[k,i] + C[i,j] * D[j,k]
		}
	}
}

# tmp56: einsum('ij,jk->ijk', C, D))
tmp56 <- array(0, dim=c(dim(C)[1], dim(C)[2], dim(D)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp56[i,j,k] <- tmp56[i,j,k] + C[i,j] * D[j,k]
		}
	}
}

# tmp57: einsum('ij,jk->ikj', C, D))
tmp57 <- array(0, dim=c(dim(C)[1], dim(D)[2], dim(D)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp57[i,k,j] <- tmp57[i,k,j] + C[i,j] * D[j,k]
		}
	}
}

# tmp58: einsum('ij,jk->jik', C, D))
tmp58 <- array(0, dim=c(dim(C)[2], dim(C)[1], dim(D)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp58[j,i,k] <- tmp58[j,i,k] + C[i,j] * D[j,k]
		}
	}
}

# tmp59: einsum('ij,jk->jki', C, D))
tmp59 <- array(0, dim=c(dim(C)[2], dim(D)[2], dim(C)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp59[j,k,i] <- tmp59[j,k,i] + C[i,j] * D[j,k]
		}
	}
}

# tmp60: einsum('ij,jk->kij', C, D))
tmp60 <- array(0, dim=c(dim(D)[2], dim(C)[1], dim(C)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp60[k,i,j] <- tmp60[k,i,j] + C[i,j] * D[j,k]
		}
	}
}

# tmp61: einsum('ij,jk->kji', C, D))
tmp61 <- array(0, dim=c(dim(D)[2], dim(C)[2], dim(C)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(D)[2]){
			tmp61[k,j,i] <- tmp61[k,j,i] + C[i,j] * D[j,k]
		}
	}
}

# tmp62: einsum('ij,ijk->', C, E))
tmp62 <- 0
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp62 <- tmp62 + C[i,j] * E[i,j,k]
		}
	}
}

# tmp63: einsum('ij,ijk->i', C, E))
tmp63 <- rep(0, dim(C)[1])
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp63[i] <- tmp63[i] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp64: einsum('ij,ijk->j', C, E))
tmp64 <- rep(0, dim(C)[2])
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp64[j] <- tmp64[j] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp65: einsum('ij,ijk->k', C, E))
tmp65 <- rep(0, dim(E)[2])
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp65[k] <- tmp65[k] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp66: einsum('ij,ijk->ij', C, E))
tmp66 <- array(0, dim=c(dim(C)[1], dim(C)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp66[i,j] <- tmp66[i,j] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp67: einsum('ij,ijk->ji', C, E))
tmp67 <- array(0, dim=c(dim(C)[2], dim(C)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp67[j,i] <- tmp67[j,i] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp68: einsum('ij,ijk->jk', C, E))
tmp68 <- array(0, dim=c(dim(C)[2], dim(E)[3]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp68[j,k] <- tmp68[j,k] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp69: einsum('ij,ijk->kj', C, E))
tmp69 <- array(0, dim=c(dim(E)[3], dim(C)[2]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp69[k,j] <- tmp69[k,j] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp70: einsum('ij,ijk->ik', C, E))
tmp70 <- array(0, dim=c(dim(C)[1], dim(E)[3]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp70[i,k] <- tmp70[i,k] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp71: einsum('ij,ijk->ki', C, E))
tmp71 <- array(0, dim=c(dim(E)[3], dim(C)[1]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp71[k,i] <- tmp71[k,i] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp72: einsum('ij,ijk->ijk', C, E))
tmp72 <- array(0, dim=c(dim(C)[1], dim(C)[2], dim(E)[3]))
for(i in 1:dim(C)[1]){
	for(j in 1:dim(C)[2]){
		for(k in 1:dim(E)[2]){
			tmp72[i,j,k] <- tmp72[i,j,k] + C[i,j] * E[i,j,k]
		}
	}
}

# tmp73: einsum('ijk,ijkl->', E, G))
tmp73 <- 0
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp73 <- tmp73 + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp74: einsum('ijk,ijkl->i', E, G))
tmp74 <- rep(0, dim(E)[1])
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp74[i] <- tmp74[i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp75: einsum('ijk,ijkl->j', E, G))
tmp75 <- rep(0, dim(E)[2])
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp75[j] <- tmp75[j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp76: einsum('ijk,ijkl->k', E, G))
tmp76 <- rep(0, dim(E)[3])
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp76[k] <- tmp76[k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp77: einsum('ijk,ijkl->l', E, G))
tmp77 <- rep(0, dim(G)[4])
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp77[l] <- tmp77[l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp78: einsum('ijk,ijkl->ij', E, G))
tmp78 <- array(0, dim=c(dim(E)[1], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp78[i,j] <- tmp78[i,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp79: einsum('ijk,ijkl->ji', E, G))
tmp79 <- array(0, dim=c(dim(E)[2], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp79[j,i] <- tmp79[j,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp80: einsum('ijk,ijkl->ik', E, G))
tmp80 <- array(0, dim=c(dim(E)[1], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp80[i,k] <- tmp80[i,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp81: einsum('ijk,ijkl->ki', E, G))
tmp81 <- array(0, dim=c(dim(E)[3], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp81[k,i] <- tmp81[k,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp82: einsum('ijk,ijkl->il', E, G))
tmp82 <- array(0, dim=c(dim(E)[1], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp82[i,l] <- tmp82[i,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp83: einsum('ijk,ijkl->li', E, G))
tmp83 <- array(0, dim=c(dim(G)[4], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp83[l,i] <- tmp83[l,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp84: einsum('ijk,ijkl->jk', E, G))
tmp84 <- array(0, dim=c(dim(E)[2], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp84[j,k] <- tmp84[j,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp85: einsum('ijk,ijkl->kj', E, G))
tmp85 <- array(0, dim=c(dim(E)[3], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp85[k,j] <- tmp85[k,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp86: einsum('ijk,ijkl->jl', E, G))
tmp86 <- array(0, dim=c(dim(E)[2], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp86[j,l] <- tmp86[j,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp87: einsum('ijk,ijkl->lj', E, G))
tmp87 <- array(0, dim=c(dim(G)[4], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp87[l,j] <- tmp87[l,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp88: einsum('ijk,ijkl->kl', E, G))
tmp88 <- array(0, dim=c(dim(E)[3], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp88[k,l] <- tmp88[k,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp89: einsum('ijk,ijkl->lk', E, G))
tmp89 <- array(0, dim=c(dim(G)[4], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp89[l,k] <- tmp89[l,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp90: einsum('ijk,ijkl->ijk', E, G))
tmp90 <- array(0, dim=c(dim(E)[1], dim(E)[2], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp90[i,j,k] <- tmp90[i,j,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp91: einsum('ijk,ijkl->ikj', E, G))
tmp91 <- array(0, dim=c(dim(E)[1], dim(E)[3], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp91[i,k,j] <- tmp91[i,k,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp92: einsum('ijk,ijkl->jik', E, G))
tmp92 <- array(0, dim=c(dim(E)[2], dim(E)[1], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp92[j,i,k] <- tmp92[j,i,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp93: einsum('ijk,ijkl->jki', E, G))
tmp93 <- array(0, dim=c(dim(E)[2], dim(E)[3], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp93[j,k,i] <- tmp93[j,k,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp94: einsum('ijk,ijkl->kij', E, G))
tmp94 <- array(0, dim=c(dim(E)[3], dim(E)[1], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp94[k,i,j] <- tmp94[k,i,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp95: einsum('ijk,ijkl->kji', E, G))
tmp95 <- array(0, dim=c(dim(E)[3], dim(E)[2], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp95[k,j,i] <- tmp95[k,j,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp96: einsum('ijk,ijkl->ijl', E, G))
tmp96 <- array(0, dim=c(dim(E)[1], dim(E)[2], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp96[i,j,l] <- tmp96[i,j,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp97: einsum('ijk,ijkl->ilj', E, G))
tmp97 <- array(0, dim=c(dim(E)[1], dim(G)[4], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp97[i,l,j] <- tmp97[i,l,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp98: einsum('ijk,ijkl->jil', E, G))
tmp98 <- array(0, dim=c(dim(E)[2], dim(E)[1], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp98[j,i,l] <- tmp98[j,i,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp99: einsum('ijk,ijkl->jli', E, G))
tmp99 <- array(0, dim=c(dim(E)[2], dim(G)[4], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp99[j,l,i] <- tmp99[j,l,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp100: einsum('ijk,ijkl->lij', E, G))
tmp100 <- array(0, dim=c(dim(G)[4], dim(E)[1], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp100[l,i,j] <- tmp100[l,i,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp101: einsum('ijk,ijkl->lji', E, G))
tmp101 <- array(0, dim=c(dim(G)[4], dim(E)[2], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp101[l,j,i] <- tmp101[l,j,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp102: einsum('ijk,ijkl->ikl', E, G))
tmp102 <- array(0, dim=c(dim(E)[1], dim(E)[3], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp102[i,k,l] <- tmp102[i,k,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp103: einsum('ijk,ijkl->ilk', E, G))
tmp103 <- array(0, dim=c(dim(E)[1], dim(G)[4], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp103[i,l,k] <- tmp103[i,l,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp104: einsum('ijk,ijkl->kil', E, G))
tmp104 <- array(0, dim=c(dim(E)[3], dim(E)[1], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp104[k,i,l] <- tmp104[k,i,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp105: einsum('ijk,ijkl->kli', E, G))
tmp105 <- array(0, dim=c(dim(E)[3], dim(G)[4], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp105[k,l,i] <- tmp105[k,l,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp106: einsum('ijk,ijkl->lik', E, G))
tmp106 <- array(0, dim=c(dim(G)[4], dim(E)[1], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp106[l,i,k] <- tmp106[l,i,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp107: einsum('ijk,ijkl->lki', E, G))
tmp107 <- array(0, dim=c(dim(G)[4], dim(E)[3], dim(E)[1]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp107[l,k,i] <- tmp107[l,k,i] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp108: einsum('ijk,ijkl->jkl', E, G))
tmp108 <- array(0, dim=c(dim(E)[2], dim(E)[3], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp108[j,k,l] <- tmp108[j,k,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp109: einsum('ijk,ijkl->jlk', E, G))
tmp109 <- array(0, dim=c(dim(E)[2], dim(G)[4], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp109[j,l,k] <- tmp109[j,l,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp110: einsum('ijk,ijkl->kjl', E, G))
tmp110 <- array(0, dim=c(dim(E)[3], dim(E)[2], dim(G)[4]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp110[k,j,l] <- tmp110[k,j,l] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp111: einsum('ijk,ijkl->klj', E, G))
tmp111 <- array(0, dim=c(dim(E)[3], dim(G)[4], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp111[k,l,j] <- tmp111[k,l,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp112: einsum('ijk,ijkl->ljk', E, G))
tmp112 <- array(0, dim=c(dim(G)[4], dim(E)[2], dim(E)[3]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp112[l,j,k] <- tmp112[l,j,k] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

# tmp113: einsum('ijk,ijkl->lkj', E, G))
tmp113 <- array(0, dim=c(dim(G)[4], dim(E)[3], dim(E)[2]))
for(i in 1:dim(E)[1]){
	for(j in 1:dim(E)[2]){
		for(k in 1:dim(E)[3]){
			for(l in 1:dim(G)[4]){
				tmp113[l,k,j] <- tmp113[l,k,j] + E[i,j,k] * G[i,j,k,l]
			}
		}
	}
}

#
# einsum (sum + multiply + transpose, three tensor)
#
# tmp114: einsum('i,ij,ijk->', A, D, F)
tmp114 <- 0
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp114 <- tmp114 + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp115: einsum('i,ij,ijk->i', A, D, F)
tmp115 <- rep(0, dim(F)[1])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp115[i] <- tmp115[i] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp116: einsum('i,ij,ijk->j', A, D, F)
tmp116 <- rep(0, dim(F)[2])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp116[j] <- tmp116[j] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp117: einsum('i,ij,ijk->k', A, D, F)
tmp117 <- rep(0, dim(F)[3])
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp117[k] <- tmp117[k] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp118: einsum('i,ij,ijk->ij', A, D, F)
tmp118 <- array(0, dim=c(dim(F)[1], dim(F)[2]))
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp118[i,j] <- tmp118[i,j] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp119: einsum('i,ij,ijk->ik', A, D, F)
tmp119 <- array(0, dim=c(dim(F)[1], dim(F)[3]))
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp119[i,k] <- tmp119[i,k] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp120: einsum('i,ij,ijk->ji', A, D, F)
tmp120 <- array(0, dim=c(dim(F)[2], dim(F)[1]))
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp120[j,i] <- tmp120[j,i] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp121: einsum('i,ij,ijk->jk', A, D, F)
tmp121 <- array(0, dim=c(dim(F)[2], dim(F)[3]))
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp121[j,k] <- tmp121[j,k] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp122: einsum('i,ij,ijk->ki', A, D, F)
tmp122 <- array(0, dim=c(dim(F)[3], dim(F)[1]))
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp122[k,i] <- tmp122[k,i] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp123: einsum('i,ij,ijk->kj', A, D, F)
tmp123 <- array(0, dim=c(dim(F)[3], dim(F)[2]))
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp123[k,j] <- tmp123[k,j] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}

# tmp124: einsum('i,ij,ijk->ijk', A, D, F)
tmp124 <- array(0, dim=c(dim(F)[1], dim(F)[2], dim(F)[3]))
for(i in 1:dim(F)[1]){
	for(j in 1:dim(F)[2]){
		for(k in 1:dim(F)[3]){
			tmp124[i,j,k] <- tmp124[i,j,k] + A[i] * D[i,j] * F[i,j,k]
		}
	}
}
