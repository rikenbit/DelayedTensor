# C <- DelayedArray(array(runif(3*3), dim=c(3,3)))
# D <- DelayedArray(array(runif(3*4), dim=c(3,4)))

# tmp50 <- DelayedArray(array(0, dim=c(dim(C)[1], dim(C)[2])))
# for(i in 1:dim(C)[1]){
# 	for(j in 1:dim(C)[2]){
# 		for(k in 1:dim(D)[2]){
# 			tmp50[i,j] <- C[i,j] * D[j,k]
# 		}
# 	}
# }
# out <- as(realize(tmp50, "HDF5Array"), "DelayedArray")

# einsum('ij,jk->ij', C, D)




# library("DelayedArray")
# library("HDF5Array")

# .sarray <- function(dim){
# 	dim <- as.integer(dim)
# 	setAutoRealizationBackend("HDF5Array")
# 	sink <- AutoRealizationSink(dim, as.sparse=TRUE)
# 	close(sink)
# 	as(sink, "DelayedArray")
# }

# # load("../data/human_mid_brain.rda")
# # load("../data/mouse_mid_brain.rda")

# human <- matrix(runif(30*40), nrow=30)
# mouse <- matrix(runif(30*50), nrow=30)

# new_modes <- c(ncol(human), ncol(mouse), nrow(human))
# darr <- .sarray(new_modes)

# for(i in seq(dim(darr)[3])){
# 	print(paste0(i, " / ", dim(darr)[3]))
# 	outer_slice <- outer(human[i,], mouse[i,])
# 	darr[,,i] <- outer_slice
# 	darr <- realize(darr, "HDF5Array")
# 	darr <- as(darr, "DelayedArray")
# 	gc(reset=TRUE, verbose=FALSE)
# }
















# #
# # Utility function to merge multiple Tensor based on
# # Einstein's summation convention
# #
# # subscripts = "ijkl->ij"
# # operands = list(G)

# einsum <- function(subscripts, ...){
# 	# Preprocessing
# 	original_subscripts <- gsub(" ", "", subscripts)
# 	operands <- list(...)

# 	# Argument check
# 	no_operands <- length(operands)
# 	no_comma <- .no_comma(subscripts)
# 	if(no_comma == -1){
# 		no_comma <- 0
# 	}
# 	if(no_operands-1 != no_comma){
# 		stop(paste0("(No. of operands - 1) and (No. of ,) are ",
# 			no_operands-1, " and ", no_comma, " and different!"))
# 	}

# 	# Setting
# 	each_dims <- .s_to_d(subscripts, operands)
# 	# ->の左
# 	lfs <- .left_sub(subscripts)
# 	# ->の右
# 	rhs <- .right_sub(subscripts)
# 	# ->の左右で同じ添字セットになっているか
# 	check_left_right <- .check_left_right(lfs, rhs)
# 	# ->の左側を,で分割したもの
# 	split_subscripts <- strsplit(lfs, ",")[[1]]
# 	# ,があるのに共通する添字が無い場合計算できない
# 	.dup_check(split_subscripts)
# 	# ->の左側を1文字ずつ分割したもの
# 	split_subscripts_2 <- gsub(",", "", strsplit(lfs, "")[[1]])
# 	# ->の右側を1文字ずつ分割したもの
# 	split_rhs <- strsplit(rhs, "")[[1]]
# 	# ->があるか
# 	check_arrow <- .check_arrow(subscripts)
# 	# 並び替えるだけか
# 	check_perm <- .check_perm(no_comma, check_arrow, rhs, check_left_right)
# 	# Command
# 	if(check_perm){
# 		# Only permutation
# 		if(check_arrow){
# 			# e.g. ij -> ji
# 			print("### Pattern 1 ###")
# 			out <- .sortOperands(operands, split_rhs, each_dims)
# 			cmd <- ""
# 		}else{
# 			# e.g. ij
# 			print("### Pattern 2 ###")
# 			out <- .sortOperands(operands, split_subscripts_2, each_dims)
# 			cmd <- ""
# 		}
# 	}else{
# 		# Setting for calculation
# 		# commaがあるか
# 		check_comma <- .check_comma(subscripts)
# 		# 共通の添字（ij,jkの時のj）
# 		cm_subscripts <- .common_subscripts(check_comma, split_subscripts,
# 			split_subscripts_2, split_rhs)
# 		# 共通の添字（->の左右で共通するもの）
# 		cm_subscripts_2 <- .common_subscripts_2(split_subscripts_2, split_rhs)
# 		# 共通じゃない添字（ij,jkの時のiとj、for文の添字になる）
# 		sp_subscripts <- .specific_subscripts(check_comma,
# 			each_dims, cm_subscripts)
# 		# 共通じゃない添字（->の左右で共通しないもの）
# 		sp_subscripts_2 <- .specific_subscripts_2(check_arrow, check_comma,
# 			each_dims, cm_subscripts_2)
# 		# 和をとるか（forループの数と、全添字の数が合わないとき）
# 		check_sum <- .check_sum(each_dims, sp_subscripts)
# 		# 和をとるか（operandが一つしかなく、かつ->の左右で添字が減っているとき）
# 		check_sum_2 <- .check_sum_2(operands, sp_subscripts_2)
# 		# 対角だけとりだすか（iiii->iみたいなとき）
# 		check_diag <- .check_diag(each_dims, lfs, rhs, operands)
# 		# アダマード積をとるか
# 		check_hadamard <- .check_hadamard(operands, each_dims,
# 			split_subscripts, split_rhs, no_comma, check_sum)
# 		# 出力オブジェクトの次元
# 		outdims <- .outdims(split_subscripts_2, each_dims, split_rhs,
# 			no_comma, check_arrow)
# 		# Calculate something
# 		out <- .sarray(outdims)
# 		cmd <- .forLoop(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard)
# 	}
# 	# Output
# 	print(subscripts)
# 	print(cmd)
# 	eval(parse(text=cmd))
# 	cat(rep("\n", 2))
# 	out
# }

# # order of subscirpts
# .order_subs <- function(subscripts){
# 	left_right <- strsplit(subscripts, "->")[[1]]
# 	if(length(left_right) == 1){
# 		right <- ""
# 	}else{
# 		right <- paste0("->", left_right[2])
# 	}
# 	tmp <- gsub(right, "", subscripts)
# 	sep_subs <- strsplit(tmp, ",")[[1]]
# 	order_subs <- lapply(sep_subs, function(x){
# 		order(strsplit(x, "")[[1]])
# 	})
# 	new_subscripts <- paste0(
# 		paste(unlist(lapply(seq_along(order_subs), function(x){
# 			paste(strsplit(sep_subs[[x]], "")[[1]][order_subs[[x]]],
# 				collapse="")
# 		})), collapse=","), right)
# 	list(subscripts=new_subscripts, order_subs=order_subs)
# }

# # permutation of operands by order_subs
# .perm_operands <- function(operands, order_subs){
# 	lapply(seq_along(operands), function(x){
# 		aperm(operands[[x]], order_subs[[x]])
# 	})
# }

# # No. of comma
# .no_comma <- function(subscripts){
# 	tmp = strsplit(subscripts, "")[[1]]
# 	length(grep(",", tmp))
# }

# # subscripts -> dims
# .s_to_d <- function(subscripts, operands){
# 	odims <- unlist(lapply(operands, function(o){dim(o)}))
# 	sbs <- strsplit(gsub(",", "", .left_sub(subscripts)), "")[[1]]
# 	common.sbs <- sort(unique(sbs))
# 	out <- unlist(lapply(common.sbs, function(c){
# 		odims[which(sbs == c)[1]]
# 	}))
# 	names(out) <- common.sbs
# 	out
# }

# # left subscripts
# .left_sub <- function(subscripts){
# 	strsplit(subscripts, "->")[[1]][1]
# }

# # right subscripts
# .right_sub <- function(subscripts){
# 	strsplit(subscripts, "->")[[1]][2]
# }

# # whether left and right are same
# .check_left_right <- function(lfs, rhs){
# 	left <- strsplit(lfs, "")[[1]]
# 	right <- strsplit(rhs, "")[[1]]
# 	identical(sort(left), sort(right))
# }

# # duplicate subscripts checker
# .dup_check <- function(split_subscripts){
# 	if(length(split_subscripts) >= 2){
# 		sapply(seq(split_subscripts), function(idx){
# 			not_target <- setdiff(seq(split_subscripts), idx)
# 			dup_split_subscripts <- sapply(not_target, function(n){
# 				length(intersect(
# 					strsplit(split_subscripts[idx], "")[[1]],
# 					strsplit(split_subscripts[n], "")[[1]]
# 				))
# 			})
# 			if(max(dup_split_subscripts) == 0){
# 				stop(paste0("All operands must have at least one index",
# 					"with at least one operand."))
# 			}else{
# 				0
# 			}
# 		})
# 	}else{
# 		0
# 	}
# }

# # whether of check
# .check_arrow <- function(subscripts){
# 	length(grep("->", subscripts)) == 1
# }

# # whether only permutation
# # 1. ,無し、->が無し、lfsのみで、rhsが無し
# # 2. ,無し、->が有り、ソートしたらlfsとrhsが同じ
# .check_perm <- function(no_comma, check_arrow, rhs, check_left_right){
# 	check1 <- no_comma == 0 && !check_arrow && is.na(rhs)
# 	check2 <- no_comma == 0 && check_arrow && check_left_right
# 	check1 || check2
# }

# # Sort (e.g. ji)
# .sortOperands <- function(operands, split_subscripts_2, each_dims){
# 	orderO <- unlist(lapply(split_subscripts_2, function(x){
# 		which(x == names(each_dims))
# 	}))
# 	aperm(operands[[1]], orderO)
# }

# # whether at least one comma exists
# .check_comma <- function(subscripts){
# 	length(grep(",", subscripts)) != 0
# }

# # common subscripts (e.g. i,ij,ijk: i)
# .common_subscripts <- function(check_comma, split_subscripts,
# 	split_subscripts_2, split_rhs){
# 	if(check_comma){
# 		tbl <- table(unlist(strsplit(split_subscripts, "")))
# 		names(which(tbl == length(split_subscripts)))
# 	}else{
# 		NA
# 	}
# }

# # not common subscripts (e.g. i,ij,ijk: j,k)
# .specific_subscripts <- function(check_comma, each_dims, cm_subscripts){
# 	if(check_comma){
# 		setdiff(names(each_dims), cm_subscripts)
# 	}else{
# 		NA
# 	}
# }

# # common subscripts（e.g. ij->j: i）
# .common_subscripts_2 <- function(split_subscripts_2, split_rhs){
# 	if(is.na(split_rhs[1])){
# 		 NA
# 	}else{
# 		setdiff(
# 			split_subscripts_2[-which(split_subscripts_2 %in% "")],
# 			split_rhs)
# 	}
# }

# # not common subscripts（e.g. ij->j: j）
# .specific_subscripts_2 <- function(check_arrow, check_comma,
# 	each_dims, cm_subscripts_2){
# 	if(check_arrow || !check_comma){
# 		setdiff(names(each_dims), cm_subscripts_2)
# 	}else{
# 		NA
# 	}
# }

# # forループの数と、全添字の数が合わないとき
# .check_sum <- function(each_dims, sp_subscripts){
# 	!identical(sort(names(each_dims)), sp_subscripts)
# }

# # operandが一つしかなく、->の左右で添字が減っている時
# .check_sum_2 <- function(operands, sp_subscripts_2){
# 	check1 <- length(operands) == 1
# 	check2 <- length(sp_subscripts_2) != 0
# 	check1 && check2
# }

# # 対角要素を取り出すだけか
# .check_diag <- function(each_dims, lfs, rhs, operands){
# 	check1 <- length(grep(",", lfs)) == 0
# 	check2 <- all(length(each_dims) == 1)
# 	if(is.na(rhs)){
# 		check3 <- FALSE
# 	}else{
# 		check3 <- all(names(each_dims) == rhs)
# 	}
# 	check4 <- all(names(each_dims) == unique(strsplit(lfs, "")[[1]][1]))
# 	check5 <- all(each_dims == unique(dim(operands[[1]])[1]))
# 	check1 && check2 && check3 && check4 && check5
# }

# # whether perform hadamard product
# .check_hadamard <- function(operands, each_dims, split_subscripts,
# 	split_rhs, no_comma, check_sum){
# 	check1 <- no_comma >= 1
# 	check2 <- all(split_subscripts == split_subscripts[1])
# 	check1 && check2
# }

# # Output dimension
# .outdims <- function(split_subscripts_2, each_dims, split_rhs,
# 	no_comma, check_arrow){
# 	if(is.na(split_rhs[1])){
# 		if(no_comma == 0 && !check_arrow){
# 			# e.g. ii
# 			each_dims
# 		}else{
# 			1
# 		}
# 	}else{
# 		target <- unlist(lapply(split_rhs, function(x){
# 			which(names(each_dims) == x)
# 		}))
# 		each_dims[target]
# 	}
# }

# # for loop generator
# .forLoop <- function(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard){
# 	# e.g. for(i in seq(each_dims[1])){
# 	#          for(k in seq(each_dims[3])){
# 	cmd1 <- .cmd1(cm_subscripts, each_dims)
# 	# e.g. out[i,k] <- out[i,k] +
# 	cmd2 <- .cmd2(cm_subscripts, outdims)
# 	# e.g. sum(operands[[1]][i,] * operands[[2]][,k])
# 	cmd3 <- .cmd3(cm_subscripts, outdims, split_subscripts)
# 	# e.g. }}
# 	cmd4 <- .cmd4(length(setdiff(names(each_dims), cm_subscripts)))
# 	# Merge
# 	paste(c(cmd1, cmd2, cmd3, cmd4), collapse="")
# }

# # each_dimsのうち、cm_subscriptsを除いた添字でfor文を回す
# .cmd1 <- function(cm_subscripts, each_dims){
# 	iter <- each_dims[setdiff(names(each_dims), cm_subscripts)]
# 	target <- unlist(lapply(names(iter), function(x){
# 		which(names(each_dims) == x)
# 	}))
# 	paste(c("count <- 1;",
# 		"pb <- txtProgressBar(1, ", prod(iter), ", style=3);",
# 		unlist(lapply(seq_along(iter),
# 			function(idx, e){
# 				paste0("for(", names(iter[idx]),
# 				" in seq(each_dims[", target[idx], "])){")}, e=each_dims))
# 	), collapse=" ")
# }

# # 1. outの中にfor文で回す添字を入れる
# # 2. 共通添字は消す
# .cmd2 <- function(cm_subscripts, outdims){
# 	iter2 <- outdims[setdiff(names(outdims), cm_subscripts)]
# 	paste(c(
# 		"out[", paste(names(iter2), collapse=","),
# 		"] <- out[", paste(names(iter2), collapse=","), "] + "), collapse="")
# }

# #
# # outdimsが1ならsumをとる
# .cmd3 <- function(cm_subscripts, outdims, split_subscripts){
# 	tmp <- unlist(lapply(seq(split_subscripts), function(x){
# 		paste(c("operands[[", x, "]]",
# 			"[", .cmd3_helper(cm_subscripts, split_subscripts[x]), "]"),
# 			collapse="")
# 	}))
# 	tmp <- paste(tmp, collapse=" * ")

# 	iter2 <- outdims[setdiff(names(outdims), cm_subscripts)]
# 	if(length(iter2) == length(outdims)){
# 		tmp <- paste(c("sum(", tmp, ")"), collapse="")
# 	}
# 	tmp
# }

# .cmd3_helper <- function(subs, sps){
# 	tmp <- strsplit(sps, "")[[1]]
# 	if(is.na(subs)){
# 		tmp <- paste(tmp, collapse=",")
# 	}else{
# 		target <- unlist(lapply(subs, function(x){
# 			which(tmp == x)
# 		}))
# 		tmp[setdiff(seq(tmp), target)] <- ""
# 		if(length(tmp) == 1){
# 			if(nchar(sps)){
# 				tmp <- sps
# 			}else{
# 				tmp <- paste0(sps, ",")
# 			}
# 		}else{
# 			tmp <- paste0(tmp, collapse=",")
# 		}
# 	}
# 	tmp
# }


# # each_dimsのうち、cm_subscriptsを除いた添字の分だけ}を出力
# .cmd4 <- function(l){
# 	paste(rep("}", length=l), collapse="")
# }

# # + サイズが合わないoutやoperandsを並び直すヘルパー















# #
# # Utility function to merge multiple Tensor based on
# # Einstein's summation convention
# #
# # subscripts = 'ij,jk->i'
# # operands = list(C, D)

# einsum <- function(subscripts, ...){
# 	# Argument check
# 	subscripts <- gsub(" ", "", subscripts)
# 	operands <- list(...)
# 	no_operands <- length(operands)
# 	no_comma <- .no_comma(subscripts)
# 	if(no_comma == -1){
# 		no_comma <- 0
# 	}
# 	if(no_operands-1 != no_comma){
# 		stop(paste0("(No. of operands - 1) and (No. of ,) are ",
# 			no_operands-1, " and ", no_comma, " and different!"))
# 	}
# 	# Setting
# 	each_dims <- .s_to_d(subscripts, operands)
# 	# ->の左
# 	lfs <- .left_sub(subscripts)
# 	# ->の右
# 	rhs <- .right_sub(subscripts)
# 	# ->の左右で同じ添字セットになっているか
# 	check_left_right <- .check_left_right(lfs, rhs)
# 	# ->の左側を,で分割したもの
# 	split_subscripts <- strsplit(lfs, ",")[[1]]
# 	# ,があるのに共通する添字が無い場合計算できない
# 	.dup_check(split_subscripts)
# 	# ->の左側を1文字ずつ分割したもの
# 	split_subscripts_2 <- gsub(",", "", strsplit(lfs, "")[[1]])
# 	# ->の右側を1文字ずつ分割したもの
# 	split_rhs <- strsplit(rhs, "")[[1]]
# 	# ->があるか
# 	check_arrow <- .check_arrow(subscripts)
# 	# 並び替えるだけか
# 	check_perm <- .check_perm(no_comma, check_arrow, rhs, check_left_right)
# 	# Command
# 	if(check_perm){
# 		# Only permutation
# 		if(check_arrow){
# 			# e.g. ij -> ji
# 			print("### Pattern 1 ###")
# 			out <- .sortOperands(operands, split_rhs, each_dims)
# 			cmd <- ""
# 		}else{
# 			# e.g. ij
# 			print("### Pattern 2 ###")
# 			out <- .sortOperands(operands, split_subscripts_2, each_dims)
# 			cmd <- ""
# 		}
# 	}else{
# 		# Setting for calculation
# 		# commaがあるか
# 		check_comma <- .check_comma(subscripts)
# 		# 共通の添字（ij,jkの時のj）
# 		cm_subscripts <- .common_subscripts(check_comma, split_subscripts,
# 			split_subscripts_2, split_rhs)
# 		# 共通の添字（->の左右で共通するもの）
# 		cm_subscripts_2 <- .common_subscripts_2(split_subscripts_2, split_rhs)
# 		# 共通じゃない添字（ij,jkの時のiとj、for文の添字になる）
# 		sp_subscripts <- .specific_subscripts(check_comma,
# 			each_dims, cm_subscripts)
# 		# 共通じゃない添字（->の左右で共通しないもの）
# 		sp_subscripts_2 <- .specific_subscripts_2(check_arrow, check_comma,
# 			each_dims, cm_subscripts_2)
# 		# 和をとるか（forループの数と、全添字の数が合わないとき）
# 		check_sum <- .check_sum(each_dims, sp_subscripts)
# 		# 和をとるか（operandが一つしかなく、かつ->の左右で添字が減っているとき）
# 		check_sum_2 <- .check_sum_2(operands, sp_subscripts_2)
# 		# 対角だけとりだすか（iiii->iみたいなとき）
# 		check_diag <- .check_diag(each_dims, lfs, rhs, operands)
# 		# アダマード積をとるか
# 		check_hadamard <- .check_hadamard(operands, each_dims,
# 			split_subscripts, split_rhs, no_comma, check_sum)
# 		# 出力オブジェクトの次元
# 		outdims <- .outdims(split_subscripts_2, each_dims, split_rhs,
# 			no_comma, check_arrow)
# 		# Calculate something
# 		out <- .sarray(outdims)
# 		cmd <- .forLoop(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard)
# 	}
# 	# Output
# 	print(subscripts)
# 	print(cmd)
# 	eval(parse(text=cmd))
# 	cat(rep("\n", 2))
# 	out
# }

# # For loop generator
# .forLoop <- function(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard){
# 	# e.g. for(i in seq(each_dims[1])){...
# 	cmd1 <- .cmd1(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard)
# 	# e.g. out[i,k] <-
# 	cmd2 <- .cmd2(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard)
# 	# e.g. operands[[1]][i,j] * operands[[2]][j,k]
# 	cmd3 <- .cmd3(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard)
# 	# e.g. }}}
# 	cmd4 <- .cmd4(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard)
# 	# Merge
# 	paste(c(cmd1, cmd2, cmd3, cmd4), collapse="")
# }

# .cmd1 <- function(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard){
# 	if(check_diag || check_hadamard){
# 		""
# 	}else{
# 		if(check_comma){
# 			# sp_subscriptsでfor文を回すパターン
# 			#（ij,jk->任意のサイズ<1,i,j,k,ij...ijk>）
# 			.cmd1_1(each_dims[sp_subscripts], each_dims)
# 		}else{
# 			if(identical(outdims, 1)){
# 				""
# 			}else{
# 				# cm_subscripts_2でfor文を回すパターン（ijkl->i）
# 				.cmd1_1(each_dims[cm_subscripts_2], each_dims)
# 			}
# 		}
# 	}
# }

# .cmd1_1 <- function(subs, each_dims){
# 	target <- unlist(lapply(names(subs),
# 		function(x){which(names(each_dims) == x)}))
# 	paste(c("count <- 1;",
# 		"pb <- txtProgressBar(1, ", prod(subs), ", style=3);",
# 		unlist(lapply(seq_along(subs),
# 			function(idx, e){
# 				paste0("for(", names(subs[idx]),
# 				" in seq(each_dims[", target[idx], "])){")}, e=each_dims))
# 	), collapse=" ")
# }

# .cmd2 <- function(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard){
# 	if(check_diag || check_hadamard){
# 		"out <- "
# 	}else if(check_sum_2){
# 		# cm_subscripts_2でfor文を回すパターン（ijkl->i）
# 		.cmd2_1(cm_subscripts_2, outdims)
# 	}else{
# 		# sp_subscriptsでfor文を回すパターン
# 		#（ij,jk->任意のサイズ<1,i,j,k,ij...ijk>）
# 		.cmd2_2(sp_subscripts, outdims)
# 	}
# }

# .cmd2_1 <- function(cm_subscripts_2, outdims){
# 	if(length(outdims) == 1){
# 		paste0("out[", names(outdims), "] <- ")
# 	}else{
# 		idx <- unlist(strsplit(cm_subscripts_2, ""))
# 		if(length(idx) == 1){
# 			tmp <- paste0(cm_subscripts_2, ",")
# 		}else{
# 			tmp <- paste0(idx, collapse=",")
# 		}
# 		paste0("out[", tmp, "] <- ")
# 	}
# }

# .cmd2_2 <- function(sp_subscripts, outdims){
# 	if(length(outdims) == 1){
# 		tmp <- paste0("out <- ",
# 			"out + ")
# 	}else{
# 		idx <- unlist(strsplit(sp_subscripts, ""))
# 		if(length(idx) == 1){
# 			tmp <- paste0(sp_subscripts, ",")
# 		}else{
# 			tmp <- paste0(idx, collapse=",")
# 		}
# 		tmp <- paste0("out[", tmp, "] <- out[", tmp, "] + ")
# 	}
# }

# .cmd3 <- function(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard){
# 	if(check_diag){
# 		print("### Pattern 3-1: Diagonal ###")
# 		".diag(operands, out, split_subscripts_2)"
# 	}else if(check_hadamard){
# 		if(identical(outdims, 1)){
# 			print("### Pattern 3-2-1: Hadamard + Sum ###")
# 			"sum(hadamard_list(operands))"
# 		}else{
# 			print("### Pattern 3-2-2: Hadamard ###")
# 			"hadamard_list(operands)"
# 		}
# 	}else if(check_sum_2){
# 		if(identical(outdims, 1)){
# 			"sum(operands[[1]])"
# 		}else{
# 			print("### Pattern 3-3: For (cm_subscripts_2) ###")
# 			# cm_subscripts_2でfor文を回すパターン（ijkl->i）
# 			.cmd3_1(cm_subscripts_2, split_subscripts, keep=TRUE)
# 		}
# 	}else{
# 		print("### Pattern 3-4: For (sp_subscripts) ###")
# 		# sp_subscriptsでfor文を回すパターン
# 		#（ij,jk->任意のサイズ<1,i,j,k,ij...ijk>）
# 		.cmd3_2(sp_subscripts, split_subscripts, outdims, keep=TRUE)
# 	}
# }

# # Remove the subs
# .cmd3_1 <- function(subs, split_subscripts, keep){
# 	tmp <- unlist(lapply(seq(split_subscripts), function(x, e){
# 		subs <- e$subs
# 		sps <- e$sps
# 		keep <- e$keep
# 		paste(c("operands[[", x, "]]",
# 			"[", .cmd3_helper(subs, sps[x], keep), "]"),
# 			collapse="")
# 	}, e=list(subs=subs, sps=split_subscripts, keep=keep)))
# 	tmp <- paste(tmp, collapse=" * ")
# 	paste(c("sum(", tmp, ")"), collapse="")
# }

# .cmd3_2 <- function(subs, split_subscripts, outdims, keep){
# 	tmp <- unlist(lapply(seq(split_subscripts), function(x, e){
# 		subs <- e$subs
# 		sps <- e$sps
# 		keep <- e$keep
# 		paste(c("operands[[", x, "]]",
# 			"[", .cmd3_helper(subs, sps[x], keep), "]"),
# 			collapse="")
# 	}, e=list(subs=subs, sps=split_subscripts, keep=keep)))
# 	tmp <- paste(tmp, collapse=" * ")
# 	if(identical(outdims, 1)){
# 		tmp <- paste(c("sum(", tmp, ")"), collapse="")
# 	}
# 	tmp
# }

# .cmd3_helper <- function(subs, sps, keep){
# 	tmp <- strsplit(sps, "")[[1]]
# 	if(keep){
# 		target <- unlist(lapply(subs, function(x){
# 			which(tmp == x)
# 		}))
# 		tmp[setdiff(seq(tmp), target)] <- ""
# 	}else{
# 		tmp <- .multi_gsub(subs, tmp)
# 	}
# 	if(length(tmp) == 1){
# 		tmp <- paste0(sps, ",")
# 	}else{
# 		tmp <- paste0(tmp, collapse=",")
# 	}
# 	tmp
# }

# .multi_gsub <- function(subs, tmp){
# 	for(i in seq(subs)){
# 		tmp <- gsub(subs[i], "", tmp)
# 	}
# 	tmp
# }

# .cmd4 <- function(split_subscripts, split_subscripts_2,
# 			outdims, each_dims, no_operands,
# 			cm_subscripts, cm_subscripts_2,
# 			sp_subscripts, sp_subscripts_2,
# 			check_comma, check_sum, check_sum_2,
# 			check_diag, check_hadamard){
# 	if(check_diag || check_hadamard){
# 		""
# 	}else{
# 		if(check_comma){
# 			# sp_subscriptsでfor文を回すパターン
# 			#（ij,jk->任意のサイズ<1,i,j,k,ij...ijk>）
# 			.cmd4_1(sp_subscripts)
# 		}else{
# 			if(identical(outdims, 1)){
# 				""
# 			}else{
# 				# cm_subscripts_2でfor文を回すパターン（ijkl->i）
# 				.cmd4_1(cm_subscripts_2)
# 			}
# 		}
# 	}
# }

# .cmd4_1 <- function(subs){
# 	paste(rep("}", length=length(subs)), collapse="")
# }

# # diagonal elements
# .diag <- function(operands, out, split_subscripts_2){
# 	cmd <- paste(c("L <- dim(operands[[1]])[1]; for(i in seq(L)){",
# 	"out[i] <- operands[[1]][",
# 	paste(rep("i", length=length(split_subscripts_2)), collapse=","),
# 	"]}"), collapse="")
# 	eval(parse(text=cmd))
# 	out
# }

# # whether
# .check_comma <- function(subscripts){
# 	length(grep(",", subscripts)) != 0
# }

# # No. of comma
# .no_comma <- function(subscripts){
# 	tmp = strsplit(subscripts, "")[[1]]
# 	length(grep(",", tmp))
# }

# # Sort (e.g. ji)
# .sortOperands <- function(operands, split_subscripts_2, each_dims){
# 	orderO <- unlist(lapply(split_subscripts_2, function(x){
# 		which(x == names(each_dims))
# 	}))
# 	aperm(operands[[1]], orderO)
# }

# # whether perform hadamard product
# .check_hadamard <- function(operands, each_dims, split_subscripts,
# 	split_rhs, no_comma, check_sum){
# 	check1 <- no_comma >= 1
# 	check2 <- all(split_subscripts == split_subscripts[1])
# 	check1 && check2
# }

# # forループの数と、全添字の数が合わないとき
# .check_sum <- function(each_dims, sp_subscripts){
# 	!identical(sort(names(each_dims)), sp_subscripts)
# }

# # operandが一つしかなく、->の左右で添字が減っている時
# .check_sum_2 <- function(operands, sp_subscripts_2){
# 	check1 <- length(operands) == 1
# 	check2 <- length(sp_subscripts_2) != 0
# 	check1 && check2
# }

# # 対角要素を取り出すだけか
# .check_diag <- function(each_dims, lfs, rhs, operands){
# 	check1 <- length(grep(",", lfs)) == 0
# 	check2 <- all(length(each_dims) == 1)
# 	if(is.na(rhs)){
# 		check3 <- FALSE
# 	}else{
# 		check3 <- all(names(each_dims) == rhs)
# 	}
# 	check4 <- all(names(each_dims) == unique(strsplit(lfs, "")[[1]][1]))
# 	check5 <- all(each_dims == unique(dim(operands[[1]])[1]))
# 	check1 && check2 && check3 && check4 && check5
# }

# # whether only permutation
# # 1. ,無し、->が無し、lfsのみで、rhsが無し
# # 2. ,無し、->が有り、ソートしたらlfsとrhsが同じ
# .check_perm <- function(no_comma, check_arrow, rhs, check_left_right){
# 	check1 <- no_comma == 0 && !check_arrow && is.na(rhs)
# 	check2 <- no_comma == 0 && check_arrow && check_left_right
# 	check1 || check2
# }

# .check_arrow <- function(subscripts){
# 	length(grep("->", subscripts)) == 1
# }

# # whether left and right are same
# .check_left_right <- function(lfs, rhs){
# 	left <- strsplit(lfs, "")[[1]]
# 	right <- strsplit(rhs, "")[[1]]
# 	identical(sort(left), sort(right))
# }

# # common subscripts (e.g. i,ij,ijk: i)
# .common_subscripts <- function(check_comma, split_subscripts,
# 	split_subscripts_2, split_rhs){
# 	if(check_comma){
# 		tbl <- table(unlist(strsplit(split_subscripts, "")))
# 		names(which(tbl == length(split_subscripts)))
# 	}else{
# 		NA
# 	}
# }

# # not common subscripts (e.g. i,ij,ijk: j,k)
# .specific_subscripts <- function(check_comma, each_dims, cm_subscripts){
# 	if(check_comma){
# 		setdiff(names(each_dims), cm_subscripts)
# 	}else{
# 		NA
# 	}
# }

# # common subscripts（e.g. ij->j: i）
# .common_subscripts_2 <- function(split_subscripts_2, split_rhs){
# 	if(is.na(split_rhs[1])){
# 		 NA
# 	}else{
# 		setdiff(
# 			split_subscripts_2[-which(split_subscripts_2 %in% "")],
# 			split_rhs)
# 	}
# }

# # not common subscripts（e.g. ij->j: j）
# .specific_subscripts_2 <- function(check_arrow, check_comma,
# 	each_dims, cm_subscripts_2){
# 	if(check_arrow || !check_comma){
# 		setdiff(names(each_dims), cm_subscripts_2)
# 	}else{
# 		NA
# 	}
# }

# # left subscripts
# .left_sub <- function(subscripts){
# 	strsplit(subscripts, "->")[[1]][1]
# }

# # right subscripts
# .right_sub <- function(subscripts){
# 	strsplit(subscripts, "->")[[1]][2]
# }

# # Output dimension
# .outdims <- function(split_subscripts_2, each_dims, split_rhs,
# 	no_comma, check_arrow){
# 	if(is.na(split_rhs[1])){
# 		if(no_comma == 0 && !check_arrow){
# 			# e.g. ii
# 			each_dims
# 		}else{
# 			1
# 		}
# 	}else{
# 		target <- unlist(lapply(split_rhs, function(x){
# 			which(names(each_dims) == x)
# 		}))
# 		each_dims[target]
# 	}
# }

# # subscripts -> dims
# .s_to_d <- function(subscripts, operands){
# 	odims <- unlist(lapply(operands, function(o){dim(o)}))
# 	sbs <- strsplit(gsub(",", "", .left_sub(subscripts)), "")[[1]]
# 	common.sbs <- sort(unique(sbs))
# 	out <- unlist(lapply(common.sbs, function(c){
# 		odims[which(sbs == c)[1]]
# 	}))
# 	names(out) <- common.sbs
# 	out
# }

# # duplicate subscripts checker
# .dup_check <- function(split_subscripts){
# 	if(length(split_subscripts) >= 2){
# 		sapply(seq(split_subscripts), function(idx){
# 			not_target <- setdiff(seq(split_subscripts), idx)
# 			dup_split_subscripts <- sapply(not_target, function(n){
# 				length(intersect(
# 					strsplit(split_subscripts[idx], "")[[1]],
# 					strsplit(split_subscripts[n], "")[[1]]
# 				))
# 			})
# 			if(max(dup_split_subscripts) == 0){
# 				stop(paste0("All operands must have at least one index",
# 					"with at least one operand."))
# 			}else{
# 				0
# 			}
# 		})
# 	}else{
# 		0
# 	}
# }






# operands = list(D, H)
# # D: 3×4
# # F: 3×4×5×6
# each_dims <- c(3,4,5,6)
# # outdim = 1の場合
# # 愚直にやった場合
# out <- 0
# for(i in seq(each_dims[1])){
# 	for(j in seq(each_dims[2])){
# 		for(k in seq(each_dims[3])){
# 			for(l in seq(each_dims[4])){
# 				out <- out + operands[[1]][i,j] * operands[[2]][i,j,k,l]
# 			}
# 		}
# 	}
# }

# # 共通したjを消す場合
# out <- 0
# for(i in seq(each_dims[1])){
# 	for(k in seq(each_dims[3])){
# 		for(l in seq(each_dims[4])){
# 			out <- out + sum(operands[[1]][i,] * operands[[2]][i,,k,l])
# 		}
# 	}
# }

# # outdimがベクトル（i）
# # 愚直にやった場合
# out <- array(0, dim=each_dims[1])
# for(i in seq(each_dims[1])){
# 	for(j in seq(each_dims[2])){
# 		for(k in seq(each_dims[3])){
# 			for(l in seq(each_dims[4])){
# 				out[i] <- out[i] + operands[[1]][i,j] * operands[[2]][i,j,k,l]
# 			}
# 		}
# 	}
# }

# # 共通したjを消す場合
# out <- array(0, dim=each_dims[1])
# for(i in seq(each_dims[1])){
# 	for(k in seq(each_dims[3])){
# 		for(l in seq(each_dims[4])){
# 			out[i] <- out[i] + sum(operands[[1]][i,] * operands[[2]][i,,k,l])
# 		}
# 	}
# }

# # outdimがベクトル（j）
# # 愚直にやった場合
# out <- array(0, dim=each_dims[2])
# for(i in seq(each_dims[1])){
# 	for(j in seq(each_dims[2])){
# 		for(k in seq(each_dims[3])){
# 			for(l in seq(each_dims[4])){
# 				out[j] <- out[j] + operands[[1]][i,j] * operands[[2]][i,j,k,l]
# 			}
# 		}
# 	}
# }

# # 共通したjを消す場合
# out <- array(0, dim=each_dims[2])
# for(i in seq(each_dims[1])){
# 	for(k in seq(each_dims[3])){
# 		for(l in seq(each_dims[4])){
# 			out <- out + operands[[1]][i,] * operands[[2]][i,,k,l]
# 		}
# 	}
# }

# # outdimが行列（k,l）
# # 愚直にやった場合
# out <- array(0, dim=each_dims[c(3,4)])
# for(i in seq(each_dims[1])){
# 	for(j in seq(each_dims[2])){
# 		for(k in seq(each_dims[3])){
# 			for(l in seq(each_dims[4])){
# 				out[k,l] <- out[k,l] + operands[[1]][i,j] * operands[[2]][i,j,k,l]
# 			}
# 		}
# 	}
# }

# # 共通したjを消す場合
# out <- array(0, dim=each_dims[c(3,4)])
# for(i in seq(each_dims[1])){
# 	for(k in seq(each_dims[3])){
# 		for(l in seq(each_dims[4])){
# 			out[k,l] <- out[k,l] + sum(operands[[1]][i,] * operands[[2]][i,,k,l])
# 		}
# 	}
# }



# # outdimが行列（i,j）
# # 愚直にやった場合
# out <- array(0, dim=each_dims[c(1,2)])
# for(i in seq(each_dims[1])){
# 	for(j in seq(each_dims[2])){
# 		for(k in seq(each_dims[3])){
# 			for(l in seq(each_dims[4])){
# 				out[i,j] <- out[i,j] + operands[[1]][i,j] * operands[[2]][i,j,k,l]
# 			}
# 		}
# 	}
# }

# # 共通したjを消す場合
# out <- array(0, dim=each_dims[c(1,2)])
# for(i in seq(each_dims[1])){
# 	for(k in seq(each_dims[3])){
# 		for(l in seq(each_dims[4])){
# 			out[i,] <- out[i,] + operands[[1]][i,] * operands[[2]][i,,k,l]
# 		}
# 	}
# }

# # 特別扱い系は廃止（hadarmard, diag）

# # Rule1: 全operandsで共通する添字は消す

# # Rule2: outの添字はoutdimで認識すれば良いが、共通添字は消す

# # Rule3: 「outがノルムならsumをとる」、「outの共通添字が消える場合はsumはとらない」

# # Rule4: 共通添字が消えた場合に、outやoperandsが同じ形になるように、事前にsubscriptsとoperandsをアルファベット順にソート

# # 最後いらない変数、関数を整理










