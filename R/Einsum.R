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