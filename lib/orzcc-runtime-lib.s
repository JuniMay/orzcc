	.text
	.align	1
	.type	threadpool_worker, @function
threadpool_worker:
	addi	sp,sp,-48
	sd	s3,8(sp)
	sext.w	s3,a0
	sd	s1,24(sp)
	addi	s1,s3,52
	slli	s1,s1,2
	slli	s3,s3,3
	sd	s0,32(sp)
	lla	s0,.LANCHOR0
	sd	ra,40(sp)
	sd	s2,16(sp)
	sd	s4,0(sp)
.L8:
	ld	a0,0(s0)
	addi	a0,a0,32
	call	pthread_mutex_lock@plt
	ld	a5,0(s0)
	add	a4,a5,s1
	lw	a4,8(a4)
	beq	a4,zero,.L2
	j	.L3
.L4:
	call	pthread_cond_wait@plt
	ld	a5,0(s0)
	add	a4,a5,s1
	lw	a4,8(a4)
	bne	a4,zero,.L3
.L2:
	addi	a0,a5,72
	addi	a1,a5,32
	lw	a5,232(a5)
	beq	a5,zero,.L4
.L5:
	mv	a0,a1
	call	pthread_mutex_unlock@plt
	ld	s0,32(sp)
	ld	ra,40(sp)
	li	a0,0
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
	addi	sp,sp,48
	jr	ra
.L3:
	lw	a4,232(a5)
	addi	a1,a5,32
	bne	a4,zero,.L5
	add	a4,a5,s3
	add	a5,a5,s1
	mv	a0,a1
	ld	s2,152(a4)
	ld	s4,184(a4)
	sw	zero,8(a5)
	call	pthread_mutex_unlock@plt
	beq	s2,zero,.L7
	mv	a0,s4
	jalr	s2
.L7:
	ld	a0,0(s0)
	addi	a0,a0,120
	call	pthread_barrier_wait@plt
	j	.L8
	.size	threadpool_worker, .-threadpool_worker
	.align	1
	.globl	orzcc_init_var_list
	.type	orzcc_init_var_list, @function
orzcc_init_var_list:
	addi	sp,sp,-32
	sd	s1,8(sp)
	mv	s1,a0
	li	a0,40
	sd	ra,24(sp)
	sd	s0,16(sp)
	call	malloc@plt
	mv	s0,a0
	slli	a0,s1,3
	call	malloc@plt
	sd	s1,16(s0)
	ld	ra,24(sp)
	sd	a0,0(s0)
	mv	a0,s0
	sd	zero,8(s0)
	ld	s0,16(sp)
	ld	s1,8(sp)
	addi	sp,sp,32
	jr	ra
	.size	orzcc_init_var_list, .-orzcc_init_var_list
	.align	1
	.globl	orzcc_free_var_list
	.type	orzcc_free_var_list, @function
orzcc_free_var_list:
	addi	sp,sp,-16
	sd	s0,0(sp)
	mv	s0,a0
	ld	a0,0(a0)
	sd	ra,8(sp)
	call	free@plt
	ld	ra,8(sp)
	mv	a0,s0
	ld	s0,0(sp)
	addi	sp,sp,16
	tail	free@plt
	.size	orzcc_free_var_list, .-orzcc_free_var_list
	.align	1
	.globl	orzcc_var_list_push_int
	.type	orzcc_var_list_push_int, @function
orzcc_var_list_push_int:
	ld	a5,8(a0)
	ld	a4,0(a0)
	slli	a3,a5,3
	add	a4,a4,a3
	addi	a5,a5,1
	sw	a1,0(a4)
	sd	a5,8(a0)
	ret
	.size	orzcc_var_list_push_int, .-orzcc_var_list_push_int
	.align	1
	.globl	orzcc_var_list_push_float
	.type	orzcc_var_list_push_float, @function
orzcc_var_list_push_float:
	ld	a5,8(a0)
	ld	a4,0(a0)
	slli	a3,a5,3
	add	a4,a4,a3
	addi	a5,a5,1
	fsw	fa0,0(a4)
	sd	a5,8(a0)
	ret
	.size	orzcc_var_list_push_float, .-orzcc_var_list_push_float
	.align	1
	.globl	orzcc_var_list_push_ptr
	.type	orzcc_var_list_push_ptr, @function
orzcc_var_list_push_ptr:
	ld	a5,8(a0)
	ld	a4,0(a0)
	slli	a3,a5,3
	add	a4,a4,a3
	addi	a5,a5,1
	sd	a1,0(a4)
	sd	a5,8(a0)
	ret
	.size	orzcc_var_list_push_ptr, .-orzcc_var_list_push_ptr
	.align	1
	.globl	orzcc_var_list_get_int
	.type	orzcc_var_list_get_int, @function
orzcc_var_list_get_int:
	ld	a5,0(a0)
	slli	a1,a1,3
	add	a5,a5,a1
	lw	a0,0(a5)
	ret
	.size	orzcc_var_list_get_int, .-orzcc_var_list_get_int
	.align	1
	.globl	orzcc_var_list_get_float
	.type	orzcc_var_list_get_float, @function
orzcc_var_list_get_float:
	ld	a5,0(a0)
	slli	a1,a1,3
	add	a5,a5,a1
	flw	fa0,0(a5)
	ret
	.size	orzcc_var_list_get_float, .-orzcc_var_list_get_float
	.align	1
	.globl	orzcc_var_list_get_ptr
	.type	orzcc_var_list_get_ptr, @function
orzcc_var_list_get_ptr:
	ld	a5,0(a0)
	slli	a1,a1,3
	add	a5,a5,a1
	ld	a0,0(a5)
	ret
	.size	orzcc_var_list_get_ptr, .-orzcc_var_list_get_ptr
	.align	1
	.globl	orzcc_var_list_get_start
	.type	orzcc_var_list_get_start, @function
orzcc_var_list_get_start:
	lw	a0,24(a0)
	ret
	.size	orzcc_var_list_get_start, .-orzcc_var_list_get_start
	.align	1
	.globl	orzcc_var_list_get_end
	.type	orzcc_var_list_get_end, @function
orzcc_var_list_get_end:
	lw	a0,28(a0)
	ret
	.size	orzcc_var_list_get_end, .-orzcc_var_list_get_end
	.align	1
	.globl	orzcc_var_list_ret_int
	.type	orzcc_var_list_ret_int, @function
orzcc_var_list_ret_int:
	sw	a1,32(a0)
	ret
	.size	orzcc_var_list_ret_int, .-orzcc_var_list_ret_int
	.align	1
	.globl	orzcc_var_list_ret_float
	.type	orzcc_var_list_ret_float, @function
orzcc_var_list_ret_float:
	fsw	fa0,32(a0)
	ret
	.size	orzcc_var_list_ret_float, .-orzcc_var_list_ret_float
	.align	1
	.globl	orzcc_va_list_copy
	.type	orzcc_va_list_copy, @function
orzcc_va_list_copy:
	addi	sp,sp,-32
	sd	s1,8(sp)
	mv	s1,a0
	li	a0,40
	sd	ra,24(sp)
	sd	s0,16(sp)
	sd	s2,0(sp)
	lw	s2,16(s1)
	call	malloc@plt
	mv	s0,a0
	slli	a0,s2,3
	call	malloc@plt
	sd	zero,8(s0)
	ld	a1,8(s1)
	sd	a0,0(s0)
	sd	s2,16(s0)
	beq	a1,zero,.L32
	ld	a5,0(s1)
	slli	a2,a1,3
	mv	a4,a0
	add	a2,a2,a5
.L33:
	ld	a3,0(a5)
	addi	a5,a5,8
	sd	a3,0(a4)
	addi	a4,a4,8
	bne	a5,a2,.L33
.L32:
	ld	ra,24(sp)
	mv	a0,s0
	sd	a1,8(s0)
	ld	s0,16(sp)
	ld	s1,8(sp)
	ld	s2,0(sp)
	addi	sp,sp,32
	jr	ra
	.size	orzcc_va_list_copy, .-orzcc_va_list_copy
	.section	.text.startup,"ax",@progbits
	.align	1
	.globl	orzcc_init
	.type	orzcc_init, @function
orzcc_init:
	addi	sp,sp,-48
	li	a0,240
	sd	ra,40(sp)
	sd	s0,32(sp)
	sd	s1,24(sp)
	li	s1,0
	sd	s2,16(sp)
	li	s2,4
	sd	s3,8(sp)
	lla	s3,threadpool_worker
	sd	s4,0(sp)
	call	malloc@plt
	mv	s0,a0
	addi	a0,a0,32
	li	a1,0
	lla	s4,.LANCHOR0
	sd	s0,0(s4)
	call	pthread_mutex_init@plt
	addi	a0,s0,72
	li	a1,0
	call	pthread_cond_init@plt
	addi	a0,s0,120
	li	a2,5
	li	a1,0
	call	pthread_barrier_init@plt
	sw	zero,232(s0)
.L41:
	sext.w	a5,s1
	slli	a0,s1,3
	addi	a5,a5,52
	add	a0,s0,a0
	slli	a5,a5,2
	mv	a3,s1
	add	s0,s0,a5
	mv	a2,s3
	addi	s1,s1,1
	li	a1,0
	sw	zero,8(s0)
	call	pthread_create@plt
	beq	s1,s2,.L39
	ld	s0,0(s4)
	j	.L41
.L39:
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
	addi	sp,sp,48
	jr	ra
	.size	orzcc_init, .-orzcc_init
	.section	.init_array,"aw"
	.align	3
	.dword	orzcc_init
	.section	.text.exit,"ax",@progbits
	.align	1
	.globl	orzcc_destroy
	.type	orzcc_destroy, @function
orzcc_destroy:
	addi	sp,sp,-32
	sd	s1,8(sp)
	lla	s1,.LANCHOR0
	ld	a0,0(s1)
	addi	a0,a0,32
	sd	ra,24(sp)
	sd	s0,16(sp)
	li	s0,0
	sd	s2,0(sp)
	call	pthread_mutex_lock@plt
	ld	a5,0(s1)
	li	a4,1
	addi	a0,a5,72
	li	s2,4
	sw	a4,232(a5)
	call	pthread_cond_broadcast@plt
	ld	a0,0(s1)
	addi	a0,a0,32
	call	pthread_mutex_unlock@plt
.L44:
	slli	a4,s0,3
	li	a1,0
	ld	a5,0(s1)
	addiw	s0,s0,1
	add	a5,a5,a4
	ld	a0,0(a5)
	call	pthread_join@plt
	bne	s0,s2,.L44
	ld	s0,0(s1)
	addi	a0,s0,32
	call	pthread_mutex_destroy@plt
	addi	a0,s0,72
	call	pthread_cond_destroy@plt
	addi	a0,s0,120
	call	pthread_barrier_destroy@plt
	ld	s1,8(sp)
	ld	ra,24(sp)
	mv	a0,s0
	ld	s2,0(sp)
	ld	s0,16(sp)
	addi	sp,sp,32
	tail	free@plt
	.size	orzcc_destroy, .-orzcc_destroy
	.section	.fini_array,"aw"
	.align	3
	.dword	orzcc_destroy
	.text
	.align	1
	.globl	orzcc_parallel_for
	.type	orzcc_parallel_for, @function
orzcc_parallel_for:
	addi	sp,sp,-144
	sd	s5,88(sp)
	lla	s5,.LANCHOR0
	sd	s11,40(sp)
	mv	s11,a0
	ld	a0,0(s5)
	addi	a0,a0,32
	sd	s0,128(sp)
	mv	s0,a3
	sd	s1,120(sp)
	sd	s2,112(sp)
	li	s2,-1
	sd	s3,104(sp)
	srli	s2,s2,32
	sd	s4,96(sp)
	sd	s6,80(sp)
	mv	s6,a1
	sd	s7,72(sp)
	sd	s9,56(sp)
	sd	ra,136(sp)
	sd	s8,64(sp)
	sd	s10,48(sp)
	sd	a2,16(sp)
	call	pthread_mutex_lock@plt
	subw	a5,s6,s11
	sext.w	s11,s11
	addiw	a5,a5,3
	sraiw	s1,a5,31
	srliw	s1,s1,30
	ld	a4,0(s5)
	addw	s1,s1,a5
	lw	s4,16(s0)
	addi	a5,a4,184
	sraiw	s1,s1,2
	slli	s7,s4,3
	addi	s3,a4,152
	addi	s9,a4,216
	sd	a4,24(sp)
	sd	a5,8(sp)
.L51:
	addw	a5,s11,s1
	sext.w	t1,s11
	sext.w	s11,a5
	and	t1,t1,s2
	ld	a4,16(sp)
	li	a0,40
	ble s11,s6,1f; mv a5,s6; 1: # movcc
	slli	a5,a5,32
	sd	a4,0(s3)
	or	s8,t1,a5
	call	malloc@plt
	mv	s10,a0
	mv	a0,s7
	call	malloc@plt
	sd	zero,8(s10)
	mv	a4,a0
	sd	a4,0(s10)
	ld	a0,8(s0)
	sd	s4,16(s10)
	beq	a0,zero,.L49
	ld	a5,0(s0)
	slli	a2,a0,3
	add	a2,a2,a5
.L50:
	ld	a3,0(a5)
	addi	a5,a5,8
	sd	a3,0(a4)
	addi	a4,a4,8
	bne	a2,a5,.L50
.L49:
	addi	s3,s3,8
	addi	s9,s9,4
	sd	a0,8(s10)
	li	a5,1
	sd	s10,24(s3)
	sd	s8,24(s10)
	sw	a5,-4(s9)
	ld	a5,8(sp)
	bne	s3,a5,.L51
	ld	a5,24(sp)
	addi	a0,a5,72
	call	pthread_cond_broadcast@plt
	ld	a0,0(s5)
	addi	a0,a0,32
	call	pthread_mutex_unlock@plt
	ld	a0,0(s5)
	addi	a0,a0,120
	call	pthread_barrier_wait@plt
	ld	s3,0(s5)
	addi	s1,s3,184
	addi	s3,s3,216
.L52:
	ld	s2,0(s1)
	addi	s1,s1,8
	ld	a0,0(s2)
	call	free@plt
	mv	a0,s2
	call	free@plt
	bne	s3,s1,.L52
	ld	a0,0(s0)
	call	free@plt
	ld	s1,120(sp)
	ld	ra,136(sp)
	mv	a0,s0
	ld	s2,112(sp)
	ld	s0,128(sp)
	ld	s3,104(sp)
	ld	s4,96(sp)
	ld	s5,88(sp)
	ld	s6,80(sp)
	ld	s7,72(sp)
	ld	s8,64(sp)
	ld	s9,56(sp)
	ld	s10,48(sp)
	ld	s11,40(sp)
	addi	sp,sp,144
	tail	free@plt
	.size	orzcc_parallel_for, .-orzcc_parallel_for
	.align	1
	.globl	orzcc_parallel_for_reduce_add_int
	.type	orzcc_parallel_for_reduce_add_int, @function
orzcc_parallel_for_reduce_add_int:
	addi	sp,sp,-144
	sd	s6,80(sp)
	lla	s6,.LANCHOR0
	sd	s7,72(sp)
	mv	s7,a0
	ld	a0,0(s6)
	addi	a0,a0,32
	sd	s0,128(sp)
	mv	s0,a3
	sd	s1,120(sp)
	mv	s1,a4
	sd	s2,112(sp)
	sd	s3,104(sp)
	li	s3,-1
	sd	s4,96(sp)
	srli	s3,s3,32
	sd	s5,88(sp)
	sd	s8,64(sp)
	sd	s9,56(sp)
	mv	s9,a1
	sd	s10,48(sp)
	sd	ra,136(sp)
	sd	s11,40(sp)
	sd	a2,16(sp)
	call	pthread_mutex_lock@plt
	subw	a5,s9,s7
	sext.w	s7,s7
	addiw	a5,a5,3
	sraiw	s2,a5,31
	srliw	s2,s2,30
	addw	s2,s2,a5
	ld	a4,0(s6)
	sraiw	a5,s2,2
	lw	s5,16(s0)
	addi	s4,a4,152
	sd	a5,0(sp)
	slli	s8,s5,3
	addi	a5,a4,184
	addi	s10,a4,216
	sd	a4,24(sp)
	sd	a5,8(sp)
.L64:
	ld	a5,0(sp)
	sext.w	t3,s7
	addw	a5,s7,a5
	and	t3,t3,s3
	sext.w	s7,a5
	li	a0,40
	ld	a4,16(sp)
	ble s7,s9,1f; mv a5,s9; 1: # movcc
	slli	a5,a5,32
	sd	a4,0(s4)
	or	s2,t3,a5
	call	malloc@plt
	mv	s11,a0
	mv	a0,s8
	call	malloc@plt
	sd	zero,8(s11)
	mv	a4,a0
	sd	a4,0(s11)
	ld	a0,8(s0)
	sd	s5,16(s11)
	beq	a0,zero,.L62
	ld	a5,0(s0)
	slli	a2,a0,3
	add	a2,a2,a5
.L63:
	ld	a3,0(a5)
	addi	a5,a5,8
	sd	a3,0(a4)
	addi	a4,a4,8
	bne	a5,a2,.L63
.L62:
	addi	s4,s4,8
	addi	s10,s10,4
	sd	a0,8(s11)
	li	a5,1
	sd	s11,24(s4)
	sd	s2,24(s11)
	sw	a5,-4(s10)
	ld	a5,8(sp)
	bne	s4,a5,.L64
	ld	a5,24(sp)
	addi	a0,a5,72
	call	pthread_cond_broadcast@plt
	ld	a0,0(s6)
	addi	a0,a0,32
	call	pthread_mutex_unlock@plt
	ld	a0,0(s6)
	addi	a0,a0,120
	call	pthread_barrier_wait@plt
	ld	s4,0(s6)
	addi	s2,s4,184
	addi	s4,s4,216
.L65:
	ld	s3,0(s2)
	addi	s2,s2,8
	ld	a0,0(s3)
	lw	a5,32(s3)
	addw	s1,a5,s1
	call	free@plt
	mv	a0,s3
	call	free@plt
	bne	s4,s2,.L65
	ld	a0,0(s0)
	call	free@plt
	mv	a0,s0
	call	free@plt
	ld	s0,128(sp)
	ld	ra,136(sp)
	mv	a0,s1
	ld	s2,112(sp)
	ld	s1,120(sp)
	ld	s3,104(sp)
	ld	s4,96(sp)
	ld	s5,88(sp)
	ld	s6,80(sp)
	ld	s7,72(sp)
	ld	s8,64(sp)
	ld	s9,56(sp)
	ld	s10,48(sp)
	ld	s11,40(sp)
	addi	sp,sp,144
	jr	ra
	.size	orzcc_parallel_for_reduce_add_int, .-orzcc_parallel_for_reduce_add_int
	.align	1
	.globl	orzcc_parallel_for_reduce_add_float
	.type	orzcc_parallel_for_reduce_add_float, @function
orzcc_parallel_for_reduce_add_float:
	addi	sp,sp,-160
	sd	s5,104(sp)
	lla	s5,.LANCHOR0
	ld	a5,0(s5)
	sd	s11,56(sp)
	mv	s11,a0
	addi	a0,a5,32
	sd	s0,144(sp)
	mv	s0,a3
	sd	s1,136(sp)
	sd	s2,128(sp)
	li	s2,-1
	sd	s3,120(sp)
	srli	s2,s2,32
	sd	s4,112(sp)
	sd	s6,96(sp)
	mv	s6,a1
	sd	s7,88(sp)
	sd	s9,72(sp)
	fsd	fs0,40(sp)
	fmv.s	fs0,fa0
	sd	ra,152(sp)
	sd	s8,80(sp)
	sd	s10,64(sp)
	sd	a2,16(sp)
	call	pthread_mutex_lock@plt
	subw	a5,s6,s11
	sext.w	s11,s11
	addiw	a5,a5,3
	sraiw	s1,a5,31
	srliw	s1,s1,30
	ld	a4,0(s5)
	addw	s1,s1,a5
	lw	s4,16(s0)
	addi	a5,a4,184
	sraiw	s1,s1,2
	slli	s7,s4,3
	addi	s3,a4,152
	addi	s9,a4,216
	sd	a4,24(sp)
	sd	a5,8(sp)
.L77:
	addw	a5,s11,s1
	sext.w	t1,s11
	sext.w	s11,a5
	and	t1,t1,s2
	ld	a4,16(sp)
	li	a0,40
	ble s11,s6,1f; mv a5,s6; 1: # movcc
	slli	a5,a5,32
	sd	a4,0(s3)
	or	s8,t1,a5
	call	malloc@plt
	mv	s10,a0
	mv	a0,s7
	call	malloc@plt
	sd	zero,8(s10)
	mv	a4,a0
	sd	a4,0(s10)
	ld	a0,8(s0)
	sd	s4,16(s10)
	beq	a0,zero,.L75
	ld	a5,0(s0)
	slli	a2,a0,3
	add	a2,a2,a5
.L76:
	ld	a3,0(a5)
	addi	a5,a5,8
	sd	a3,0(a4)
	addi	a4,a4,8
	bne	a5,a2,.L76
.L75:
	addi	s3,s3,8
	addi	s9,s9,4
	sd	a0,8(s10)
	li	a5,1
	sd	s10,24(s3)
	sd	s8,24(s10)
	sw	a5,-4(s9)
	ld	a5,8(sp)
	bne	s3,a5,.L77
	ld	a5,24(sp)
	addi	a0,a5,72
	call	pthread_cond_broadcast@plt
	ld	a0,0(s5)
	addi	a0,a0,32
	call	pthread_mutex_unlock@plt
	ld	a0,0(s5)
	addi	a0,a0,120
	call	pthread_barrier_wait@plt
	ld	s3,0(s5)
	addi	s1,s3,184
	addi	s3,s3,216
.L78:
	ld	s2,0(s1)
	addi	s1,s1,8
	ld	a0,0(s2)
	flw	fa5,32(s2)
	fadd.s	fs0,fs0,fa5
	call	free@plt
	mv	a0,s2
	call	free@plt
	bne	s3,s1,.L78
	ld	a0,0(s0)
	call	free@plt
	mv	a0,s0
	call	free@plt
	ld	s0,144(sp)
	ld	ra,152(sp)
	fmv.s	fa0,fs0
	ld	s1,136(sp)
	ld	s2,128(sp)
	ld	s3,120(sp)
	ld	s4,112(sp)
	ld	s5,104(sp)
	ld	s6,96(sp)
	ld	s7,88(sp)
	ld	s8,80(sp)
	ld	s9,72(sp)
	ld	s10,64(sp)
	ld	s11,56(sp)
	fld	fs0,40(sp)
	addi	sp,sp,160
	jr	ra
	.size	orzcc_parallel_for_reduce_add_float, .-orzcc_parallel_for_reduce_add_float
	.bss
	.align	3
	.set	.LANCHOR0,. + 0
	.type	pool, @object
	.size	pool, 8
pool:
	.zero	8
	.section	.note.GNU-stack,"",@progbits
