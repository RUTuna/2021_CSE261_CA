
.global matmul


matmul:
/* save return address (in x1) in stack*/
/* first argument (x10): the address of output buffer */
/* second argument (x11): the start address of a */
/* third argument (x12): the start address of b */
/* fourth argument (x13): the dimension */
/* your matmul code from here */
	addi	sp, sp, -8
	sd	x1, 0(sp)
	
	addi	x14, x0, 0
	beq	x14, x13, exit

L1:
	addi	x15, x0, 0

L2:
	jal	x1, matmul_idx
	addi	x15, x15, 1
	bne	x15, x13, L2

	addi	x14, x14, 1
	bne	x14, x13, L1

exit:
	ld	x1, 0(sp)
	addi	sp, sp, 8
/* your matmul code to here */
	jr	x1


.global matmul_idx
matmul_idx:
/* Recommanded arguments */
/* first argument (x10): the address of output buffer */
/* second argument (x11): the start address of a */
/* third argument (x12): the start address of b */
/* fourth argument (x13): the dimension */
/* fourth argument (x14): row index of the result matrix to fill out, i */
/* fourth argument (x15): column index of the result matrix to fill out, j */
/* your matmul_idx (helper function) code from here */

/* (x5): k */
/* (x6): a[i][k] address, (x7): b[k][j] address (x31): res[i][j] adress*/
/* (x28): a[i][k] value, (x29): b[k][j] value, (x30): res[i][j] value) */

	addi	x5, x0, 0 /* k = 0 */
	beq	x5, x13, out	
	addi	x30, x0, 0 /* x30 = 0 */
	mul	x31, x13, x14 /* (x31) = n*i */
	add	x31, x31, x15 /* (x31) = n*i + j */
	slli	x31, x31, 2 /* (x31) = byte offset of [i][j] */
	add	x31, x31, x10 /* (x31) = byte address of res[i][j] */
	
loop:
	mul	x6, x13, x14 /* (x6) = n*i */
	add	x6, x6, x5 /* (x6) = n*i + k */
	slli	x6, x6, 2 /* (x6) = byte offset of [i][k] */
	add	x6, x6, x11 /* (x6) = byte address of a[i][k] */
	ld	x28, 0(x6) /* (x28) = a[i][k] */

	mul	x7, x13, x5 /* (x7) = n*k */
	add	x7, x7, x15 /* (x7) = n*k + j */
	slli	x7, x7, 2 /* (x7) = byte offset of [k][j] */
	add	x7, x7, x12 /* (x7) = byte address of b[k][j] */
	ld	x29, 0(x7) /* (x29) = b[k][j] */

	mul	x28, x28, x29 /* (x28) = a[i][k] * b[k][j] */
	add	x30, x30, x28 /* (x30) = x30 + a[i][k] * b[k][j] */

	addi	x5, x5, 1 /* k++ */
	bne	x5, x13, loop

	sd	x30, 0(x31) /* res[i][j] = (x30) */
out:
/* your matmul_idx (helper function) code to here */
	jr	x1
