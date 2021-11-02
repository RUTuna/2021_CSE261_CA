.global sort
sort:
/* first  argument (x10): the address of output buffer */
/* second argument (x11): the start address of the incoming list */
/* third  argument (x12): the length of incoming list, n */
/* your sort code from here */
/* (x13): i, (x5): j, (x6): k */
/* (x14): currVal, (x28): temp */
/* (x30): add1, (x31): add2 */
	addi	sp, sp, -8
	sw	x1, 0(sp)

	addi	x13, x0, 0 /* i = 0 */

L1:
	slli	x30, x13, 2 /* add1(x30) = byte offset of [i] */
	add	x30, x30, x11 /* add1(x30) = byte address of x11[i] */
	lw	x14, 0(x30) /* currVal(x14) = x11[i] */
	jal	x1, insert
	addi	x13, x13, 1 /* i = i + 1 */
	bne	x13, x12, L1

exit:
	lw	x1, 0(sp)
	addi	sp, sp, 8
/* your sort code to here */
	jr	x1

.global insert
insert:
/* Recommanded arguments */
/* first argument (x10): the address of output buffer */
/* third  argument (x11): the current length of output buffer */
/* third  argument (x12): the integer to insert */
/* your insert code from here */
/* (x13): i, (x5): j, (x6): k */
/* (x14): currVal, (x28): temp */
/* (x30): add1, (x31): add2 */

	addi	x5, x0, 0 /* j = 0 */
	addi	x6, x13, 0 /* k = i */

loop:
	beq	x5, x13, insNow /* if(j==i) insNow */
	slli	x31, x5, 2 /* add2(x31) = byte offset of [j] */
	add	x31, x31, x10 /* add2(x31) = byte address of x10[j] */
	lw	x28, 0(x31) /* temp(x28) = x10[j] */
	addi	x5, x5, 1 /* j = j + 1 */
	blt	x28, x14, loop /* if(temp(x10[j]) < currVal(x11[i])) loop */

/* temp >= currVal */
	addi	x5, x5, -1 /* j = j - 1 */
insloop: 
	addi	x31, x6, -1 /* add2(x31) = k - 1 */
	slli	x31, x31, 2 /* add2(x31) = byte offset of [(k - 1)] */
	add	x31, x31, x10 /* add2(x31) = byte address of x10[(k - 1)] */
	lw	x28, 0(x31) /* temp(x28) = x10[(k - 1)] */
	addi	x31, x31, 4 /* add2(x31) = byte address of x10[k] */
	sw	x28, 0(x31) /* x10[k] = temp(x28) */
	addi	x6, x6, -1 /* k = k - 1 */
	bne	x6, x5, insloop /* if(k!=j) insloop */

insNow:
	slli	x31, x5, 2 /* add2(x31) = byte offset of [j] */
	add	x31, x31, x10 /* add2(x31) = byte address of x10[j] */
	sw	x14, 0(x31) /* x10[j] = currVal(x14) = x11[i] */

/* your insert code to here */	
	jr	x1

/*뒤에서부터 shift하며 탐색하는 방법 시도해보기*/

