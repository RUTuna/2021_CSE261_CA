
.global gcd
gcd:
/* Your Code for gcd from here */
/* a @ x10, b @ x11 returns the gcd*/
Start:
	beq	x11, x0, Exit
	rem	x12, x10, x11
	add	x10, x11, x0
	add	x11, x12, x0
	beq	x0, x0, Start
Exit:
/* Your Code for gcd to here */
	jr x1


.global lcm
lcm:
/* Your Code for lcm from here */
/* a @ x10, b @ x11*/
	addi	sp, sp, -8
	sd	x1, 0(sp) /* store caller address */

	add	x13, x10, x0
	add	x14, x11, x0
	jal	x1, gcd
	div	x14, x14, x10
	mul	x10, x13, x14

	ld	x1, 0(sp) /* load caller address */
	addi	sp, sp, 8
/* Your Code for lcm to here */
	jr x1
