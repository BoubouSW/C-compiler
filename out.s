	.text
main:
	li	$a0,10
	addi	$a0,$a0,1
	li	$v0,1
	syscall
	li	$a0,0
	jr	$ra
	.data