	.data 
NL:	.asciiz	"\n"
	.text 
main:
	addi	$sp,$sp,-4
	sw	$ra,0($sp)	
	li	$a0,3
	jal	_fibrec
	move	$a0,$v0
	jal	_printint
	li	$a0,3
	jal	_fibiter
	move	$a0,$v0
	jal	_printint
	lw	$ra,0($sp)	
	addi	$sp,$sp,4
	jr	$ra
	
_fibrec:
	addi $sp, $sp, -12
	sw $ra, -8($sp)
	sw $s0, -4($sp)
	move $s0, $a0
	li $v0, 1
	bleu $s0, 1, _fibrec_end
	addi $a0, $s0, -2
	jal _fibrec
	sw $v0, 0($sp)
	addi $a0, $s0, -1
	jal _fibrec
	move $t1, $v0
	lw $t0, 0($sp)
	add $v0, $t0, $t1
_fibrec_end:
	lw $s0, -4($sp)
	lw $ra, -8($sp)
	addi $sp, $sp, 12
	jr $ra
	
_fibiter:
	li $t0, 1
	li $t1, 1
	beqz $a0, _fibiter_end
_fibiter_loop:
	addi $a0, $a0, -1
	move $t2, $t0
	move $t0, $t1
	add $t1, $t1, $t2
	bgt $a0, 0, _fibiter_loop
_fibiter_end:
	move $v0, $t0
	jr $ra

_printint:
	li 	$v0, 1		# $v0 := 1
	syscall 
	la 	$a0, NL	# $a0 := NL
	li 	$v0, 4		# $v0 := 4
	syscall 
	jr 	$ra		
       
