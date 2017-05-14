	.data 
NL:	.asciiz	"\n"
	.text 
main:
	move 	$x0, $ra	# $x0 := $ra
	li 	$a0, 32000		# $a0 := 32000
	li 	$v0, 9		# $v0 := 9
	syscall 
	sw 	$v0, 0($gp)	# [$gp+0] := $v0
	jal 	_main		# call _main
	move 	$ra, $x0	# $ra := $x0
main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
alloc:
	lw 	$v0, 0($gp)	# $v0 := [$gp+0]
	add	$t0, $v0, $a0	# $t0 := $v0+$a0
	sw 	$t0, 0($gp)	# [$gp+0] := $t0
alloc.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_printint:
	li 	$v0, 1		# $v0 := 1
	syscall 
	la 	$a0, NL	# $a0 := NL
	li 	$v0, 4		# $v0 := 4
	syscall 
_printint.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_addsome:
	move 	$x1, $s0	# $x1 := $s0
	move 	$x2, $s1	# $x2 := $s1
	move 	$x3, $s2	# $x3 := $s2
	move 	$x4, $s3	# $x4 := $s3
	move 	$x5, $s4	# $x5 := $s4
	move 	$x6, $s5	# $x6 := $s5
	move 	$x7, $s6	# $x7 := $s6
	move 	$x8, $s7	# $x8 := $s7
	move 	$x9, $ra	# $x9 := $ra
	move 	$x10, $a0	# $x10 := $a0
	addi 	$x11, $x10, 5	# $x11 := $x10+5
	move 	$v0, $x11	# $v0 := $x11
	move 	$s0, $x1	# $s0 := $x1
	move 	$s1, $x2	# $s1 := $x2
	move 	$s2, $x3	# $s2 := $x3
	move 	$s3, $x4	# $s3 := $x4
	move 	$s4, $x5	# $s4 := $x5
	move 	$s5, $x6	# $s5 := $x6
	move 	$s6, $x7	# $s6 := $x7
	move 	$s7, $x8	# $s7 := $x8
	move 	$ra, $x9	# $ra := $x9
_addsome.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_add:
	move 	$x12, $s0	# $x12 := $s0
	move 	$x13, $s1	# $x13 := $s1
	move 	$x14, $s2	# $x14 := $s2
	move 	$x15, $s3	# $x15 := $s3
	move 	$x16, $s4	# $x16 := $s4
	move 	$x17, $s5	# $x17 := $s5
	move 	$x18, $s6	# $x18 := $s6
	move 	$x19, $s7	# $x19 := $s7
	move 	$x20, $ra	# $x20 := $ra
	move 	$x21, $a0	# $x21 := $a0
	lw 	$x22, 0($x21)	# $x22 := [$x21+0]
	lw 	$x23, 4($x21)	# $x23 := [$x21+4]
	add	$x24, $x22, $x23	# $x24 := $x22+$x23
	move 	$v0, $x24	# $v0 := $x24
	move 	$s0, $x12	# $s0 := $x12
	move 	$s1, $x13	# $s1 := $x13
	move 	$s2, $x14	# $s2 := $x14
	move 	$s3, $x15	# $s3 := $x15
	move 	$s4, $x16	# $s4 := $x16
	move 	$s5, $x17	# $s5 := $x17
	move 	$s6, $x18	# $s6 := $x18
	move 	$s7, $x19	# $s7 := $x19
	move 	$ra, $x20	# $ra := $x20
_add.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_gt:
	move 	$x25, $s0	# $x25 := $s0
	move 	$x26, $s1	# $x26 := $s1
	move 	$x27, $s2	# $x27 := $s2
	move 	$x28, $s3	# $x28 := $s3
	move 	$x29, $s4	# $x29 := $s4
	move 	$x30, $s5	# $x30 := $s5
	move 	$x31, $s6	# $x31 := $s6
	move 	$x32, $s7	# $x32 := $s7
	move 	$x33, $ra	# $x33 := $ra
	move 	$x34, $a0	# $x34 := $a0
	lw 	$x35, 0($x34)	# $x35 := [$x34+0]
	lw 	$x36, 4($x34)	# $x36 := [$x34+4]
	slt	$x39, $x35, $x36	# $x39 := $x35<$x36
	beqz	$x39, L3   	# if (signed) $x39 == 0 goto L3
	seq	$x42, $x35, $x36	# $x42 := $x35==$x36
	li 	$x43, 0		# $x43 := 0
	seq	$x44, $x42, $x43	# $x44 := $x42==$x43
	beqz	$x44, L7   	# if (signed) $x44 == 0 goto L7
	li 	$x45, 1		# $x45 := 1
	move 	$x41, $x45	# $x41 := $x45
	j 	L8		# goto L8
L7:
	li 	$x46, 0		# $x46 := 0
	move 	$x41, $x46	# $x41 := $x46
L8:
	beqz	$x41, L5   	# if (signed) $x41 == 0 goto L5
	li 	$x47, 1		# $x47 := 1
	move 	$x40, $x47	# $x40 := $x47
	j 	L6		# goto L6
L5:
	li 	$x48, 0		# $x48 := 0
	move 	$x40, $x48	# $x40 := $x48
L6:
	move 	$x38, $x40	# $x38 := $x40
	j 	L4		# goto L4
L3:
	li 	$x49, 0		# $x49 := 0
	move 	$x38, $x49	# $x38 := $x49
L4:
	li 	$x50, 0		# $x50 := 0
	seq	$x51, $x38, $x50	# $x51 := $x38==$x50
	beqz	$x51, L1   	# if (signed) $x51 == 0 goto L1
	li 	$x52, 1		# $x52 := 1
	move 	$x37, $x52	# $x37 := $x52
	j 	L2		# goto L2
L1:
	li 	$x53, 0		# $x53 := 0
	move 	$x37, $x53	# $x37 := $x53
L2:
	move 	$v0, $x37	# $v0 := $x37
	move 	$s0, $x25	# $s0 := $x25
	move 	$s1, $x26	# $s1 := $x26
	move 	$s2, $x27	# $s2 := $x27
	move 	$s3, $x28	# $s3 := $x28
	move 	$s4, $x29	# $s4 := $x29
	move 	$s5, $x30	# $s5 := $x30
	move 	$s6, $x31	# $s6 := $x31
	move 	$s7, $x32	# $s7 := $x32
	move 	$ra, $x33	# $ra := $x33
_gt.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_main:
	move 	$x54, $s0	# $x54 := $s0
	move 	$x55, $s1	# $x55 := $s1
	move 	$x56, $s2	# $x56 := $s2
	move 	$x57, $s3	# $x57 := $s3
	move 	$x58, $s4	# $x58 := $s4
	move 	$x59, $s5	# $x59 := $s5
	move 	$x60, $s6	# $x60 := $s6
	move 	$x61, $s7	# $x61 := $s7
	move 	$x62, $ra	# $x62 := $ra
	move 	$x63, $a0	# $x63 := $a0
	li 	$x64, 8		# $x64 := 8
	li 	$x65, 7		# $x65 := 7
	mulo	$x66, $x64, $x65	# $x66 := $x64*$x65
	addi 	$x67, $x66, 7	# $x67 := $x66+7
	la 	$x69, _gt	# $x69 := _gt
	li 	$a0, 8		# $a0 := 8
	jal 	alloc		# call alloc
	move 	$x71, $v0	# $x71 := $v0
	sw 	$x66, 0($x71)	# [$x71+0] := $x66
	li 	$x72, 0		# $x72 := 0
	sw 	$x72, 4($x71)	# [$x71+4] := $x72
	move 	$a0, $x71	# $a0 := $x71
	jalr 	$ra, $x69	# () := call $ra()
	move 	$x70, $v0	# $x70 := $v0
	beqz	$x70, L9   	# if (signed) $x70 == 0 goto L9
	li 	$x75, 7		# $x75 := 7
	seq	$x76, $x66, $x75	# $x76 := $x66==$x75
	li 	$x77, 0		# $x77 := 0
	seq	$x78, $x76, $x77	# $x78 := $x76==$x77
	beqz	$x78, L13   	# if (signed) $x78 == 0 goto L13
	li 	$x79, 1		# $x79 := 1
	move 	$x74, $x79	# $x74 := $x79
	j 	L14		# goto L14
L13:
	li 	$x80, 0		# $x80 := 0
	move 	$x74, $x80	# $x74 := $x80
L14:
	beqz	$x74, L11   	# if (signed) $x74 == 0 goto L11
	la 	$x81, _addsome	# $x81 := _addsome
	la 	$x83, _add	# $x83 := _add
	li 	$a0, 8		# $a0 := 8
	jal 	alloc		# call alloc
	move 	$x85, $v0	# $x85 := $v0
	sw 	$x66, 0($x85)	# [$x85+0] := $x66
	sw 	$x67, 4($x85)	# [$x85+4] := $x67
	move 	$a0, $x85	# $a0 := $x85
	jalr 	$ra, $x83	# () := call $ra()
	move 	$x84, $v0	# $x84 := $v0
	move 	$a0, $x84	# $a0 := $x84
	jalr 	$ra, $x81	# () := call $ra()
	move 	$x82, $v0	# $x82 := $v0
	move 	$x73, $x82	# $x73 := $x82
	j 	L12		# goto L12
L11:
	li 	$x86, -1		# $x86 := -1
	move 	$x73, $x86	# $x73 := $x86
L12:
	move 	$x68, $x73	# $x68 := $x73
	j 	L10		# goto L10
L9:
	li 	$x88, -2		# $x88 := -2
	beqz	$x88, L15   	# if (signed) $x88 == 0 goto L15
	li 	$x89, 0		# $x89 := 0
	move 	$x87, $x89	# $x87 := $x89
	j 	L16		# goto L16
L15:
	li 	$x90, 1		# $x90 := 1
	move 	$x87, $x90	# $x87 := $x90
L16:
	move 	$x68, $x87	# $x68 := $x87
L10:
	move 	$v0, $x68	# $v0 := $x68
	move 	$s0, $x54	# $s0 := $x54
	move 	$s1, $x55	# $s1 := $x55
	move 	$s2, $x56	# $s2 := $x56
	move 	$s3, $x57	# $s3 := $x57
	move 	$s4, $x58	# $s4 := $x58
	move 	$s5, $x59	# $s5 := $x59
	move 	$s6, $x60	# $s6 := $x60
	move 	$s7, $x61	# $s7 := $x61
	move 	$ra, $x62	# $ra := $x62
_main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
