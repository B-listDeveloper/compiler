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
	move 	$x9, $a0	# $x9 := $a0
	move 	$x10, $ra	# $x10 := $ra
	li 	$x11, 5		# $x11 := 5
	add	$x12, $a0, $x11	# $x12 := $a0+$x11
	move 	$v0, $x12	# $v0 := $x12
	move 	$s7, $x8	# $s7 := $x8
	move 	$s6, $x7	# $s6 := $x7
	move 	$s5, $x6	# $s5 := $x6
	move 	$s4, $x5	# $s4 := $x5
	move 	$s3, $x4	# $s3 := $x4
	move 	$s2, $x3	# $s2 := $x3
	move 	$s1, $x2	# $s1 := $x2
	move 	$s0, $x1	# $s0 := $x1
	move 	$ra, $x10	# $ra := $x10
	move 	$a0, $x9	# $a0 := $x9
_addsome.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_add:
	move 	$x13, $s0	# $x13 := $s0
	move 	$x14, $s1	# $x14 := $s1
	move 	$x15, $s2	# $x15 := $s2
	move 	$x16, $s3	# $x16 := $s3
	move 	$x17, $s4	# $x17 := $s4
	move 	$x18, $s5	# $x18 := $s5
	move 	$x19, $s6	# $x19 := $s6
	move 	$x20, $s7	# $x20 := $s7
	move 	$x21, $a0	# $x21 := $a0
	move 	$x22, $ra	# $x22 := $ra
	addi 	$x24, $a0, 0	# $x24 := $a0+0
	lw 	$x23, 0($x24)	# $x23 := [$x24+0]
	addi 	$x26, $a0, 4	# $x26 := $a0+4
	lw 	$x25, 0($x26)	# $x25 := [$x26+0]
	add	$x27, $x23, $x25	# $x27 := $x23+$x25
	move 	$v0, $x27	# $v0 := $x27
	move 	$s7, $x20	# $s7 := $x20
	move 	$s6, $x19	# $s6 := $x19
	move 	$s5, $x18	# $s5 := $x18
	move 	$s4, $x17	# $s4 := $x17
	move 	$s3, $x16	# $s3 := $x16
	move 	$s2, $x15	# $s2 := $x15
	move 	$s1, $x14	# $s1 := $x14
	move 	$s0, $x13	# $s0 := $x13
	move 	$ra, $x22	# $ra := $x22
	move 	$a0, $x21	# $a0 := $x21
_add.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_gt:
	move 	$x28, $s0	# $x28 := $s0
	move 	$x29, $s1	# $x29 := $s1
	move 	$x30, $s2	# $x30 := $s2
	move 	$x31, $s3	# $x31 := $s3
	move 	$x32, $s4	# $x32 := $s4
	move 	$x33, $s5	# $x33 := $s5
	move 	$x34, $s6	# $x34 := $s6
	move 	$x35, $s7	# $x35 := $s7
	move 	$x36, $a0	# $x36 := $a0
	move 	$x37, $ra	# $x37 := $ra
	addi 	$x39, $a0, 0	# $x39 := $a0+0
	lw 	$x38, 0($x39)	# $x38 := [$x39+0]
	addi 	$x42, $a0, 4	# $x42 := $a0+4
	lw 	$x41, 0($x42)	# $x41 := [$x42+0]
	slt	$x46, $x38, $x41	# $x46 := $x38<$x41
	beqz	$x46, L3   	# if (signed) $x46 == 0 goto L3
	seq	$x49, $x38, $x41	# $x49 := $x38==$x41
	li 	$x50, 0		# $x50 := 0
	seq	$x51, $x49, $x50	# $x51 := $x49==$x50
	beqz	$x51, L7   	# if (signed) $x51 == 0 goto L7
	li 	$x52, 1		# $x52 := 1
	move 	$x48, $x52	# $x48 := $x52
	j 	L8		# goto L8
L7:
	li 	$x53, 0		# $x53 := 0
	move 	$x48, $x53	# $x48 := $x53
L8:
	beqz	$x48, L5   	# if (signed) $x48 == 0 goto L5
	li 	$x54, 1		# $x54 := 1
	move 	$x47, $x54	# $x47 := $x54
	j 	L6		# goto L6
L5:
	li 	$x55, 0		# $x55 := 0
	move 	$x47, $x55	# $x47 := $x55
L6:
	move 	$x45, $x47	# $x45 := $x47
	j 	L4		# goto L4
L3:
	li 	$x56, 0		# $x56 := 0
	move 	$x45, $x56	# $x45 := $x56
L4:
	li 	$x57, 0		# $x57 := 0
	seq	$x58, $x45, $x57	# $x58 := $x45==$x57
	beqz	$x58, L1   	# if (signed) $x58 == 0 goto L1
	li 	$x59, 1		# $x59 := 1
	move 	$x44, $x59	# $x44 := $x59
	j 	L2		# goto L2
L1:
	li 	$x60, 0		# $x60 := 0
	move 	$x44, $x60	# $x44 := $x60
L2:
	move 	$x43, $x44	# $x43 := $x44
	move 	$x40, $x43	# $x40 := $x43
	move 	$v0, $x40	# $v0 := $x40
	move 	$s7, $x35	# $s7 := $x35
	move 	$s6, $x34	# $s6 := $x34
	move 	$s5, $x33	# $s5 := $x33
	move 	$s4, $x32	# $s4 := $x32
	move 	$s3, $x31	# $s3 := $x31
	move 	$s2, $x30	# $s2 := $x30
	move 	$s1, $x29	# $s1 := $x29
	move 	$s0, $x28	# $s0 := $x28
	move 	$ra, $x37	# $ra := $x37
	move 	$a0, $x36	# $a0 := $x36
_gt.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
_main:
	move 	$x61, $s0	# $x61 := $s0
	move 	$x62, $s1	# $x62 := $s1
	move 	$x63, $s2	# $x63 := $s2
	move 	$x64, $s3	# $x64 := $s3
	move 	$x65, $s4	# $x65 := $s4
	move 	$x66, $s5	# $x66 := $s5
	move 	$x67, $s6	# $x67 := $s6
	move 	$x68, $s7	# $x68 := $s7
	move 	$x69, $a0	# $x69 := $a0
	move 	$x70, $ra	# $x70 := $ra
	li 	$x71, 5		# $x71 := 5
	li 	$x72, 3		# $x72 := 3
	add	$x73, $x71, $x72	# $x73 := $x71+$x72
	li 	$x74, 7		# $x74 := 7
	mulo	$x75, $x73, $x74	# $x75 := $x73*$x74
	li 	$x77, 7		# $x77 := 7
	add	$x78, $x75, $x77	# $x78 := $x75+$x77
	move 	$x82, $a0	# $x82 := $a0
	li 	$a0, 8		# $a0 := 8
	jal 	alloc		# call alloc
	move 	$x83, $v0	# $x83 := $v0
	sw 	$x75, 0($x83)	# [$x83+0] := $x75
	li 	$x84, 0		# $x84 := 0
	sw 	$x84, 4($x83)	# [$x83+4] := $x84
	move 	$a0, $x83	# $a0 := $x83
	la 	$x85, _gt	# $x85 := _gt
	jalr 	$ra, $x85	# ($s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7) := call $ra($a0,$a1,$a2,$a3,$t0,$t1,$t2,$t3,$t4,$t5,$t6,$t7,$t8,$t9,$v0,$v1)
	move 	$a0, $x82	# $a0 := $x82
	move 	$x81, $v0	# $x81 := $v0
	beqz	$x81, L9   	# if (signed) $x81 == 0 goto L9
	li 	$x88, 7		# $x88 := 7
	seq	$x89, $x75, $x88	# $x89 := $x75==$x88
	li 	$x90, 0		# $x90 := 0
	seq	$x91, $x89, $x90	# $x91 := $x89==$x90
	beqz	$x91, L13   	# if (signed) $x91 == 0 goto L13
	li 	$x92, 1		# $x92 := 1
	move 	$x87, $x92	# $x87 := $x92
	j 	L14		# goto L14
L13:
	li 	$x93, 0		# $x93 := 0
	move 	$x87, $x93	# $x87 := $x93
L14:
	beqz	$x87, L11   	# if (signed) $x87 == 0 goto L11
	move 	$x95, $a0	# $x95 := $a0
	move 	$x97, $a0	# $x97 := $a0
	li 	$a0, 8		# $a0 := 8
	jal 	alloc		# call alloc
	move 	$x98, $v0	# $x98 := $v0
	sw 	$x75, 0($x98)	# [$x98+0] := $x75
	sw 	$x78, 4($x98)	# [$x98+4] := $x78
	move 	$a0, $x98	# $a0 := $x98
	la 	$x99, _add	# $x99 := _add
	jalr 	$ra, $x99	# ($s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7) := call $ra($a0,$a1,$a2,$a3,$t0,$t1,$t2,$t3,$t4,$t5,$t6,$t7,$t8,$t9,$v0,$v1)
	move 	$a0, $x97	# $a0 := $x97
	move 	$x96, $v0	# $x96 := $v0
	move 	$a0, $x96	# $a0 := $x96
	la 	$x100, _addsome	# $x100 := _addsome
	jalr 	$ra, $x100	# ($s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7) := call $ra($a0,$a1,$a2,$a3,$t0,$t1,$t2,$t3,$t4,$t5,$t6,$t7,$t8,$t9,$v0,$v1)
	move 	$a0, $x95	# $a0 := $x95
	move 	$x94, $v0	# $x94 := $v0
	move 	$x86, $x94	# $x86 := $x94
	j 	L12		# goto L12
L11:
	li 	$x101, 0		# $x101 := 0
	li 	$x102, 1		# $x102 := 1
	sub	$x103, $x101, $x102	# $x103 := $x101-$x102
	move 	$x86, $x103	# $x86 := $x103
L12:
	move 	$x80, $x86	# $x80 := $x86
	j 	L10		# goto L10
L9:
	li 	$x105, 0		# $x105 := 0
	li 	$x106, 2		# $x106 := 2
	sub	$x107, $x105, $x106	# $x107 := $x105-$x106
	beqz	$x107, L15   	# if (signed) $x107 == 0 goto L15
	li 	$x108, 0		# $x108 := 0
	move 	$x104, $x108	# $x104 := $x108
	j 	L16		# goto L16
L15:
	li 	$x109, 1		# $x109 := 1
	move 	$x104, $x109	# $x104 := $x109
L16:
	move 	$x80, $x104	# $x80 := $x104
L10:
	move 	$x79, $x80	# $x79 := $x80
	move 	$x76, $x79	# $x76 := $x79
	move 	$v0, $x76	# $v0 := $x76
	move 	$s7, $x68	# $s7 := $x68
	move 	$s6, $x67	# $s6 := $x67
	move 	$s5, $x66	# $s5 := $x66
	move 	$s4, $x65	# $s4 := $x65
	move 	$s3, $x64	# $s3 := $x64
	move 	$s2, $x63	# $s2 := $x63
	move 	$s1, $x62	# $s1 := $x62
	move 	$s0, $x61	# $s0 := $x61
	move 	$ra, $x70	# $ra := $x70
	move 	$a0, $x69	# $a0 := $x69
_main.epilog:
	jr 	$ra		# also uses: $v0,$s0,$s1,$s2,$s3,$s4,$s5,$s6,$s7
