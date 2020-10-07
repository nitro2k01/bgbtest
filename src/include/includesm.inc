IF !DEF(_INCLUDESM)
_INCLUDESM equ 1


BREAK:	MACRO
	ld	b,b
	ENDM


WAITVBL:MACRO
.waitvbl\@
	ldh	A,[rLY]
	cp	144
	jr	nz,.waitvbl\@
	ENDM

WAITNOVBL:MACRO
.waitnovbl\@
	ldh	A,[rLY]
	cp	144
	jr	z,.waitnovbl\@
	ENDM


ENDLESS:MACRO
.endless\@
	jr	.endless\@
	ENDM

;;	"ld	HL,[HL]"
GET_POINTER_AT_HL:MACRO
	ld	A,[HL+]	; Get target address
	ld	H,[HL]
	ld	L,A	; HL now holds target address
	ENDM


pusha:	MACRO
	push	AF
	push	BC
	push	DE
	push	HL
	ENDM

popa:	MACRO
	pop	HL
	pop	DE
	pop	BC
	pop	AF
	ENDM

PUSHA:	MACRO
	pusha
	ENDM

POPA:	MACRO
	popa
	ENDM


ENDC