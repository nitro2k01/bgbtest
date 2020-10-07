	include "hardware.inc"		; Community standard hardware.inc
	include "includesm.inc"		; My own macros.

	; Suggested tab width: 8

	; bgbtest.gb has the nibbles of the joypad variables swapped compared to
	; the community hardware.inc. I happened to standardize on the opposite
	; order compared to most other software, as well as the definitions in
	; hardware.inc. The below purges and redefines the EQUs from hardware.inc.
	;
	; This is needed to produce a binary that's identical to the one shipped 
	; with BGB. The code would mainly work fine if these lines were removed
	; and the swap A instruction in READPAD was moved to after the D pad
	; group instead of the button group. However, this would change the
	; order of the musical scale and the easter egg button sequence. 

PURGE	PADF_DOWN,PADF_UP,PADF_LEFT,PADF_RIGHT,PADF_START,PADF_SELECT,PADF_B,PADF_A
PURGE	PADB_DOWN,PADB_UP,PADB_LEFT,PADB_RIGHT,PADB_START,PADB_SELECT,PADB_B,PADB_A

PADF_DOWN   EQU $08
PADF_UP     EQU $04
PADF_LEFT   EQU $02
PADF_RIGHT  EQU $01
PADF_START  EQU $80
PADF_SELECT EQU $40
PADF_B      EQU $20
PADF_A      EQU $10

PADB_DOWN   EQU $3
PADB_UP     EQU $2
PADB_LEFT   EQU $1
PADB_RIGHT  EQU $0
PADB_START  EQU $7
PADB_SELECT EQU $6
PADB_B      EQU $5
PADB_A      EQU $4

	; Crash handler: endless loop of ld B,B (break).
	; We should certainly never reach this code, but this may be useful for
	; emulator authors debugging their emulator. If a crash would happen on
	; hardware due to a hardware glitch, this might also decrease the risk
	; of graphical corruption from the recursive call to $0038 that would
	; otherwise ensue.
SECTION "CRASH",ROM0[$38]
CRASH::
	BREAK
	jr	CRASH

SECTION "VBLINT",ROM0[$40]
	; Slim interrupt handler: push only AF and HL for potential faster interrupt return.
	push	AF
	push	HL
	jp	HANDLE_VBL

SECTION "LCDINT",ROM0[$48]
LCDINT::
	; Slim interrupt handler: push only AF and HL for potential faster interrupt return.
	push	AF
	push	HL

	; Sine wave effect.
	ld	HL,slowfc		; Slow frame counter.
	ldh	A,[rLY]			; Current scanline.
	cp	80			; Check if we're on the last scanline that should be affected by the effect.
	jr	nc,.lastline
	add	[HL]			; Add frame counter to animate the effect.
	and	$1f			; MOD 32 using bit masking.
	
	ld	H,$0a			; Sine table, high address byte.
	ld	L,A			; Table index.
	ld	A,[HL]
	
	sub	$a			; Add some offset to center the logo
	ldh	[rSCX],A		; Change BG X scroll position.
	pop	HL
	pop	AF
	reti

.lastline
	xor	A			; A=0
	ldh	[rSCX],A		; Restore SCX.
	inc	A			; A=1, only VBlank interrupt enabled.
	ldh	[rIE],A
	pop	HL
	pop	AF
	reti

SECTION "ENTRY",ROM0[$100]
	nop
	jp      ENTRY

SECTION "HEADERFILLER",ROM0[$104]
	; Fill the header with null bytes to prevent other sections from
	; getting placed there. rgbfix will fill in header data.
	rept	$4c
	DB	$00
	endr

SECTION "MAIN",ROM0[$150]
ENTRY::
	di
	ld	SP,$fffe
	WAITVBL			; Wait for VBlank.
	xor	A
	ldh	[rLCDC],A	; Turn off LCD (may only be done in VBlank).

	; Clear memory
	ld	HL,_VRAM	; Clear video RAM.
	ld	DE,$2000	; Length.
	call	CLEAR_MEM	

	ld	HL,_RAM		; Clear work RAM.
	ld	DE,$2000	; Length.
	call	CLEAR_MEM

	; Copy text tiles
	ld	HL,$8010	; Start at tile 1, to keep tile 0 empty.
	ld	BC,TEXT_TILES
	ld	DE,TEXT_TILES_End-TEXT_TILES	; Length.
	call	COPY_MEM_V1BIT	;

	; Copy sprite tiles.
	ld	HL,$8800	; Tile $80.
	ld	BC,SPRITE_TILES
	ld	DE,SPRITE_TILES_End-SPRITE_TILES; Length.
	call	COPY_MEM

	; Copy OAM DMA routine.
	ld	HL,OAM_DMA	; High RAM.
	ld	BC,OAM_DMA_Src
	ld	DE,OAM_DMA_End-OAM_DMA_Src	; Length.
	call	COPY_MEM

	ld	HL,$9800
	ld	DE,TILEDEF
	call	DECOMPRESS

	ld	HL,OAMSOURCE	; The OAM DMA source in WRAM.
	ld	BC,OAMSOURCE_INITVALUES	; Initial OAM contents.
	ld	DE,$a0		; Size of OAM.
	call	COPY_MEM

	call	INIT_SOUND
	
	; Turn on LCD.
	ld	A,LCDCF_ON|LCDCF_WIN9C00|LCDCF_WINOFF|LCDCF_BG8000|LCDCF_BG9800|LCDCF_OBJ8|LCDCF_OBJON|LCDCF_BGON
	ld	[rLCDC],A

	; Set up palettes.
	ld	A,%00011011	; Inverted palette.
	ldh	[rBGP],A
	ldh	[rOBP0],A
	ld	A,%01000000	; OBP1 palette. (Used for the stars.)
	ldh	[rOBP1],A

	; Set up interrupts and scroll positions.
	ld	A,STATF_MODE00	; LCD interrupt reason (every HBlank).
	ldh	[rSTAT],A

	ld	A,IEF_VBLANK|IEF_LCDC
	ldh	[rIE],A

	ld	A,IEF_VBLANK	; Leave wavy effect turned off by default.
	ld	[current_ie],A
	
	ld	A,8		; BG scroll Y position
	ldh	[rSCY],A

	xor	A		; A=0
	ldh	[rIF],A
	ldh	[rSCX],A

	; Init RNG and misc variables.
	ld	hl,SHIFTREG	; Seed shift register with some random-looking values.
	ld	[hl],$c5
	inc	HL
	ld	[hl],$a7
	
	ld	A,1
	ld	[tickcounter],A	; Trigger wave channel on next tick
	
	ld	A,$80
	ld	[easteregg_nextbutton],A	; Init next expected button for the easter egg.

	; All initialization is done. Enable interrupts globally and start the main program.
	ei

	; Main idle loop
.el	halt
	nop
	jr	.el

HANDLE_VBL::
	push	BC		; Push the other two registers that weren't pushed initially.
	push	DE

	; High priority tasks. (Needs VRAM/OAM access. Should be finished within the VBlank period.)
	call	READPAD		; Read button inputs.
	call	KEYINDICATORS	; Draw graphics indicating which buttons are currently held.
	call	OAM_DMA		; OAM DMA. (Updates the stars as well as the D pad indicators.)
	
	; Process frame counters.	
	ld	HL,framecounter	; Frame counter.
	inc	[HL]		; Increments on each frame.
	ld	A,[HL+]		; Read back value. (HL now points to slowfc)
	srl	A		; Right shift used as division. (/2)
	srl	A		; Right shift used as division. (/4)
	ld	[HL],A		; Write slowfc. Slow frame counter that increments every 4th frame. Used for the easter egg sine animation.

	; Re-init some value at the start of the frame.
	ld	A,%01000000	; OBP1 palette. (Used for the stars.)
	ldh	[rOBP1],A

	xor	A
	ldh	[rSCX],A

	ld	A,[current_ie]	; Current value of IE.
	ldh	[rIE],A

	; Low priority tasks. (Don't need to be done within VBlank.)
	call	STARFIELD_ADVANCE	; Advance the state of the starfield animation in shadow OAM.
	call	PLAYNOTES		; Play notes if new buttons have been detected.
	call	MELODY_ADVANCE		; Advance the state of the pseudorandom ch3 melody.
	call	CHECK_SCALE		; Check whether the user is pressing the buttons in the order of the musical scale.

	pop	DE			; Unusual pop order due to slim interrupt handling.
	pop	BC
	pop	HL
	pop	AF
	reti



STARFIELD_ADVANCE::
	ld	BC,OAMSOURCE+4*$10	; Start at sprite slot $10.

	; +0
.starloop
	inc	C			; +1

	inc	C			; +2
	inc	C			; +3
	ld	A,[BC]			; Get attribute byte.
	and	$03			; Get lower bits (speed). Here, we're using memory bits that are unused by the hardware to store the current speed of a star. 
	ld	E,A
	dec	C			; +2
	dec	C			; +1

	ld	A,[BC]			; Get X position.

	sub	E			; Subtract speed value to produce left motion.
	jr	c,.reinitstar		; If carry is set (moved past left side of screen) reinit the star with a random position and speed.
	
	ld	[BC],A			; Get X position.
	inc	C			; +2
	inc	C			; +3
	inc	C			; +4 (next array index)
.starcheck
	ld	A,C			; Check if we're at the end of the OAM shadow buffer.
	cp	$A0
	jr	nz,.starloop
	
	ret

.reinitstar
	ld	A,168
	ld	[BC],A			; Write X position.
	dec	C			; +0
	ld	A,[BC]			; Check if Y position = 01 (reinit X pos needed).
	dec	A
	jr	nz,.nofirstinit
	call	RANDOM			; Generate new random number. (X pos.)
	inc	C			; +1
	ld	[BC],A			; Reinit X pos
	dec	C			; +0
.nofirstinit
	call	RANDOM
	and	$7f			; Limit Y position to 0-$7f
	add	2
	ld	[BC],A			; Write Y position
	inc	C			; +1
	inc	C			; +2
	call	RANDOM			; Generate new random number. (Star type.)
	and	$01			; Limit random value to 0/1
	add	$90			; Star tile offset.
	ld	[BC],A			; Write tile index
	inc	C			; +3
	ld	A,L			; Get some randomness for the scroll speed

	and	$0f
.subloop				; Determine (simplified) YPOS%3
	ld	E,A
	sub	3
	jr	nc,.subloop
	inc	E			; Make sure E is >0

	or	A,OAMF_PRI|OAMF_PAL1
	ld	[BC],A			; Write sprite attributes
	inc	C			; +4 (next array index)

	jr	.starcheck	

; Check for secret button sequence to unlock the sine wave effect.
; Toggle the effect if the notes are played in ascending pitch.
;    04       se st    b  a
; 02 08 01    40 80    20 10
; Start, Select, B, A, Down, Up, Left, Right
CHECK_SCALE::
	ld	A,[joyp_new_buttons]
	or	A
	ret	z			; Return if no buttons were pressed.

	ld	HL,easteregg_nextbutton	; v

	bit	7,A			; v Always check for start. (Or whichever button is represented by bit 7, see the remark at the start.)
	jr	nz,.start		; +->

	and	[HL]			; Check if pressed button matches current expected button
	jr	z,.wrongkey
	ld	A,[HL]
	srl	A			; Move the sentinel bit one step to the right to detect the next button in the sequence.
	jr	c,.sequence_final	; If the bit has reached, toggle the effect
	ld	[HL],A

	ret

.sequence_final
	dec	L			; current_ie
	ld	A,[HL]
	xor	IEF_LCDC
	ld	[HL+],A			; HL+ makes HL=easteregg_nextbutton
	; Fallthrough
.wrongkey
	ld	[HL],$80		; Reset to start value
	ret

.start	
	ld	[HL],$40		; Make sure you can always press start as the first part of a sequence.
					; If start triggers the .wrongkey condition, you'd need to press it again
					; to advance the sequence.
	ret


; Out data
;    04       se st    b  a
; 02 08 01    40 80    20 10
PLAYNOTES::
	ld	A,[joyp_held_buttons]
	xor	PADF_SELECT|PADF_START	; Check if select and start are simultaneously held.
	and	PADF_SELECT|PADF_START	; Sets z if both are held.
	
	ld	A,[joyp_new_buttons]	; Load the newly pressed buttons this frame.
	jr	z,.togglemelody		; Flags are preserved from the check above.
.notogglemelody
	ld	B,A
	ld	HL,SHIFTREG		; Mix up the shift register a bit for more randomness
	xor	[HL]
	ld	A,[HL]
	
	ld	A,B
	ld	HL,NOTETABLE
	
.noteloop				; Iterate through all of the bits in the joypad register
	add	A			; Left shift out one bit into c.

	
	jr	c,.triggernote		; If the button was newly pressed this frame, trigger a note.

	inc	HL			; Step over one entry in the note table for each loop iteration.
	inc	HL
	;or	A			; Zero check. Not needed - the z flag presists from the add A instruction above.
	jr	nz,.noteloop
	
	ret

	; If select is held and start pushed, toggle the wave channel.
	; From the check above, we know that select and start are both held.
	; Additionally check if start was newly pressed this frame.
.togglemelody
	bit	PADB_START,A		; Start
	jr	z,.notogglemelody	; Start was not newly pressed this frame. Go back and check for other buttons...
	ldh	A,[rAUDTERM]		; Start was pressed. Toggle the enable bits for channel 3.
	xor	$44
	ldh	[rAUDTERM],A
	ret				; Return and do not play note for this start press. (This is intended behavior.)
					; However, this means that any other buttons pressed this exact frame will also be ignored.

.triggernote
	push	AF			; Temporarily save AF.
	ld	C,LOW(rAUD1LOW)		; PU1 freq lo.
	ld	B,0			; Don't detune.
	
	ld	A,[currentchannel]	; Toggle which channel is active.
	xor	1
	ld	[currentchannel],A
	jr	nz,.channel1
	ld	C,LOW(rAUD2LOW)		; PU2 freq lo.
	ld	B,2			; Detune.
.channel1
	ld	A,[HL+]			; Read low byte of frequency value.
	add	B			; Add detune parameter.
	ld	[$ff00+C],A		; Write the low byte of the frequency value.
	inc	C			; PU1/2 freq hi is the next byte after freq lo.
	ld	A,[HL+]			; Read high byte of the frequency value.
	or	$80			; Trigger the sound.
	ld	[$ff00+C],A		; Write the high byte of the frequency value.
	
	pop	AF			; Restore AF
	jr	.noteloop		; Continue the loop.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8 specific note values for each of the 8 buttons.
NOTETABLE::
	dw	1546		; C5
	dw	1602		; D5
	dw	1650		; E5
	dw	1673		; F5
	dw	1714		; G5
	dw	1750		; A5
	dw	1767		; A#5
	dw	1798		; C6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advance the state of the pseudorandom CH3 melody.
MELODY_ADVANCE::
	ld	BC,tickcounter	; Tick countdown until the next step.
	ld	A,[BC]
	dec	A
	ld	[BC],A
	ret	nz		; Return of the the tick counter is still >0.

	ld	A,4		; Reload the tick counter. (A new music event happens every 4th frame.)
	ld	[BC],A
	
	ld	C,LOW(rAUD3LEVEL)	; CH3 audio level.
	ld	A,[$ff00+C]	
	and	%01100000	; Mask out the volume bits

	jr	z,.doplay	; z=currently muted. (Play a new note.)
	add	$20		; Cycle through the values %10, %11, %00 (1/2, 1/4, 0).
	ld	[$ff00+C],A
	ret
.doplay
	ld	HL,barcounter	; Counter that is incremented on each step.
	inc	[HL]
	ld	A,[HL]
	and	$02		; Check which step we're on.
	ld	C,A		; Save.
	
	call	RANDOM		; Generate random numbers. Leaves a random value in HL.
	
	ld	A,C		; Restore.
	; On barcounter%4 == 0,1 always play a new note.
	; On barcounter%4 == 2,3 randomly skip the current note with a 50% probability.
	or	A
	jr	z,.noexit
	
	bit	0,L
	ret	z
.noexit

	ld	D,0		; Prepare upper byte of DE for 16-bit addition.
	
	ld	A,H		; Get part of the random value from before.
	and	$07		; A%=8 (8 entries in the note table.)
	add	A		; A*=2 (Each entry is 2 bytes big.)
	ld	E,A		; Prepare lower byte of DE for 16-bit addition.

	ld	HL,NOTETABLE
	add	HL,DE

	ld	A,$80
	ldh	[rAUD3ENA],A	; Turn on CH3 playback.

	ld	A,$40		; 1/2 volume.
	ldh	[rAUD3LEVEL],A

	ld	C,LOW(rAUD3LOW)	; CH3 freq lo.
	
	ld	A,[HL+]		; Read low byte.
	ld	[$ff00+C],A	; Write CH3 freq lo.
	inc	C		; C is now pointing to CH3 freq hi.
	
	ld	A,[HL+]		; Read high byte.
	or	$80		; Trigger note.
	ld	[$ff00+C],A	; Write CH3 freq hi.

	

	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate new random data using a LFSR.
RANDOM::
	ld	DE,SHIFTREG
	ld	A,[DE]
	ld	H,A
	inc	E
	ld	A,[DE]
	ld	L,A


	add	HL,HL
	jr	nc,.nocarry
	set	0,l
.nocarry

	ld	A,L		; Useful for ld	[DE],A later
	bit	7,H
	jr	z,.nocarry2
	xor	1
.nocarry2

	ld	[DE],A
	dec	E
	ld	A,H
	ld	[DE],A

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse the currently held buttons and indicate them on screen.
KEYINDICATORS:
	ld	A,[joyp_held_buttons]	; Currently held buttons.
	ld	B,A
	
	; The button group (select, start, B, A) is indicated in the BG map to save on the number of sprites.

	;;;; Check for select.
	ld	A,$94		; First tile index for the tiles of the select indicator.
	bit	PADB_SELECT,B	; Check if select is pressed.
	jr	nz,.sel
	add	$10		; If not pressed, add $10 to the tile index to produce an unused tiles for clearing the indicator.
.sel
	ld	HL,$9a07	; Position in VRAM for the indicator.
	ld	[HL+],A		; Write or clear the indicator. (Two tiles.)
	inc	A
	ld	[HL+],A
	inc	L

	;;;; Start
	ld	A,$96		; First tile index for the tiles of the start indicator.
	bit	PADB_START,B	; Check if start is pressed.
	jr	nz,.start
	add	$10		; If not pressed, add $10 to the tile index to produce an unused tiles for clearing the indicator.
.start
	;ld	HL,$9a0a	; HL=$9a0a from the select code so this line is redundant and commented out.
	ld	[HL+],A		; Write or clear the indicator. (Two tiles.)
	inc	A
	ld	[HL+],A
	
	;;;; B
	ld	A,$9c		; First tile index for the tiles of the B indicator.
	bit	PADB_B,B	; Check if B is pressed.
	jr	nz,.butb
	add	$10		; If not pressed, add $10 to the tile index to produce an unused tiles for clearing the indicator.
.butb
	ld	HL,$99ad	; Position in VRAM for the indicator. (Four tiles.)
	ld	[HL+],A
	inc	A
	ld	[HL+],A
	inc	L
	inc	A
	ld	L,$cd		; Second row.
	ld	[HL+],A
	inc	A
	ld	[HL-],A

	;;;; A
	ld	A,$98		; First tile index for the tiles of the A indicator.
	bit	PADB_A,B	; Check if A is pressed.
	jr	nz,.buta
	add	$10		; If not pressed, add $10 to the tile index to produce an unused tiles for clearing the indicator.
.buta
	ld	HL,$9990	; Position in VRAM for the indicator. (Four tiles.)
	ld	[HL+],A
	inc	A
	ld	[HL+],A
	inc	L
	inc	A
	ld	L,$b0		; Second row.
	ld	[HL+],A
	inc	A
	ld	[HL-],A

	; The D pad group is indicated using sprites to make use of X and Y mirroring to produce all 4 graphics using a single tile.
	; XOFFSET and YOFFSET set global offsets for tweaking the position of the D pad indicator group.
XOFFSET = 8  +24
YOFFSET = 16 +96
	ld	H,HIGH(OAMSOURCE)		; OAM source high byte.

	;;;; D
	ld	A,YOFFSET+8		; Intended Y position.
	bit	PADB_DOWN,B
	jr	nz,.butD
	xor	A			; If not pressed, Y=0.
.butD
	ld	L,0			; Set Y position for sprites slots 0 and 1.
	ld	[HL],A
	ld	L,4
	ld	[HL],A

	;;;; U
	ld	A,YOFFSET-16		; Intended Y position.
	bit	PADB_UP,B
	jr	nz,.butU
	xor	A			; If not pressed, Y=0.
.butU
	ld	L,8			; Set Y position for sprites slots 2 and 3.
	ld	[HL],A
	ld	L,$c
	ld	[HL],A

	;;;; L
	ld	A,XOFFSET-16		; Intended X position. (Here we set X instead of Y since L and R are graphically vertical.)
	bit	PADB_LEFT,B
	jr	nz,.butL
	xor	A			; If not pressed, X=0.
.butL
	ld	L,$10+1			; Set X position for sprites slots 4 and 5.
	ld	[HL],A
	ld	L,$14+1
	ld	[HL],A

	;;;; R
	ld	A,XOFFSET+8		; Intended X position. (Here we set X instead of Y since L and R are graphically vertical.)
	bit	PADB_RIGHT,B
	jr	nz,.butR
	xor	A			; If not pressed, X=0.
.butR
	ld	L,$18+1			; Set X position for sprites slots 6 and 7.
	ld	[HL],A
	ld	L,$1c+1
	ld	[HL],A

	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clear memory.
;; HL = Destination address
;; DE = Number of bytes to clear
CLEAR_MEM::
	call	ADJUST_E
.clearloop
	ld	[HL+],A
	dec	E
	jr	nz,.clearloop
	dec	D
	jr	nz,.clearloop
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy memory
;; HL = Destination address
;; BC = Source address
;; DE = Number of bytes to copy
COPY_MEM::
	call	ADJUST_E
.clearloop
	ld	A,[BC]
	ld	[HL+],A
	inc	BC
	dec	E
	jr	nz,.clearloop
	dec	D
	jr	nz,.clearloop
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy memory and expand 1 bit compression 
;; HL = Destination address
;; BC = Source address
;; DE = Number of source bytes to copy.
;;      Will produce twice as many output bytes
COPY_MEM_V1BIT::
	call	ADJUST_E
.clearloop
	ld	A,[BC]
	ld	[HL+],A
	;xor	A
	ld	[HL+],A
	inc	BC
	dec	E
	jr	nz,.clearloop
	dec	D
	jr	nz,.clearloop
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adjust length parameter for copy/clear loops
ADJUST_E::
	push	AF
	ld	A,E		
	or	A		; Check if lower byte of length is 0. If not, increment D to give correct to be used in the loop
	jr	z,.lb_is_zero
	inc	D
.lb_is_zero
	pop	AF
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decompress RLE etc data
DECOMPRESS::
tileprint_loop::
	ld	A,[DE]
	inc	DE
	cp	$c0			; RLE (Run length encoding)
	jr	z,rle

	cp	$e0			; Extended command.
	jr	z,ext_cmd

	ld	[HL+],A			; Misc character. Continue.
	jr	tileprint_loop

seq_plus::
	ld	A,[DE]			; Read start value.
	ld	B,A
	inc	DE
	ld	A,[DE]			; Read length.
	ld	C,A
	ld	A,B
	inc	DE
seq_plus_loop:
	ld	[HL+],A

	inc	A
	dec	C
	jr	nz,seq_plus_loop
	jr	tileprint_loop

rle::
	ld	A,[DE]			; Read the byte to repeat.
	ld	B,A
	inc	DE
	ld	A,[DE]			; Number of times.
	ld	C,A
	ld	A,B
	inc	DE
rle_loop::
	ld	[HL+],A
	dec	C
	jr	nz,rle_loop
	jr	tileprint_loop

ext_cmd::
	ld	A,[DE]
	inc	DE
	
	cp	$ff
	ret	z			; EOF. Return.

	cp	$f3
	jr	z,seq_plus		; Sequence.

.el
	ld	b,b			; Error condition. Break.
	jr	.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The OAM DMA routine.
OAM_DMA_Src::
	ld   a,HIGH(OAMSOURCE)
	ldh   [rDMA],a
	ld   a,$28
.dmaloop
	dec  a
	jr   nz,.dmaloop
	ret
OAM_DMA_End::



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reads the joypad state and puts the result in joyp_held_buttons and joyp_new_buttons.
READPAD::
; Out data
;    04       se st    b  a
; 02 08 01    40 80    20 10
	ld	A,P1F_GET_DPAD	; Select D pad.
	ldh	[rP1],A
	ldh	A,[rP1]
	ldh	A,[rP1]
	ldh	A,[rP1]
	ldh	A,[rP1]
	cpl			; Invert a to make pressed keys 1.
	and	$0f		; Mask lower nibble.
	ld	B,A		; Store result in B temporarily.


	ld	A,P1F_GET_BTN	; Select button group.
	ldh	[rP1],A
	ldh	A,[rP1]
	ldh	A,[rP1]
	ldh	A,[rP1]
	ldh	A,[rP1]
	cpl			; Invert A to make pressed keys 1
	and	$0f		; Mask lower nibble
	swap	A		; Put button group data into the upper nibble.
	or	B		; Combine the values.
	ld	B,A		; Store result in B temporarily.
	ld	A,[joyp_held_buttons]	; Load which buttons were held in the previous frame.
	xor	B		; Produce the difference.
	and	B		; Mask by the currently held buttons to produce the newly pressed buttons.
	ld	[joyp_new_buttons],A
	ld	A,B
	ld	[joyp_held_buttons],a	; Store which buttons are currently held.
	ld	A,P1F_GET_NONE	; Deactivate joypad readout.
	ld	[rP1],A
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the sound hardware
INIT_SOUND:
	xor	A
	ldh	[rAUDENA],A	; Turn all sound off.
	nop
	nop
	nop
	nop
	
	ld	A,$80
	ldh	[rAUDENA],A	; Turn the sound circutry on again.

	ld	A,$ff
	ldh	[rAUDTERM],A	; Route all sound everywhere.

	ld	A,$77
	ldh	[rAUDVOL],A	; Master volume = full.
	
	xor	A
	ldh	[rAUD1SWEEP],A	; No sweep for ch1.
	ldh	[rAUD3LEVEL],A	; CH3 level.


	ld	A,$40		; 25% duty.
	ldh	[rAUD1LEN],A
	ldh	[rAUD2LEN],A

	ld	A,$94		; Volume envelope.
	ldh	[rAUD1ENV],A
	ldh	[rAUD2ENV],A
	
	ld	HL,CH3_WAVE
	ld	C,LOW(_AUD3WAVERAM)	; CH3 wave RAM.
.loop
	ld	A,[HL+]			; Copy the waveform into CH3 wave RAM.
	ld	[$ff00+C],A
	inc	C
	ld	A,C
	cp	LOW(_AUD3WAVERAM)+$10	; Is C==the byte after wave RAM?
	jr	nz,.loop	

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A sawtooth wave, as well as a few other waveform types I experimented with. 
CH3_WAVE::
	db	$00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ee,$ff		; Sawtooth wave
	;db	$01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98,$76,$54,$32,$10		; Triangle wave
	;db	$00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff		; Square wave
	;db	$00,$00,$00,$00,$cc,$cc,$cc,$cc,$44,$44,$44,$44,$ff,$ff,$ff,$ff		; Octave interval square wave

SECTION "GRAPHICS",ROM0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All tile data used by the ROM.

	; 1 bpp tiles
TEXT_TILES::
	; 1 bpp tile for BGB text
	incbin "res/bgb-text-1b.bin"
	; "presbuton" tiles
	incbin "res/presbuton-1b.bin"
TEXT_TILES_End::

	; 2 bpp tiles
SPRITE_TILES::
	; 2 bpp tiles for sprites
	incbin "res/bgb-linv.bin"	

	; Stars. Manually created graphics.
	db	%11000000
	db	%11000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000

	db	%11110000
	db	%11110000
	db	%11110000
	db	%11110000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000
	db	%00000000

	; 2 bpp tiles for sprites.
	incbin "res/buttons.bin"
SPRITE_TILES_End::


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compressed definition for the tilemap data. "Write once, read never" style data.
TILEDEF::
	; BGB icon and text.
	db	$c0, $00, $20*3+6-5	; RLE: $c0, <data>, <times>
	
	db	$e0, $f3, $80, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$00
	db	$e0, $f3, $01, $08	; Sequence: $c0, $f3, <start value>, <times>
	db	$e0, $f3, $01, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$c0, $00, $20-$0c-5	; RLE: $c0, <data>, <times>

	db	$e0, $f3, $84, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$00
	db	$e0, $f3, $09, $08	; Sequence: $c0, $f3, <start value>, <times>
	db	$e0, $f3, $09, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$c0, $00, $20-$0c-5	; RLE: $c0, <data>, <times>

	db	$e0, $f3, $88, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$00
	db	$e0, $f3, $11, $08	; Sequence: $c0, $f3, <start value>, <times>
	db	$e0, $f3, $11, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$c0, $00, $20-$0c-5	; RLE: $c0, <data>, <times>

	db	$e0, $f3, $8c, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$00
	db	$e0, $f3, $19, $08	; Sequence: $c0, $f3, <start value>, <times>
	db	$e0, $f3, $19, $04	; Sequence: $c0, $f3, <start value>, <times>
	db	$c0, $00, 2*$20+$0e+3	; RLE: $c0, <data>, <times>
PB_offset	= $21		; Tile index offset at which the letters are stored.
	;PRESS BUTTONS text.
	db	PB_offset,PB_offset+1,PB_offset+2,PB_offset+3,PB_offset+3,0,PB_offset+4,PB_offset+5,PB_offset+6,PB_offset+6,PB_offset+7,PB_offset+8,PB_offset+3
	db	$c0, $00, $20-10	; RLE: $c0, <data>, <times>
	db	PB_offset+6,PB_offset+7,0,PB_offset+6,PB_offset+2,PB_offset+3,PB_offset+6
	
	db	$e0, $ff		; EOF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial OAM contents.
OAMSOURCE_INITVALUES::
	; Initial values for the D pad graphics. (Those sprites are actually 
	; shown/hidden by changing the tile index so the Y/X values stay 
	; constant forever.)
XOFFSET = 8  +24
YOFFSET = 16 +96
PATOFFSET = $92		; Tile index for the D pad corner piece.
	; D
	db	YOFFSET+8,	XOFFSET-8,	PATOFFSET+0, OAMF_YFLIP
	db	YOFFSET+8,	XOFFSET+0,	PATOFFSET+0, OAMF_YFLIP|OAMF_XFLIP
	
	; U
	db	YOFFSET-16,	XOFFSET-8,	PATOFFSET+0, 0
	db	YOFFSET-16,	XOFFSET+0,	PATOFFSET+0, OAMF_XFLIP

	; L
	db	YOFFSET-8,	XOFFSET-16,	PATOFFSET+0, 0
	db	YOFFSET+0,	XOFFSET-16,	PATOFFSET+0, OAMF_YFLIP

	; R
	db	YOFFSET-8,	XOFFSET+8,	PATOFFSET+0, OAMF_XFLIP
	db	YOFFSET+0,	XOFFSET+8,	PATOFFSET+0, OAMF_XFLIP|OAMF_YFLIP


	rept	8
	db	0,0,0,0
	endr

	; Sprites for sel, start, B, A
	; Actually unused and commented out using "IF 0" because those
	; indicators live in the tilemap. 
	IF	0;;;; Start of commented out code.
XOFFSET = 8  +8				; Pos -8
YOFFSET = 16 +24			; Pos -16
PATOFFSET = $80

	db	YOFFSET+0,	XOFFSET+0,	PATOFFSET+0, 0
	db	YOFFSET+0,	XOFFSET+8,	PATOFFSET+1, 0
	db	YOFFSET+0,	XOFFSET+16,	PATOFFSET+2, 0
	db	YOFFSET+0,	XOFFSET+24,	PATOFFSET+3, 0

	db	YOFFSET+8,	XOFFSET+0,	PATOFFSET+4, 0
	db	YOFFSET+8,	XOFFSET+8,	PATOFFSET+5, 0
	db	YOFFSET+8,	XOFFSET+16,	PATOFFSET+6, 0
	db	YOFFSET+8,	XOFFSET+24,	PATOFFSET+7, 0

	db	YOFFSET+16,	XOFFSET+0,	PATOFFSET+8, 0
	db	YOFFSET+16,	XOFFSET+8,	PATOFFSET+9, 0
	db	YOFFSET+16,	XOFFSET+16,	PATOFFSET+10, 0
	db	YOFFSET+16,	XOFFSET+24,	PATOFFSET+11, 0

	db	YOFFSET+24,	XOFFSET+0,	PATOFFSET+12, 0
	db	YOFFSET+24,	XOFFSET+8,	PATOFFSET+13, 0
	db	YOFFSET+24,	XOFFSET+16,	PATOFFSET+14, 0
	db	YOFFSET+24,	XOFFSET+24,	PATOFFSET+15, 0
	ENDC	;;;; End of commented out code.

	rept	$a0-(4*16)
	db	$01
	endr

	; Sine table for the easter egg animation.
SECTION "SINETABLE",ROM0[$0a00]
	db	0, 1, 1, 2, 3, 4, 5, 7, 8, 9, 11, 12, 13, 14, 15, 15, 15, 15, 15, 14, 13, 12, 11, 9, 8, 7, 5, 4, 3, 2, 1, 1

SECTION "VARS",WRAM0
SHIFTREG:	dw		; Shift register state (RNG)
joyp_held_buttons:	db	; Currently held buttons.
joyp_new_buttons:	db	; Newly pressed buttons this frame.

currentchannel:	db		; Which of the pulse channels is in use.

tickcounter:	db		; Counter that counts down until the next note step.

barcounter:	db		; Counter that keeps track of which "step" the notes are on.


; framecounter and slowfc must be consecutive. (Relies on pointer increments to access the second variable.)
framecounter:	db		; Frame counter (increments each frame.)
slowfc:		db		; Slow frame counter that increments every 
				; 4th frame. Used for the easter egg sine animation.

; current_ie and easteregg_nextbutton must be consecutive. (Relies on pointer increments to access the second variable.)
current_ie:	db		; The current global value of rIE. (The easter
				; egg effect turns itself off at a certain 
				; line, and the VBL handler restores this value.)
easteregg_nextbutton:	db	; The next expected button press for the easter 
				; egg activation sequence.

SECTION "OAMSOURCE",WRAM0[$c100]
OAMSOURCE:	ds	$A0	; Shadow OAM storage.

SECTION "HIVARS",HRAM[$FF80]
OAM_DMA:
	ds	OAM_DMA_End-OAM_DMA_Src	; Space for the OAM DMA routine.
