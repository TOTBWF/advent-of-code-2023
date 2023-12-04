[bits 16]
[org 0x7c00]     ; BIOS will load us to this address
section .text

%define disk_num (0x7c00 + 0x200)
%define disk_buffer disk_num ; Retain the disk number, don't want unaligned reads
%define data_start (0x7c00 + 0x600)
%define num_sectors 63

%macro begin_print 0
	mov ax, 0xb800          ; Console memory is at 0xb8000; set up a segment
        mov es, ax              ; for the start of the console text.
	xor di, di              ; Clear the data index so that 'stosb' pushes to video memory.
	mov ax, 0               ; Set the data segment to 0, makes 'xlat' lookups resolve correctly
	mov ds, ax 		
%endmacro

boot:
        mov ax, 0x7000		; Place the stack segment in the upper half of memory
        mov ss, ax
	mov sp, 0x2000       	; 8k stack
        mov [disk_num], dl      ; Stash the disk number into memory.
        cld

main:
	; Clear the screen
	mov ah, 0x07            ; We want to scroll the screen
	mov al, 0x00            ; Setting %al to 0 clears the screen
	mov bh, 0x07            ; Background color is black, foreground is white
	mov cx, 0x00            ; Top of screen is (0,0)
	mov dh, 24              ; 24 rows of chars
	mov dl, 79              ; 79 cols of chars
	int 0x10                ; Ask the BIOS to scroll the screen

        call load_file		; Read all the data from dist

        call part1

	begin_print
	mov ax, bx
        call print_digits
	mov ax, cx
        call print_digits
sleep:
        hlt                      ; Halts CPU until the next external interrupt is fired
        jmp sleep                ; Loop forever

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading from Disk

;;; Load the input file into memory, starting at 0x7e00
load_file:
        mov ah, 0x02                       ; We want to read sectors, so we set %ah to 0x02
        mov al, num_sectors                ; Number of sectors to read
        mov ch, 0                          ; Cylinder index
        mov cl, 2                          ; Sector index
        mov dh, 0                          ; Head index
        mov dl, [disk_num]                 ; Disk number
        xor bx, bx                         ; INT 0x13 will write to %es:%bx,
        mov es, bx                         ; so let's set the sector to 0x0,
        mov bx, disk_buffer                ; Write to 0x7e00
        int 0x13                           ; Ask the BIOS to read the data
        
        mov si, 0x00                       ; Set up the source index for 'lodsb'
        mov bx, (data_start / 0x10)          ; and set the data segment to the start of the data.
        mov ds, bx
        ret

times 510-($-$$) db 0     ; Pad to 510 bytes
dw 0xaa55                 ; Add boot magic word to mark us as bootable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Solution

;; Calling Convention:
;; %ds stores address of 1st character of string
;; %si stores offset into string
;; Clobbers %ax, %bx, %cx, %dx
;; Updates %si to the end of the string.
;; Outputs low portion in %cx, high in %bx

part1:
	push 0
	push 0
.read_line:
	xor ax, ax              ; Initialize %ax to 0
        xor bx, bx              ; Initialize %bx to 0 
        xor cx, cx              ; Initialize %cx to 0
	xor dx, dx              ; Holds first digit seen
.read_char:
        lodsb                   ; Load a byte into %al, incrementing %si
	cmp al, 0               ; If we are looking at 0, then we have hit the end of the input.
	je .done                ; This means we are done, so we bail out.
        cmp al, `\n`            ; Check if we are looking at a newline,
        je .add_digits          ; if we are, then we proceed to add the 2 digits we've seen.
        cmp al, `a`             ; Check if we are looking at a letter,
        jge .read_char          ; If we are, then continue the loop.
        sub ax, `0`             ; Convert from ASCII to the actual value
        cmp dl, 0               ; Check to see if this is the first digit we've seen
	cmove dx, ax            ; Only store the first digit we've seen
	mov cx, ax              ; Always update the second digit
        jmp .read_char
.add_digits:
        mov al, dl              ; Move the first digit into %ax,
        mov dx, 10              ; multiply %ax by 10
        mul dx                  
        add al, cl              ; Next, add on the second digit.
	pop bx                  ; Now that we have our number, let's pop off the low part of the accumulator
	add ax, bx              ; Add the number and accumulator, check for overflow
	jnc .no_carry
.carry:
	pop cx                  ; If there was overflow, then we need to update the high portion
	inc cx                  ; Pop, increment, and push
	push cx
.no_carry:
	push ax                 ; Finally, push the low portion.
	jmp .read_line
.done:
	pop cx
	pop bx
        ret

;; State machine

;; 1,  n -> 7
;; 7,  e -> 5  (output 1)
;; 7,  i -> 23

;; 2,  w -> 8
;; 2,  h -> 9

;; 8,  o -> 1 (output 2)

;; 9,  r -> 10
;; 10, e -> 11
;; 11, e -> 5  (output 3)
;; 11, i -> 20

;; 3,  o -> 12
;; 3,  i -> 14
;; 12, u -> 13
;; 12, n -> 7
;; 13, r -> 0  (output 4)

;; 14, v -> 15
;; 15, e -> 5  (output 5)

;; 4,  i -> 16
;; 4,  e -> 17
;; 16, x -> 0  (output 6)

;; 17, i -> 20
;; 17, v -> 18
;; 18, e -> 19
;; 19, i -> 20
;; 19, n -> 6  (output 7)

;; 5,  i -> 20
;; 20, g -> 21
;; 21, h -> 22
;; 22, t -> 2  (output 8)

;; 6,  i -> 23
;; 23, n -> 24
;; 24, e -> 5  (output 9)
;; 24, i -> 23

;; Fallthrough cases:
;; *, o -> 1
;; *, t -> 2
;; *, f -> 3
;; *, s -> 4
;; *, e -> 5
;; *, n -> 6

; jump_table:
; 	dw 0
; 	dw (part2.s01 - part2.s00)
; 	dw (part2.s01 - part2.s00)
; 	dw (part2.s02 - part2.s00)
; 	dw (part2.s03 - part2.s00)
; 	dw (part2.s04 - part2.s00)
; 	dw (part2.s05 - part2.s00)
; 	dw (part2.s06 - part2.s00)
; 	dw (part2.s07 - part2.s00)
; 	dw (part2.s08 - part2.s00)
; 	dw (part2.s09 - part2.s00)
; 	dw (part2.s10 - part2.s00)
; 	dw (part2.s11 - part2.s00)
; 	dw (part2.s12 - part2.s00)
; 	dw (part2.s13 - part2.s00)
; 	dw (part2.s14 - part2.s00)
; 	dw (part2.s15 - part2.s00)
; 	dw (part2.s16 - part2.s00)
; 	dw (part2.s17 - part2.s00)
; 	dw (part2.s18 - part2.s00)
; 	dw (part2.s19 - part2.s00)
; 	dw (part2.s20 - part2.s00)
; 	dw (part2.s21 - part2.s00)
; 	dw (part2.s22 - part2.s00)
; 	dw (part2.s23 - part2.s00)
; 	dw (part2.s24 - part2.s00)

; state_0_table:
; 	db '00005300000006100042000000'


; %macro transition 2
; 	cmp ax, $1
; 	mov di, $2
; 	cmove dx, di
; 	je .read_char
; 	jne .s00
; %endmacro

; %macro transition_accept 3
; 	cmp al, $1
; 	mov di, $2
; 	cmove dx, di
; 	mov di, $3
; 	cmove ax, di
; 	je .set_digit
; 	jne .s00
; %endmacro

; %macro transition_branch 4
; 	cmp al, $1
; 	mov di, $2
; 	cmove dx, di
; 	je .read_char
; 	cmp ax, $3
; 	mov di, $4
; 	cmove dx, di
; 	je .read_char
; 	jne .s00
; %endmacro

; %macro transition_branch_accept 5
; 	cmp al, $1
; 	mov di, $2
; 	cmove dx, di
; 	mov di, $3
; 	cmove ax, di
; 	je .set_digit
; 	cmp al, $4
; 	mov di, $5
; 	cmove dx, di
; 	je .read_char
; 	jne .s00
; %endmacro

; part2:
; 	push 0
; 	push 0
; .read_line:
; 	xor ax, ax              ; Initialize %ax to 0
;         xor bx, bx              ; Initialize %bx to 0 
;         xor cx, cx              ; Initialize %cx to 0
;         xor dx, dx              ; Initialize %dx to 0
; 	;; General Algorithm
; .read_char:
;         lodsb                   ; Load a byte into %al, incrementing %si
; 	cmp al, 0               ; If we are looking at 0, then we have hit the end of the input.
; 	je .done                ; This means we are done, so we bail out.
;         cmp al, `\n`            ; Check if we are looking at a newline,
;         je .add_digits          ; if we are, then we proceed to add the 2 digits we've seen.
;         cmp al, `a`             ; Check if we are looking at a letter,
;         jle .digit              ; If we are, then enter the state machine. If not, then we are looking at a digit.
; 	push bx
; 	mov bx, jump_table 	
; 	xchg al, dl 		; Swap %al and %dl so we can do a table lookup
; 	xlat 			; TODO: These should be byte offsets?
; 	xchg al, dl
; .s00: 			
; 	push bx 		; Grab the new state from the table
; 	mov bx, state_0_table
; 	xlat
; 	mov dl, al
; 	pop bx
; 	je .read_char
; .s01: transition               `n`, 7
; .s02: transition_branch        `w`, 8, `h`, 9
; .s03: transition_branch        `o`, 12, `i`, 4
; .s04: transition_branch        `i`, 16, `e`, 17
; .s05: transition               `i`, 20
; .s06: transition               `i`, 23
; .s07: transition_branch_accept `e`, 5, 1, `i`, 23
; .s08: transition_accept        `o`, 1, 2
; .s09: transition               `r`, 10
; .s10: transition               `e`, 11
; .s11: transition_branch_accept `e`, 5, 3, `i`, 21
; .s12: transition_branch        `u`, 13, `n`, 7
; .s13: transition_accept        `r`, 0, 4
; .s14: transition               `v`, 15
; .s15: transition_accept        `e`, 5, 5
; .s16: transition_accept        `x`, 0, 6
; .s17: transition_branch        `v`, 18, `g`, 21
; .s18: transition               `e`, 19
; .s19: transition_branch_accept `n`, 6, 7, `g`, 21
; .s20: transition               `g`, 21
; .s21: transition               `h`, 22
; .s22: transition_accept        `t`, 2, 8
; .s23: transition               `n`, 24
; .s24: transition_branch_accept `e`, 5, 9, `i`, 23
	

; .digit:
; 	sub al, `0` 		; Convert to ASCII
; .set_digit:
;         cmp bl, 0               ; Check to see if this is the first digit we've seen
; 	cmove bx, ax 		; If it is, move it into both positions
; 	cmove cx, ax
; 	cmovne cx, ax		; If it isn't, only update the second digit
	; 	jmp .read_char

hex_table:
	db '0123456789abcdef'

;; Prints contents of %ax as hex.
;; Clobbers %bx
print_digits:
	push bx
	push ax			; Save contents of %ax, push '0x' to video memory.
	mov ax, `0`
	stosb			
	mov al, 0x07		
	stosb			
	mov ax, `x`
	stosb			
	mov al, 0x07		
	stosb

	pop ax
	push ax
	mov bx, hex_table

	mov al, ah 		; We will print one byte at a time, starting with the high byte
	shr al, 4 		; %al now has the low nibble.
	and ah, 0x0f 		; %ah now has the high nibble.
	xlat 			; %al now contains the right character for the high nibble.
	stosb			; Push that byte to video memory
	mov al, 0x07		; Add color information
	stosb			; Push color information
	xchg al, ah		; Do the same for the low nibble
	xlat 			; %al now contains the right character for the low nibble.
	stosb			; Push that byte to video memory
	mov al, 0x07		; Add color information
	stosb			; Push color information

	pop ax
	mov ah, al 		; On to the low byte
	shr al, 4 		; %al now has the low nibble.
	and ah, 0x0f 		; %ah now has the high nibble.
	xlat 			; %al now contains the right character for the high nibble.
	stosb			; Push that byte to video memory
	mov al, 0x07		; Add color information
	stosb			; Push color information
	xchg al, ah		; Do the same for the low nibble
	xlat 			; %al now contains the right character for the low nibble.
	stosb			; Push that byte to video memory
	mov al, 0x07		; Add color information
	stosb			; Push color information

	mov ax, ` `  		; print a space character
	stosb			
	mov al, 0x07		
	stosb

	pop bx
	ret
times (3 * 512)-($-$$) db 0    ; Pad out to a multiple of 512 bytes

incbin "day-1-input.txt"

times (63 * 512)-($-$$) db 0    ; Pad out to a multiple of 512 bytes
