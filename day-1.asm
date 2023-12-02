[bits 16]
[org 0x7c00]     ; BIOS will load us to this address
section .text

%define disk_num (0x7c00 + 512)
%define disk_buffer (disk_num + 0x02) ; Retain the disk number, don't want unaligned reads
%define num_sectors 63

%macro begin_print 0
	push ax
	mov ax, 0xb800          ; Console memory is at 0xb8000; set up a segment
        mov es, ax              ; for the start of the console text.
	xor di, di              ; Clear the data index so that 'stosb' pushes to video memory.
	pop ax
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
	push bx
	push cx
	begin_print
	mov ax, 1
        call print_digits
	mov ax, 2
        call print_digits
	mov ax, 3
        call print_digits
	mov ax, 4
        call print_digits
	mov ax, 
        call print_digits
sleep:
        hlt                      ; Halts CPU until the next external interrupt is fired
        jmp sleep                ; Loop forever

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading from Disk

;;; Load the input file into memory, starting at 0x7e00
load_file:
        mov ah, 0x02            ; We want to read sectors, so we set %ah to 0x02
        mov al, num_sectors     ; Number of sectors to read
        mov ch, 0               ; Cylinder index
        mov cl, 2               ; Sector index
        mov dh, 0               ; Head index
        mov dl, [disk_num]      ; Disk number
        xor bx, bx              ; INT 0x13 will write to %es:%bx,
        mov es, bx              ; so let's set the sector to 0x0,
        mov bx, disk_buffer     ; Write to 0x7e00
        int 0x13                ; Ask the BIOS to read the data
        
        mov si, 0x02                ; Set up the source index for 'lodsb'
        mov bx, (disk_num / 0x10)   ; and set the data segment to '0x7e00'
        mov ds, bx
        ret

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
.read_char:
        lodsb                   ; Load a byte into %al, incrementing %si
	cmp al, 0               ; If we are looking at 0, then we have hit the end of the input.
	je .done                ; This means we are done, so we bail out.
        cmp al, `\n`            ; Check if we are looking at a newline,
        je .add_digits          ; if we are, then we proceed to add the 2 digits we've seen.
        cmp al, `a`             ; Check if we are looking at a letter,
        jge .read_char          ; If we are, then continue the loop.
        sub ax, `0`             ; Convert from ASCII to the actual value
        cmp bl, 0               ; Check to see if this is the first digit we've seen
        jne .not_first          ; If it isn't skip ahead to next part of state machine
.first:
        mov bl, al              ; Store the first digit we've seen in %bx
        jmp .read_char
.not_first:
        mov cl, al              ; Store the digit in %cx, clobbering any previous value
        jmp .read_char
.add_digits:
        mov al, bl              ; Move the first digit into %ax,
	test cl, cl             ; Check to see if we've seen a second digit
	jz .accumulate
.add_second_digit:
        mov dx, 10              ; If we have, multiply %ax then multiply by 10.
        mul dx                  
        add al, cl              ; Next, add on the second digit.
.accumulate:
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

hex_table:
	db '0123456789abcdef'


;; Prints contents of %ax as hex.
;; Clobbers %bx
print_digits:
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

	ret


; ;; Input:
; ;; %bx holds number to be printed
; ;; %cx holds number of digits, 1 indexed.
; print_digits:
;         mov ax, bx              ; Move the number into %ax,
;         xor cx, cx              ; We will use %cx to count the number of digits
; .push:
;         cmp ax, 0
;         je .print
;         mov dx, 0               ; Clear %dx so that we don't have an offset for the division 
;         mov bx, 10              ; Set %cx to the constant 10 for the DIV
;         div bx                  ; Divide %ax by 10, storing renaminder in %dx
;         add dx, `0`             ; Convert %dx to ASCII
;         push dx                 ; Push %dl to the stack, so we can pop off in reverse
;         inc cx                  ; Increment the count of digits
;         jmp .push
; .print:
;         cmp cx, 0
;         je .done
;         pop ax
;         stosb
;         mov ax, 0x07
;         stosb
;         dec cx
;         jmp .print
; .done:
;         ret

times 510-($-$$) db 0     ; Pad to 510 bytes
dw 0xaa55                 ; Add boot magic word to mark us as bootable

incbin "day-1-input.txt"

times (63 * 512)-($-$$) db 0    ; Pad out to a multiple of 512 bytes
