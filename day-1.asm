[bits 16]
[org 0x7c00]     ; BIOS will load us to this address
section .text

%define data 0x7e00
%define disk_num data
%define disk_buffer data                 ; Don't want unaligned reads
%define num_sectors ((22521 / 512) + 1)  ; Input file is 22521 bytes, so we need to make sure that we have enough space

%macro print_char 1
        mov ax, $1
        stosb
        mov ax, 0x07
        stosb
%endmacro

boot:
        mov ax, (data / 0x10)
        mov ds, ax
        mov ax, 0xffff
        mov ss, ax
        mov [disk_num], dl      ; Stash the disk number into memory.
        cld

main:
        ; Clear the screen
        mov ax, 0xb800           ; Console memory is at 0xb8000; set up a segment
        mov es, ax               ; for the start of the console text.
        mov ax, es
        xor di, di
        mov cx, 80*25            ; Number of chars in the screen
        mov al, ` `              ; Space character
        mov ah, 0x0f             ; Color (white on black)
        repne stosw              ; Copy!

                                ; Read data from disk
        call load_file
        call part1
	pop bx
        call print_digits
	; pop bx
        ; call print_digits
sleep:
        hlt                      ; Halts CPU until the next external interrupt is fired
        jmp sleep                ; Loop forever

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading from Disk

;;; Load the input file into memory, starting at 0x7e00
load_file:
        mov ah, 0x02            ; We want to read sectors, so we set %ah to 0x02
        mov al, 1               ; Number of sectors to read
        mov ch, 0               ; Cylinder index
        mov cl, 2               ; Sector index
        mov dh, 0               ; Head index
        mov dl, [disk_num]      ; Disk number
        xor bx, bx              ; INT 0x13 will write to %es:%bx,
        mov es, bx              ; so let's set the sector to 0x0,
        mov bx, disk_buffer     ; Write to 0x7e00
        int 0x13                ; Ask the BIOS to read the data

        
        mov si, 0x00            ; Set up the source index for 'lodsb',
        ; mov bx, (data / 0x10)   ; and set the data segment to '0x7e00'
        ; mov ds, bx
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Solution

;; Calling Convention:
;; %ds stores address of 1st character of string
;; %si stores offset into string
;; Clobbers %ax, %bx, %cx, %dx
;; Updates %si to the end of the string.
part1:
        mov dx, 10              ; Set %dx to 10 for later multiplications
	push 0 			; We will use the stack to hold our accumulator.
	push 0 			; The value may exceed 65535, so we need to use 2 registers worth of space.
.read_file:
	xor ax, ax              ; Initialize %ax to 0
        xor bx, bx              ; Initialize %bx to 0 
        xor cx, cx              ; Initialize %cx to 0
.read_line:
        lodsb                   ; Load a byte into %al, incrementing %si
        cmp al, `\n`            ; Check if we are looking at a newline,
        je .add_digits          ; if we are, then we proceed to add the 2 digits we've seen.
	; cmp al, 0               ; If we are looking at 0, then we have hit EOf
	je .done                ; This means we are done, so we bail out.
        cmp al, `a`             ; Check if we are looking at a letter,
        jge .read_line          ; If we are, then continue the loop.
        sub ax, `0`             ; Convert from ASCII to the actual value
        cmp bl, 0               ; Check to see if this is the first digit we've seen
        jne .not_first          ; If it isn't skip ahead to next part of state machine
.first:
        mov bl, al              ; Store the first digit we've seen in %bx
        jmp .read_line
.not_first:
        mov cl, al              ; Store the digit in %cx, clobbering any previous value
        jmp .read_line
.add_digits:
        mov al, bl              ; Move the first digit into %ax,
        mul dx                  ; and then multiply by 10.
        add al, cl              ; Next, add on the second digit.
	pop bx 			; Pop off the lower portion of the accumulator
	add ax, bx              ; and then add the low portions together.
	jno .no_overflow	; If there was no overflow, then we do not need to modify the upper portion
.overflow
	pop cx                  ; The sum may exceed 65535, so if we overflow,
	inc cx 			; we need to update the upper portion.
	push cx      		; Push the higher portion
.no_overflow
	push ax                 ; Push the lower portion
.done:
        ret

;; Calling Convention:
;; %bx holds number
;; %cx holds number of digits, 1 indexed.
print_digits:
        mov ax, 0xb800          ; Console memory is at 0xb8000; set up a segment
        mov es, ax              ; for the start of the console text.
        mov ax, bx              ; Move the number into %ax,
        xor cx, cx              ; We will use %bx to count the number of digits
        xor di, di              ; Clear %si, will be used for pushing bytes
.push:
        cmp ax, 0
        je .print
        mov dx, 0               ; Clear %dx so that we don't have an offset for the division 
        mov bx, 10              ; Set %cx to the constant 10 for the DIV
        div bx                  ; Divide %ax by 10, storing renaminder in %dx
        add dx, `0`             ; Convert %dx to ASCII
        push dx                 ; Push %dl to the stack, so we can pop off in reverse
        inc cx                  ; Increment the count of digits
        jmp .push
.print:
        cmp cx, 0
        je .done
        pop ax
        stosb
        mov ax, 0x07
        stosb
        dec cx
        jmp .print
.done:
        ret

times 510-($-$$) db 0     ; Pad to 510 bytes
dw 0xaa55                 ; Add boot magic word to mark us as bootable

incbin "day-1-input.txt"

times ((num_sectors + 1) * 512)-($-$$) db 0    ; Pad out to a multiple of 512 bytes
