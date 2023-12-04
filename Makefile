.PHONY: day-1

day-1.img: day-1.asm day-1-input.txt
	nasm day-1.asm -f bin -o day-1.img

day-1: day-1.img day-1.cfg
	bochs -qf day-1.cfg -rc bochs-input.txt
