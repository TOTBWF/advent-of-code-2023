.PHONY: day-1

day-1.img: day-1.asm
	nasm day-1.asm -f bin -o day-1.img

day-1: day-1.img
	rlwrap bochs -qf /dev/null 'ata0-master: type=disk, path="day-1.img", mode=flat, cylinders=1, heads=1, spt=2' 'boot: disk' 'display_library: sdl2' 'megs: 128'
