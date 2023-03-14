;*********************************************
; JigSaw for SFG-0x / Programmed by BKC
;*********************************************
;
;
; 2013-01-12
; * ステータス・レジスタのアドレスを修正($3FF0では読めない)
;
; 2007/06/01
; * 初期リリース
;
; 2007/05/23
; * Copy all from MoonDriver
;
;
;********************************************
; メモリ仕様
; Page 0 ( 0000 - 3fff ) : SFG-05 device
; Page 1 ( 4000 - 7fff ) : Main
; Page 2 ( 8000 - bfff ) : Data
;********************************************

BDOS:		equ	$0005
FCB:		equ	$005C

H_TIMI:		equ	$FD9F
EXPTBL:		equ	$FCC1

IO_PAGE3:	equ	$00FE
IO_PSLT:	equ	$00A8
MIO_SSLT:	equ	$FFFF

; 実機では読めない為
OPM_STAT:	equ	$3FF1

OPM_ADDR:	equ	$3FF0
OPM_DATA:	equ	$3FF1



PG2RAM: 	equ	$F342

USE_CH:		equ	8

;********************************************
; JIG format
S_DEVICE_FLAGS:	equ	$8007

S_TRACK_TABLE:	equ	$8010
S_TRACK_BANK:	equ	S_TRACK_TABLE+2
S_LOOP_TABLE:	equ	S_TRACK_TABLE+4
S_LOOP_BANK:	equ	S_TRACK_TABLE+6
S_VENV_TABLE:	equ	S_TRACK_TABLE+8
S_VENV_LOOP:	equ	S_TRACK_TABLE+10
S_PENV_TABLE:	equ	S_TRACK_TABLE+12
S_PENV_LOOP:	equ	S_TRACK_TABLE+14
S_NENV_TABLE:	equ	S_TRACK_TABLE+16
S_NENV_LOOP:	equ	S_TRACK_TABLE+18
S_LFO_TABLE:	equ	S_TRACK_TABLE+20
S_INST_TABLE:	equ	S_TRACK_TABLE+22
S_OPL3_TABLE:	equ	S_TRACK_TABLE+24


;********************************************
; エントリーポイント
;********************************************

	org	$100

	jp	x86_trap
x86_trap:

	ld	de,str_drvname
	ld	c,$09
	call	BDOS
	call	sfg_init
	ret	nz

main_load_file:
 	call	load_file
	or	a
	jr	z,main_check_file

	ld	de,str_file_error
	ld	c,$09
	jp	BDOS

main_check_file:

	call	check_file
	jr	z,main_file_ok

	ld	de,str_format_error
	ld	c,$09
	jp	BDOS

main_file_ok:
	call	start_play
	call	loop_play
	call	stop_play

; Terminate
	ret

;********************************************
; 再生ルーチン
;********************************************

start_play:

	call	seq_init

	ld	de,str_sfg_play
	ld	c,$09
	call	BDOS

	call	set_timi

	ret

stop_play:

	call	restore_timi
	ret


;********************************************
; ファイル関連
;********************************************

; load_file
; Load sequence file from disk
; in   : FCB ( a parameter from DOS )
; dest : DE

load_file:
	ld	de,FCB
	ld	c,$0f
	call	BDOS
	or	a
	ret	nz

	ld	de,dos_dta
	ld	c,$1a   ; set DTA
	call	BDOS

	ld	hl,$8000
	xor	a
	ld	(seq_loaded_bank),a
load_file_lp01:

	push	hl
	ld	de,FCB
	ld	c,$14   ; sequencial read
	call	BDOS
	pop	hl

	push	af
	ld	a,(seq_loaded_bank)
	call	change_page3
	ex	de,hl
	ld	hl,dos_dta
	ld	bc,$0080
	ldir
	ex	de,hl
	pop	af

	or	a
	jr	nz,load_file_eof
	ld	a,h
	cp	$c0
	jr	c,load_file_lp01
	ld	hl,$8000

	ld	a,(seq_loaded_bank)
	add	a,$02
	ld	(seq_loaded_bank),a

	jr	load_file_lp01
	
load_file_eof:
	ld	de,FCB
	ld	c,$10  ; close
	jp	BDOS


; check_file
;

check_file:
	ld	hl,$8000
	ld	de,check_file_id
	ld	c,$4
	xor	a
	call	change_page3

check_file_lp:
	ld	a,(de)
	cp	(hl)
	ret	nz
	inc	hl
	inc	de
	dec	c
	jr	nz,check_file_lp
	ret


check_file_id:
	db  "JIGF"


;********************************************
; Strings
str_drvname:
	db "JIGSAW VER 130121",$0d,$0a,'$'

str_sfg_fnd:
	db "SFG-0X DETECTED",$0d,$0a,'$'

str_sfg_not:
	db "SFG-0X IS NOT FOUND",$0d,$0a,'$'

str_sfg_play:
	db "PLAYING...",$0d,$0a,'$'

str_ram_error:
	db "RAM ERROR",$0d,$0a,'$'
str_file_error:
	db "FILE ERROR",$0d,$0a,'$'
str_format_error:
	db "FILE FORMAT ERROR",$0d,$0a,'$'


;********************************************
; 割り込み関連
;********************************************

;set_timi
;set interrupt to use an user interrupt routine
;
set_timi:
	ld	bc,$05
	ld	hl,H_TIMI
	ld	de,save_hook
	ldir
	di
	ld	a,$f7 ; rst30
	ld	(H_TIMI),a

	ld	a,(PG2RAM)
	ld	(H_TIMI+1),a ; slot on RAM in page2

	ld	hl,usrint ; use my interrupt routine
	ld	a,l
	ld	(H_TIMI+2),a
	ld	a,h
	ld	(H_TIMI+3),a
	ei
	ret

;restore_timi
;restore interrupt
;dest : AF,BC,DE,HL
restore_timi:
	di
	ld	bc,$05
	ld	de,H_TIMI
	ld	hl,save_hook
	ldir
	ei
	ret

save_hook:
	ds	$05


;********************************************
; 文字出力
;********************************************

; out_ch
; output a charactor
; in : A = char
;
out_ch:
	push	de
	push	bc
	ld	e,a
	ld	c,2
	call	BDOS
	pop	bc
	pop	de
	ret

; output LF controle 
;
out_lf:
	ld	a,$0d
	call	out_ch
	ld	a,$0a
	jp	out_ch

; disp_hex
; in   : A = num
; dest : AF

disp_hex:
	push	hl
	push	de
	push	af
	rra
	rra
	rra
	rra
	and	$0f

	ld	d,$00
	ld	e,a
	ld	hl,str_hextbl
	add	hl,de

	ld	a,(hl)
	call	out_ch

	pop	af
	and	$0f
	ld	d,$00
	ld	e,a
	ld	hl,str_hextbl
	add	hl,de
	ld	a,(hl)
	call	out_ch

	pop	de
	pop	hl
	ret

str_hextbl:
	db	"0123456789ABCDEF"

dos_dta:
	ds	$80



;********************************************
; ドライバ本体
;********************************************

	org	$4000

;usrint
;an user interrupt routine called from H_TIMI

usrint:
	push	hl
	push	de
	push	bc
	push	af
	exx
	ex	af,af'
	push	hl
	push	de
	push 	bc
	push	af
	push	iy
	push	ix


	call	dec_timer

usrint_end:
	pop	ix
	pop	iy
	pop	af
	pop	bc
	pop	de
	pop	hl
	ex	af,af'
	exx
	pop	af
	pop	bc
	pop	de
	pop	hl
	ret

; cnt_timer
;

cnt_timer:
	db $00

dec_timer:
	ld	a,(cnt_timer)
	or	a
	jr	z,skip_dec_cnt
	dec	a
skip_dec_cnt:
	ld	(cnt_timer),a
	ret

;********************************************
; スロット関連
;********************************************

;read_pg0slot
; in   : none ( requied DI to call )
; out  : A: SLOT in Page0
; dest : destory them all
read_pg0slot:
	in	a,(IO_PSLT)
	and	$03
	ld	hl,EXPTBL
	ld	c,a
	ld	b,$00
	add	hl,bc
	ld	a,(hl) ; expand flag
	and	$80
	or	c
	ld	c,a
	inc	hl
	inc	hl
	inc	hl
	inc	hl
	ld	a,(hl)
	and	$03
	rlca
	rlca
	or	c
	ret

;write_pg0slot
; in   : A = SLOT ( requied DI to call )
; out  : Page0
; dest : noone survive
write_pg0slot:
	push	de

	ld	hl,EXPTBL+4
	ld	d,a
	in	a,(IO_PSLT)
	ld	e,a

	ld	a,d
	and	$80
	jr	z,skip_write_pg0ext
	ld	a,d
	and	$03
	ld	c,a
	ld	b,$00
	add	hl,bc

	ld	a,(hl)
	ld	c,a

	ld	a,e
	and	$3f
	ld	b,a
	ld	a,d
	and	$03
	rrca
	rrca
	or	b
	out	(IO_PSLT),a

	ld	a,c
	and	$fc
	ld	b,a
	ld	a,d
	rrca
	rrca
	and	$03
	or	b
	ld	(MIO_SSLT),a

skip_write_pg0ext:
	ld	a,e
	and	$fc
	ld	b,a
	ld	a,d
	and	$03
	or	b

	out	(IO_PSLT),a
	pop	de
	ret


;restore_pg0_ext
; in   : A = SLOT ( required DI to call )
; out  : Page0
; dest : all of us
restore_pg0_ext:
	push	de

	ld	hl,EXPTBL+4
	ld	d,a
	in	a,(IO_PSLT)
	ld	e,a

	ld	a,d
	and	$03
	ld	c,a
	ld	b,$00
	add	hl,bc
	ld	a,(hl)
	ld	c,a

	ld	a,e
	and	$3f
	ld	b,a
	ld	a,d
	and	$03
	rrca
	rrca
	or	b
	out	(IO_PSLT),a
	ld	a,c
	ld	(MIO_SSLT),a

	ld	a,e
	out	(IO_PSLT),a
	pop	de
	ret


;********************************************
; カートリッジ検出
;********************************************

;check_mem
;search string in all slots
; out : Z = found, NZ = not found, D = slot
;

check_mem:
	di

	in	a,(IO_PSLT)
	ld	e,a ; E = initial primary slot
	xor	a
	ld	d,a ; D = current slot

check_mem_lp:
	ld	a,d
	and	$0f
	push	af

	and	$03
	ld	hl,EXPTBL
	ld	b,$00
	ld	c,a
	add	hl,bc

	pop	af
	ld	c,a
	ld	a,(hl)
	and	$80
	or	c
	ld	d,a
	and	$80
	jr	nz,check_mem_ext
	ld	a,d
	call	write_pg0slot
	call	check_str
	jr	z,check_found

check_mem_next:
	ld	a,d
	inc	a
	ld	d,a
	and	$03
	jr	nz,check_mem_lp
check_notfnd:
	ld	a,e
	out	(IO_PSLT),a
	ld	a,$ff
	or	a
	ei
	ret

check_found:
	ld	a,e
	out	(IO_PSLT),a
	xor	a
	ei
	ret

check_found_ext:
	ld	a,d
	call	restore_pg0_ext
	jr	check_found

check_mem_ext:
	ld	a,d
	call	write_pg0slot
	call	check_str
	jr	z,check_found_ext

	ld	a,d
	add	a,$04
	ld	d,a
	and	$0c
	jr	nz,check_mem_ext

	ld	a,d
	call	restore_pg0_ext

	jr	check_mem_next



check_str:
	push	de
	ld	hl,chkstr_sfg
	ld	de,$0080
	ld	c,$6
check_str_lp:
	ld	a,(de)
	cp	(hl)
	jr	nz,check_str_fin
	inc	hl
	inc	de
	dec	c
	jr	nz,check_str_lp
	xor	a
check_str_fin:
	pop	de
	ret


chkstr_sfg:
	db	"MCHFM0"

page0_old:
	db	$00
page0_sfg:
	db	$00


; Initializes SFG
; out : NZ if not found
;
sfg_init:
	di
	call	read_pg0slot
	ei
	ld	(page0_old),a

	call	check_mem
	jr	z,sfg_init_01

; sfg is not found

	ld	de,str_sfg_not
	ld	c,$09
	call	BDOS
	ld	a,$ff
	and	a
	ret

sfg_init_01:

; sfg is found
	ld	a,d
	ld	(page0_sfg),a

	ld	de,str_sfg_fnd
	ld	c,$09
	call	BDOS

	xor	a
	ret

;********************************************
; シーケンス部分
;********************************************

loop_play:

	di
	ld	a,(page0_sfg)
	call	write_pg0slot

	call	proc_tracks

	ld	a,(page0_old)
	call	write_pg0slot

	ld	a,$01
	ld	(cnt_timer),a
	ei

loop_play_lp:
	ld	c,$0b
	call	BDOS
	or	a
	jr	nz,loop_play_fin
	halt
	ld	a,(cnt_timer)
	or	a
	jr	nz,loop_play_lp
	jr	loop_play

loop_play_fin:


	call	sfg_fm_keyoff_all
	ret


;********************************************
; 音源制御ルーチン
;********************************************

sfg_fm_keyoff_all:
	di
	ld	a,(page0_sfg)
	call	write_pg0slot

	ld	c,$00
sfg_keyoff_lp:
	ld	a,c
	ld	e,a
	ld	d,$08
	call	sfg_fm_out

	inc	c
	ld	a,c
	cp	$08
	jr	c,sfg_keyoff_lp

	ld	a,(page0_old)
	call	write_pg0slot
	ei
	ret

;********************************************
;Work memory for the driver


	db "work"
seq_cur_ch:
	db $00
seq_use_ch:
	db $00
seq_start_fm:
	db $00
seq_cur_bank:
	db $00
seq_loaded_bank:
	db $00
seq_opsel:
	db $00
seq_reg_bd:
	db $00


;********************************************
;Work memory for channels
;
seq_work:
seq_ch1_dsel:
	db $00
seq_ch1_opsel:
	db $00
seq_ch1_synth:
	db $00
seq_ch1_cnt:
	db $00
seq_ch1_loop:
	db $00
seq_ch1_bank:
	db $00
seq_ch1_addr:
	dw $0000
seq_ch1_tadr:
	dw $0000
seq_ch1_tone:
	dw $0000
seq_ch1_key:
	db $00
seq_ch1_damp:
	db $00
seq_ch1_lfo:
	db $00
seq_ch1_lfo_vib:
	db $00
seq_ch1_ar_d1r:
	db $00
seq_ch1_dl_d2r:
	db $00
seq_ch1_rc_rr:
	db $00
seq_ch1_am:
	db $00
seq_ch1_note:
	db $00
seq_ch1_pitch:
	dw $0000
seq_ch1_p_ofs:
	dw $0000
seq_ch1_oct:
	db $00
seq_ch1_fnum:
	dw $0000
seq_ch1_reverb:
	db $00
seq_ch1_vol:
	db $00
seq_ch1_pan:
	db $00
seq_ch1_detune:
	db $00
seq_ch1_venv:
	db $00
seq_ch1_nenv:
	db $00
seq_ch1_penv:
	db $00
seq_ch1_nenv_adr:
	dw $0000
seq_ch1_penv_adr:
	dw $0000
seq_ch1_venv_adr:
	dw $0000
seq_work_end:

IDX_DSEL:     equ (seq_ch1_dsel    - seq_work) ; Device Select
IDX_OPSEL:    equ (seq_ch1_opsel   - seq_work) ; Operator Select
IDX_SYNTH:    equ (seq_ch1_synth   - seq_work) ; FeedBack,Synth and OpMode
IDX_CNT:      equ (seq_ch1_cnt     - seq_work) ; Counter
IDX_LOOP:     equ (seq_ch1_loop    - seq_work) ; Loop
IDX_BANK:     equ (seq_ch1_bank    - seq_work) ; Which bank
IDX_ADDR:     equ (seq_ch1_addr    - seq_work) ; Address to data
IDX_TADR:     equ (seq_ch1_tadr    - seq_work) ; Address of a Tone Table
IDX_TONE:     equ (seq_ch1_tone    - seq_work) ; Tone number in OPL4
IDX_KEY:      equ (seq_ch1_key     - seq_work) ; data in Key register
IDX_DAMP:     equ (seq_ch1_damp    - seq_work) ; Damp switch
IDX_LFO:      equ (seq_ch1_lfo     - seq_work) ; LFO switch
IDX_LFO_VIB:  equ (seq_ch1_lfo_vib - seq_work) ; LFO and VIB
IDX_AR_D1R:   equ (seq_ch1_ar_d1r  - seq_work) ; AR and D1R
IDX_DL_D2R:   equ (seq_ch1_dl_d2r  - seq_work) ; DL and D2R 
IDX_RC_RR:    equ (seq_ch1_rc_rr   - seq_work) ; RC and RR
IDX_AM:       equ (seq_ch1_am      - seq_work) ; AM
IDX_NOTE:     equ (seq_ch1_note    - seq_work) ; Note data
IDX_PITCH:    equ (seq_ch1_pitch   - seq_work) ; Pitch data
IDX_P_OFS:    equ (seq_ch1_p_ofs   - seq_work) ; Offset for pitch
IDX_OCT:      equ (seq_ch1_oct     - seq_work) ; Octave in OPL4
IDX_FNUM:     equ (seq_ch1_fnum    - seq_work) ; F-number in OPL4
IDX_REVERB:   equ (seq_ch1_reverb  - seq_work) ; Pseudo reverb
IDX_VOL:      equ (seq_ch1_vol     - seq_work) ; Volume in OPL4
IDX_PAN:      equ (seq_ch1_pan     - seq_work) ; Pan in OPL4
IDX_DETUNE:   equ (seq_ch1_detune  - seq_work) ; Detune
IDX_VENV:     equ (seq_ch1_venv    - seq_work) ; Volume envelope in data
IDX_NENV:     equ (seq_ch1_nenv    - seq_work) ; Vote envelope  in data
IDX_PENV:     equ (seq_ch1_penv    - seq_work) ; Pitch envelope in data
IDX_NENV_ADR: equ (seq_ch1_nenv_adr - seq_work)
IDX_PENV_ADR: equ (seq_ch1_penv_adr - seq_work)
IDX_VENV_ADR: equ (seq_ch1_venv_adr - seq_work)

IDX_VOLOP:    equ (seq_ch1_reverb  - seq_work) ; Volume Operator in connect
IDX_OPVOL1:   equ (seq_ch1_ar_d1r  - seq_work) ; Volume Data for 1stOP


SEQ_WORKSIZE: equ (seq_work_end - seq_work)

	ds	(SEQ_WORKSIZE * (USE_CH-1))

;	dw	(SEQ_WORKSIZE * USE_CH)

;********************************************


fm_deftone:
	db	$78 ; OPMASK ($08)
	db	$00 ; Noise ($0F)

	db	$00 ; LFO freq  (Reg$18)
	db	$00 ; Mod depth (Reg$19)
	db	$00 ; CT and WF (Reg$1B)
	db	$c0 ; Channel Control (Reg$20)
	db	$00 ; Phase and Amp (Reg$38)

	db	$01 ; Reg$40
	db	$0f ; Reg$60
	db	$05 ; Reg$80
	db	$00 ; Reg$A0
	db	$00 ; Reg$C0
	db	$05 ; Reg$E0

	db	$01 ; Reg$40
	db	$0f ; Reg$60
	db	$05 ; Reg$80
	db	$00 ; Reg$A0
	db	$00 ; Reg$C0
	db	$05 ; Reg$E0

	db	$00 ; Reg$40
	db	$0f ; Reg$60
	db	$05 ; Reg$80
	db	$00 ; Reg$A0
	db	$00 ; Reg$C0
	db	$05 ; Reg$E0

	db	$00 ; Reg$40
	db	$0f ; Reg$60
	db	$0f ; Reg$80
	db	$00 ; Reg$A0
	db	$00 ; Reg$C0
	db	$05 ; Reg$E0


;********************************************
; set page3 to the bank of current channel
; dest : AF
set_page3_ch:
	ld	a,(ix + IDX_BANK)
	jr	change_page3

;********************************************
; changes page3
; in   : A = page
; dest : AF
change_page3:

	srl	a
	add	a,$04
	out	(IO_PAGE3),a
	ret

;********************************************
; get_table
; in   : A = index , HL = address
; out  : HL = (HL + (A * 2) )
; dest : AF,DE
get_table:
	ld	e,a
	ld	d,$00
	jr	get_table_hl_2de

;********************************************
; get_hl_table
; in   : HL = address
; out  : HL = (HL + (cur_ch * 2) )
; dest : AF,DE
get_hl_table:
	ld	a,(seq_cur_ch)
	ld	e,a
	ld	d,$00

get_table_hl_2de:
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a

	add	hl,de
	add	hl,de

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a

	ret



;********************************************
; get_a_table
; in   : HL = address
; out  : A = (HL + (cur_ch * 2) )
; dest : HL,DE
get_a_table
	ld	a,(seq_cur_ch)
	ld	e,a
	ld	d,$00

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a

	add	hl,de

	ld	a,(hl)

	ret


;********************************************
; seq_init
; initializes all channel's work
; dest : ALL
seq_init:
	xor	a
	ld	(seq_use_ch),a
	ld	(seq_cur_ch),a
	call	change_page3  ; Page to Top

	ld	a,(S_DEVICE_FLAGS)
	or	a
	jr	nz,skip_def_device
	ld	a,$01 ; SFG by default
skip_def_device:
	ld	b,a

	ld	ix,seq_work

	ld	d,$00
	ld	e,$08 ; 8channels ; OPM

	rr	b
	call	c,seq_init_chan

	ld	ix,seq_work
	ret

;********************************************
;seq_init_chan
; in   : D = device, E = channels
; dest : AF,E
seq_init_chan:

	ld	a,(seq_use_ch)
	add	a,e
	ld	(seq_use_ch),a


seq_init_chan_lp:
	xor	a
	ld	(ix + IDX_CNT),a
	ld	a,d
	ld	(ix + IDX_DSEL),a

	push	de
	ld	a,$ff
	ld	(ix + IDX_VENV),a
	ld	(ix + IDX_PENV),a
	ld	(ix + IDX_NENV),a
	ld	(ix + IDX_DETUNE),a


init_fmtone:
	ld	hl,fm_deftone
	ld	(ix + IDX_TADR),l
	ld	(ix + IDX_TADR+1),h
	ld	a,$30
	ld	(ix + IDX_PAN),a
	ld	a,$0c
	ld	(ix + IDX_VOLOP),a
	ld	a,$3f
	ld	(ix + IDX_VOL),a

init_tone_fin:

	ld	hl,S_TRACK_TABLE
	call	get_hl_table
	ld	(ix + IDX_ADDR),l
	ld	(ix + IDX_ADDR+1),h

	ld	hl,S_TRACK_BANK
	call	get_a_table
	ld	(ix + IDX_BANK),a

	ld	de,SEQ_WORKSIZE
	add	ix,de

	pop	de

	ld	a,(seq_cur_ch)
	inc	a
	ld	(seq_cur_ch),a


	dec	e
	jr	nz,seq_init_chan_lp
	ret


;********************************************
; proc_tracks
; Process tracks in 1/60 interrupts
;
proc_tracks:
	ld	ix,seq_work
	xor	a
	ld	(seq_cur_ch),a
proc_tracks_lp:

	call	proc_venv
	call	proc_penv
	call	proc_nenv

	call	proc_venv_reg
	call	proc_freq_reg

	call	seq_track

	ld	de,SEQ_WORKSIZE
	add	ix,de



	ld	a,(seq_use_ch)
	ld	e,a
	ld	a,(seq_cur_ch)
	inc	a
	cp	e
	jr	nc,proc_tracks_end
	ld	(seq_cur_ch),a
	jr	proc_tracks_lp
	
proc_tracks_end:
	ret



;********************************************
;seq_track
;Count down and process in a channel
;
seq_track:
	ld	a,(ix + IDX_CNT)
	or	a
	jr	z,seq_cnt_zero
	dec	a
	ld	(ix + IDX_CNT),a
	ret

seq_cnt_zero:
	call	set_page3_ch
	ld	l,(ix + IDX_ADDR)
	ld	h,(ix + IDX_ADDR+1)
seq_track_lp:
	ld	a,(hl)
	inc	hl
	cp	$e0
	jr	nc,seq_command
	jp	seq_repeat_or_tone

seq_command:
	ld	bc,seq_next
	push	bc
	push	hl
	add	a,$20
	sla	a
	ld	l,a
	ld	h,$00
	ld	bc,seq_jmptable
	add	hl,bc

	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a

	jp	(hl)

;********************************************
; seq_next
; preserves  address and do next
; in   : HL
; dest : AF
seq_next:
	ld	(ix + IDX_ADDR),l
	ld	(ix + IDX_ADDR+1),h
	jr	seq_track

	ret

;********************************************
; seq_repeat_or_tone
;
; n < $90 =  note
; $A0     =  repeat_end
; $A1     =  repeat_esc

seq_repeat_or_tone:
	cp	$90
	jr	c,seq_tone

	cp	$a1
	jr	z,seq_repeat_esc

;********************************************
seq_repeat_end:
	ld	a,(ix + IDX_LOOP)
	cp	$01
	jr	z,seq_skip_rep_jmp

	or	a
	jr	nz,seq_skip_set_repcnt_end

	ld	a,(hl)    ; read repeat counter

seq_skip_set_repcnt_end:
	jr	seq_rep_jmp

;*********************
seq_repeat_esc:
	ld	a,(ix + IDX_LOOP)
	cp	$01
	jr	z,seq_rep_jmp

	or	a
	jr	nz,seq_skip_set_repcnt_esc

	ld	a,(hl)      ; read repeat counter

seq_skip_set_repcnt_esc:
	jr	seq_skip_rep_jmp

seq_skip_rep_jmp:
	inc	hl
	dec	a
	ld	(ix + IDX_LOOP),a
	inc	hl  ; bank
	inc	hl  ; addr l
	inc	hl  ; addr h
	jr	seq_next

seq_rep_jmp:
	inc	hl
	dec	a
	ld	(ix + IDX_LOOP),a

	ld	bc,seq_next
	push	bc
	push	hl

	ld	hl,seq_bank ; go to address
	jp	(hl)

seq_tone:
	push	hl

	push	af
	xor	a
	call	change_page3

seq_tone_fmnote:
	pop	af
	ld	(ix + IDX_NOTE),a
	call 	sfg_set_fmnote

set_tone_fin:
	call	set_page3_ch

	call	start_venv
	call	start_penv
	call	start_nenv
	call	sfg_key_on

	pop	hl
	ld	a,(hl)
	ld	(ix + IDX_CNT),a
	inc	hl
	jr	seq_next

seq_jmptable:
	dw	seq_nop    ; $e0
	dw	seq_nop    ; $e1
	dw	seq_nop    ; $e2
	dw	seq_fbs    ; $e3 : set fbs
	dw	seq_tvp    ; $e4 : set tvp
	dw	seq_ld2ops ; $e5 : load 2ops
	dw	seq_setop  ; $e6 : set opbase
	dw	seq_nop    ; $e7 : pitch shift
	dw	seq_nop    ; $e8 
	dw	seq_nop    ; $e9 : slar
	dw	seq_revbsw ; $ea : reverb switch / VolumeOP
	dw	seq_damp   ; $eb : damp switch / OPMODE
	dw	seq_nop    ; $ec : lfo freq
	dw	seq_nop    ; $ed : lfo mode
	dw	seq_bank   ; $ee : bank change
	dw	seq_lfosw  ; $ef : mode change
	dw	seq_pan    ; $f0 : pan
	dw	seq_inst   ; $f1 : set instruction
	dw	seq_drum   ; $f2 : drum
	dw	seq_nop	   ; $f3
	dw	seq_wait   ; $f4 : wait
	dw	seq_nop    ; $f5
	dw	seq_nop	   ; $f6
	dw	seq_nenv   ; $f7 : note  env
	dw	seq_penv   ; $f8 : pitch env
	dw	seq_skip_1 ; $f9 
	dw	seq_detune ; $fa : detune
	dw	seq_nop    ; $fb : lfo
	dw	seq_rest   ; $fc : rest
	dw	seq_volume ; $fd : volume
	dw	seq_skip_1 ; $fe : not used
	dw	seq_loop   ; $ff : loop

;********************************************
; Volume envelope stuff
;
start_venv:
	ld	a,(ix + IDX_VENV)
	cp	$ff
	ret	z

	call	set_venv_head
	jr	proc_venv_start

proc_venv:
	ld	a,(ix + IDX_VENV)
	cp	$ff
	ret	z

proc_venv_start:
	ld	l,(ix + IDX_VENV_ADR)
	ld	h,(ix + IDX_VENV_ADR + 1)
	ld	a,l
	or	h
	ret	z

	call	read_effect_value
	cp	$ff
	jr	z,proc_venv_end
	inc	hl
	ld	(ix + IDX_VENV_ADR),l
	ld	(ix + IDX_VENV_ADR + 1),h

	ld	(ix + IDX_VOL),a
	ret


proc_venv_end:
	jp	set_venv_loop

;********************************************
; Set venv's volume to register actually
;
proc_venv_reg:
	ld	a,(ix + IDX_VENV)
	cp	$ff
	ret	z
	jp	sfg_set_vol_ch


;********************************************
; Pitch envelope stuff
;
start_penv:
	ld	a,(ix + IDX_PENV)
	cp	$ff
	ret	z

	call	set_penv_head
	jr	proc_penv_start


proc_penv:
	ld	a,(ix + IDX_PENV)
	cp	$ff
	ret	z
proc_penv_start:
	ld	l,(ix + IDX_PENV_ADR)
	ld	h,(ix + IDX_PENV_ADR + 1)

	call	read_effect_value
	cp	$ff
	jr	z,proc_penv_end

	inc	hl
	ld	(ix + IDX_PENV_ADR),l
	ld	(ix + IDX_PENV_ADR + 1),h

proc_penv_fm:
	call	sfg_add_detune
	jp	sfg_set_freq


proc_penv_end:
	jp	set_penv_loop




;********************************************
; Note envelope stuff
;
start_nenv:
	ld	a,(ix + IDX_NENV)
	cp	$ff
	ret	z

	call	set_nenv_head
	jr	proc_nenv_start

proc_nenv:
	ld	a,(ix + IDX_NENV)
	cp	$ff
	ret	z

proc_nenv_start:
	ld	l,(ix + IDX_NENV_ADR)
	ld	h,(ix + IDX_NENV_ADR + 1)

	call	read_effect_value
	cp	$ff
	jr	z,proc_nenv_end

	inc	hl
	ld	(ix + IDX_NENV_ADR),l
	ld	(ix + IDX_NENV_ADR + 1),h

	bit	7,a
	jr	nz,proc_nenv_nega
	ld	e,(ix + IDX_NOTE)
	add	a,e
	ld	(ix + IDX_NOTE),a
	jr	proc_nenv_note

proc_nenv_nega:
	and	$7f
	ld	e,a
	ld	a,(ix + IDX_NOTE)
	sub	e
	ld	(ix + IDX_NOTE),a

proc_nenv_note:
	ret


proc_nenv_end:
	jp	set_nenv_loop

;********************************************
;Set frequency to registers actually
;
proc_freq_reg:
	ld	a,(ix + IDX_PENV)
	cp	$ff
	jr	nz,proc_freq_to_moon
	ld	a,(ix + IDX_NENV)
	cp	$ff
	jr	nz,proc_freq_to_moon
	ret
proc_freq_to_moon:
	jp	moon_set_freq_ch



;********************************************
;Subroutines to process sequence

; no program
seq_nop:
	pop	hl
	ret

; skip the command with an augment
seq_skip_1:
	pop	hl
	inc	hl
	ret


; cmd $FF : loop point
seq_loop:
	pop	hl
	xor	a
	call	change_page3

	ld	hl,S_LOOP_TABLE
	call	get_hl_table

	push	hl

	ld	hl,S_LOOP_BANK
	call	get_a_table
	ld	(ix + IDX_BANK),a
	call	change_page3

	pop	hl


	ret


;cmd $FD : volume
seq_volume:
	pop	hl

	ld	a,(hl)
	ld	(ix + IDX_VENV),a

	bit	7,a
	jr	z,seq_venv
	and	$7f

	ld	(ix + IDX_VOL),a

	ld	a,$ff
	ld	(ix + IDX_VENV),a  ; venv = off

	call	sfg_set_vol_ch

	inc	hl
	ret

seq_venv:

	call	set_venv_head

	inc	hl
	ret

; cmd $FC : rest
seq_rest:
	pop	hl
	call	sfg_key_off
	ld	a,(hl)
	ld	(ix + IDX_CNT),a
	inc	hl
	ret

; cmd $FA : detune
seq_detune:
	pop	hl

	ld	a,(hl)
	ld	(ix + IDX_DETUNE),a
	inc	hl
	ret

; cmd $F8 : pitch env
seq_penv:
	pop	hl

	ld	a,(hl)
	inc	hl

	ld	(ix + IDX_PENV),a
	cp	$ff
	call	nz,set_penv_head

	ret

; cmd $F7 : note env
seq_nenv:
	pop	hl

	ld	a,(hl)
	inc	hl

	ld	(ix + IDX_NENV),a
	cp	$ff
	call	nz,set_nenv_head

	ret

; cmd $F4 : wait
seq_wait:
	pop	hl
	ld	a,(hl)
	ld	(ix + IDX_CNT),a
	inc	hl
	ret

; cmd $F2 : drum
seq_drum:
	pop	hl
	ld	a,(hl)
	and	$1f
	ld	e,a
	ld	a,(seq_reg_bd)
	and	$e0
	or	e
	ld	(seq_reg_bd),a
	ld	e,a
	ld	d,$bd
	call	moon_fm1_out
	inc	hl
	ret


; TODO : load fmreg from Table
; cmd $F1 : inst
seq_inst:
	pop	hl
	ld	a,(hl)

	push	af
	xor	a
	call	change_page3

	pop	af
	push	hl
	ld	hl,S_OPL3_TABLE
	call	get_table

	ld	(ix + IDX_TADR),l
	ld	(ix + IDX_TADR+1),h

	call	sfg_fmtone

seq_inst_fin:

	call	set_page3_ch
	pop	hl

	inc	hl
	ret

; cmd $F0 : pan
seq_pan:
	pop	hl
	; Device select
	ld	a,(ix + IDX_DSEL)
	or	a
	jr	z,seq_pan_opl4

seq_pan_fm:
	ld	a,(hl)
	and	$0f
	rlca
	rlca
	rlca
	rlca
	ld	(ix + IDX_PAN),a
	jr	seq_pan_fin
seq_pan_opl4:
	ld	a,(hl)
	ld	(ix + IDX_PAN),a
seq_pan_fin:
	inc	hl
	ret

; cmd $EF : mode change
seq_lfosw:
	pop	hl

	ld	a,(hl)
	ld	(ix + IDX_LFO),a
	inc	hl
	ret



; cmd $EE : change bank
seq_bank:
	pop	hl

	ld	a,(hl) ; read number of bank
	inc	hl
	
	push	af
	
	ld	a,(hl) ; read address
	inc	hl
	ld	h,(hl)
	ld	l,a

	pop	af

	ld	(ix + IDX_BANK),a
	call	change_page3
	ret

; cmd $EB : damp switch / OPMODE
seq_damp:
	pop	hl


	; Device select
	ld	a,(ix + IDX_DSEL)
	or	a
	jr	z,seq_damp_opl4

	ld	a,(hl)
	and	$3f
	ld	e,a
	ld	d,$04
	call	moon_fm2_out
	jr	seq_damp_fin
	
seq_damp_opl4:
	ld	a,(hl)
	ld	(ix + IDX_DAMP),a
seq_damp_fin:
	inc	hl
	ret

; cmd $EA : reverb sw / VolumeOP
seq_revbsw:
	pop	hl
	ld	a,(hl)
	ld	(ix + IDX_REVERB),a
	inc	hl
	ret

; cmd $E6 : set opbase
seq_setop:
	pop	hl
	ld	a,(hl)
	ld	(ix + IDX_OPSEL),a
	inc	hl
	ret

; TODO : load from table
; cmd $E5 : load 2ops
seq_ld2ops:
	ld	a,(ix + IDX_DSEL)
	or	a
	jr	nz,seq_ld2ops_fm

	pop	hl
	inc	hl
	ret
seq_ld2ops_fm:
	pop	hl

	ld	a,(hl)

	push	af
	xor	a
	call	change_page3
	pop	af

	push	hl
	ld	hl,S_OPL3_TABLE
	call	get_table

	ld	(ix + IDX_TADR),l
	ld	(ix + IDX_TADR+1),h

	call	sfg_fmtone

	call	set_page3_ch
	pop	hl
	inc	hl
	ret


; cmd $E4 : tvp
seq_tvp:
	pop	hl
	ld	a,(hl)
	and	$07
	rrca
	rrca
	rrca
	ld	e,a
	ld	a,(seq_reg_bd)
	and	$1f
	or	e

	ld	(seq_reg_bd),a
	ld	e,a
	ld	d,$bd
	call	moon_fm1_out

	inc	hl
	ret

; cmd $E3 : fb
seq_fbs:
	pop	hl
	ld	a,(hl)
	and	$7
	rlca
	rlca
	ld	e,a
	ld	a,(ix + IDX_SYNTH)
	and	$e3
	or	e
	ld	(ix + IDX_SYNTH),a
	inc	hl
	ret


;********************************************
;pause_venv
;dest : AF
pause_venv:
	xor	a
	ld	(ix + IDX_VENV_ADR),a
	ld	(ix + IDX_VENV_ADR+1),a
	ret

;********************************************
;read_effect_table
; in   : A  = index
;      : HL = table address 
;      : DE = pointer to value in work
; out  : (ix + de) = (HL + 2A)
; dest : AF
read_effect_table:
	push	ix
	add	ix,de

	push	af
	xor	a
	call	change_page3
	pop	af

	call	get_table

	ld	(ix),l
	ld	(ix+1),h

	pop	ix
	call	set_page3_ch
	ret


;********************************************
;set_venv_loop
;dest : AF,DE
set_venv_loop:
	push	hl
	ld	hl,S_VENV_LOOP
	jr	set_venv_hl

;********************************************
;set_venv_head
;dest : AF,DE
set_venv_head:
	push	hl
	ld	hl,S_VENV_TABLE
set_venv_hl:
	ld	de,IDX_VENV_ADR
	ld	a,(ix + IDX_VENV)
	and	$7f
	call	read_effect_table
	pop	hl
	ret


;********************************************
;set_penv_loop
;dest : AF,DE
set_penv_loop:
	push	hl
	ld	hl,S_PENV_LOOP
	jr	set_penv_hl

;********************************************
;set_penv_head
;dest : AF,DE
set_penv_head:
	push	hl
	ld	hl,S_PENV_TABLE
set_penv_hl:
	ld	de,IDX_PENV_ADR
	ld	a,(ix + IDX_PENV)
	call	read_effect_table
	pop	hl
	ret

;********************************************
;set_nenv_loop
;dest : AF,DE
set_nenv_loop:
	push	hl
	ld	hl,S_NENV_LOOP
	jr	set_nenv_hl

;********************************************
;set_nenv_head
;dest : AF,DE
set_nenv_head:
	push	hl
	ld	hl,S_NENV_TABLE
set_nenv_hl:
	ld	de,IDX_NENV_ADR
	ld	a,(ix + IDX_NENV)
	call	read_effect_table
	pop	hl
	ret

;********************************************
;read_effect_value
;read_effect_value
; in  : HL = address
; out : A = data
read_effect_value:
	xor	a
	call	change_page3

	ld	a,(hl)
	push	af
	call	set_page3_ch
	pop	af
	ret


;********************************************
; moon_fmtone
; load and set 4 oprators from data in table
; in   : work
; dest : DE
; 


;********************************************
; sfg_fmtone
;
; in   : work
; dest : DE
; 
sfg_fmtone:
	push	af
	push	bc
	push	hl

	ld	c,$04

sfg_fmtone_start_lp:
	ld	l,(ix + IDX_TADR)
	ld	h,(ix + IDX_TADR+1)

	xor	a
	ld	(seq_opsel),a

	; OPMASK($08)
	ld	a,(hl)
	ld	(ix + IDX_KEY),a
	inc	hl

	; Noise($0F)
	ld	a,(hl)
	ld	e,a
	ld	d,$0f
	call	sfg_fm_out
	inc	hl

	; LFO freq($18)
	ld	a,(hl)
	ld	e,a
	ld	d,$18
	call	sfg_fm_out
	inc	hl

	; Mod depth($19)
	ld	a,(hl)
	ld	e,a
	ld	d,$19
	call	sfg_fm_out
	inc	hl

	; CT and waveform($1B)
	ld	a,(hl)
	ld	e,a
	ld	d,$1b
	call	sfg_fm_out
	inc	hl

	; Chan Ctrl ($20)
	ld	a,(hl)
	ld	e,a
	ld	d,$20
	call	sfg_write_fmch
	inc	hl

	; Phase and Amp ($38)
	ld	a,(hl)
	ld	e,a
	ld	d,$38
	call	sfg_write_fmch
	inc	hl

	call	sfg_opvol
	call	sfg_optone

	pop	hl
	pop	bc
	pop	af

	ret

sfg_opvol:
	push	hl
	push	ix
	ld	e,$04
sfg_opvol_lp:
	inc	hl ; $40
	ld	a,(hl)
	ld	(ix + IDX_OPVOL1),a
	inc	ix
	inc	hl ; $60
	inc	hl ; $80
	inc	hl ; $A0
	inc	hl ; $C0
	inc	hl ; $E0
	dec	e
	jr	nz,sfg_opvol_lp
	pop	ix
	pop	hl
	ret

sfg_optone:
	ld	a,(hl)
	ld	e,a
	ld	d,$40    ; DT
	call	sfg_write_fmop
	inc	hl

	ld	a,(hl)
	ld	e,a
	ld	d,$60    ; TL
	call	sfg_write_fmop
	inc	hl

	ld	a,(hl)
	ld	e,a
	ld	d,$80    ; AR
	call	sfg_write_fmop
	inc	hl

	ld	a,(hl)
	ld	e,a
	ld	d,$A0    ; DR1
	call	sfg_write_fmop
	inc	hl

	ld	a,(hl)
	ld	e,a
	ld	d,$C0    ; DR2
	call	sfg_write_fmop
	inc	hl

	ld	a,(hl)
	ld	e,a
	ld	d,$E0    ; RR
	call	sfg_write_fmop
	inc	hl

	ld	a,(seq_opsel)
	inc	a
	ld	(seq_opsel),a
	cp	$04
	jr	c,sfg_optone
	ret


;********************************************
; sfg_add_detune
; in   : A = detune
; out  : note
; dest : AF BC DE

sfg_add_detune:
	cp	$ff
	ret	z

	ld	c,$04

	ld	e,a
sfg_add_detune_lp:
	bit	7,a
	jr	nz,add_detune_nega

	ld	a,(ix + IDX_P_OFS)
	add	a,e
	ld	d,a
	and	$c0
	call	nz,sfg_inc_halftone
	ld	a,d
	and	$3f
	ld	(ix + IDX_P_OFS),a
	jr	sfg_add_detune_fin
add_detune_nega:
	and	$7f
	ld	b,a
	ld	a,(ix + IDX_P_OFS)
	sub	b
	ld	d,a
	and	$c0
	call	nz,sfg_dec_halftone
	ld	a,d
	and	$3f
	ld	(ix + IDX_P_OFS),a
sfg_add_detune_fin:
	ld	a,e
	dec	c
	jr	nz,sfg_add_detune_lp
	ret

;********************************************
; sfg_inc_halftone
; in  : note
; out : note

sfg_inc_halftone:
	push	af
	push	bc
	ld	a,(ix + IDX_NOTE)
	inc	a
	ld	b,a
	and	$0f
	cp	$0c
	jr	c,inc_halftone_fin
	ld	a,b
	add	a,$10
	and	$f0
	ld	b,a
inc_halftone_fin:
	ld	a,b
	ld	(ix + IDX_NOTE),a
	pop	bc
	pop	af
	ret

;********************************************
; sfg_dec_halftone
; in  : note
; out : note and oct

sfg_dec_halftone:
	push	af
	push	bc
	ld	a,(ix + IDX_NOTE)
	dec	a
	ld	b,a
	and	$0f
	cp	$0c
	jr	c,dec_halftone_fin
	ld	a,b
	and	$f0
	or	$0b
	ld	b,a
dec_halftone_fin:
	ld	a,b
	ld	(ix + IDX_NOTE),a
	pop	bc
	pop	af
	ret



sfg_note_tbl:
	db	$0e ; 0  C
	db	$00 ; 1  C+
	db	$01 ; 2  D
	db	$02 ; 3  D+
	db	$04 ; 4  E
	db	$05 ; 5  F
	db	$06 ; 6  F+
	db	$08 ; 7  G
	db	$09 ; 8  G+
	db	$0a ; 9  A
	db	$0c ; 10 A+
	db	$0d ; 11 B
	db	$0e ; 12 C
	db	$0e ; 13 C
	db	$0e ; 14 C
	db	$0e ; 15 C


;********************************************
; sfg_set_fmnote
; in   : none
; dest : AF
sfg_set_fmnote
	xor	a
	ld	(ix + IDX_P_OFS),a

	ld	a,(ix + IDX_DETUNE)
	call	sfg_add_detune

sfg_set_freq:
	push	bc
	push	de
	push	hl
	ld	a,(ix + IDX_NOTE)
	ld	e,a ; E = note
	and	$0f
	jr	nz,skip_dec_oct
	ld	a,e
	sub	$10
	ld	e,a
skip_dec_oct:
	ld	hl,sfg_note_tbl
	ld	a,e
	and	$0f

	ld	c,a
	ld	b,$00
	add	hl,bc
	ld	a,(hl)

	ld	d,a
	ld	a,e
	and	$f0
	or	d

	ld	e,a
	ld	d,$28
	call	sfg_write_fmch ; KEY CODE

	ld	a,(ix + IDX_P_OFS)
	and	$3f
	rlca
	rlca
	ld	e,a
	ld	d,$30
	call	sfg_write_fmch ; FRACTION
	pop	hl
	pop	de
	pop	bc
	ret

;********************************************
; moon_write_fmop
; Write an OPL3 reg for op
; in   : seq_opsel , D = addr, E = data
; dest : AF,DE
moon_write_fmop:
	ret



;********************************************
; moon_write_fmkey
; Write an OPL3 reg for fnum/keyon
; in : D = addr, E = data
;
moon_write_fmkey:
	ld	a,(seq_cur_ch)
	jr	moon_write_fmkey_nch


;********************************************
; moon_write_fmkey_nch
; Write an OPL3 reg for fnum/keyon
; in : A = ch, D = addr, E = data
;
moon_write_fmkey_nch:
	push	de
	ld	e,a
	ld	a,(seq_start_fm)
	ld	d,a
	ld	a,e
	sub	d
	pop	de
	
	cp	$9
	jr	c,moon_write_fm1key

	sub	$9
	add	a,d
	ld	d,a
	jp	moon_fm2_out

moon_write_fm1key:
	add	a,d
	ld	d,a
	jp	moon_fm1_out




;********************************************
; wait while BUSY
; dest : AF

moon_wait:
	ret

;********************************************
; write moonsound fm1 register
; in   : D = an address of register , E = data
; dest : AF

moon_fm1_out:
	ret

;********************************************
; write moonsound fm2 register
; in   : D = an address of register , E = data
; dest : AF

moon_fm2_out:
	ret

;********************************************
;sfg_fm_wait
; Wait clear BUSY flag

sfg_fm_wait:
	ld	a,(OPM_STAT)
	and	$80
	jr	nz,sfg_fm_wait
	ret

;********************************************
;sfg_fm_out
; Write data to FMchip
; in : D = address , E = data
sfg_fm_out:
	call	sfg_fm_wait
	ld	a,d
	ld	(OPM_ADDR),a
	call	sfg_fm_wait
	ld	a,e
	ld	(OPM_DATA),a
	ret
;********************************************
;sfg_write_fmop
;
; in   : D = base addr, E = data
; dest : AF,DE
;
sfg_write_fmop:
	ld	a,(seq_cur_ch)
	add	a,d
	ld	d,a
	ld	a,(seq_opsel)
	and	$03
	rlca
	rlca
	rlca
	add	a,d
	ld	d,a
	jr	sfg_fm_out

;********************************************
;sfg_write_fmch
;
; in   : D = base addr, E = data
; dest : AF,DE
;
sfg_write_fmch:
	ld	a,(seq_cur_ch)
	add	a,d
	ld	d,a
	jr	sfg_fm_out
;
;********************************************
; add number of channel to the index of register
; in : D = index of register
; dest : AF
moon_add_reg_ch:
	ld	a,(seq_cur_ch)
	add	a,d
	ld	d,a
	ret

;
;********************************************
; set frequency on the channel
; in   : work
; dest : AF,DE
moon_set_freq_ch:
	ret


;********************************************
; sfg_set_vol_ch
; Set volume on the channel
; in   : work
; dest : AF,DE
sfg_set_vol_ch:
	push	ix
	push	hl
	push	bc

	xor	a
	ld	(seq_opsel),a

	ld	b,(ix + IDX_VOL)
	ld	h,(ix + IDX_VOLOP)

	ld	c,$04
sfg_set_fmvol_lp:
	rr	h
	jr	nc,skip_set_fmvol
	call	sfg_calc_fmvol

	ld	e,a
	ld	d,$60    ; TL
	call	sfg_write_fmop
skip_set_fmvol:
	inc	ix
	ld	a,(seq_opsel)
	inc	a
	ld	(seq_opsel),a

	dec	c
	jr	nz,sfg_set_fmvol_lp

	pop	bc
	pop	hl
	pop	ix
	ret


sfg_calc_fmvol:
	; A = (OL + (127-VOL))
	ld	a,b ; B = (IDX_VOL)
	and	$7f
	xor	$7f
	ld	e,a

	ld	a,(ix + IDX_OPVOL1)
	and	$7f
	add	a,e
	cp	$80
	jr	nc,set_fmvol_min
	ld	e,a
	jr	set_fmvol_ks
set_fmvol_min:
	ld	e,$7f
set_fmvol_ks:
	ld	a,e
	ret


;********************************************
; sfg_key_off
; this function does : key-off
; in   : work
; dest : almost all
sfg_key_off:
	; key off

sfg_key_fmoff:
	ld	a,(seq_cur_ch)
	and	$07
	ld	e,a
	ld	d,$08
	jp	sfg_fm_out     ; KEY ON


;********************************************
; sfg_key_on
; Set tone number,frequency and key-on
; in   : work
; dest : almost all
;
sfg_key_on:
	call	sfg_key_off


sfg_key_fmon:
	ld	a,(seq_cur_ch)
	and	$07
	ld	e,a
	ld	a,(ix + IDX_KEY)
	or	e
	ld	e,a
	ld	d,$08
	jp	sfg_fm_out     ; KEY ON



