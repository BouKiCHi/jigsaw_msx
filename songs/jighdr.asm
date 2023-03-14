
;
; JIGS file header ver 1 by BKC
;
; 2007/05/24 first
;
;;

DATA_BANK .equ 0

	.bank 0
	.org  $8000

	ds    $80     ; fill the header with zero

	.org  $8000

	db    "JIGF"
	dw    $0003   ; version
	db    $00     ; num of used channels ( 0 = auto )

	db    SOUND_GENERATOR  ; device flags

	dw    $0000   ; adr title string ( terminated with zero )
	dw    $0000   ; adr artist string ( terminated with zero )
	dw    $0000   ; adr comment string ( terminated with zero )
	dw    $0000   ; reserved

	dw    sound_data_table   ; adr track table
	dw    sound_data_bank    ; adr track bank table

	dw    loop_point_table   ; adr loop table
	dw    loop_point_bank    ; adr loop bank table

	dw    softenve_table     ; adr venv table
	dw    softenve_lp_table  ; adr venv lp table

	dw    pitchenve_table    ; adr penv table
	dw    pitchenve_lp_table ; adr penv lp table

	dw    arpeggio_table     ; adr nenv table
	dw    arpeggio_lp_table  ; adr nenv lp table

	dw    $0000              ; adr lfo  table
	dw    ttbl_data_table    ; adr inst table

	dw    opl3tbl_data_table ; adr opl3 table

	.org  $8080

	.include "effect.h"


