;*******************************************************************************************************************
;*
;* GLOBAL REGISTER DEFINITIONS
;* r0..r1 are temporary registers used for SPM/MUL instructions
;* r26..r31 is used for XL, XH, YL, YH, ZL and ZH registers
;*
;*******************************************************************************************************************
.def zero               = r0    ;zero register (not hardwired!)

.def ff                 = r1    ;always set to 0xFF
.equ ff_addr            =  1    ;

.def wdie_const         = r2    ;constant to enable watchdog interrupt
.equ wdie_const_addr    =  2    ;

.def chnl_sel           = r3    ;channel selector is element of(1,2,4,8,16,255)
.equ chnl_sel_addr      =  3    ;

.def rx_state           = r4    ;actual condition of the finite state machine
.equ rx_state_addr      =  4    ;
.def rx_midi_cmd        = r5    ;buffer for one midi command during reception
.def rx_midi_key        = r6    ;

.def midi_cmd           = r7    ;holds the received midi command, release it by setting midi_cmd to zero
.equ midi_cmd_addr      =  7    ;
.def midi_key           = r8    ;

.def out_buf            = r9    ;buffers output
.equ out_buf_addr       =  9    ;

.def sreg_bak           = r10   ;backup registers
.def sat                = r11   ;

;def unused             = r12   ;unused register
;def unused             = r13   ;
;def unused             = r14   ;

.def v1                 = r15   ;second function return value


.def at                 = r16   ;temporary register

.def v0                 = r17   ;function return value

.def t0                 = r18   ;temporary registers
.def t1                 = r19   ;
.def t2                 = r20   ;
.def t3                 = r21   ;

.def a0                 = r22   ;function arguments
.def a1                 = r23   ;
.def a2                 = r24   ;
.def a3                 = r25   ;
