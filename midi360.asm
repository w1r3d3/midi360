;*******************************************************************************************************************************
;*
;* Title:     midi360 firmware
;* Version:   1.0 (09/26/2009)
;* Target:    ATtiny2313
;* Filename:  midi360.asm
;* Author:    Michael Rentschler <http://w1r3.de/>
;*
;* copyright (c) 2009 Michael Rentschler
;*
;* This file is licensed under the GNU GPL v3.
;* No warranty of any kind is explicitly or implicitly stated.
;* For more information read the LICENSE file attached to this project.
;*
;*******************************************************************************************************************************
;*
;* DESCRIPTION
;*
;* interface between edrums with midi-out and the xbox360 portable drumset controller from "madcatz".
;* this device allows gamers to play rockband/guitarhero with their real drumsets.
;* a far way better solution than modding the original rockband drum.
;*
;*******************************************************************************************************************************
;*
;* KNOWN BUGS / TODOs
;*
;* - improove input button debouncing algorithm, sometimes long button presses get lost
;* - timer1 can be used to pwm/flash the pwr and err led
;* - optimization: since we have an 8-bit address space on the ATtiny, all "ldz ADR" macros can be replaced by "ldi zl, ADR"
;*
;*******************************************************************************************************************************
;*
;* CHIP CONFIGURATION
;*
;* ext. osc. 16 MHz, startup 16K CK+4.1ms, brown-out disabled, EESAVE, SPIEN (lfuse=0xef, hfuse=0x9f, efuse=0xff)
;*
;*******************************************************************************************************************************




;*******************************************************************************************************************************
;*
;* GLOBAL INCLUDES
;*
;*******************************************************************************************************************************
.include "tn2313def.inc"
.include "macros.inc"
.include "regdef.inc"
.include "equates.inc"




;*******************************************************************************************************************************
;*
;* EEPROM SEGMENT
;*
;*******************************************************************************************************************************
.ESEG

eep_chnl_map:   ;byte CHNL_MAP_SIZE     ;used to save the maptbl from memory




;*******************************************************************************************************************************
;*
;* DATA SEGMENT
;*
;*******************************************************************************************************************************
.DSEG

chnl_map:       .byte 1 ;CHNL_MAP_SIZE (bug inside AVRA Version 1.2.2 Build 94)




;*******************************************************************************************************************************
;*
;* CODE SEGMENT
;*
;*******************************************************************************************************************************
.CSEG
                .org  0                         ;interrupt handler table
                rjmp  main                      ;0x00: reset handler
                rjmp  ext_int                   ;0x01: external interrupt 0 handler
                reti                            ;0x02: external interrupt 1 handler
                reti                            ;0x03: timer1 capture event handler
                cbi   OUT_C5_PORT, OUT_C5       ;0x04: timer1 compare match A handler
                reti                            ;0x05: timer1 overflow handler
                reti                            ;0x06: timer0 overflow handler
                rjmp  rx_int                    ;0x07: USART: Rx complete handler
                reti                            ;0x08: USART: data register empty
                reti                            ;0x09: USART: Tx complete handler
                reti                            ;0x0A: ADC conversion complete handler
                reti                            ;0x0B: pin change interrupt handler
                reti                            ;0x0C: timer1 compare match B handler
                rjmp  t0a_int                   ;0x0D: timer0 compare match A handler
                rjmp  t0b_int                   ;0x0E: timer0 compare match B handler
                reti                            ;0x0F: usi start condition handler
                reti                            ;0x10: usi overflow handler
                reti                            ;0x11: EEPROM ready handler
                rjmp  wdt_int                   ;0x12: watchdog timer overflow




;===================================================================================================================
; INIT TABLE
;===================================================================================================================
init_tbl:       .db   ff_addr,          0xFF            ;init register 'ff'
                .db   wdie_const_addr,  WDIE_CONST_INIT ;init register 'wdie_const'
                .db   chnl_sel_addr,    1               ;init register 'chnl_sel'
                .db   rx_state_addr,    0               ;init register 'rx_state'
                .db   midi_cmd_addr,    0               ;init register 'midi_cmd'
                .db   out_buf_addr,     0               ;init register 'out_buf'

                .db   0x20+ACSR,        (1<<ACD)        ;switch off power to the comparator, just for an energy star

                .db   0x20+EECR,        0               ;ensure EEPMx=0

                .db   0x20+PORTB,       PORTB_INIT      ;init port b
                .db   0x20+DDRB,        DDRB_INIT       ;
                .db   0x20+PORTD,       PORTD_INIT      ;init port d
                .db   0x20+DDRD,        DDRD_INIT       ;

                .db   0x20+UBRRH,       HIGH(UBRR_INIT) ;init usart
                .db   0x20+UBRRL,       LOW(UBRR_INIT)  ;
                ;db   0x20+UCSRA,       UCSRA_INIT      ;
                .db   0x20+UCSRB,       UCSRB_INIT      ;
                .db   0x20+UCSRC,       UCSRC_INIT      ;
                .db   0x20+UDR,         MIDI_CMD_SYSEX  ;

                .db   0x20+TCCR0A,      TCCR0A_INIT     ;init timer0
                .db   0x20+TCCR0B,      TCCR0B_INIT     ;
                .db   0x20+OCR0A,       OCR0A_INIT      ;
                .db   0x20+OCR0B,       OCR0B_INIT      ;

                .db   0x20+TCCR1A,      TCCR1A_INIT     ;init timer1
                .db   0x20+TCCR1B,      TCCR1B_INIT     ;
                .db   0x20+OCR1AH,      HIGH(OCR1A_INIT);
                .db   0x20+OCR1AL,      LOW(OCR1A_INIT) ;

                .db   0x20+MCUCR,       MCUCR_INIT      ;init MCU control register
                .db   0x20+GIMSK,       GIMSK_INIT      ;init general interrupt mask register
                .db   0x20+TIMSK,       TIMSK_INIT      ;init timer interrupt mask register

                .db   0x20+SPL,         RAMEND          ;init stack

                .equ  INIT_TBL_EOT = 0x20+SPL           ;declare dynamic list terminator




;===============================================================================================================================
; FUNCTION      main
; DESCRIPTION   firmware entry point
;===============================================================================================================================
main:          ;cli                             ;disable interrupts
                clr   zero                      ;init zero register

                ldz   init_tbl*2                ;init registers/memory from table
                clr   xh                        ;
  main_init_lp: lpm   xl, z+                    ;  load memory address from table
                lpm   at, z+                    ;  load data byte from table
                st    x, at                     ;  store data byte
                cpi   xl, INIT_TBL_EOT          ;  was list terminator?
                brne  main_init_lp              ;    no -> loop

                rcall delay500ms                ;wait some time

                rcall eeprom_read               ;read mapping table from eeprom

;-------------------------------------------------------------------------------------------------------------------------------

  main_reenter: rcall leds_off                  ;turn all leds off (except pwr-led)
                cbi   LED_ERR_PORT, LED_ERR     ;

                sei                             ;enable interrupts

                rcall input_flush               ;clear input buffer

;-------------------------------------------------------------------------------------------------------------------------------

  main_lp:      sleep                           ;wait for event

                rcall leds_off                  ;all leds off
                mov   a0, out_buf               ;show triggered channels
                rcall leds_toggle               ;

                sbic  INPUT, INPUT_FLAG_BTNLNG  ;long button press?
                rjmp  setup_enter               ;  yes -> enter setup mode

                sbrs  midi_cmd, 7               ;midi command present?
                rjmp  main_lp                   ;   no -> loop
                mov   a0, midi_cmd              ;  yes -> load midi cmd into a1:a0
                mov   a1, midi_key              ;
                clr   midi_cmd                  ;         release midi command
                rcall midi_translate            ;         translate midi command into output channels
                or    out_buf, v0               ;         merge result with output

                sbrs  v0, 4                     ;output for C5 (basedrum) set?
                rjmp  main_lp                   ;   no -> back to loop
                out   TCNT1H, zero              ;  yes -> C5 will get switched off on timer1 compare-match A
                out   TCNT1L, zero              ;         reset timer1 counter
                sbi   OUT_C5_PORT, OUT_C5       ;         switch C5 on now
                rjmp  main_lp                   ;         back to loop

;===============================================================================================================================

  setup_enter:  sbi   LED_ERR_PORT, LED_ERR     ;turn error led on
                cbi   LED_PWR_PORT, LED_PWR     ;turn power led off
                rcall input_flush               ;clear input buffer
                clr   out_buf                   ;clear any pending output

                rcall leds_off                  ;turn channel led on
                mov   a0, chnl_sel              ;
                rcall leds_toggle               ;

  setup_lp:     sleep                           ;wait for event

                sbic  INPUT, INPUT_FLAG_BTNLNG  ;long button press?
                rjmp  setup_btnlng              ;  yes ->

                sbic  INPUT, INPUT_FLAG_BTNREL  ;short button press?
                rjmp  setup_btn                 ;  yes ->

                sbrs  midi_cmd, 7               ;midi command present?
                rjmp  setup_lp                  ;  no -> loop

                rcall midi_remove               ;remove midi note from the mapping table
                mov   v1, v0                    ;store result
                cpse  chnl_sel, ff              ;always mark selected channel (if not all selected)
                or    v1, chnl_sel              ;
                rcall midi_insert               ;insert midi note into the mapping table
                cpi   v0, 1                     ;  verify result
                brne  su_mins_ok                ;
                sbi   LED_PWR_PORT, LED_PWR     ;    overflow -> turn power led on
  su_mins_ok:

                ldi   t3, 2*3                   ;flash channel leds whenever an entry was removed
  su_ledtgl_lp: mov   a0, v1                    ;  v1 contains the mask of all leds to toggle
                rcall leds_toggle               ;
                sbi   LED_PWR_PIN, LED_PWR      ;  toggle power led
                sbi   LED_ERR_PIN, LED_ERR      ;  toggle error led
                lda   100*100                   ;  delay and make flashing visible
                rcall delay                     ;
                dec   t3                        ;  loop
                brne  su_ledtgl_lp              ;

                clr   midi_cmd                  ;release midi command
                rjmp  setup_enter               ;  and return to setup

;-------------------------------------------------------------------------------------------------------------------------------

  setup_btn:    lsl   chnl_sel                  ;select next channel by shifting bit (1, 2, 4, 8, 16, 255, then restart with 1)
                brcc  chnl_sel_nof              ;special channel 0xFF was selected?
                com   chnl_sel                  ;  then set chnl_sel to 0x01
  chnl_sel_nof: sbrc  chnl_sel, CHANNELS        ;already stepped through all channels?
                mov   chnl_sel, ff              ;  then set chnl_sel to 0xFF

                rjmp  setup_enter               ;update changes, stay in setup

;-------------------------------------------------------------------------------------------------------------------------------

  setup_btnlng: cp    chnl_sel, ff              ;selected special channel 0xFF?
                brne  setup_leave               ;  no -> then we have to leave setup now

                sbi   LED_PWR_PORT, LED_PWR     ;turn power led on
                rcall leds_off                  ;turn all channel leds off
                rcall delay500ms                ;wait some time

                ldz   chnl_map                  ;reset the whole mapping table in memory to default state (empty)
                ldi   at, CHNL_MAP_SIZE         ;
  etbl_clr_lp:  st    z+, ff                    ;
                dec   at                        ;
                brne  etbl_clr_lp               ;

                rjmp  setup_enter               ;update changes, stay in setup

;-------------------------------------------------------------------------------------------------------------------------------

  setup_leave:  sbi   LED_PWR_PORT, LED_PWR     ;turn power led on
                rcall leds_off                  ;turn all channel leds off

                rcall eeprom_write              ;write mapping table to eeprom

                rjmp  main_reenter              ;return to default mode




;===============================================================================================================================
; INTERRUPT     ext_int
; DESCRIPTION   called whenever the button status changes
; WASTE         -
;===============================================================================================================================
ext_int:        sbic  BTN_PIN, BTN              ;button pressed?
                rjmp  ext_btn_up                ;  no -> branch

  ext_btn_down: sbi   INPUT, INPUT_FLAG_BTN     ;remember button was pressed
                cbi   INPUT, INPUT_FLAG_BTNREL  ;
                out   WDTCR, wdie_const         ;enable watchdog timer
                reti                            ;return from interrupt

  ext_btn_up:   sbi   INPUT, INPUT_FLAG_BTNREL  ;remember button was released
                out   WDTCR, zero               ;disable watchdog
                reti                            ;return from interrupt


;===============================================================================================================================
; INTERRUPT     rx_int
; DESCRIPTION   called whenever the usart received a character
; WASTE         -
;===============================================================================================================================
rx_int:         in    sreg_bak, sreg            ;backup registers and flags
                mov   sat, at                   ;

                in    at, UCSRA                 ;read status
                andi  at, (1<<FE)|(1<<DOR)      ;rx error?
                in    at, UDR                   ;  read character (doesn't alter the flags)
                brne  rx_error                  ;  -> on rx error
                cpi   at, 0xF8                  ;ignore some specific SysRealtime commands
                brsh  rx_reti                   ;  status byte >= 0xF8? -> ignore
                sbrc  at, 7                     ;received midi command status byte?
  rx_error:     clr   rx_state                  ;  -> reset finite state machine (s1)

  rx_switch:    tst   rx_state                  ;switch(machine state)
                breq  rx_run_s1                 ;   0 => s1
                brmi  rx_run_s2                 ;  -1 => s2
               ;brpl  rx_run_s3                 ;  +1 => s3
;-------------------------------------------------------------------------------------------------------------------------------
  rx_run_s3:    neg   rx_state                  ;advance to previous machine state (s2)
                cpi   at, MIDI_VEL_THRESHOLD    ;velocity of midi note below threshold?
                brlo  rx_reti                   ;  yes -> ignore midi note

                                                ;store received midi note...
                sbrc  midi_cmd, 7               ;  midi command buffer locked?
                rjmp  rx_reti                   ;   yes -> throw away midi note and return
                mov   midi_key, rx_midi_key     ;    no -> store midi note
                mov   midi_cmd, rx_midi_cmd     ;
                rjmp  rx_reti                   ;          and return
;-------------------------------------------------------------------------------------------------------------------------------
  rx_run_s2:    neg   rx_state                  ;advance to next machine state (s3)
                mov   rx_midi_key, at           ;received key value of midi note-on command, buffer it
                rjmp  rx_reti                   ;and return
;-------------------------------------------------------------------------------------------------------------------------------
  rx_run_s1:    andi  at, MIDI_CMD_MASK         ;received a valid midi command?
                cpi   at, MIDI_CMD_VALUE        ;
                brne  rx_reti                   ;   no -> return
                dec   rx_state                  ;  yes -> advance to next machine state (s2)
                mov   rx_midi_cmd, at           ;         buffer command value
               ;rjmp  rx_reti                   ;         and return
;-------------------------------------------------------------------------------------------------------------------------------
  rx_reti:      mov   at, sat                   ;restore registers and flags
                out   sreg, sreg_bak            ;
                reti                            ;  return from interrupt


;===============================================================================================================================
; INTERRUPT     tx_int
; DESCRIPTION   called when the usart data register is empty
; WASTE         -
;===============================================================================================================================
;tx_int:        out   UDR, ?                    ;send next byte
;               cbi   UCSRB, UDRIE              ;disable this interrupt (nothing more to transmit)
;               reti                            ;  and return


;===============================================================================================================================
; INTERRUPT     t0a_int
; DESCRIPTION   timer0 match a handler, timer counter will reset (CTC)
;               updates the output and resets the output buffer (time-discrete output, on-phase)
; WASTE         -
;===============================================================================================================================
t0a_int:        sbrc  out_buf, 0                ;set C1?
                sbi   OUT_C1_PORT, OUT_C1       ;
                sbrc  out_buf, 1                ;set C2?
                sbi   OUT_C2_PORT, OUT_C2       ;
                sbrc  out_buf, 2                ;set C3?
                sbi   OUT_C3_PORT, OUT_C3       ;
                sbrc  out_buf, 3                ;set C4?
                sbi   OUT_C4_PORT, OUT_C4       ;
               ;sbrc  out_buf, 4                ;set C5?
               ;sbi   OUT_C5_PORT, OUT_C5       ;

                cpse  out_buf, zero             ;transmit midi command for debugging purposes
                out   UDR, out_buf              ;  if output is not zero

                mov   out_buf, zero             ;reset output buffer

                reti                            ;return from interrupt


;===============================================================================================================================
; INTERRUPT     t0b_int
; DESCRIPTION   timer0 match b handler, timer counter stays unchanged
;               turn off all output signals (time-discrete output, off-phase)
; WASTE         -
;===============================================================================================================================
t0b_int:        cbi   OUT_C1_PORT, OUT_C1       ;reset output
                cbi   OUT_C2_PORT, OUT_C2       ;
                cbi   OUT_C3_PORT, OUT_C3       ;
                cbi   OUT_C4_PORT, OUT_C4       ;
               ;cbi   OUT_C5_PORT, OUT_C5       ;

                reti                            ;return from interrupt


;===============================================================================================================================
; INTERRUPT     wdt_int
; DESCRIPTION   called whenever the button was held down for a while (see WDT_ENABLE for more informations)
; WASTE         -
;===============================================================================================================================
wdt_int:        sbi   INPUT, INPUT_FLAG_BTNLNG  ;remember button was pressed long
                out   WDTCR, zero               ;disable watchdog
                reti                            ;return from interrupt




;===============================================================================================================================
; FUNCTION      eeprom_read
; DESCRIPTION   read mapping table from eeprom
; INPUT         n/a
; WASTE         flags, at, t0, z
;===============================================================================================================================
eeprom_read:    ldi   t0, eep_chnl_map          ;clear eeprom address counter
                ldz   chnl_map                  ;let z point to mapping table
  eerd_lp:      sbic  EECR, EEPE                ;wait for last operation to complete
                rjmp  eerd_lp                   ;

                out   EEAR, t0                  ;setup target address
                sbi   EECR, EERE                ;read one byte from eeprom
                in    at, EEDR                  ;  grab it from the data register
                st    z+, at                    ;  and store data byte into memory

                inc   t0                                ;increment address
                cpi   t0, eep_chnl_map + CHNL_MAP_SIZE  ;still inside bounds?
                brlo  eerd_lp                           ;  yes -> loop until all data was written
                ret                                     ;   no -> return


;===============================================================================================================================
; FUNCTION      eeprom_write
; DESCRIPTION   write mapping table to eeprom
; INPUT         n/a
; WASTE         flags, at, t0, z
;===============================================================================================================================
eeprom_write:   ldi   t0, eep_chnl_map          ;clear eeprom address counter
                ldz   chnl_map                  ;let z point to mapping table
  eewr_lp:      sbic  EECR, EEPE                ;wait for last operation to complete
                rjmp  eewr_lp                   ;

                out   EEAR, t0                  ;setup target address
                sbi   EECR, EERE                ;read one byte from eeprom
                in    at, EEDR                  ;  grab it from the data register
                ld    t1, z+                    ;  load next data byte from memory
                cp    t1, at                    ;  already valid?
                breq  eewr_skip                 ;    yes -> skip writing

                out   EEDR, t1                  ;write data byte into data register
                sbi   EECR, EEMPE               ;enable writing eeprom
                sbi   EECR, EEPE                ;start writing eeprom

  eewr_skip:    inc   t0                                ;increment address
                cpi   t0, eep_chnl_map + CHNL_MAP_SIZE  ;still inside bounds?
                brlo  eewr_lp                           ;  yes -> loop until all data was written
                ret                                     ;   no -> return


;===============================================================================================================================
; FUNCTION      notetbl_chksum
; DESCRIPTION   calculate fletcher16 checksum over the entire mapping table in memory
;               also see: http://en.wikipedia.org/wiki/Fletcher's_checksum
; INPUT         n/a
; OUTPUT        v0 = checksum
; WASTE         flags, ?
;===============================================================================================================================
;notetbl_chksum: ldz   chnl_map
;                ldi   t0, CHNL_MAP_SIZE
;  ntcs_lp:      ld    at, z+
;               ;TODO
;                dec   t0
;                brne  ntcs_lp
;               ;TODO
;                ret


;===============================================================================================================================
; FUNCTION      midi_translate
; DESCRIPTION   translate a midi note into 5-channel output according the mapping table
; INPUT         a0 = midi_cmd
;               a1 = midi_key
; OUTPUT        v0 = flags for triggered channels 1..5
; WASTE         flags, at, t0, t1, t2, z, v0
;===============================================================================================================================
midi_translate: clr   v0                        ;clear results
                ldz   chnl_map                  ;point z to mapping table
                ldi   t0, 1<<0                  ;start with channel 1
  mtrl_chnl_lp: ldi   t1, CHANNEL_ENTRIES       ;number of entries per channel
  mtrl_note_lp: ld    t2, z+                    ;load first midi command byte
                ld    at, z+                    ;load second midi command byte
                cp    t2, a0                    ;midi note equal?
                brne  mtrl_skip                 ;
                cp    at, a1                    ;
                brne  mtrl_skip                 ;
                or    v0, t0                    ;  yes -> set result flag
  mtrl_skip:    dec   t1                        ;stepped through all channel entries?
                brne  mtrl_note_lp              ;  no -> loop
                lsl   t0                        ;select next channel
                cpi   t0, 1<<CHANNELS           ;stepped through all channels?
                brlo  mtrl_chnl_lp              ;   no -> loop
                ret                             ;  yes -> return


;===============================================================================================================================
; FUNCTION      midi_remove
; DESCRIPTION   remove midi note from the mapping table
; INPUT         midi_cmd:midi_key = midi note
; OUTPUT        v0 = contains a flag for each channel which indicates if the channel was altered
; WASTE         flags, at, t0, t1, t2, z, v0
;===============================================================================================================================
midi_remove:    clr   v0                        ;clear result
                ldz   chnl_map                  ;point z to mapping table
                ldi   t0, 1<<0                  ;start with channel 1
  mrmv_chnl_lp: ldi   t1, CHANNEL_ENTRIES       ;number of entries per channel
  mrmv_note_lp: ld    t2, z+                    ;load first midi command byte
                ld    at, z+                    ;load second midi command byte
                cp    t2, midi_cmd              ;equal?
                brne  mrmv_skip                 ;
                cp    at, midi_key              ;
                brne  mrmv_skip                 ;
                                                ;  yes -> remove this entry...
                or    v0, t0                    ;           mark this channel
                sbiw  zh:zl, 2                  ;           move pointer back one entry, pointing to destination now
                lsl   t1                        ;           move all remaining entries one row up...
  mrmv_move_lp: ldd   at, z+2                   ;
                st    z+, at                    ;
                dec   t1                        ;
                brne  mrmv_move_lp              ;
                st    -z, ff                    ;           finally remove last note
                st    -z, ff                    ;
                sbiw  zh:zl, CHANNEL_SIZE-2     ;           reset pointer to the start of this channel
                ldi   t1, CHANNEL_ENTRIES       ;           reset channel counter
                                                ;
  mrmv_skip:    dec   t1                        ;stepped through all channel entries?
                brne  mrmv_note_lp              ;  no -> loop
                lsl   t0                        ;select next channel
                cpi   t0, 1<<CHANNELS           ;stepped through all channels?
                brlo  mrmv_chnl_lp              ;   no -> loop
                ret                             ;  yes -> return


;===============================================================================================================================
; FUNCTION      midi_insert
; DESCRIPTION   add midi note to the selected channel inside the mapping table (if a unique channel was selected)
; INPUT         chnl_sel = selected channel (may also contain 0xFF for special channel)
;               midi_cmd:midi_key = midi note
; OUTPUT        v0 = status (-1 => nothing inserted, 0 => success, 1 => oldest entry was overwritten)
; WASTE         flags, at, t0, t1, t2, z, v0
;===============================================================================================================================
midi_insert:    cp    chnl_sel, ff              ;selected special channel 0xFF?
                brne  mins_start                ;
                ldi   v0, -1                    ;  yes -> set result value
                ret                             ;         and return

  mins_start:   ldi   v0, 0                     ;set result value

                ldz   chnl_map-CHANNEL_SIZE     ;calculate address of selected channel inside mapping table
                mov   at, chnl_sel              ;
  mins_adr_lp:  adiw  zh:zl, CHANNEL_SIZE       ;
                lsr   at                        ;
                brcc  mins_adr_lp               ;

                ldi   t0, CHANNEL_ENTRIES       ;step through all entries, search the next free table entry
  mins_srch_lp: ld    at, z+                    ;
                adiw  zh:zl, 1                  ;
                cpi   at, 0xff                  ;  found?
                breq  mins_write                ;    yes -> write entry
                dec   t0                        ;
                brne  mins_srch_lp              ;

                ldi   v0, 1                     ;channel full, set result value

                sbiw  zh:zl, CHANNEL_SIZE       ;reset pointer to the start of selected channel
                ldi   t0, CHANNEL_ENTRIES*2     ;move all entries one row up, overwriting the oldest entry
  mins_move_lp: ldd   at, z+2                   ;
                st    z+, at                    ;
                dec   t0                        ;
                brne  mins_move_lp              ;

  mins_write:   st    -z, midi_key              ;write midi note into mapping table
                st    -z, midi_cmd              ;
                ret                             ;  and return


;===============================================================================================================================
; FUNCTION      input_flush
; DESCRIPTION   wait until button was released and clear the input flags
; INPUT         n/a
; WASTE         flags, at
;===============================================================================================================================
input_flush:    clr   at                        ;reset counter
  inpfl_lp:     sbis  BTN_PIN, BTN              ;wait until button was released
                clr   at                        ;  still pressed? -> reset counter
                out   INPUT, zero               ;clear input buffer
                inc   at                        ;increment counter
                brne  inpfl_lp                  ;  was not long enough released? -> loop
                ret                             ;return


;===============================================================================================================================
; FUNCTION      leds_off
; INPUT         n/a
; WASTE         -
;===============================================================================================================================
leds_off:       sbi   LED_C1_PORT, LED_C1       ;turn all leds off...
                sbi   LED_C2_PORT, LED_C2       ;
                sbi   LED_C3_PORT, LED_C3       ;
                sbi   LED_C4_PORT, LED_C4       ;
                sbi   LED_C5_PORT, LED_C5       ;
                ret


;===============================================================================================================================
; FUNCTION      leds_toggle
; INPUT         a0 = led mask (bits 0..4 = led c0..c4)
; WASTE         -
;===============================================================================================================================
leds_toggle:    sbrc  a0, 0                     ;toggle selected leds...
                sbi   LED_C1_PIN, LED_C1        ;
                sbrc  a0, 1                     ;
                sbi   LED_C2_PIN, LED_C2        ;
                sbrc  a0, 2                     ;
                sbi   LED_C3_PIN, LED_C3        ;
                sbrc  a0, 3                     ;
                sbi   LED_C4_PIN, LED_C4        ;
                sbrc  a0, 4                     ;
                sbi   LED_C5_PIN, LED_C5        ;
                ret


;===============================================================================================================================
; FUNCTION      delay, delay250ms, delay500ms
; INPUT         a1:a0 = time in 10us-units
; WASTE         flags, at, a0, a1
;===============================================================================================================================
delay500ms:     rcall delay250ms
delay250ms:     lda   250*100
delay:          ldi   at, ((XTAL+399999)/400000) - 1
  delay_lp:     nop
                dec   at
                brne  delay_lp

                subi  a0, 1
                sbci  a1, 0
                brne  delay
                ret




;***************************************************************************************************************************************
;*
;* END OF FILE
;*
;***************************************************************************************************************************************
