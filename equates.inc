;*******************************************************************************************************************************
;*
;* GLOBAL EQUATES
;*
;*******************************************************************************************************************************
.equ XTAL               = 16000000              ;clock frequency
.equ USART_BAUDRATE     = 31250                 ;serial port baudrate


.equ CHANNELS           = 5                     ;number of output channels
.equ CHANNEL_ENTRIES    = 10                    ;number of entries per channel
.equ CHANNEL_SIZE       = CHANNEL_ENTRIES*2     ;size of one channel inside mapping table
.equ CHNL_MAP_SIZE      = CHANNEL_SIZE*CHANNELS ;size for the entire mapping table


.equ MIDI_CMD_MASK      = 0xF0                  ;mask for the midi status byte before comparison
.equ MIDI_CMD_VALUE     = 0x90                  ;receiver only accepts this midi command (note-on)
.equ MIDI_VEL_THRESHOLD = 1                     ;receiver ignores midi note-on messages with lower velocity
.equ MIDI_CMD_SYSEX     = 0xF4                  ;system exclusive midi command, sent once when the device starts up


.equ INPUT              = GPIOR0                ;define the general purpose register used as an input buffer
.equ INPUT_FLAG_BTN     = 0                     ;declare flags for the INPUT register
.equ INPUT_FLAG_BTNLNG  = 1                     ;  long button press
.equ INPUT_FLAG_BTNREL  = 2                     ;  button released


.equ LED_PWR            = PORTB4                ;green power led (0 = off, 1 = on)
.equ LED_PWR_PIN        = PINB                  ;
.equ LED_PWR_PORT       = PORTB                 ;
.equ LED_ERR            = PORTB3                ;red error led (0 = off, 1 = on)
.equ LED_ERR_PIN        = PINB                  ;
.equ LED_ERR_PORT       = PORTB                 ;

.equ BTN                = PORTD2                ;input button (connected to INT0)
.equ BTN_PIN            = PIND                  ;

.equ LED_C1             = PORTD5                ;led for channel X (0 = on, 1 = off)
.equ LED_C1_PIN         = PIND                  ;
.equ LED_C1_PORT        = PORTD                 ;
.equ LED_C2             = PORTD6                ;
.equ LED_C2_PIN         = PIND                  ;
.equ LED_C2_PORT        = PORTD                 ;
.equ LED_C3             = PORTB0                ;
.equ LED_C3_PIN         = PINB                  ;
.equ LED_C3_PORT        = PORTB                 ;
.equ LED_C4             = PORTB1                ;
.equ LED_C4_PIN         = PINB                  ;
.equ LED_C4_PORT        = PORTB                 ;
.equ LED_C5             = PORTB2                ;
.equ LED_C5_PIN         = PINB                  ;
.equ LED_C5_PORT        = PORTB                 ;
.equ LED_CX_PB_MASK     = (1<<LED_C3)|(1<<LED_C4)|(1<<LED_C5)
.equ LED_CX_PD_MASK     = (1<<LED_C1)|(1<<LED_C2)

.equ OUT_C1             = PORTD3                ;output pins for channel X (0 = off, 1 = on)
.equ OUT_C1_PORT        = PORTD                 ;
.equ OUT_C2             = PORTB5                ;
.equ OUT_C2_PORT        = PORTB                 ;
.equ OUT_C3             = PORTB6                ;
.equ OUT_C3_PORT        = PORTB                 ;
.equ OUT_C4             = PORTB7                ;
.equ OUT_C4_PORT        = PORTB                 ;
.equ OUT_C5             = PORTD4                ;
.equ OUT_C5_PORT        = PORTD                 ;
.equ OUT_CX_PB_MASK     = (1<<OUT_C2)|(1<<OUT_C3)|(1<<OUT_C4)
.equ OUT_CX_PD_MASK     = (1<<OUT_C1)|(1<<OUT_C5)


;
; start-up values for misc. registers...
;
.equ WDIE_CONST_INIT    = (1<<WDIF)|(1<<WDIE)|(6<<WDP0) ;watchdog interrupt after 1sec

.equ PORTB_INIT         = (1<<LED_PWR)|(1<<LED_ERR)
.equ DDRB_INIT          = LED_CX_PB_MASK|OUT_CX_PB_MASK|(1<<LED_PWR)|(1<<LED_ERR)
.equ PORTD_INIT         = (1<<BTN)
.equ DDRD_INIT          = LED_CX_PD_MASK|OUT_CX_PD_MASK

.equ UBRR_INIT          = XTAL/16/USART_BAUDRATE - 1
.equ UCSRB_INIT         = (1<<RXEN)|(1<<TXEN)|(1<<RXCIE) ;(1<<UDRIE)
.equ UCSRC_INIT         = (1<<UCSZ0)|(1<<UCSZ1)

.equ TCCR0A_INIT        = (2<<WGM00)                    ;ctc-mode (clear timer on match A)
.equ TCCR0B_INIT        = (4<<CS00)                     ;clksrc = f/256
.equ OCR0A_INIT         = 255                           ;output frequency => ~244Hz
.equ OCR0B_INIT         = OCR0A_INIT*(( 50 ))/100       ;on-phase is 50% of one output period

.equ TCCR1A_INIT        = 0                             ;ctc-mode (clear timer on match A)
.equ TCCR1B_INIT        = (1<<WGM12)|(4<<CS10)          ;clksrc = f/256
.equ OCR1A_INIT         = (( 50 ))*1000/16              ;basedrum output on-phase => 50ms

.equ MCUCR_INIT         = (1<<SE)|(1<<ISC00)    ;enable idle sleep mode, enable int0 on any edge
.equ GIMSK_INIT         = (1<<INT0)             ;enable interrupts
.equ TIMSK_INIT         = (1<<OCIE0A)|(1<<OCIE0B)|(1<<OCIE1A)
