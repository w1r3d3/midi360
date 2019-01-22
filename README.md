# midi360 edrum interface
Midi360 is an interface between an electronic drum set with midi-out and the Xbox360 portable drum kit controller from Madcatz. This device allows gamers to play Rockband or Guitar Hero with their real drumset. A far way better solution than modding the original rockband drum, it's just real plug-n-play. And even more important, it's really cheap to build.

## device usage
all leds light up on startup for half a second, then the status led goes green, all other leds turn off. the device now listens to the midi stream on the midi-in port and recognizes all midi note-on commands, ignoring the velocity value. connect the
five output channels to the madcatz portable drum module, plug5 must get connected as the basedrum, all other plugs can be placed randomly. take care not to connect a plug into the headphone output.

any received midi note can be redirected to a single output channel, this mapping gets done by a programmable mapping table. a mapping to multiple output channels is not provided, even if it may be possible. the table gets loaded from eeprom into memory
on startup, leaving setup stores changed configurations back into the eeprom. the table allows a maximum of 10 entries per channel. to setup different sets, just select a different midi channel on your edrum.

a long button press enters setup and the status led goes red. each of the 5 channels has an own led for channel selection. select another channel during setup mode by pressing the button shortly, the repeating sequence is: 1,2,3,4,5,all. while all channels are selected, a long button press will reset the entire mapping table. playing a valid midi note while all channels are selected will remove the played note from the table, if found. playing a valid midi note while a single channel is selected appends this note to this channel and the status led blinks rd/gr. if the table for this channel is full, then the status led flashes orange and the last entered note gets removed, giving a place for the new one. if the new midi note was already registered for the same or another channel, then this entry gets removed and the related channel led flashes. do a long button press to save changes and leave setup, but only while a single channel was selected, or you will delete anything.

once at startup and for every channel output the device will send a midi message to the midi-out for debugging/testing purposes.

## part list

  * 1x dsub-15 female plug
  * 1x ATtiny2313
  * 1x crystal 16MHz
  * 2x capacitor 18pF
  * 1x usb (hub)
  * 1x usb (host)
  * 1x usb cable (to host)
  * 1x button for input
  * 1x led for status (rd/gr, 3mm)
  * 5x 3.5mm mono plug and cable
  * 5x led for channel selection (rd, 3mm)

  * 1x midi-2-gameport adapter
    or build your own adapter out of
    * 1x opto 6N138
    * 1x 5pol din socket
    * 1x dsub-15 male plug
    * 1x midi cable

  * and misc. resistors

## circuit
<pre>
                   _________________
  VCC   ___   ISP |        ^        | ISP   dsub1+8+9
   o---|50k|---o--|RESET         VCC|--o-----------o
  dsub15  ___     |                 | ISP        out4
   o-----|220|----|RXD           PB7|--o----------->
  dsub12  ___     |        A        | ISP        out3
   o-----|220|----|TXD     T     PB6|--o----------->
  GND             |        t        | ISP        out2
   o-----||---+---|XTAL2   i     PB5|--o----------->
  GND   16MHz $   |        n        |   ___   gr  GND
   o-----||---+---|XTAL1   y     PB4|--|147|--|>---o
  GND  button     |        2        |   ___   rd  GND
   o----o_/o------|INT0    3     PB3|--|147|--|>---o
  out1            |        1        |   ___  led5 VCC
   <--------------|PD3     3     PB2|--|147|--<|---o
  out5            |                 |   ___  led4 vCC
   <--------------|PD4           PB1|--|147|--<|---o
  VCC led1 ___    |                 |   ___  led3 VCC
   o--|>--|147|---|PD5           PB0|--|147|--<|---o
  dsub4+5     ISP |                 |   ___  led2 VCC
   o-----------o--|GND           PD6|--|147|--<|---o
                  |_________________|

  o out1..4
  |
 | |
 | | r1 = 5k
 |_|
  |
  +--------o plug1..4 (+)
  |
 | |
 | | r2 = 10k
 |_|
  |
  +--------o plug1..4 (-)
  |
  o
 GND
         ___
  +-----|___|---o plug5 (+)
  |      10k
   \|    ___
    |---|___|---o out5
   /|    10k
  | npn
  +-------------o plug5 (-)
  |
  o
 GND
</pre>

## links
  * http://de.wikipedia.org/wiki/Musical_Instrument_Digital_Interface
  * http://www.compuphase.com/electronics/midi_rs232.htm
  * http://www.mikrocontroller.net/articles/Midi_Rekorder_mit_MMC/SD-Karte
  * http://www.avrfreaks.net/index.php?func=viewItem&item_id=382&module=Freaks%20Tools
