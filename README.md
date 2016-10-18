# QR-Code Generator (Bar generator)

**QR-Code Generator 1.2**

(formerly named Bar Generator)

This Windows/MacOS/Linux desktop software generates formatted [QR codes](https://en.wikipedia.org/wiki/QR_code), like standard texts, browser URLs, phone call numbers, contacts, emails, SMS, MMS, calendar notes, youtube URLs, geographic locations, wifi configuration, Google Play links, etc.

[Download the Windows version](https://github.com/Ircama/qr-code-generator/releases) from this [link](https://github.com/Ircama/qr-code-generator/releases/download/1.2/bar_generator.zip).

A typical usage of this tool is to easily transfer preformatted text data from a PC to a smartphone without usage of any wireless or wired link.

Sending a message to a smartphone (e.g., to write an email, an SMS text, or a long string) can be done without the need to digit the text through the smartphone touchscreen (or similar standard input device): simply open *QR-Code Generator* on a desktop computer, write there the message (or paste it from a PC application, including integration with Microsoft Word), then open your preferred [Scan Code App](https://en.wikipedia.org/wiki/Barcode_Scanner_(application)) on your smartphone and get the formatted text from the desktop computer by reading its QR code, ready to be subsequently copied and pasted to any other smartphone App.

The source code of *QR-Code Generator* is in [Object Pascal](https://en.wikipedia.org/wiki/Object_Pascal) language, developed through the cross-platform [Lazarus IDE](http://www.lazarus-ide.org/). It can be freely compiled to your favorite OS including Windows, MacOS, Linux.

See [Installing Lazarus](http://wiki.freepascal.org/Installing_Lazarus) for setting up this [IDE](https://en.wikipedia.org/wiki/Lazarus_(IDE)).

Features:

* Selection of QR Message types:
  - Memo (standard),
  - SMS (supports SMS and SMSTO formats),
  - MMS,
  - Email (supports mail and mailto formats),
  - URI (18 different types),
  - Telephone,
  - Geo-positioning,
  - Contact (both vCard and meCard formats),
  - Event (with automatic local time),
  - Wifi Configuration (WEP, WPA/2 and no encryption). Wifi is only supported by Android.
* Integration with Microsoft Word (options to send and receive from Microsoft Word)
* Setting for QR Code, Aztec, Data Matrix Code generator and Textual (debug).
* Selection of message type formats: Standard, UTF8, UNICODE
* CR+LN toggle (carriage return + line feed for Windows scan processing or line feed for UNIX/smartphones scan processing)
* Setting for automatic or manual ECC level/Error Correction (codewords which can be restored: L=7%, M=15%, Q=25%, H=30%)
* Buttons to generate SMS or Email from the clipboard
* Button to load mesage from text
* Keyboard shortcuts:
  - <kbd>Alt</kbd> <kbd>M</kbd> to change the message format
  - <kbd>Alt</kbd> <kbd>T</kbd>: input text
  - <kbd>Alt</kbd> <kbd>E</kbd>: ECC Level
  - <kbd>Alt</kbd> <kbd>Q</kbd>: bar format
  - <kbd>Alt</kbd> <kbd>C</kbd>: CR-NL toggle
  - <kbd>Esc</kbd>: quit
  - <kbd>Alt</kbd> <kbd>L</kbd>: load text file

*QR-Code Generator* can also be linked to a keyboard shortcut (e.g., Alt-Grp S) via [AutoHotkey](https://autohotkey.com/) or similar software. Example of [AutoHotkey](https://github.com/AutoHotkey/AutoHotkey) script <kbd>Alt Gr</kbd>+<kbd>S</kbd> in case the QR Code Generator software is installed to *C:\Program Files\Qr\bar_generator.exe*:

```AutoHotkey
SetCapslockState AlwaysOff
SetTitleMatchMode, 2

;Open QR-Code Generator with AltGr S
<^>!s::
IfWinExist QR code generator
WinActivate QR code generator
else
Run, "C:\Program Files\Qr\bar_generator.exe", , max, OutputVarPID
WinWait, ahk_pid %OutputVarPID%
WinActivate, ahk_pid %OutputVarPID%
return
; note: set "SetTitleMatchMode, 2" at the beginning of the file
```

Some screenshots of *QR-Code Generator*:

Memo

![qr-code-memo](https://cloud.githubusercontent.com/assets/8292987/19455469/6c9c710a-94bd-11e6-8033-3abe121793e5.png)

SMS

![qr-code-sms](https://cloud.githubusercontent.com/assets/8292987/19455474/6cac0f16-94bd-11e6-89f9-3cfeece736b6.png)

Email

![qr-code-email](https://cloud.githubusercontent.com/assets/8292987/19455476/6cb06d5e-94bd-11e6-9f99-458c2348323f.png)

URI

![qr-code-uri](https://cloud.githubusercontent.com/assets/8292987/19455475/6caed1b0-94bd-11e6-8e24-6fd3725dbcfb.png)

Telephone

![qr-code-tel](https://cloud.githubusercontent.com/assets/8292987/19455470/6c9f7ea4-94bd-11e6-860b-6d8c36bee30a.png)

Geo-positioning

![qr-code-geo](https://cloud.githubusercontent.com/assets/8292987/19455471/6ca5ada6-94bd-11e6-9416-635ed1afb115.png)

Contact

![qr-code-contact](https://cloud.githubusercontent.com/assets/8292987/19455472/6ca79f08-94bd-11e6-9fc2-70a5de07bd43.png)

Event

![qr-code-event](https://cloud.githubusercontent.com/assets/8292987/19455468/6c9aa708-94bd-11e6-933b-b59c1bbd5086.png)

WiFi Configuration (only Android)

![qr-code-wifi](https://cloud.githubusercontent.com/assets/8292987/19455473/6caa255c-94bd-11e6-8346-1349f58cbbda.png)

## Credits

Original code (version 1.0) by by Dariusz Rorat ([dariuszrorat](http://sourceforge.net/users/dariuszrorat)).

Original sources imported from [sourceforge](https://sourceforge.net/projects/bargenerator/).
