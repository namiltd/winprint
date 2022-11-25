
ABOUT
-----

    WinPrint - Print Spooler for DOS Programs.

    Takes standard printer output produced by a DOS application, and forwards it 
    to a default Windows printer. Converts code page, strips empty pages, supports 
    BOX DRAWINGS chars. Works on all Windows platforms. Written in Borland DELPHI.


COPYRIGHT
---------

    Copyright (C) 2004-2008 Przemyslaw Czerkas <przemekc@users.sourceforge.net>
                  2008-2022 Mieczyslaw Nalewaj <namiltd@users.sourceforge.net>

    See GPL.TXT for copyright and license details.

    WinPrint is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    WinPrint is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with WinPrint; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
    MA  02111-1307  USA


COMPILATION
-----------

    As of now this project compiles from command-line (dcc32.exe - makefile included),
    compilation under IDE requires that source\auxiliary units are installed as Components.
    English translation 'under construction' ...


EXECUTION
---------

    The program works on windows version 95 and higher and NT version 4.0 and higher.
    For Windows 95 and 98 it requires DCOM Fix and the unicows.dll library for Windows 95.


SUPPORTED CODEPAGES
-------------------

    CodePageInfo                           CodePageNr
    -------------------------------------------------
    MS-DOS Latin US (CP-437)                437
    Polish Mazovia ZL (CP-620,CP-991)       620
    Polish Mazovia (CP-667,CP-790)          667
    Polish CP-852 compatible (CP-668)       668
    MS-DOS Arabic ASMO (CP-708)             708
    MS-DOS Arabic (CP-720)                  720
    MS-DOS Greek (CP-737)                   737
    Lithuanian (CP-774,CP-1118)             774
    MS-DOS Baltic Rim (CP-775)              775
    MS-DOS Cyrillic CIS 1 + Euro (CP-808)   808
    MS-DOS Latin 1 (CP-850)                 850
    MS-DOS Greek 1 (CP-851)                 851
    MS-DOS Latin 2 (CP-852)                 852
    MS-DOS Latin 3 (CP-853)                 853
    MS-DOS Cyrillic (CP-855)                855
    MS-DOS Hebrew (CP-856)                  856
    MS-DOS Turkish (CP-857)                 857
    MS-DOS Latin 1 + Euro (CP-858)          858
    MS-DOS Latin 9 (CP-859)                 859
    MS-DOS Portuguese (CP-860)              860
    MS-DOS Icelandic (CP-861)               861
    MS-DOS Hebrew (CP-862)                  862
    MS-DOS French Canada (CP-863)           863
    MS-DOS Arabic (CP-864)                  864
    MS-DOS Nordic (CP-865)                  865
    MS-DOS Cyrillic CIS 1 (CP-866)          866
    MS-DOS Hebrew + Euro (CP-867)           867
    MS-DOS Pakistan (CP-868)                868
    MS-DOS Modern Greek (CP-869)            869
    MS-DOS Cyrillic + Euro (CP-872)         872
    Thai (Win-874)                          874
    Japanese (Win-932)                      932
    Simplified Chinese (Win-936)            936
    Korean (Win-949)                        949
    Traditional Chinese (Win-950)           950
    Traditional Chinese HKSCS (Win-951)     951
    Central European (Win-1250)             1250
    Cyrillic (Win-1251)                     1251
    Western European (Win-1252)             1252
    Greek (Win-1253)                        1253
    Turkish (Win-1254)                      1254
    Hebrew (Win-1255)                       1255
    Arabic (Win-1256)                       1256
    Baltic (Win-1257)                       1257
    Vietnamese (Win-1258)                   1258
    Bulgarian MIK (CP-3021)                 3021
    Czech Kamenicky (CP-3844)               3844
    Hungarian CWI-2 (CP-3845)               3845
    Apple Western European (Mac-10000)      10000
    Apple Japanese (Mac-10001)              10001
    Apple Traditional Chinese (Mac-10002)   10002
    Apple Korean (Mac-10003)                10003
    Apple Arabic (Mac-10004)                10004
    Apple Hebrew (Mac-10005)                10005
    Apple Greek (Mac-10006)                 10006
    Apple Cyrillic (Mac-10007)              10007
    Apple Simplified Chinese (Mac-10008)    10008
    Apple Romanian (Mac-10010)              10010
    Apple Ukrainian (Mac-10017)             10017
    Apple Thai (Mac-10021)                  10021
    Apple Central Europe (Mac-10029)        10029
    Apple Icelandic (Mac-10079)             10079
    Apple Turkish (Mac-10081)               10081
    Apple Croatian (Mac-10082)              10082
    Russian KOI8-R (Win-20866)              20866
    Ukrainian KOI8-U (Win-21866)            21866
    Latin 1 (ISO-8859-1)                    28591
    Latin 2 (ISO-8859-2)                    28592
    Latin 3 (ISO-8859-3)                    28593
    Baltic (ISO-8859-4)                     28594
    Cyrillic KOI8-E (ISO-8859-5)            28595
    Arabic (ISO-8859-6)                     28596
    Greek (ISO-8859-7)                      28597
    Hebrew (ISO-8859-8)                     28598
    Turkish (ISO-8859-9)                    28599
    Latin 6 (ISO-8859-10)                   28600
    Thai (ISO-8859-11)                      28601
    Estonian (ISO-8859-13)                  28603
    Latin 8 (ISO-8859-14)                   28604
    Latin 9 (ISO-8859-15)                   28605
    Latin 10 (ISO-8859-16)                  28606
    Polish Combined ASCII                   60000
    Polish Microvex                         60001
    Polish CSK                              60002
    Polish Cyfromat                         60003
    Polish DHN                              60004
    Unicode (UTF-7)                         65000
    Unicode (UTF-8)                         65001


SUPPORTED EPSON ESCAPE CODES
----------------------------

    ASCII         DEC       Effect
    ------------------------------
    CR            13        Carriage Return
    LF            10        (Line Feed) New Line
    FF            12        Form Feed
    BS            8         Backspace
    SI            15 and
    ESC SI        27 15     Select condensed mode (17/20 CPI) (if 10 or 12 CPI)
    DC2           18        Cancel condensed mode (return to 10 or 12 CPI) (if 17 or 20 CPI)
    ESC M         27 77     Select 12 CPI (or 20 if condensed)
    ESC P         27 80     Select 10 CPI (or 17 if condensed)
    ESC g         27 103    Select 15-pitch chars
    SO            14 and
    ESC SO        27 14     Select double-wide mode (one line)
    DC4           20        Cancel double-wide mode (one line)
    ESC W {0,1}   27 87 0,1 Turn double-wide mode Off/On
    ESC @         27 64     Initialize printer
    ESC 0         27 48     Select 1/8-inch line spacing
    ESC 2         27 50     Select 1/6-inch line spacing
    ESC 1         27 49     Select 7/72-inch line spacing
    ESC E         27 69     Select bold mode
    ESC F         27 70     Cancel bold mode 
    ESC G         27 71     Select double-strike mode
    ESC H         27 72     Cancel double-strike mode
    ESC 4         27 52     Select italic mode
    ESC 5         27 53     Cancel Italic mode
    ESC S {0,1}   27 83 0,1 Select superscript/subscript mode
    ESC T         27 84     Deselect superscript/subscript mode
    ESC - {0,1}   27 45 0,1 Turn underline mode Off/On
    ESC ! {n}     27 33 n   Master select
    ESC $ {l} {h} 27 36 l h Set absolute horizontal print position


FRM FILE FORMAT
---------------

Sample frm file: (parameters must be defined in second line)
>>>>>
;FRM File: FontSize Orientation MarginLeft MarginRight MarginTop MarginBottom LinesPerPage CodePageNr
12 0 12,7 12,7 12,7 12,7 80 852
>>>>>
or
>>>>>
;FRM File: FontSize Orientation MarginLeft MarginRight MarginTop MarginBottom LinesPerPage CodePageNr
* * * * * * * 852
>>>>>
