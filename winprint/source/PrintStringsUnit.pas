{******************************************************************************}
{                                                                              }
{   WinPrint - Print Spooler for DOS Programs                                  }
{                                                                              }
{   Copyright (C) 2004 Przemyslaw Czerkas <przemekc@users.sourceforge.net>     }
{                 2008 Mieczyslaw Nalewaj <namiltd@users.sourceforge.net>      }
{   See GPL.TXT for copyright and license details.                             }
{                                                                              }
{******************************************************************************}

{******************************************************************************}
{                                                                              }
{   This file is part of WinPrint.                                             }
{                                                                              }
{   WinPrint is free software; you can redistribute it and/or modify           }
{   it under the terms of the GNU General Public License as published by       }
{   the Free Software Foundation; either version 2 of the License, or          }
{   (at your option) any later version.                                        }
{                                                                              }
{   WinPrint is distributed in the hope that it will be useful,                }
{   but WITHOUT ANY WARRANTY; without even the implied warranty of             }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              }
{   GNU General Public License for more details.                               }
{                                                                              }
{   You should have received a copy of the GNU General Public License          }
{   along with WinPrint; if not, write to the Free Software                    }
{   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                      }
{   MA  02111-1307  USA                                                        }
{                                                                              }
{******************************************************************************}

unit PrintStringsUnit;

interface

uses
  Windows, Classes, Graphics, Printers, ConversionUnit;

const
//  cMILTOINCH = 0.03937;
    cMILTOINCH = (1/25.4);
type
 { Prototype for a callback method that PrintString will call
   when it is time to print a header or footer on a page. The
   parameters that will be passed to the callback are:
   aCanvas   : the canvas to output on
   aPageCount: page number of the current page, counting from 1
   aTextRect : output rectangle that should be used. This will be
               the area available between non-printable margin and
               top or bottom margin, in device units (dots). Output
               is not restricted to this area, though.
   continue  : will be passed in as True. If the callback sets it
               to false the print job will be aborted. }
  THeaderFooterProc = procedure(aCanvas: TCanvas;
                                aPageCount: Integer;
                                aTextrect: TRect;
                                var continue: Boolean) of object;

{+------------------------------------------------------------
 | Function PrintStrings
 |
 | Parameters :
 |   lines:
 |     contains the text to print, already formatted into
 |     lines of suitable length. No additional wordwrapping
 |     will be done by this routine and also no text clipping
 |     on the right margin!
 |   leftmargin, topmargin, rightmargin, bottommargin:
 |     define the print area. Unit is inches, the margins are
 |     measured from the edge of the paper, not the printable
 |     area, and are positive values! The margin will be adjusted
 |     if it lies outside the printable area.
 |   linesPerInch:
 |     used to calculate the line spacing independent of font
 |     size.
 |   aFont:
 |     font to use for printout, must not be Nil.
 |   measureonly:
 |     If true the routine will only count pages and not produce any
 |     output on the printer. Set this parameter to false to actually
 |     print the text.
 |   OnPrintheader:
 |     can be Nil. Callback that will be called after a new page has
 |     been started but before any text has been output on that page.
 |     The callback should be used to print a header and/or a watermark
 |     on the page.
 |   OnPrintfooter:
 |     can be Nil. Callback that will be called after all text for one
 |     page has been printed, before a new page is started. The callback
 |     should be used to print a footer on the page.
 | Returns:
 |   number of pages printed. If the job has been aborted the return
 |   value will be 0.
 | Description:
 |   Uses the Canvas.TextOut function to perform text output in
 |   the rectangle defined by the margins. The text can span
 |   multiple pages.
 | Nomenclature:
 |   Paper coordinates are relative to the upper left corner of the
 |   physical page, canvas coordinates (as used by Delphis Printer.Canvas)
 |   are relative to the upper left corner of the printable area. The
 |   printorigin variable below holds the origin of the canvas coordinate
 |   system in paper coordinates. Units for both systems are printer
 |   dots, the printers device unit, the unit for resolution is dots
 |   per inch (dpi).
 | Error Conditions:
 |   A valid font is required. Margins that are outside the printable
 |   area will be corrected, invalid margins will raise an EPrinter
 |   exception.
 | Created: 13.05.99 by P. Below
 +------------------------------------------------------------}
function PrintStrings(Title: string;
                      lines: TStrings;
                      const CpNr: integer;
                      const PrinterId: integer;
                      const leftmargin,rightmargin,topmargin,bottommargin: single;
                      const orientation: TPrinterOrientation;
                      const linesPerInch: single;
                      const LinesPerPage: integer;
                      const SkipEmptyPages: boolean;
                      const aFont: TFont;
                      const Info: PBitmapInfo;
                      const Image: TmemoryStream;
                      const leftlogo,toplogo: single;
                      const firstpageonlylogo: boolean;
                      const EOPcodes: TCharCodes;
                      measureonly: boolean;
                      OnPrintheader,OnPrintfooter: THeaderFooterProc): integer;


implementation

uses
  Math;

function PrintStrings(Title: string;
                      lines: TStrings;
                      const CpNr: integer;
                      const PrinterId: integer;
                      const leftmargin,rightmargin,topmargin,bottommargin: single;
                      const orientation: TPrinterOrientation;
                      const linesPerInch: single;
                      const LinesPerPage: integer;
                      const SkipEmptyPages: boolean;
                      const aFont: TFont;
                      const Info: PBitmapInfo;
                      const Image: TmemoryStream;
                      const leftlogo,toplogo: single;
                      const firstpageonlylogo: boolean;
                      const EOPcodes: TCharCodes;
                      measureonly: boolean;
                      OnPrintheader,OnPrintfooter: THeaderFooterProc): integer;
var
  continuePrint: Boolean;     { continue/abort flag for callbacks }
  pagecount    : Integer;     { number of current page }
  textrect     : TRect;       { output area, in canvas coordinates }
  headerrect   : TRect;       { area for header, in canvas coordinates }
  footerrect   : TRect;       { area for footes, in canvas coordinates }
  lineheight   : Integer;     { line spacing in dots}
  charheight   : Integer;     { font height in dots  }
  charheightco : Integer;     { font height coefficient (10,12,15,17,20)}
  charstyleco  : TFontStyles; { font style coefficient }
  lineheightco : Integer;     { line spacing in dots coefficient (6,8,10)}
  sscriptco    : Integer;     { subscript/superscript coefficient (0,1,2)}
  doublewidthco: Integer;     { double width coefficient  (10 normal, 14 double)}
  textstart    : Integer;     { index of first line to print on
                                current page, 0-based. }
  ll,tl        : integer;     {logo position}

  { Calculate text output and header/footer rectangles. }
  procedure CalcPrintRects;
  var
    X_resolution: Integer;  { horizontal printer resolution, in dpi }
    Y_resolution: Integer;  { vertical printer resolution, in dpi }
    pagerect    : TRect;    { total page, in paper coordinates }
    printorigin : TPoint;   { origin of canvas coordinate system in
                              paper coordinates. }

    { Get resolution, paper size and non-printable margin from
      printer driver. }
    procedure GetPrinterParameters;
    begin
      with Printer.Canvas do
      begin
        X_resolution  := GetDeviceCaps( handle, LOGPIXELSX );
        Y_resolution  := GetDeviceCaps( handle, LOGPIXELSY );
        printorigin.X := GetDeviceCaps( handle, PHYSICALOFFSETX );
        printorigin.Y := GetDeviceCaps( handle, PHYSICALOFFSETY );
        pagerect.Left := 0;
        pagerect.Right:= GetDeviceCaps( handle, PHYSICALWIDTH );
        pagerect.Top  := 0;
        pagerect.Bottom := GetDeviceCaps( handle, PHYSICALHEIGHT );
      end;
    end;

    { Calculate area between the requested margins, paper-relative.
      Adjust margins if they fall outside the printable area.
      Validate the margins, raise EPrinter exception if no text area
      is left. }
    procedure CalcRects;
    var
      max: Integer;
    begin
      with textrect do
      begin
        { Figure textrect in paper coordinates }
        left:=round(leftmargin*X_resolution);
        if (left<printorigin.x) then
          left:=printorigin.x;

        top:=round(topmargin*Y_resolution);
        if (top<printorigin.y) then
          top:=printorigin.y;

        { Printer.PageWidth and PageHeight return the size of the
          printable area, we need to add the printorigin to get the
          edge of the printable area in paper coordinates. }
        right:=pagerect.right-round(rightmargin*X_resolution);
        max:=Printer.PageWidth+printorigin.X;
        if (right>max) then
          right:=max;

        bottom:=pagerect.bottom-round(bottommargin*Y_resolution);
        max:=Printer.PageHeight+printorigin.Y;
        if (bottom>max) then
          bottom:=max;

        { Validate the margins. }
        if (left>=right) or (top>=bottom) then
          raise EPrinter.Create(RString(600));
      end;

      { Convert textrect to canvas coordinates. }
      OffsetRect( textrect, -printorigin.X, -printorigin.Y );

      { Build header and footer rects. }
      headerrect := Rect( textrect.left, 0,
                          textrect.right, textrect.top );
      footerrect := Rect( textrect.left, textrect.bottom,
                          textrect.right, Printer.PageHeight );
    end;

  begin
    GetPrinterParameters;
    CalcRects;
    if (linesPerInch<>0) then
      lineheight:=round(Y_resolution/linesPerInch)
    else
      if (LinesPerPage<>0) then
        lineheight:=round((textrect.bottom-textrect.top)/LinesPerPage)
      else
        raise EPrinter.Create(RString(601));

    ll:=round(leftlogo*X_resolution);
    if (ll<printorigin.x) then
          ll:=printorigin.x;
    tl:=round(toplogo*Y_resolution);
    if (tl<printorigin.y) then
          tl:=printorigin.y;
  end;

  { Print a page with headers and footers. }
  procedure PrintPage;
   var
      y: Integer;
      r: TRect;
      do_break: boolean;
      len: integer;
      ws: WideString;
      wstmp: WideString;
      licz : integer; tw: integer; //text width
      ks: boolean; //kod sterujacy
      ig: integer; //ignore next znakow
      kodig: integer; //kod dwu (nie liczac ESC) lub wiecej znakowy (dziala dla ig>0)
      ep: boolean; //empty page
      pcfsm:integer; //Printer.Canvas.Font.size memory

    procedure FireHeaderFooterEvent(event: THeaderFooterProc; r: TRect);
    begin
      if assigned(event) then
      begin
        event(
           Printer.Canvas,
           pagecount,
           r,
           ContinuePrint );
        { Revert to our font, in case event handler changed
          it. }
        Printer.Canvas.Font := aFont;
      end;
    end;

    procedure DoHeader;
    begin
      FireHeaderFooterEvent( OnPrintHeader, headerrect );
    end;

    procedure DoFooter;
    begin
      FireHeaderFooterEvent( OnPrintFooter, footerrect );
    end;

     procedure DoLogo(const Info: PBitmapInfo; const Image: TmemoryStream; const ll,tl:integer);
     begin
     if (Image<>nil) and (Info<>nil) then begin
       try
//          Printer.Canvas.Draw(ll, tl, Bitmap);
         if (Info^.bmiHeader.biXPelsPerMeter=0) or (Info^.bmiHeader.biYPelsPerMeter=0)
          then
            StretchDIBits(Printer.Canvas.Handle,
             ll, tl, Info^.bmiHeader.biWidth, Info^.bmiHeader.biHeight,
             0,  0, Info^.bmiHeader.biWidth, Info^.bmiHeader.biHeight,
             Image.Memory, Info^, DIB_RGB_COLORS, SRCCOPY)
          else StretchDIBits(Printer.Canvas.Handle,
             ll, tl,
             Round(GetDeviceCaps( Printer.Canvas.handle, LOGPIXELSX )/(Info^.bmiHeader.biXPelsPerMeter/(cMILTOINCH*1000))
              *Info^.bmiHeader.biWidth),
             Round(GetDeviceCaps( Printer.Canvas.handle, LOGPIXELSY )/(Info^.bmiHeader.biYPelsPerMeter/(cMILTOINCH*1000))
              *Info^.bmiHeader.biHeight),
             0,  0, Info^.bmiHeader.biWidth, Info^.bmiHeader.biHeight,
             Image.Memory, Info^, DIB_RGB_COLORS, SRCCOPY);
       finally
       end;
     end;
    end;

    function Filter: boolean;
    var
        index: integer;

        licz: integer;
        linialength: integer;
        linia: string;

    begin
        result:=false;

//EOPcodes finding  char(12) = FF,char(26) = EOF ??
        linia:=lines[textStart];
        linialength:=length(lines[textStart]);
        index:=0;
        if (linialength>0) then for licz:=1 to linialength do begin //min finding
          if ord(linia[licz]) in EOPCodes then begin
            index:=licz;
            break;
          end;
        end;

        if (index>1) then
        begin
          lines.Insert(textStart+1,copy(lines[textStart],index,length(lines[textStart])-index+1));
          lines[textStart]:=copy(lines[textStart],0,index-1);
        end
        else
          if (index=1) then
          begin
            while (index<length(lines[textStart])) and (ord(lines[textStart][index]) in EOPCodes) do
              inc(index);
            if (length(lines[textStart])>index) then
              lines[textStart]:=copy(lines[textStart],index,length(lines[textStart])-index+1)
            else
              inc(textStart);
            result:=true;
          end;
    end;

    begin
      ep:= true;
             { Figure textrect in paper coordinates }
      if not SkipEmptyPages then begin
             ep:=false;
             Inc(pagecount);
      	     if pagecount>1 then Printer.NewPage;
             DoHeader;
             if (Image<>nil) and (Info<>nil) then begin
               if (pagecount<2) or
                  ( (firstpageonlylogo=false) and (pagecount>1)) then
                    DoLogo(Info,Image,ll ,tl);
             end;
             if not ContinuePrint then exit;
      end;

      SetLength(wstmp,1);
      y:=textrect.top;
      tw:=0;
      while (textStart<lines.count) and Filter do;
      while (textStart<lines.count) and (y<=(textrect.bottom-charheight)) do
      begin
        do_break:=false;
        if doublewidthco<128 then doublewidthco:=10; //10/10=1

        while (textStart<lines.count) and Filter do
          do_break:=true;

        if do_break then break;

        //oryginalnie
        //printer.canvas.TextOut( textrect.left, y, lines[textStart]);

        r:=Rect(textrect.left,y,textrect.right,y+charheight);
        len:=MultiByteToWideCharMy(CpNr,0,PChar(lines[textStart]),length(lines[textStart]),nil,0);
        if (len>0) then
        begin
          SetLength(ws,len);
          MultiByteToWideCharMy(CpNr,0,PChar(lines[textStart]), length(lines[textStart]),PWideChar(ws),len);

          ks:=false;
          ig:=0;
          kodig:=0;
          for licz:=1 to len do begin
	           wstmp[1]:=ws[licz];
             if ig>0 then begin //ignore next znakow
                            if (kodig<>0) then begin
                                case kodig of
                                 87: if ig=1 then case integer(wstmp[1]) of
                                      0,48: doublewidthco:=10; //10/10=1
                                      1,49: doublewidthco:=128+14; //+128 bo ma sie nie kasowac po nastepnej linii  14/10=1.4
                                     end;
                                 83: if ig=1 then case integer(wstmp[1]) of
                                      0,48: sscriptco:=0;
                                      1,49: sscriptco:=2;
                                     end;
                                 91: if ig=1 then case integer(wstmp[1]) of
                                      100: ig:=4; //ESC [d  Set Print Quality
                                       73: ig:=5; //ESC [I  Select Font and Pitch
                                       75: ig:=3; //ESC [K  Set initial condition
                                      1,49: sscriptco:=2;
                                     end;
                                end;
                                kodig:=0;
                            end;
                            dec(ig);
                            continue;
                          end;
             if ks then case integer(wstmp[1]) of
                15:  case charheightco of //ESC SI to samo co SI
                       10: charheightco:=17;
                       12: charheightco:=20;
                     end;
                64: begin //ESC @ reset ustawieñ
                      charheightco:=10;
                      lineheightco:=6;
                      charstyleco:=[];
                      doublewidthco:=10; //10/10=1
                      sscriptco:=1;
                    end;
                14: if doublewidthco<128 then doublewidthco:=14; //ESC SO to sam co SO  14/10=1.4
                48: lineheightco:=8;  //ESC 0
                50: lineheightco:=6;  //ESC 2
                49: lineheightco:=10; //ESC 1
                77: case charheightco of //ESC M
                      17,20: charheightco:=20;
                     else charheightco:=12;
                     end;
                80: case charheightco of //ESC P
                      17,20: charheightco:=17;
                     else charheightco:=10;
                     end;
                84: sscriptco:=1;     //ESC T
               103: charheightco:=15; //ESC g
             71,69: charstyleco:=charstyleco+[fsBold]; //ESC G i ESC E
             72,70: charstyleco:=charstyleco-[fsBold]; //ESC H i ESC F
                52: charstyleco:=charstyleco+[fsItalic]; //ESC 4
                53: charstyleco:=charstyleco-[fsItalic]; //ESC 5


                87: begin //ESC W  druk podwojnie szeroki
                     ig:=1;
                     kodig:=87;
                    end;

                83: begin //ESC S  indeks gorny/dolny
                     ig:=1;
                     kodig:=83;
                    end;

                91: begin //ESC [  rozne ignorowane kody ... okaze sie pozniej jakie
                     ig:=1;
                     kodig:=91;
                    end;

               120: ig:=1; //ESC x zignor.ustawianie NLQ
               116: ig:=1; //ESC t zignor.ustawianie chartable

               119: ig:=1; //ESC w zignor.ustawianie podwojna wysokosc
                85: ig:=1; //ESC U zignor.ustawianie drukowanie jednokierunkowe
               112: ig:=1; //ESC p zignor.ustawianie druk proporcjonalny

                67: ig:=1; //ESC C ignore page length

             end
             else case integer(wstmp[1]) of
                0..7,9..12,16,17,19,21..31: ; //puste by nic nie malowalo
                 8: if tw>0 then begin
                     pcfsm:=Printer.Canvas.Font.size; //save 
                     if (doublewidthco mod 128)=10 then Printer.Canvas.Font.size:=(aFont.Size * 10) div charheightco
                                                   else Printer.Canvas.Font.size:=(aFont.Size * 20) div charheightco;
                     tw:=tw-printer.canvas.TextWidth('A');
                     Printer.Canvas.Font.size:=pcfsm; //restore
                     if tw<0 then tw:=0;                   
                   end;

            //     8: if tw< printer.canvas.TextWidth('A') then tw:=0 //backspace
            //        else tw:=tw-printer.canvas.TextWidth('A');

            //     8: if tw< printer.canvas.TextWidth(wstmp) then tw:=0 //backspace
            //        else tw:=tw-printer.canvas.TextWidth(wstmp);

                13: tw:=0;	       //CR czyli cofnij do poczatku linii
                20: if doublewidthco<128 then doublewidthco:=10; //DC4  10/10=1
                14: if doublewidthco<128 then doublewidthco:=14; //SO to sam co ESC SO  14/10=1.4
                15: case charheightco of //SI to samo co ESC SI
                       10: charheightco:=17;
                       12: charheightco:=20;
                    end;
                18: case charheightco of //DC2
                       17: charheightco:=10;
                       20: charheightco:=12;
                     end;
                else begin
                   if SkipEmptyPages and ep and (integer(wstmp[1])<>32) then begin
                          ep:=false;
                          Inc(pagecount);
      	                  if pagecount>1 then Printer.NewPage;
                          DoHeader;
                          if (Image<>nil) and (Info<>nil) then begin
                            if (pagecount<2) or
                               ( (firstpageonlylogo=false) and (pagecount>1)) then
                                   DoLogo(Info,Image,ll,tl);
                          end;
                          if not ContinuePrint then exit;
                   end;
                   Printer.Canvas.Font.size:=(aFont.Size * (doublewidthco mod 128)) div ((charheightco*3) div (3-abs(1-sscriptco)));
                   Printer.Canvas.Font.style:=aFont.style + charstyleco;
                   // srodek - ((charheight-((charheight*(doublewidthco mod 128)) div ((charheightco*3) div (2+abs(1-sscriptco))))) div 2) przesuwa w dó³ by byly w jednej linii
                   // troche nizej (4*(charheight-((charheight*(doublewidthco mod 128)) div ((charheightco*3) div (2+abs(1-sscriptco))))) div 5) przesuwa w dó³ by byly w jednej linii

                   if integer(wstmp[1])<>32 then ExtTextOutW(Printer.Canvas.Handle,
                      r.Left+tw,r.Top+(((charheight-((charheight*(doublewidthco mod 128)) div ((charheightco*3) div (3-abs(1-sscriptco)))))*sscriptco) div 2),
                      ETO_CLIPPED,
                      @r,
                      PWideChar(wstmp),
                      1,
                      nil);

                   pcfsm:=Printer.Canvas.Font.size; //save 
                   if (doublewidthco mod 128)=10 then Printer.Canvas.Font.size:=(aFont.Size * 10) div charheightco
                                                 else Printer.Canvas.Font.size:=(aFont.Size * 20) div charheightco;
                   tw:=tw+printer.canvas.TextWidth('A');
                   Printer.Canvas.Font.size:=pcfsm; //restore

                   //tw:=tw+printer.canvas.TextWidth(wstmp);
                end;
             end;
             if integer(wstmp[1])=27 then ks:=true
                                     else ks:=false;

          end;
        end;

        //DrawTextW(Printer.Canvas.Handle,
        //         PWideChar(ws){PChar(lines[textStart])},-1,r,
        //         DT_SINGLELINE or DT_LEFT or DT_TOP or DT_EXPANDTABS or DT_NOPREFIX);

        inc(textStart);
        inc(y,(lineheight * 6) div lineheightco);
      end;
      if not ep then DoFooter;
    end;

begin
  if Lines.Count>0 then begin
    assert(Assigned(afont),'PrintString: requires a valid aFont parameter!');
    continuePrint := True;
    pagecount     := 0;
    textstart     := 0;

//   Printer.Refresh; //odœwie¿ zainstalowane drukarki
//   Printer.PrinterIndex:=-1; //wybierz domyœln¹ drukarkê
    Printer.PrinterIndex:= PrinterId;
    Printer.Copies:= 1;
    Printer.Title:=Title; //tytu³ dokumentu wyœwietlany w menad¿erze kolejki
    Printer.Orientation:=orientation;
    Printer.BeginDoc;
    try
      CalcPrintRects;
      {$IFNDEF WIN32}
        { Fix for Delphi 1 bug. }
        Printer.Canvas.Font.PixelsPerInch := Y_resolution;
      {$ENDIF }
      Printer.Canvas.Font := aFont;
      Printer.Canvas.Brush.Style:=bsClear; //przeŸroczyste t³o czcionek
      Printer.Canvas.Font.style:=Printer.Canvas.Font.style+[fsBold]; //bold do testu wielkosci bo wy¿sze
      charheight:=printer.canvas.TextHeight('Äy');
      Printer.Canvas.Font.style:=Printer.Canvas.Font.style-[fsBold]; //normal
      charheightco:=10; //10/10=1
      lineheightco:=6; //6/6=1
      doublewidthco:=10; //10/10=1
      sscriptco:=1;
      charstyleco:=[];
      while (textstart<lines.count) and continuePrint do
        PrintPage;
    finally
      if continuePrint and not measureonly then
        Printer.EndDoc
      else
        Printer.Abort;
    end;

    if continuePrint then
      result:=pagecount
    else
      result:=0;
  end else result:=0;
end;

end.