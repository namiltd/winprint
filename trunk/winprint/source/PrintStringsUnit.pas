unit PrintStringsUnit;

interface

uses
  Windows, Classes, Graphics, Printers;

const
  cMILTOINCH = 0.03937;

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
                      const leftmargin,rightmargin,topmargin,bottommargin: single;
                      const orientation: TPrinterOrientation;
                      const linesPerInch: single;
                      const LinesPerPage: integer;
                      aFont: TFont;
                      measureonly: boolean;
                      OnPrintheader,OnPrintfooter: THeaderFooterProc): integer;


implementation

uses
  Math;

function PrintStrings(Title: string;
                      lines: TStrings;
                      const leftmargin,rightmargin,topmargin,bottommargin: single;
                      const orientation: TPrinterOrientation;
                      const linesPerInch: single;
                      const LinesPerPage: integer;
                      aFont: TFont;
                      measureonly: boolean;
                      OnPrintheader,OnPrintfooter: THeaderFooterProc): integer;
var
  continuePrint: Boolean;     { continue/abort flag for callbacks }
  pagecount    : Integer;     { number of current page }
  textrect     : TRect;       { output area, in canvas coordinates }
  headerrect   : TRect;       { area for header, in canvas coordinates }
  footerrect   : TRect;       { area for footes, in canvas coordinates }
  lineheight   : Integer;     { line spacing in dots }
  charheight   : Integer;     { font height in dots  }
  textstart    : Integer;     { index of first line to print on
                                current page, 0-based. }

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
          raise EPrinter.Create(
            'PrintString: podane marginesy s¹ za du¿e, brak miejsca by wydrukowaæ stronê');
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
        raise EPrinter.Create(
          'PrintString: iloœæ linii na cal i iloœæ linii na stronê nie mog¹ byæ obie równe zero.');
  end;

  { Print a page with headers and footers. }
  procedure PrintPage;

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

    procedure DoPage;
    var
      y: Integer;
      r: TRect;
      do_break: boolean;
      len: integer;
      ws: WideString;

      function Filter: boolean;
      var
        ind1,ind2,index: integer;
      begin
        result:=false;
        ind1:=pos(char(12),lines[textStart]);
        ind2:=pos(char(26),lines[textStart]);
        index:=min(ind1,ind2); //char(12) = FF,char(26) = EOF ??
        if (index=0) then index:=max(ind1,ind2);
        if (index>1) then
        begin
          lines.Insert(textStart+1,copy(lines[textStart],index,length(lines[textStart])-index+1));
          lines[textStart]:=copy(lines[textStart],0,index-1);
        end
        else
          if (index=1) then
          begin
            while (index<length(lines[textStart])) and (lines[textStart][index] in [char(12),char(26)]) do
              inc(index);
            if (length(lines[textStart])>index) then
              lines[textStart]:=copy(lines[textStart],index,length(lines[textStart])-index+1)
            else
              inc(textStart);
            result:=true;
          end;
      end;

    begin
      y:=textrect.top;
      while (textStart<lines.count) and Filter do;
      while (textStart<lines.count) and (y<=(textrect.bottom-charheight)) do
      begin
        do_break:=false;

        while (textStart<lines.count) and Filter do
          do_break:=true;

        if do_break then break;

        //oryginalnie
        //printer.canvas.TextOut( textrect.left, y, lines[textStart]);

        r:=Rect(textrect.left,y,textrect.right,y+charheight);

        len:=MultiByteToWideChar(852,0,PChar(lines[textStart]),-1,nil,0);
        if (len>0) then
        begin
          SetLength(ws,len-1);
          MultiByteToWideChar(852,0,PChar(lines[textStart]),-1,PWideChar(ws),len-1);
          ExtTextOutW(Printer.Canvas.Handle,
                      r.Left,r.Top,
                      ETO_CLIPPED,
                      @r,
                      PWideChar(ws),
                      len-1,
                      nil);
        end;

        //DrawTextW(Printer.Canvas.Handle,
        //         PWideChar(ws){PChar(lines[textStart])},-1,r,
        //         DT_SINGLELINE or DT_LEFT or DT_TOP or DT_EXPANDTABS or DT_NOPREFIX);

        inc(textStart);
        inc(y,lineheight);
      end;
    end;

  begin
    DoHeader;
    if ContinuePrint then
    begin
      DoPage;
      DoFooter;
      if (textStart<lines.count) and ContinuePrint then
      begin
        Inc(pagecount);
        Printer.NewPage;
      end;
    end;
  end;

begin
  assert(Assigned(afont),'PrintString: requires a valid aFont parameter!');
  continuePrint := True;
  pagecount     := 1;
  textstart     := 0;
  Printer.Refresh; //odœwie¿ zainstalowane drukarki
  Printer.PrinterIndex:=-1; //wybierz domyœln¹ drukarkê
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
    charheight:=printer.canvas.TextHeight('Äy');
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
end;

{
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage( Format( '%d pages printed',
               [ PrintStrings(
                   memo1.lines,
                   0.75, 0.5, 0.75, 1,
                   6,
                   memo1.font,
                   false,
                   PrintHeader, PrintFooter )
                ] ));
end;

procedure TForm1.PrintFooter(aCanvas: TCanvas; aPageCount: Integer;
  aTextrect: TRect; var continue: Boolean);
var
  S: String;
  res: Integer;
begin
  with aCanvas do begin
    // Draw a gray line one point wide below the text
    res := GetDeviceCaps( handle, LOGPIXELSY );
    pen.Style := psSolid;
    pen.Color := clGray;
    pen.Width := Round( res / 72 );
    MoveTo( aTextRect.Left, aTextRect.Top );
    LineTo( aTextRect.Right, aTextRect.Top );
    // Print the page number in Arial 8pt, gray, on right side of
    //  footer rect.
    S:= Format('Page %d', [aPageCount] );
    font.name := 'Arial';
    font.Size := 8;
    font.Color := clGray;
    TextOut( aTextRect.Right - TextWidth(S), aTextRect.Top + res div 18,
             S );
  end;
end;

procedure TForm1.PrintHeader(aCanvas: TCanvas; aPageCount: Integer;
  aTextrect: TRect; var continue: Boolean);
var
  res: Integer;
begin
  with aCanvas do begin
    // Draw a gray line one point wide 4 points above the text
    res := GetDeviceCaps( handle, LOGPIXELSY );
    pen.Style := psSolid;
    pen.Color := clGray;
    pen.Width := Round( res / 72 );
    MoveTo( aTextRect.Left, aTextRect.Bottom - res div 18);
    LineTo( aTextRect.Right, aTextRect.Bottom - res div 18 );
    // Print the company name in Arial 8pt, gray, on left side of
    //  footer rect.
    font.name := 'Arial';
    font.Size := 8;
    font.Color := clGray;
    TextOut( aTextRect.Left, aTextRect.Bottom - res div 10 - TextHeight('W'),
             'W. W. Shyster & Cie.' );
  end;
end;}

end.