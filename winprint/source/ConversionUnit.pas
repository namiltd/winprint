{      Many will recall a series of messages that I posted a few weeks
      ago regarding the Implementation of XLAT in BAsm.

      I have revisited it With the idea of using it not For filtering
      but just For up- and low-casing Pascal Strings. I came With a
      pure Assembler Function With a loop of only 4 instructions (TXlat
      in Unit TXLATU.PAS). The acCompanying Program TXLATE1.PAS shows
      examples on how to use TXlat both For up- or low-casing a String.

      The intriguing finding was that when I bench-marked it against
      other Assembler Upcasing routines posted in this echo or against
      the one in Hax 144 in PC-Techniques (Vol.3, No.6, Feb 1993, p.40)
      TXlat got to be 20-30% faster! if anyone is interested I could
      upload the benchmarking routines.

      So, here is my question: could this possibly be the fastest
      routine For String conversion in Turbo Pascal?

      Please note that XLAT has special requirements respect to the
      location of the source and destination buffers as well as the
      translation table. Turbo Pascal memory model places global
      Variables in the data segment wh    local Variables are located in
      the stack segment. The code in TXlat requires that both the table
      and the source buffer be located in the data segment.

      Another point of interest is that a Pascal String Variabe (Table) is
      used as the 256-Byte long table required by XLAT.

      -Jose- (1:163/513.3)

   ============================================================================}

    Unit ConversionUnit;

   {ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ?}
   {3Unit TXlatU.PAS by Jos‚ Campione, Feb.1993.3}
   {3This Unit implements Function TXlat and    3}
   {3declares Variables in the data segment.    3}
   {AÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄU}

   Interface

   uses
     Windows, Classes;


type
  TCodePage = (cp1250, cp620, cp852, cp28592,
              cp1251, cp20866, cp866, cp28595,
              cp1252, cp437, cp850, cp28591,
              cp65001);


  TCharCode = byte;
  TCharCodes = set of TCharCode;

  TConversionItem = class(TCollectionItem)
  private
    fInCode: TCharCode;
    fOutCode: TCharCode;
    fDescription: string;
    function GetDisplayText: string;
  public
    property DisplayText: string read GetDisplayText;
    procedure Assign(Source: TPersistent); override;
  published
    property InCode: TCharCode read fInCode write fInCode;
    property OutCode: TCharCode read fOutCode write fOutCode;
    property Description: string read fDescription write fDescription;
  end;

  TConversionItems = class(TCollection)
  public
    constructor Create;
    function Add: TConversionItem;
    function Insert(Index: Integer): TConversionItem;
    function GetItem(Index: Integer): TConversionItem;
    procedure SetItem(Index: Integer; Value: TConversionItem);
    property Items[Index: Integer]: TConversionItem read GetItem write SetItem; default;
  end;

type
//  TUTF8Table=array[char] of string;
  TUTF8Table=array[128..255] of string;

const
  CodePageLow = low(TCodePage);
  CodePageHigh = high(TCodePage);


(* Mazovia (Polish) aka CP620*/
 	  	  	* from "Mazowia to Unicode table", 04/24/96, Mikolaj Jedrzejak
 	  	  	static const wchar_t mazovia[] = {
 	  	  	/* Code point 0x9B is "zloty" symbol (z&#0142;), which is not
 	  	  	* widely used and for which there is no Unicode equivalent.
 	  	  	* One reference shows 0xA8 as U+00A7 SECTION SIGN, but we're
 	  	  	* told that's incorrect. */
	  	  	0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x0105, 0x00E7,
 	  	  	0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x0107, 0x00C4, 0x0104,
 	  	  	0x0118, 0x0119, 0x0142, 0x00F4, 0x00F6, 0x0106, 0x00FB, 0x00F9,
 	  	  	0x015a, 0x00D6, 0x00DC, 0xFFFD, 0x0141, 0x00A5, 0x015b, 0x0192,
 	  	  	0x0179, 0x017b, 0x00F3, 0x00d3, 0x0144, 0x0143, 0x017a, 0x017c,
 	  	  	0x00BF, 0x2310, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
 	  	  	0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x2561, 0x2562, 0x2556,
 	  	  	0x2555, 0x2563, 0x2551, 0x2557, 0x255D, 0x255C, 0x255B, 0x2510,
 	  	  	0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x255E, 0x255F,
 	  	  	0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x2567,
 	  	  	0x2568, 0x2564, 0x2565, 0x2559, 0x2558, 0x2552, 0x2553, 0x256B,
 	  	  	0x256A, 0x2518, 0x250C, 0x2588, 0x2584, 0x258C, 0x2590, 0x2580,
 	  	  	0x03B1, 0x00DF, 0x0393, 0x03C0, 0x03A3, 0x03C3, 0x00B5, 0x03C4,
 	  	  	0x03A6, 0x0398, 0x03A9, 0x03B4, 0x221E, 0x03C6, 0x03B5, 0x2229,
 	  	  	0x2261, 0x00B1, 0x2265, 0x2264, 0x2320, 0x2321, 0x00F7, 0x2248,
 	  	  	0x00B0, 0x2219, 0x00B7, 0x221A, 0x207F, 0x00B2, 0x25A0, 0x00A0
 	  	  	};*)

 Mazovia2UTF8: TUTF8Table=(
{   (#$00),(#$01),(#$02),(#$03),(#$04),(#$05),(#$06),(#$07),(#$08),(#$09),(#$0A),(#$0B),(#$0C),(#$0D),(#$0E),(#$0F),
   (#$10),(#$11),(#$12),(#$13),(#$14),(#$15),(#$16),(#$17),(#$18),(#$19),(#$1A),(#$1B),(#$1C),(#$1D),(#$1E),(#$1F),
   (#$20),(#$21),(#$22),(#$23),(#$24),(#$25),(#$26),(#$27),(#$28),(#$29),(#$2A),(#$2B),(#$2C),(#$2D),(#$2E),(#$2F),
   (#$30),(#$31),(#$32),(#$33),(#$34),(#$35),(#$36),(#$37),(#$38),(#$39),(#$3A),(#$3B),(#$3C),(#$3D),(#$3E),(#$3F),
   (#$40),(#$41),(#$42),(#$43),(#$44),(#$45),(#$46),(#$47),(#$48),(#$49),(#$4A),(#$4B),(#$4C),(#$4D),(#$4E),(#$4F),
   (#$50),(#$51),(#$52),(#$53),(#$54),(#$55),(#$56),(#$57),(#$58),(#$59),(#$5A),(#$5B),(#$5C),(#$5D),(#$5E),(#$5F),
   (#$60),(#$61),(#$62),(#$63),(#$64),(#$65),(#$66),(#$67),(#$68),(#$69),(#$6A),(#$6B),(#$6C),(#$6D),(#$6E),(#$6F),
   (#$70),(#$71),(#$72),(#$73),(#$74),(#$75),(#$76),(#$77),(#$78),(#$79),(#$7A),(#$7B),(#$7C),(#$7D),(#$7E),(#$7F),
}   (#$C3+#$87),(#$C3+#$BC),(#$C3+#$A9),(#$C3+#$A2),
   (#$C3+#$A4),(#$C3+#$A0),(#$C4+#$85),(#$C3+#$A7),
   (#$C3+#$AA),(#$C3+#$AB),(#$C3+#$A8),(#$C3+#$AF),
   (#$C3+#$AE),(#$C4+#$87),(#$C3+#$84),(#$C4+#$84),
   (#$C4+#$98),(#$C4+#$99),(#$C5+#$82),(#$C3+#$B4),
   (#$C3+#$B6),(#$C4+#$86),(#$C3+#$BB),(#$C3+#$B9),
   (#$C5+#$9A),(#$C3+#$96),(#$C3+#$9C),(#$EF+#$BF+#$BD),
   (#$C5+#$81),(#$C2+#$A5),(#$C5+#$9B),(#$C6+#$92),
   (#$C5+#$B9),(#$C5+#$BB),(#$C3+#$B3),(#$C3+#$93),
   (#$C5+#$84),(#$C5+#$83),(#$C5+#$BA),(#$C5+#$BC),
   (#$C2+#$BF),(#$E2+#$8C+#$90),(#$C2+#$AC),(#$C2+#$BD),
   (#$C2+#$BC),(#$C2+#$A1),(#$C2+#$AB),(#$C2+#$BB),
   (#$E2+#$96+#$91),(#$E2+#$96+#$92),(#$E2+#$96+#$93),(#$E2+#$94+#$82),
   (#$E2+#$94+#$A4),(#$E2+#$95+#$A1),(#$E2+#$95+#$A2),(#$E2+#$95+#$96),
   (#$E2+#$95+#$95),(#$E2+#$95+#$A3),(#$E2+#$95+#$91),(#$E2+#$95+#$97),
   (#$E2+#$95+#$9D),(#$E2+#$95+#$9C),(#$E2+#$95+#$9B),(#$E2+#$94+#$90),
   (#$E2+#$94+#$94),(#$E2+#$94+#$B4),(#$E2+#$94+#$AC),(#$E2+#$94+#$9C),
   (#$E2+#$94+#$80),(#$E2+#$94+#$BC),(#$E2+#$95+#$9E),(#$E2+#$95+#$9F),
   (#$E2+#$95+#$9A),(#$E2+#$95+#$94),(#$E2+#$95+#$A9),(#$E2+#$95+#$A6),
   (#$E2+#$95+#$A0),(#$E2+#$95+#$90),(#$E2+#$95+#$AC),(#$E2+#$95+#$A7),
   (#$E2+#$95+#$A8),(#$E2+#$95+#$A4),(#$E2+#$95+#$A5),(#$E2+#$95+#$99),
   (#$E2+#$95+#$98),(#$E2+#$95+#$92),(#$E2+#$95+#$93),(#$E2+#$95+#$AB),
   (#$E2+#$95+#$AA),(#$E2+#$94+#$98),(#$E2+#$94+#$8C),(#$E2+#$96+#$88),
   (#$E2+#$96+#$84),(#$E2+#$96+#$8C),(#$E2+#$96+#$90),(#$E2+#$96+#$80),
   (#$CE+#$B1),(#$C3+#$9F),(#$CE+#$93),(#$CF+#$80),
   (#$CE+#$A3),(#$CF+#$83),(#$C2+#$B5),(#$CF+#$84),
   (#$CE+#$A6),(#$CE+#$98),(#$CE+#$A9),(#$CE+#$B4),
   (#$E2+#$88+#$9E),(#$CF+#$86),(#$CE+#$B5),(#$E2+#$88+#$A9),
   (#$E2+#$89+#$A1),(#$C2+#$B1),(#$E2+#$89+#$A5),(#$E2+#$89+#$A4),
   (#$E2+#$8C+#$A0),(#$E2+#$8C+#$A1),(#$C3+#$B7),(#$E2+#$89+#$88),
   (#$C2+#$B0),(#$E2+#$88+#$99),(#$C2+#$B7),(#$E2+#$88+#$9A),
   (#$E2+#$81+#$BF),(#$C2+#$B2),(#$E2+#$96+#$A0),(#$C2+#$A0));


  CodePageInfo: array[CodePageLow..CodePageHigh] of record
         name: string;
         CpNr: integer;
         utf8: ^TUTF8Table;
       end = (
              (name:'WINDOWS EE (CP1250)'; CpNr:1250; utf8:nil),
              (name:'MAZOVIA (CP620)'; CpNr:65001; utf8:@Mazovia2UTF8), //Posredniczaco UTF8
              (name:'IBM LATIN2 (CP852)'; CpNr:852; utf8:nil),
              (name:'ISO LATIN2 (CP8859-2)'; CpNr:28592; utf8:nil),

              (name:'WINDOWS RUSSIAN (CP1251)'; CpNr:1251; utf8:nil),
              (name:'KOI8-R (CP20866)'; CpNr:20866; utf8:nil),
              (name:'IBM RUSSIAN (CP866)'; CpNr:866; utf8:nil),
              (name:'ISO IR-113 (CP8859-5)'; CpNr:28595; utf8:nil),

              (name:'WINDOWS WE (CP1252)'; CpNr:1252; utf8:nil),
              (name:'IBM WE (CP437)'; CpNr:437; utf8:nil),
              (name:'IBM LATIN1 (CP850)'; CpNr:850; utf8:nil),
              (name:'ISO LATIN1 (CP8859-1)'; CpNr:28591; utf8:nil),

              (name:'UTF-8 (CP65001)'; CpNr:65001; utf8:nil)
              );


   Var

  LANG: word = 61000;

(*     Source, Table : String;   {ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ?}
                               {3This Forces these Variables to be  3}
                               {3in the data segment. Both Variables3}
                               {3passed to TXlat must be created in 3}
                               {3this segment.                      3}
                               {AÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄU} *)

(*   Function TXlat(Var Source: String; Var Table: String):String; *)

   procedure ReadANDConvert(CodePage: TCodePage;  //CodePage index
                            FileName: string;     //input file
                              var SL: TStringList;//output strings
            UseCustomConversionTable: boolean;
                     ConversionItems: TConversionItems);

   function RString(ID: WORD):string;


   Implementation

   uses
     SysUtils;

(*
   {ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ?}
   {3This Function translates or filters a String as per the Byte values3}
   {3in the Table buffer. It implements the Assembler XLAT instruction. 3}
   {AÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄU}
   Function TXlat(Var Source: String; Var Table: String):String; Assembler;
   Asm
              push ds           {preserve data segment}
              lds  bx,table[0]  {load ds:bx With table address}
              lds  si,source[0] {load ds:si With source address}
                                {both are in datasegment...}
              les  di,@result[0]{load es:di With result}
              cld               {si will increment}
              lodsb             {load al With length of source}
              stosb             {store al in es:di}
              mov  cx,ax        {assign length of source to counter}
              or   cx,cx        {if counter = 0}
              jz   @end         {jump to end}
     @filter: lodsb             {load Byte in ax}
              xlat              {tans-xlat-e...}
              stosb             {store it in destination Array}
              loop @filter      {loop back}
        @end: pop ds            {restore data segment}
   end;
*)

function RString(ID: WORD):string;
var
  a : array[0..255] of char;
begin
 result:='';
 if ID<1000 then ID:=ID+LANG; //multilingual resources
 if (LoadString(hInstance,ID,@a,sizeof(a)) <> 0)
   then result:=StrPas(a);
end;

{ TConversionItem }

function TConversionItem.GetDisplayText: string;
begin
  result:=IntToStr(fInCode)+' => '+IntToStr(fOutCode)+' ('+fDescription+')';
end;

procedure TConversionItem.Assign(Source: TPersistent);
begin
  if Source is TConversionItem then
  begin
    if assigned(Collection) then Collection.BeginUpdate;
    try
      with TConversionItem(Source) do
      begin
        self.InCode:=InCode;
        self.OutCode:=OutCode;
        self.Description:=Description;
      end;
    finally
      if assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited assign(Source);
end;

{ TConversionItems }

constructor TConversionItems.Create;
begin
  inherited Create(TConversionItem);
end;

function TConversionItems.Add: TConversionItem;
begin
  result:=inherited Add as TConversionItem;
end;

function TConversionItems.Insert(Index: integer): TConversionItem;
begin
  result:=inherited Insert(Index) as TConversionItem;
end;

function TConversionItems.GetItem(Index: Integer): TConversionItem;
begin
  result:=inherited GetItem(Index) as TConversionItem;
end;

procedure TConversionItems.SetItem(Index: Integer; Value: TConversionItem);
begin
  inherited SetItem(Index,Value);
end;


Function GetNextLineMy(SL: Tstrings; Const Value : String; Var S : String; Var P : Integer) : Boolean;

Var
  PS : PChar;
  IP,L : Integer;

begin
  L:=Length(Value);
  S:='';
  Result:=False;
  If ((L-P)<0) then
    exit;
  if ((L-P)=0) and (not (value[P] in [#10,#13])) Then
    Begin
      s:=value[P];
      inc(P);
      Result:=True;
      Exit;
//      Exit(True);
    End;
  PS:=PChar(Value)+P-1;
  IP:=P;
  While ((L-P)>=0) and (not (PS^ in [#10,#13])) do
    begin
    P:=P+1;
    Inc(PS);
    end;
  SetLength (S,P-IP);
  System.Move (Value[IP],Pointer(S)^,P-IP);
  If (P<=L) and (Value[P]=#13) then
    Inc(P);
  If (P<=L) and (Value[P]=#10) then
    Inc(P); // Point to character after #10(#13)
  Result:=True;
end;

Function GetNextLineMy10(SL: Tstrings; Const Value : String; Var S : String; Var P : Integer) : Boolean;

Var
  PS : PChar;
  IP,L : Integer;

begin
  L:=Length(Value);
  S:='';
  Result:=False;
  If ((L-P)<0) then
    exit;
  if ((L-P)=0) and (not (value[P] = #10)) Then
    Begin
      s:=value[P];
      inc(P);
      Result:=True;
      Exit;
//      Exit(True);
    End;
  PS:=PChar(Value)+P-1;
  IP:=P;
  While ((L-P)>=0) and (not (PS^ = #10)) do
    begin
    P:=P+1;
    Inc(PS);
    end;
  SetLength (S,P-IP);
  System.Move (Value[IP],Pointer(S)^,P-IP);
  If (P<=L) and (Value[P]=#10) then
    Inc(P); // Point to character after #10(#13)
  Result:=True;
end;


Procedure SetTextStrMy(SL: Tstrings; const Value: string);

Var
  S : String;
  P : Integer;

begin
  Try
    SL.beginUpdate;
    SL.Clear;
    P:=1;
    While GetNextLineMy10(SL, Value,S,P) do
      SL.Add(S);
  finally
    SL.EndUpdate;
  end;
end;

Procedure LoadFromStreamMy(SL: TStrings; Stream: TStream);
{
   Borlands method is no good, since a pipe for
   instance doesn't have a size.
   So we must do it the hard way.
}
Const
  BufSize = 1024;
  MaxGrow = 1 shl 29;

Var
  Buffer     : AnsiString;
  BytesRead,
  BufLen,
  I,BufDelta     : Longint;
begin
  // reread into a buffer
  try
    SL.beginupdate;
    Buffer:='';
    BufLen:=0;
    I:=1;
    Repeat
      BufDelta:=BufSize*I;
      SetLength(Buffer,BufLen+BufDelta);
      BytesRead:=Stream.Read(Buffer[BufLen+1],BufDelta);
      inc(BufLen,BufDelta);
      If I<MaxGrow then
        I:=I shl 1;
    Until BytesRead<>BufDelta;
    SetLength(Buffer, BufLen-BufDelta+BytesRead);
    SetTextStrMy(SL, Buffer);
    SetLength(Buffer,0);
  finally
    SL.EndUpdate;
  end;
end;


procedure ReadANDConvert;
var
  FS: TFileStream;
  Buffer: string;
  Count,i: integer;
  SS: TStringStream;
  XLATTable: array[char] of char;
//  UTF8Table: TUTF8Table;
  UTF8Table: array[char] of string;

  ci: TConversionItem;
begin
  for i:=0 to 255 do XLATTable[char(i)]:=char(i);
  if (CodePageInfo[CodePage].CpNr=65001)
        and (CodePageInfo[CodePage].UTF8<>nil)then begin
     for i:=0 to 127 do UTF8Table[char(i)]:=char(i);
     for i:=128 to 255 do UTF8Table[char(i)]:=CodePageInfo[CodePage].UTF8^[i];
  end;
  if UseCustomConversionTable then begin
    Count:=ConversionItems.Count;
    if Count>0 then
       for i:=0 to Count-1 do begin
         ci:=ConversionItems.Items[i];
         if ci<>nil then XLATTable[char(ci.Incode)]:=char(ci.Outcode);
       end;
  end;
  FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  try
    SetLength(Buffer,1024);
    SS:=TStringStream.Create('');
    try
      while (FS.Position<FS.Size) do
      begin
        Count:=FS.Read(Buffer[1],1024);
        if (CodePageInfo[CodePage].CpNr=65001)
        and (CodePageInfo[CodePage].UTF8<>nil)then begin
          for i:=1 to Count do
              SS.WriteString(UTF8Table[XLATTable[Buffer[i]]]);
//              SS.WriteString(CodePageInfo[CodePage].UTF8^[XLATTable[Buffer[i]]]);
        end
        else begin
          for i:=1 to Count do
            Buffer[i]:=XLATTable[Buffer[i]];
          SS.WriteString(copy(Buffer,1,Count));
        end;
      end;
    finally
      SS.Seek(0,soFromBeginning);
//      SL.LoadFromStream(SS);
      LoadFromStreamMy(SL,SS); //New function those accepted #0 char
    end;
  finally
    FS.Free;
  end;
end;

end.

