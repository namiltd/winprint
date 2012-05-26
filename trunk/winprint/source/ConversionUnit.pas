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
  TCodePage = (cp437,cp620,cp708,cp720,cp737,cp775,cp790,cp850,cp852,cp855,cp857,cp862,cp865,cp866,cp869,
               cp874,cp932,cp936,cp949,cp950,
               cp1250,cp1251,cp1252,cp1253,cp1254,cp1255,cp1256,cp1257,cp1258,
               cp20866,cp21866,
               cp28591,cp28592,cp28593,cp28594,cp28595,cp28596,cp28597,cp28598,cp28599,
               cp65000,cp65001);

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
  TUTF8Table=array[128..255] of string;

const
  CodePageLow = low(TCodePage);
  CodePageHigh = high(TCodePage);

 CP620toUTF8: TUTF8Table=(
   (#$C3+#$87),(#$C3+#$BC),(#$C3+#$A9),(#$C3+#$A2),
   (#$C3+#$A4),(#$C3+#$A0),(#$C4+#$85),(#$C3+#$A7),
   (#$C3+#$AA),(#$C3+#$AB),(#$C3+#$A8),(#$C3+#$AF),
   (#$C3+#$AE),(#$C4+#$87),(#$C3+#$84),(#$C4+#$84),
   (#$C4+#$98),(#$C4+#$99),(#$C5+#$82),(#$C3+#$B4),
   (#$C3+#$B6),(#$C4+#$86),(#$C3+#$BB),(#$C3+#$B9),
   (#$C5+#$9A),(#$C3+#$96),(#$C3+#$9C),(#$EF+#$BF+#$BD), // Code point 0x9B is "zloty" symbol (z&#0142;), which is not widely used and for which there is no Unicode equivalent.
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

 CP790toUTF8: TUTF8Table=(
   (#$C3+#$87),(#$C3+#$BC),(#$C3+#$A9),(#$C3+#$A2),
   (#$C3+#$A4),(#$C3+#$A0),(#$C4+#$85),(#$C3+#$A7),
   (#$C3+#$AA),(#$C3+#$AB),(#$C3+#$A8),(#$C3+#$AF),
   (#$C3+#$AE),(#$C4+#$87),(#$C3+#$84),(#$C4+#$84),
   (#$C4+#$98),(#$C4+#$99),(#$C5+#$82),(#$C3+#$B4),
   (#$C3+#$B6),(#$C4+#$86),(#$C3+#$BB),(#$C3+#$B9),
   (#$C5+#$9A),(#$C3+#$96),(#$C3+#$9C),(#$C2+#$A2), // Code point 0x9B is "cent"
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

 CP865toUTF8: TUTF8Table=(
   (#$C3+#$87),(#$C3+#$BC),(#$C3+#$A9),(#$C3+#$A2),
   (#$C3+#$A4),(#$C3+#$A0),(#$C3+#$A5),(#$C3+#$A7),
   (#$C3+#$AA),(#$C3+#$AB),(#$C3+#$A8),(#$C3+#$AF),
   (#$C3+#$AE),(#$C3+#$AC),(#$C3+#$84),(#$C3+#$85),
   (#$C3+#$89),(#$C3+#$A6),(#$C3+#$86),(#$C3+#$B4),
   (#$C3+#$B6),(#$C3+#$B2),(#$C3+#$BB),(#$C3+#$B9),
   (#$C3+#$BF),(#$C3+#$96),(#$C3+#$9C),(#$C3+#$B8),
   (#$C2+#$A3),(#$C3+#$98),(#$E2+#$82+#$A7),(#$C6+#$92),
   (#$C3+#$A1),(#$C3+#$AD),(#$C3+#$B3),(#$C3+#$BA),
   (#$C3+#$B1),(#$C3+#$91),(#$C2+#$AA),(#$C2+#$BA),
   (#$C2+#$BF),(#$E2+#$8C+#$90),(#$C2+#$AC),(#$C2+#$BD),
   (#$C2+#$BC),(#$C2+#$A1),(#$C2+#$AB),(#$C2+#$A4),
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
              (name:'MS-DOS Latin US (CP-437)'; CpNr:437; utf8:nil),
              (name:'Mazovia (CP-620)'; CpNr:65001; utf8:@CP620toUTF8), //Posredniczaco UTF8
              (name:'MS-DOS Arabic ASMO (CP-708)'; CpNr:708; utf8:nil),
              (name:'MS-DOS Arabic (CP-720)'; CpNr:720; utf8:nil),
              (name:'MS-DOS Greek (CP-737)'; CpNr:737; utf8:nil),
              (name:'MS-DOS Baltic Rim (CP-775)'; CpNr:775; utf8:nil),
              (name:'FreeDOS Mazovia (CP-790)'; CpNr:65001; utf8:@CP790toUTF8), //Posredniczaco UTF8
              (name:'MS-DOS Latin 1 (CP-850)'; CpNr:850; utf8:nil),
              (name:'MS-DOS Latin 2 (CP-852)'; CpNr:852; utf8:nil),
              (name:'MS-DOS Cyrillic (CP-855)'; CpNr:855; utf8:nil),
              (name:'MS-DOS Turkish (CP-857)'; CpNr:857; utf8:nil),
              (name:'MS-DOS Hebrew (CP-862)'; CpNr:862; utf8:nil),
              (name:'MS-DOS Nordic (CP-865)'; CpNr:65001; utf8:@CP865toUTF8), //Posredniczaco UTF8, bo brak NLS dla Win95 i win98
              (name:'MS-DOS Cyrillic CIS 1 (CP-866)'; CpNr:866; utf8:nil),
              (name:'MS-DOS Modern Greek (CP-869)'; CpNr:869; utf8:nil),

              (name:'Thai (Win-874)'; CpNr:874; utf8:nil),
              (name:'Japanese (Win-932)'; CpNr:932; utf8:nil),
              (name:'Simplified Chinese (Win-936)'; CpNr:936; utf8:nil),
              (name:'Korean (Win-949)'; CpNr:949; utf8:nil),
              (name:'Traditional Chinese (Win-950)'; CpNr:950; utf8:nil),

              (name:'Central European (Win-1250)'; CpNr:1250; utf8:nil),
              (name:'Cyrillic (Win-1251)'; CpNr:1251; utf8:nil),
              (name:'Western European (Win-1252)'; CpNr:1252; utf8:nil),
              (name:'Greek (Win-1253)'; CpNr:1253; utf8:nil),
              (name:'Turkish (Win-1254)'; CpNr:1254; utf8:nil),
              (name:'Hebrew (Win-1255)'; CpNr:1255; utf8:nil),
              (name:'Arabic (Win-1256)'; CpNr:1256; utf8:nil),
              (name:'Baltic (Win-1257)'; CpNr:1257; utf8:nil),
              (name:'Vietnamese (Win-1258)'; CpNr:1258; utf8:nil),

              (name:'Russian KOI8-R (Win-20866)'; CpNr:20866; utf8:nil),
              (name:'Ukrainian KOI8-U (Win-21866)'; CpNr:21866; utf8:nil),

              (name:'Latin 1 (ISO-8859-1)'; CpNr:28591; utf8:nil),
              (name:'Latin 2 (ISO-8859-2)'; CpNr:28592; utf8:nil),
              (name:'Latin 3 (ISO-8859-3)'; CpNr:28593; utf8:nil),
              (name:'Baltic (ISO-8859-4)'; CpNr:28594; utf8:nil),
              (name:'Cyrillic KOI8-E (ISO-8859-5)'; CpNr:28595; utf8:nil),
              (name:'Arabic (ISO-8859-6)'; CpNr:28596; utf8:nil),
              (name:'Greek (ISO-8859-7)'; CpNr:28597; utf8:nil),
              (name:'Hebrew (ISO-8859-8)'; CpNr:28598; utf8:nil),
              (name:'Turkish (ISO-8859-9)'; CpNr:28599; utf8:nil),
              (name:'Unicode (UTF-7)'; CpNr:65000; utf8:nil),
              (name:'Unicode (UTF-8)'; CpNr:65001; utf8:nil)
              );


   Var

   	LANG: Word = 61000; //English

   	hUniLib: HMODULE = 0;

   	MultiByteToWideCharMy: Function(CodePage: UINT; dwFlags: DWORD; const lpMultiByteStr: LPCSTR; cchMultiByte: Integer; lpWideCharStr: LPWSTR; cchWideChar: Integer): Integer; stdcall;


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

   function LoadUnicode : Boolean;


   Implementation

   uses
     SysUtils;


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
    End;
  IP:=P;
  While ((L-P)>=0) and (value[P]<>#10) do Inc(P);
  SetLength (S,P-IP);
  System.Move (Value[IP],Pointer(S)^,P-IP);
  If (P<=L) and (Value[P]=#10) then
    Inc(P); // Point to character after #10
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
    While GetNextLineMy(SL, Value,S,P) do
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

function LoadUnicode : Boolean;
var
  osVerInfo: TOSVersionInfo;
begin
    osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    if GetVersionEx(osVerInfo)
      and (
               ((osVerInfo.dwPlatformId=VER_PLATFORM_WIN32_NT) and (osVerInfo.dwMajorVersion>=4)) //WIN NT4 or above
            or ((osVerInfo.dwPlatformId=VER_PLATFORM_WIN32_WINDOWS)
                   and ( ((osVerInfo.dwMajorVersion=4) and (osVerInfo.dwMinorVersion>0)) or (osVerInfo.dwMajorVersion>4) )) //WIN 98 or above
           )
    then begin
        @MultiByteToWideCharMy := @MultiByteToWideChar;
        result := true
    end
    else begin //Win 95 and below
        If hUniLib = 0 Then hUniLib := GetModuleHandle('Unicows.dll');
        If hUniLib = 0 Then hUniLib := LoadLibrary('Unicows.dll');
//        If hUniLib = 0 Then hUniLib := LoadLibrary('Kernel32.dll');

        If hUniLib <> 0 Then
            @MultiByteToWideCharMy := GetProcAddress(hUniLib, 'MultiByteToWideChar')
        else
            @MultiByteToWideCharMy := NIL;
        result:=(hUniLib<>0) and (@MultiByteToWideChar<>NIL)
    end;
end;

end.

