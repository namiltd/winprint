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
     Classes;


type
  TCodePage = (cpWIN, cpMAZ, cpLAT, cpISO);
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

const
  CodePageLow = low(TCodePage);
  CodePageHigh = high(TCodePage);

  CodePageNames: array[CodePageLow..CodePageHigh] of string =
        ('WINDOWS (CP1250)',
         'MAZOVIA (CP620)',
         'LATIN2 (CP852)',
         'ISO (CP8859-2)');


   Var
     Source, Table : String;   {ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ?}
                               {3This Forces these Variables to be  3}
                               {3in the data segment. Both Variables3}
                               {3passed to TXlat must be created in 3}
                               {3this segment.                      3}
                               {AÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄU}

(*   Function TXlat(Var Source: String; Var Table: String):String; *)

   procedure ReadANDConvert(CodePage: TCodePage;
                            FileName: string; 
                              var SL: TStringList;
            UseCustomConversionTable: boolean;  
                     ConversionItems: TConversionItems);

   Implementation

   uses
     SysUtils;


const
  PLConvTable : array [CodePageLow..CodePageHigh,1..18] of byte =
  (
       { ¥   Æ   Ê   £   Ñ   Ó   Œ      ¯   ¹   æ   ê   ³   ñ   ó   œ   Ÿ   ¿ }
{ WIN }(165,198,202,163,209,211,140,143,175,185,230,234,179,241,243,156,159,191),
{ MAZ }(143,149,144,156,165,163,152,160,161,134,141,145,146,164,162,158,166,167),
{ LAT }(164,143,168,157,227,224,151,141,189,165,134,169,136,228,162,152,171,190),
{ ISO }(161,198,202,163,209,211,166,172,175,177,230,234,179,241,243,182,188,191)
   );




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


procedure ReadANDConvert;
var
  FS: TFileStream;
  Buffer: string;
  Count,Prev,i: integer;
  SS: TStringStream;
  XLATTable: array[char] of char;
  ci: TConversionItem;
begin
  for i:=0 to 255 do XLATTable[char(i)]:=char(i);
  if CodePage<>cpLAT then 
        for i:=1 to 18 do XLATTable[char(PLConvTable[CodePage,i])]:=char(PLConvTable[cpLAT,i]);
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
        Prev:=0;
        for i:=1 to Count do
        begin
          Buffer[i]:=XLATTable[Buffer[i]];
          if (Buffer[i]=#0) then
          begin
            inc(Prev);
            if (i>Prev) then SS.WriteString(copy(Buffer,Prev,i-Prev));
            Prev:=i;
          end;
        end;
        if (Prev<Count) then SS.WriteString(copy(Buffer,Prev+1,Count-Prev));
      end;
    finally
      SS.Seek(0,soFromBeginning);
      SL.LoadFromStream(SS);
    end;
  finally
    FS.Free;
  end;
end;

end.

