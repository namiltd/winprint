{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit XmlRS;

interface

uses
  Classes, SysUtils, QRTTI, XmlUtils;

type
  TXml10nString = class(TCollectionItem)
  private
    FSID: Integer;
    FText: TStrings;
    function GetValue: string;
    procedure SetValue(const Value: string);
    procedure SetText(const Value: TStrings);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Value: string read GetValue write SetValue;
  published
    property SID: Integer read FSID write FSID;
    property Text: TStrings read FText write SetText;
  end;

  TXml10nStrings = class(TCollection)
  private
    FSIDGenerator: Integer;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    function Find(const SID: Integer): TXml10nString;
    function ReadString(SID: Integer; const Default: string): string;
    function WriteString(const Value: string; SID: Integer = -1): Integer;
  published
    property SIDGenerator: Integer read FSIDGenerator write FSIDGenerator;
  end;

  TXml10nResourceStrings = class(TXMLAware)
  private
    FStrings: TXml10nStrings;
    procedure SetStrings(const Value: TXml10nStrings);
  public
    constructor Create(RTTIEnabler: TCustomXMLRTTI); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  published
    property Strings: TXml10nStrings read FStrings write SetStrings;
  end;

implementation

{ TXml10nString }

constructor TXml10nString.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSID := -1;
  FText := TStringList.Create;
end;

destructor TXml10nString.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TXml10nString.GetValue: string;
begin
  Result := DecodeXmlString(TrimRight(FText.Text));
end;

procedure TXml10nString.SetValue(const Value: string);
begin
  FText.Text := EncodeXmlString(Value);
end;

procedure TXml10nString.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

{ TXml10nStrings }

constructor TXml10nStrings.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FSIDGenerator := 0;
end;

function TXml10nStrings.Find(const SID: Integer): TXml10nString;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Pred(Count) do
    if SID = TXml10nString(Items[I]).SID then
    begin
      Result := TXml10nString(Items[I]);
      Break;
    end;
end;

function TXml10nStrings.ReadString(SID: Integer;
  const Default: string): string;
var
  S: TXml10nString;
begin
  S := Find(SID);
  if Assigned(S) then
    Result := S.Value
  else
    Result := Default;
end;

function TXml10nStrings.WriteString(const Value: string;
  SID: Integer): Integer;
var
  S: TXml10nString;
begin
  if SID < 0 then
    S := nil
  else
    S := Find(SID);
  if not Assigned(S) then
  begin
    S := TXml10nString(Add);
    if SID > FSIDGenerator then
    begin
      S.SID := SID;
      FSIDGenerator := Succ(SID);
    end
    else
    begin
      S.SID := FSIDGenerator;
      Inc(FSIDGenerator);
    end;
  end;
  S.Value := Value;
  Result := S.SID;
end;

{ TXml10nResourceStrings }

procedure TXml10nResourceStrings.Clear;
begin
  FStrings.Clear;
  FStrings.SIDGenerator := 0;
end;

constructor TXml10nResourceStrings.Create(RTTIEnabler: TCustomXMLRTTI);
begin
  inherited Create(RTTIEnabler);
  FStrings := TXml10nStrings.Create(TXml10nString);
end;

destructor TXml10nResourceStrings.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

procedure TXml10nResourceStrings.LoadFromFile(const FileName: string);
var
  XML: TStrings;
begin
  XML := TStringList.Create;
  try
    XML.LoadFromFile(FileName);
    LoadFromXML('XMLRS', XML.Text);
  finally
    XML.Free;
  end;
end;

procedure TXml10nResourceStrings.SaveToFile(const FileName: string);
var
  XML: TStrings;
begin
  XML := TStringList.Create;
  try
    XML.Text := SaveToXML('XMLRS');
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

procedure TXml10nResourceStrings.SetStrings(const Value: TXml10nStrings);
begin
  FStrings.Assign(Value);
end;

initialization
  RegisterClass(TXml10nString);
  RegisterClass(TXml10nStrings);

end.
