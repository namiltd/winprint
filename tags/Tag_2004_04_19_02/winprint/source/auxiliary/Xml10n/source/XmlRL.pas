{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit XmlRL;

interface

uses
  Classes, SysUtils, QRTTI, XmlUtils;

type
  TXml10nCollectionItem = class(TCollectionItem)
  private
    FItemName: string;
    function GetName: string;
    procedure SetName(const Value: string);
    procedure SetItemName(const Value: string);
  protected
    procedure ItemAlreadyExistsError(const Name: string);
  public
    constructor Create(Collection: TCollection); override;
    property ItemName: string read FItemName write SetItemName;
  published
    property Name: string read GetName write SetName;
  end;

  TXml10nCollection = class(TCollection)
  public
    function Find(const Name: string): TXml10nCollectionItem;
  end;

  TXml10nSymbol = class(TXml10nCollectionItem)
  private
    FSID: Integer;
  public
    constructor Create(Collection: TCollection); override;
  published
    property SID: Integer read FSID write FSID;
  end;

  TXml10nSymbols = class(TXml10nCollection)
  public
    function GetSymbol(const Name: string): TXml10nSymbol;
  end;

  TXml10nProperty = class(TXml10nSymbol);

  TXml10nProperties = class(TXml10nCollection)
  public
    function GetProperty(const Name: string): TXml10nProperty;
  end;

  TXml10nComponent = class(TXml10nCollectionItem)
  private
    FProperties: TXml10nProperties;
    procedure SetProperties(const Value: TXml10nProperties);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Properties: TXml10nProperties read FProperties write SetProperties;
  end;

  TXml10nComponents = class(TXml10nCollection)
  public
    function GetComponent(const Name: string): TXml10nComponent;
  end;

  TXml10nForm = class(TXml10nComponent)
  private
    FComponents: TXml10nComponents;
    procedure SetComponents(const Value: TXml10nComponents);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Components: TXml10nComponents read FComponents write SetComponents;
  end;

  TXml10nForms = class(TXml10nCollection)
  public
    function GetForm(const Name: string): TXml10nForm;
  end;

  TXml10nResourceLinks = class(TXMLAware)
  private
    FForms: TXml10nForms;
    FSymbols: TXml10nSymbols;
    procedure SetForms(const Value: TXml10nForms);
    procedure SetSymbols(const Value: TXml10nSymbols);
  public
    constructor Create(RTTIEnabler: TCustomXMLRTTI); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  published
    property Forms: TXml10nForms read FForms write SetForms;
    property Symbols: TXml10nSymbols read FSymbols write SetSymbols;
  end;

implementation

{ TXml10nCollectionItem }

procedure TXml10nCollectionItem.ItemAlreadyExistsError(const Name: string);
begin
  raise EL10nXmlError.CreateFmt('An item called %s already exists.', [Name]);
end;

constructor TXml10nCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FItemName := '';
end;

function TXml10nCollectionItem.GetName: string;
begin
  Result := EncodeXmlString(FItemName);
end;

procedure TXml10nCollectionItem.SetName(const Value: string);
begin
  ItemName := DecodeXmlString(Value);
end;

procedure TXml10nCollectionItem.SetItemName(const Value: string);
begin
  if FItemName <> Value then
  begin
    if (Value <> '')
      and Assigned(TXml10nCollection(Collection).Find(Value)) then
      ItemAlreadyExistsError(Value);
    FItemName := Value;
  end;
end;

{ TXml10nCollection }

function TXml10nCollection.Find(const Name: string): TXml10nCollectionItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Pred(Count) do
    if SameText(Name, TXml10nCollectionItem(Items[I]).ItemName) then
    begin
      Result := TXml10nCollectionItem(Items[I]);
      Break;
    end;
end;

{ TXml10nSymbol }

constructor TXml10nSymbol.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSID := -1;
end;

{ TXml10nSymbols }

function TXml10nSymbols.GetSymbol(const Name: string): TXml10nSymbol;
begin
  Result := TXml10nSymbol(Find(Name));
  if not Assigned(Result) then
  begin
    Result := TXml10nSymbol(Add);
    try
      Result.ItemName := Name;
    except
      Result.Free;
      raise;
    end;
  end;
end;

{ TXml10nProperties }

function TXml10nProperties.GetProperty(const Name: string): TXml10nProperty;
begin
  Result := TXml10nProperty(Find(Name));
  if not Assigned(Result) then
  begin
    Result := TXml10nProperty(Add);
    try
      Result.ItemName := Name;
    except
      Result.Free;
      raise;
    end;
  end;
end;

{ TXml10nComponent }

constructor TXml10nComponent.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FProperties := TXml10nProperties.Create(TXml10nProperty);
end;

destructor TXml10nComponent.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

procedure TXml10nComponent.SetProperties(const Value: TXml10nProperties);
begin
  FProperties.Assign(Value);
end;

{ TXml10nComponents }

function TXml10nComponents.GetComponent(const Name: string): TXml10nComponent;
begin
  Result := TXml10nComponent(Find(Name));
  if not Assigned(Result) then
  begin
    Result := TXml10nComponent(Add);
    try
      Result.ItemName := Name;
    except
      Result.Free;
      raise;
    end;
  end;
end;

{ TXml10nForm }

constructor TXml10nForm.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FComponents := TXml10nComponents.Create(TXml10nComponent);
end;

destructor TXml10nForm.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

procedure TXml10nForm.SetComponents(const Value: TXml10nComponents);
begin
  FComponents.Assign(Value);
end;

{ TXml10nForms }

function TXml10nForms.GetForm(const Name: string): TXml10nForm;
begin
  Result := TXml10nForm(Find(Name));
  if not Assigned(Result) then
  begin
    Result := TXml10nForm(Add);
    try
      Result.ItemName := Name;
    except
      Result.Free;
      raise;
    end;
  end;
end;

{ TXml10nResourceLinks }

procedure TXml10nResourceLinks.Clear;
begin
  FForms.Clear;
  FSymbols.Clear;
end;

constructor TXml10nResourceLinks.Create(RTTIEnabler: TCustomXMLRTTI);
begin
  inherited Create(RTTIEnabler);
  FForms := TXml10nForms.Create(TXml10nForm);
  FSymbols := TXml10nSymbols.Create(TXml10nSymbol);
end;

destructor TXml10nResourceLinks.Destroy;
begin
  FForms.Free;
  FSymbols.Free;
  inherited Destroy;
end;

procedure TXml10nResourceLinks.LoadFromFile(const FileName: string);
var
  XML: TStrings;
begin
  XML := TStringList.Create;
  try
    XML.LoadFromFile(FileName);
    LoadFromXML('XMLRL', XML.Text);
  finally
    XML.Free;
  end;
end;

procedure TXml10nResourceLinks.SaveToFile(const FileName: string);
var
  XML: TStrings;
begin
  XML := TStringList.Create;
  try
    XML.Text := SaveToXML('XMLRL');
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

procedure TXml10nResourceLinks.SetForms(const Value: TXml10nForms);
begin
  FForms.Assign(Value);
end;

procedure TXml10nResourceLinks.SetSymbols(const Value: TXml10nSymbols);
begin
  FSymbols.Assign(Value);
end;

initialization
  RegisterClass(TXml10nSymbol);
  RegisterClass(TXml10nSymbols);
  RegisterClass(TXml10nProperty);
  RegisterClass(TXml10nProperties);
  RegisterClass(TXml10nComponent);
  RegisterClass(TXml10nComponents);
  RegisterClass(TXml10nForm);
  RegisterClass(TXml10nForms);

end.
