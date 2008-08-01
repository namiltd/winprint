{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit Xml10n;

interface

uses
  Classes, Forms, SysUtils, QRTTI, QXmlRTTI, XmlRL, XmlRS;

type
  TXml10n = class(TComponent)
  private
    FRTTI: TCustomXmlRTTI;
    FResourceLinks: TXml10nResourceLinks;
    FResourceStrings: TXml10nResourceStrings;
    FResourceLinksFile: string;
    FResourceStringsFile: string;
    function GetSymbols(Name: string): string;
    procedure SetSymbols(Name: string; const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure Save;
    procedure ComponentToXml(const Component: TComponent;
      const XmlForm: TXml10nForm);
    procedure XmlToComponent(const Component: TComponent;
      const XmlForm: TXml10nForm);
    procedure FormToXml(const Form: TCustomForm);
    procedure XmlToForm(const Form: TCustomForm);
    property ResourceLinks: TXml10nResourceLinks read FResourceLinks;
    property ResourceStrings: TXml10nResourceStrings read FResourceStrings;
    property Symbols[Name: string]: string read GetSymbols write SetSymbols;
  published
    property ResourceLinksFile: string
      read FResourceLinksFile write FResourceLinksFile;
    property ResourceStringsFile: string
      read FResourceStringsFile write FResourceStringsFile;
  end;

implementation

uses
  Xml10nLnks, Xml10nVcl;

{$R Xml10n.dcr}

function EncodeFormName(const ClassName: string): string;
begin
  if (Length(ClassName) > 1) and SameText(ClassName[1], 'T') then
    Result := Copy(ClassName, 2, MaxInt)
  else
    Result := ClassName;
end;

{ TXml10n }

constructor TXml10n.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRTTI := TMiddleXQuickRTTI.Create;
  FResourceLinks := TXml10nResourceLinks.Create(FRTTI);
  FResourceStrings := TXml10nResourceStrings.Create(FRTTI);
  FResourceLinksFile := '';
  FResourceStringsFile := '';
end;

destructor TXml10n.Destroy;
begin
  FResourceStrings.Free;
  FResourceLinks.Free;
  FRTTI.Free;
  inherited Destroy;
end;

procedure TXml10n.FormToXml(const Form: TCustomForm);
var
  XmlForm: TXml10nForm;
  I: Integer;
begin
  XmlForm := ResourceLinks.Forms.GetForm(EncodeFormName(Form.ClassName));
  ComponentToXml(Form, XmlForm);
  for I := 0 to Pred(Form.ComponentCount) do
    ComponentToXml(Form.Components[I], XmlForm);
end;

procedure TXml10n.XmlToForm(const Form: TCustomForm);
var
  XmlForm: TXml10nForm;
  I: Integer;
begin
  XmlForm := TXml10nForm(ResourceLinks.Forms.Find(
    EncodeFormName(Form.ClassName)));
  if Assigned(XmlForm) then
  begin
    XmlToComponent(Form, XmlForm);
    for I := 0 to Pred(Form.ComponentCount) do
      XmlToComponent(Form.Components[I], XmlForm);
  end;
end;

procedure TXml10n.Clear;
begin
  ResourceLinks.Clear;
  ResourceStrings.Clear;
end;

procedure TXml10n.Load;
begin
  Clear;
  ResourceLinks.LoadFromFile(ResourceLinksFile);
  ResourceStrings.LoadFromFile(ResourceStringsFile);
end;

procedure TXml10n.Save;
begin
  ResourceLinks.SaveToFile(ResourceLinksFile);
  ResourceStrings.SaveToFile(ResourceStringsFile);
end;

procedure TXml10n.ComponentToXml(const Component: TComponent;
  const XmlForm: TXml10nForm);
var
  PropLinks: TXml10nComponentPropLinks;
  XmlComponent: TXml10nComponent;
  XmlProperty: TXml10nProperty;
  I: Integer;
begin
  PropLinks := CreateComponentPropLinks(Component);
  if Assigned(PropLinks) then
  try
    if Component is TCustomForm then
      XmlComponent := XmlForm
    else
      XmlComponent := XmlForm.Components.GetComponent(Component.Name);
    for I := 0 to Pred(PropLinks.Count) do
      with XmlComponent, ResourceStrings do
        if PropLinks[I].IsStored then
        begin
          XmlProperty := Properties.GetProperty(PropLinks[I].Name);
          XmlProperty.SID := Strings.WriteString(PropLinks[I].Value,
            XmlProperty.SID);
        end;
  finally
    PropLinks.Free;
  end;
end;

procedure TXml10n.XmlToComponent(const Component: TComponent;
  const XmlForm: TXml10nForm);
var
  PropLinks: TXml10nComponentPropLinks;
  XmlComponent: TXml10nComponent;
  XmlProperty: TXml10nProperty;
  I: Integer;
begin
  if Component is TCustomForm then
    XmlComponent := XmlForm
  else
    XmlComponent := TXml10nComponent(XmlForm.Components.Find(Component.Name));
  if Assigned(XmlComponent) then
  begin
    PropLinks := CreateComponentPropLinks(Component);
    if Assigned(PropLinks) then
    try
      for I := 0 to Pred(PropLinks.Count) do
        with XmlComponent, ResourceStrings do
        begin
          XmlProperty := TXml10nProperty(Properties.Find(PropLinks[I].Name));
          if Assigned(XmlProperty) then
            PropLinks[I].Value :=
              Strings.ReadString(XmlProperty.SID, PropLinks[I].Value);
        end;
    finally
      PropLinks.Free;
    end;
  end;
end;

function TXml10n.GetSymbols(Name: string): string;
var
  XmlSymbol: TXml10nSymbol;
begin
  Result := '';
  XmlSymbol := TXml10nSymbol(ResourceLinks.Symbols.Find(Name));
  if Assigned(XmlSymbol) then
    Result := ResourceStrings.Strings.ReadString(XmlSymbol.SID, '');
end;

procedure TXml10n.SetSymbols(Name: string; const Value: string);
var
  XmlSymbol: TXml10nSymbol;
begin
  XmlSymbol := ResourceLinks.Symbols.GetSymbol(Name);
  XmlSymbol.SID := ResourceStrings.Strings.WriteString(Value, XmlSymbol.SID);
end;

end.
