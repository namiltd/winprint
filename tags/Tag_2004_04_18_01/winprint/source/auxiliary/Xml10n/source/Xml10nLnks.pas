{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit Xml10nLnks;

interface

uses
  Classes, TypInfo, SysUtils, Controls;

type
  TXml10nPropertyLink = class
  private
    FInstance: TComponent;
    FName: string;
  protected
    function GetIsStored: Boolean; virtual;
    function GetValue: string; virtual; abstract;
    procedure SetValue(const Value: string); virtual; abstract;
  public
    constructor Create(Instance: TComponent; const PropertyName: string);
    property Instance: TComponent read FInstance;
    property IsStored: Boolean read GetIsStored;
    property Name: string read FName;
    property Value: string read GetValue write SetValue;
  end;

  TXml10nPropertyLinkClass = class of TXml10nPropertyLink;

  TXml10nStringPropertyLink = class(TXml10nPropertyLink)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TXml10nStringListPropertyLink = class(TXml10nPropertyLink)
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TXml10nComponentPropLinks = class
  private
    FInstance: TComponent;
    FLinks: TList;
    function GetCount: Integer;
    function GetLinks(Index: Integer): TXml10nPropertyLink;
  protected
    function CreatePropertyLink(LinkClass: TXml10nPropertyLinkClass;
      const PropertyName: string): TXml10nPropertyLink;
  public
    constructor Create(Instance: TComponent); virtual;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Instance: TComponent read FInstance;
    property Links[Index: Integer]: TXml10nPropertyLink read GetLinks; default;
  end;

  TXml10nComponentPropLinksClass = class of TXml10nComponentPropLinks;

procedure RegisterComponentPropLinks(InstanceClass: TComponentClass;
  LinksClass: TXml10nComponentPropLinksClass);

function FindComponentPropLinks(
  Component: TComponent): TXml10nComponentPropLinksClass;

function CreateComponentPropLinks(
  Component: TComponent): TXml10nComponentPropLinks;

implementation

type
  TPropLinksRegistryItem = class
  public
    InstanceClass: TComponentClass;
    LinksClass: TXml10nComponentPropLinksClass;
  end;

var
  PropLinksRegistry: TList;

procedure RegisterComponentPropLinks(InstanceClass: TComponentClass;
  LinksClass: TXml10nComponentPropLinksClass);
var
  I: Integer;
begin
  for I := 0 to Pred(PropLinksRegistry.Count) do
    if InstanceClass = TPropLinksRegistryItem(
      PropLinksRegistry[I]).InstanceClass then
    begin
      TPropLinksRegistryItem(PropLinksRegistry[I]).LinksClass := LinksClass;
      Exit;
    end;
  I := PropLinksRegistry.Add(TPropLinksRegistryItem.Create);
  TPropLinksRegistryItem(PropLinksRegistry[I]).InstanceClass := InstanceClass;
  TPropLinksRegistryItem(PropLinksRegistry[I]).LinksClass := LinksClass;
end;

function FindComponentPropLinks(
  Component: TComponent): TXml10nComponentPropLinksClass;
var
  I: Integer;
begin
  Result := nil;
  for I := Pred(PropLinksRegistry.Count) downto 0 do
    if Component is TPropLinksRegistryItem(
      PropLinksRegistry[I]).InstanceClass then
    begin
      Result := TPropLinksRegistryItem(PropLinksRegistry[I]).LinksClass;
      Break;
    end;
end;

function CreateComponentPropLinks(
  Component: TComponent): TXml10nComponentPropLinks;
var
  LinksClass: TXml10nComponentPropLinksClass;
begin
  Result := nil;
  LinksClass := FindComponentPropLinks(Component);
  if Assigned(LinksClass) then
    Result := LinksClass.Create(Component);
end;

procedure FinalizePropLinksRegistry;
var
  I: Integer;
begin
  for I := 0 to Pred(PropLinksRegistry.Count) do
    TPropLinksRegistryItem(PropLinksRegistry[I]).Free;
  PropLinksRegistry.Free;
end;

{ TXml10nPropertyLink }

constructor TXml10nPropertyLink.Create(Instance: TComponent;
  const PropertyName: string);
begin
  inherited Create;
  FInstance := Instance;
  FName := PropertyName;
end;

function TXml10nPropertyLink.GetIsStored: Boolean;
begin
  Result := IsStoredProp(FInstance, FName);
end;

{ TXml10nStringPropertyLink }

function TXml10nStringPropertyLink.GetValue: string;
begin
  Result := GetStrProp(Instance, Name);
end;

procedure TXml10nStringPropertyLink.SetValue(const Value: string);
begin
  SetStrProp(Instance, Name, Value);
end;

{ TXml10nStringListPropertyLink }

function TXml10nStringListPropertyLink.GetValue: string;
begin
  Result := (GetObjectProp(Instance, Name) as TStrings).Text;
end;

procedure TXml10nStringListPropertyLink.SetValue(const Value: string);
begin
  (GetObjectProp(Instance, Name) as TStrings).Text := Value;
end;

{ TXml10nComponentPropLinks }

constructor TXml10nComponentPropLinks.Create(Instance: TComponent);
begin
  inherited Create;
  FInstance := Instance;
  FLinks := TList.Create;
end;

function TXml10nComponentPropLinks.CreatePropertyLink(
  LinkClass: TXml10nPropertyLinkClass;
  const PropertyName: string): TXml10nPropertyLink;
begin
  Result := LinkClass.Create(Instance, PropertyName);
  FLinks.Add(Result);
end;

destructor TXml10nComponentPropLinks.Destroy;
var
  I: Integer;
begin
  for I := 0 to Pred(FLinks.Count) do
    TXml10nPropertyLink(FLinks[I]).Free;
  FLinks.Free;
  inherited Destroy;
end;

function TXml10nComponentPropLinks.GetCount: Integer;
begin
  Result := FLinks.Count;
end;

function TXml10nComponentPropLinks.GetLinks(
  Index: Integer): TXml10nPropertyLink;
begin
  Result := TXml10nPropertyLink(FLinks[Index]);
end;

initialization
  PropLinksRegistry := TList.Create;

finalization
  FinalizePropLinksRegistry;

end.
