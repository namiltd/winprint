{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit Xml10nVcl;

interface

uses
  Classes, Xml10nLnks;

type
  TXml10nCustomFormLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomActionLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nMenuItemLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomLabelLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomEditLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nButtonLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomCheckBoxLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nRadioButtonLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomListBoxLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomComboBoxLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomGroupBoxLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomRadioGroupLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nCustomPanelLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

  TXml10nListColumnsPropertyLink = class(TXml10nPropertyLink)
  protected
    function GetIsStored: Boolean; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TXml10nListViewLinks = class(TXml10nComponentPropLinks)
  public
    constructor Create(Instance: TComponent); override;
  end;

implementation

uses
  TypInfo, Controls, Forms, ActnList, Menus, StdCtrls, ExtCtrls, ComCtrls;

{ TXml10nCustomFormLinks }

constructor TXml10nCustomFormLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nCustomActionLinks }

constructor TXml10nCustomActionLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nMenuItemLinks }

constructor TXml10nMenuItemLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nCustomLabelLinks }

constructor TXml10nCustomLabelLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nCustomEditLinks }

constructor TXml10nCustomEditLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nButtonLinks }

constructor TXml10nButtonLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nCustomCheckBoxLinks }

constructor TXml10nCustomCheckBoxLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nRadioButtonLinks }

constructor TXml10nRadioButtonLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nCustomListBoxLinks }

constructor TXml10nCustomListBoxLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
  CreatePropertyLink(TXml10nStringListPropertyLink, 'Items');
end;

{ TXml10nCustomComboBoxLinks }

constructor TXml10nCustomComboBoxLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
  CreatePropertyLink(TXml10nStringListPropertyLink, 'Items');
end;

{ TXml10nCustomGroupBoxLinks }

constructor TXml10nCustomGroupBoxLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nCustomRadioGroupLinks }

constructor TXml10nCustomRadioGroupLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
  CreatePropertyLink(TXml10nStringListPropertyLink, 'Items');
end;

{ TXml10nCustomPanelLinks }

constructor TXml10nCustomPanelLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nStringPropertyLink, 'Caption');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

{ TXml10nListColumnsPropertyLink }

function TXml10nListColumnsPropertyLink.GetIsStored: Boolean;
begin
  Result := True;
end;

function TXml10nListColumnsPropertyLink.GetValue: string;
var
  Columns: TListColumns;
  Captions: TStrings;
  I: Integer;
begin
  Columns := GetObjectProp(Instance, Name) as TListColumns;
  Captions := TStringList.Create;
  try
    for I := 0 to Pred(Columns.Count) do
      Captions.Add(TListColumn(Columns.Items[I]).Caption);
    Result := Captions.Text;
  finally
    Captions.Free;
  end;
end;

procedure TXml10nListColumnsPropertyLink.SetValue(const Value: string);
var
  Columns: TListColumns;
  Captions: TStrings;
  I: Integer;
begin
  Columns := GetObjectProp(Instance, Name) as TListColumns;
  Captions := TStringList.Create;
  try
    Captions.Text := Value;
    for I := 0 to Pred(Columns.Count) do
      if I < Captions.Count then
        TListColumn(Columns.Items[I]).Caption := Captions[I]
      else
        Break;
  finally
    Captions.Free;
  end;
end;

{ TXml10nListViewLinks }

constructor TXml10nListViewLinks.Create(Instance: TComponent);
begin
  inherited Create(Instance);
  CreatePropertyLink(TXml10nListColumnsPropertyLink, 'Columns');
  CreatePropertyLink(TXml10nStringPropertyLink, 'Hint');
end;

initialization
  RegisterComponentPropLinks(TCustomForm, TXml10nCustomFormLinks);
  RegisterComponentPropLinks(TCustomAction, TXml10nCustomActionLinks);
  RegisterComponentPropLinks(TMenuItem, TXml10nMenuItemLinks);
  RegisterComponentPropLinks(TCustomLabel, TXml10nCustomLabelLinks);
  RegisterComponentPropLinks(TCustomEdit, TXml10nCustomEditLinks);
  RegisterComponentPropLinks(TButton,  TXml10nButtonLinks);
  RegisterComponentPropLinks(TCustomCheckBox, TXml10nCustomCheckBoxLinks);
  RegisterComponentPropLinks(TRadioButton, TXml10nRadioButtonLinks);
  RegisterComponentPropLinks(TCustomListBox, TXml10nCustomListBoxLinks);
  RegisterComponentPropLinks(TCustomComboBox, TXml10nCustomComboBoxLinks);
  RegisterComponentPropLinks(TCustomGroupBox, TXml10nCustomGroupBoxLinks);
  RegisterComponentPropLinks(TCustomRadioGroup, TXml10nCustomRadioGroupLinks);
  RegisterComponentPropLinks(TCustomPanel, TXml10nCustomPanelLinks);
  RegisterComponentPropLinks(TListView, TXml10nListViewLinks);

end.
