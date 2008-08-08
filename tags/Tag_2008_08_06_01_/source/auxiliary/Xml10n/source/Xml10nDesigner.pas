{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit Xml10nDesigner;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, ExtCtrls, StdCtrls, ActnList, ToolWin, Menus,
  DesignEditors, Xml10n, XmlRL, Xml10nLnks;

type
  TXml10nDesignerForm = class(TForm)
    ActionList: TActionList;
    Clear: TAction;
    Load: TAction;
    Save: TAction;
    Post: TAction;
    Cancel: TAction;
    AddForm: TAction;
    AddComponent: TAction;
    AddSymbol: TAction;
    Delete: TAction;
    Rename: TAction;
    ToolBar: TToolBar;
    ToolBarImages: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    PageControl: TPageControl;
    FormsTab: TTabSheet;
    SymbolsTab: TTabSheet;
    FormsExplorer: TTreeView;
    SymbolsExplorer: TTreeView;
    ExplorerImages: TImageList;
    VSplitter: TSplitter;
    Editor: TMemo;
    PopupMenu: TPopupMenu;
    Addform1: TMenuItem;
    Addcomponent1: TMenuItem;
    Addsymbol1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    Rename1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ExplorerChange(Sender: TObject; Node: TTreeNode);
    procedure ExplorerEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ExplorerEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure ClearExecute(Sender: TObject);
    procedure LoadExecute(Sender: TObject);
    procedure SaveExecute(Sender: TObject);
    procedure PostExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure EditorUpdate(Sender: TObject);
    procedure AddFormExecute(Sender: TObject);
    procedure AddFormUpdate(Sender: TObject);
    procedure AddComponentExecute(Sender: TObject);
    procedure AddComponentUpdate(Sender: TObject);
    procedure AddSymbolExecute(Sender: TObject);
    procedure AddSymbolUpdate(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure DeleteUpdate(Sender: TObject);
    procedure RenameExecute(Sender: TObject);
    procedure RenameUpdate(Sender: TObject);
  private
    FExplorer: TTreeView;
    FL10n: TXml10n;
    procedure SetExplorer(const Value: TTreeView);
    procedure SetL10n(const Value: TXml10n);
  protected
    procedure LoadForms;
    function LoadForm(const XmlForm: TXml10nForm): TTreeNode;
    function LoadComponent(const XmlComponent: TXml10nComponent;
      const ParentNode: TTreeNode): TTreeNode;
    function LoadProperty(const XmlProperty: TXml10nProperty;
      const ParentNode: TTreeNode): TTreeNode;
    procedure LoadSymbols;
    function LoadSymbol(const XmlSymbol: TXml10nSymbol): TTreeNode;
    procedure NodeToEditor(Node: TTreeNode);
    procedure EditorToNode(Node: TTreeNode);
    function CanDeleteOrRenameNode(Node: TTreeNode): Boolean;
    property Explorer: TTreeView read FExplorer write SetExplorer;
  public
    property L10n: TXml10n read FL10n write SetL10n;
  end;

  TXml10nDesigner = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

var
  Xml10nDesignerForm: TXml10nDesignerForm;

implementation

uses
  Xml10nListDlg;

{$R *.dfm}

procedure TXml10nDesignerForm.FormCreate(Sender: TObject);
begin
  Explorer := FormsExplorer;
end;

procedure TXml10nDesignerForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = FormsTab then
    Explorer := FormsExplorer
  else
    Explorer := SymbolsExplorer;
end;

procedure TXml10nDesignerForm.ExplorerChange(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node) and (TXml10nCollectionItem(Node.Data) is TXml10nSymbol) then
  begin
    Editor.Enabled := True;
    Editor.Color := clWindow;
    NodeToEditor(Node);
  end
  else
  begin
    Editor.Enabled := False;
    Editor.Color := clBtnFace;
    Editor.Clear;
  end;
end;

procedure TXml10nDesignerForm.ExplorerEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := CanDeleteOrRenameNode(Node);
end;

procedure TXml10nDesignerForm.ExplorerEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
  TXml10nCollectionItem(Node.Data).ItemName := S;
end;

procedure TXml10nDesignerForm.ClearExecute(Sender: TObject);
begin
  L10n.Clear;
  LoadForms;
  LoadSymbols;
end;

procedure TXml10nDesignerForm.LoadExecute(Sender: TObject);
begin
  try
    L10n.Load;
  finally
    LoadForms;
    LoadSymbols;
  end;
end;

procedure TXml10nDesignerForm.SaveExecute(Sender: TObject);
begin
  L10n.Save;
end;

procedure TXml10nDesignerForm.PostExecute(Sender: TObject);
begin
  EditorToNode(Explorer.Selected);
end;

procedure TXml10nDesignerForm.CancelExecute(Sender: TObject);
begin
  NodeToEditor(Explorer.Selected);
end;

procedure TXml10nDesignerForm.EditorUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Explorer.Selected)
    and (TXml10nCollectionItem(Explorer.Selected.Data) is TXml10nSymbol)
    and Editor.Modified;
end;

procedure TXml10nDesignerForm.AddFormExecute(Sender: TObject);
var
  Forms: TStringList;
  I: Integer;
begin
  Forms := TStringList.Create;
  try
    for I := 0 to Pred(Screen.CustomFormCount) do
      if csDesigning in Screen.CustomForms[I].ComponentState then
        Forms.AddObject(Screen.CustomForms[I].Name, Screen.CustomForms[I]);
    Forms.Sort;
    I := ListDlg('Add/update form', Forms);
    if I <> -1 then
    begin
      L10n.FormToXml(TCustomForm(Forms.Objects[I]));
      LoadForms;
    end;
  finally
    Forms.Free;
  end;
end;

procedure TXml10nDesignerForm.AddFormUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := Explorer = FormsExplorer;
end;

procedure TXml10nDesignerForm.AddComponentExecute(Sender: TObject);
var
  Components: TStringList;
  XmlForm: TXml10nForm;
  Form: TCustomForm;
  I: Integer;
begin
  Form := nil;
  XmlForm := TXml10nForm(FormsExplorer.Selected.Data);
  for I := 0 to Pred(Screen.CustomFormCount) do
    if SameText(Screen.CustomForms[I].Name, XmlForm.ItemName) then
    begin
      Form := Screen.CustomForms[I];
      Break;
    end;
  if Assigned(Form) then
  begin
    Components := TStringList.Create;
    try
      for I := 0 to Pred(Form.ComponentCount) do
        if Assigned(FindComponentPropLinks(Form.Components[I])) then
          Components.AddObject(Form.Components[I].Name, Form.Components[I]);
      Components.Sort;
      I := ListDlg('Add/update component', Components);
      if I <> -1 then
      begin
        L10n.ComponentToXml(TComponent(Components.Objects[I]), XmlForm);
        LoadForms;
      end;
    finally
      Components.Free;
    end;
  end
  else
    MessageDlg('Form not available.', mtError, [mbOK], 0);
end;

procedure TXml10nDesignerForm.AddComponentUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := Explorer = FormsExplorer;
  TAction(Sender).Enabled := Assigned(Explorer.Selected) and
    (TXml10nCollectionItem(Explorer.Selected.Data) is TXml10nForm);
end;

procedure TXml10nDesignerForm.AddSymbolExecute(Sender: TObject);
var
  XmlSymbol: TXml10nSymbol;
  Name: string;
begin
  if InputQuery('Add symbol', 'Symbol name:', Name) then
  begin
    XmlSymbol := TXml10nSymbol(L10n.ResourceLinks.Symbols.Add);
    try
      XmlSymbol.Name := Name;
    except
      XmlSymbol.Free;
      raise;
    end;
    Explorer.Selected := LoadSymbol(XmlSymbol);
  end;
end;

procedure TXml10nDesignerForm.AddSymbolUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := Explorer = SymbolsExplorer;
end;

procedure TXml10nDesignerForm.DeleteExecute(Sender: TObject);
begin
  Explorer.Items.BeginUpdate;
  try
    TXml10nCollectionItem(Explorer.Selected.Data).Free;
    Explorer.Selected.Free;
  finally
    Explorer.Items.EndUpdate;
  end;
end;

procedure TXml10nDesignerForm.DeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Explorer.Selected)
    and CanDeleteOrRenameNode(Explorer.Selected);
end;

procedure TXml10nDesignerForm.RenameExecute(Sender: TObject);
begin
  Explorer.Selected.EditText;
end;

procedure TXml10nDesignerForm.RenameUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Assigned(Explorer.Selected)
    and CanDeleteOrRenameNode(Explorer.Selected);
end;

procedure TXml10nDesignerForm.SetExplorer(const Value: TTreeView);
begin
  if FExplorer <> Value then
  begin
    FExplorer := Value;
    ExplorerChange(Explorer, Explorer.Selected);
  end;
end;

procedure TXml10nDesignerForm.SetL10n(const Value: TXml10n);
begin
  FL10n := Value;
  LoadForms;
  LoadSymbols;
end;

procedure TXml10nDesignerForm.LoadForms;
var
  I: Integer;
begin
  FormsExplorer.Items.BeginUpdate;
  try
    FormsExplorer.Items.Clear;
    if Assigned(L10n) then
      for I := 0 to Pred(L10n.ResourceLinks.Forms.Count) do
        LoadForm(TXml10nForm(L10n.ResourceLinks.Forms.Items[I]));
  finally
    FormsExplorer.Items.EndUpdate;
  end;
  FormsExplorer.OnChange(FormsExplorer, nil);
end;

function TXml10nDesignerForm.LoadForm(const XmlForm: TXml10nForm): TTreeNode;
var
  I: Integer;
begin
  Result := FormsExplorer.Items.AddObject(nil, XmlForm.ItemName, XmlForm);
  for I := 0 to Pred(XmlForm.Properties.Count) do
    LoadProperty(TXml10nProperty(XmlForm.Properties.Items[I]), Result);
  for I := 0 to Pred(XmlForm.Components.Count) do
    LoadComponent(TXml10nComponent(XmlForm.Components.Items[I]), Result);
end;

function TXml10nDesignerForm.LoadComponent(const XmlComponent: TXml10nComponent;
  const ParentNode: TTreeNode): TTreeNode;
var
  I: Integer;
begin
  Result := FormsExplorer.Items.AddChildObject(ParentNode,
    XmlComponent.ItemName, XmlComponent);
  Result.ImageIndex := 1;
  Result.SelectedIndex := 1;
  for I := 0 to Pred(XmlComponent.Properties.Count) do
    LoadProperty(TXml10nProperty(XmlComponent.Properties.Items[I]), Result);
end;

function TXml10nDesignerForm.LoadProperty(const XmlProperty: TXml10nProperty;
  const ParentNode: TTreeNode): TTreeNode;
begin
  Result := FormsExplorer.Items.AddChildObject(ParentNode,
    XmlProperty.ItemName, XmlProperty);
  Result.ImageIndex := 2;
  Result.SelectedIndex := 2;
end;

procedure TXml10nDesignerForm.LoadSymbols;
var
  I: Integer;
begin
  SymbolsExplorer.Items.BeginUpdate;
  try
    SymbolsExplorer.Items.Clear;
    if Assigned(L10n) then
      for I := 0 to Pred(L10n.ResourceLinks.Symbols.Count) do
        LoadSymbol(TXml10nSymbol(L10n.ResourceLinks.Symbols.Items[I]));
  finally
    SymbolsExplorer.Items.EndUpdate;
  end;
  FormsExplorer.OnChange(SymbolsExplorer, nil);
end;

function TXml10nDesignerForm.LoadSymbol(
  const XmlSymbol: TXml10nSymbol): TTreeNode;
begin
  Result := SymbolsExplorer.Items.AddObject(nil, XmlSymbol.ItemName, XmlSymbol);
  Result.ImageIndex := 2;
  Result.SelectedIndex := 2;
end;

procedure TXml10nDesignerForm.NodeToEditor(Node: TTreeNode);
begin
  Editor.Text := L10n.ResourceStrings.Strings.ReadString(
    TXml10nSymbol(Node.Data).SID, '');
  Editor.Modified := False;
end;

procedure TXml10nDesignerForm.EditorToNode(Node: TTreeNode);
begin
  TXml10nSymbol(Node.Data).SID := L10n.ResourceStrings.Strings.WriteString(
    Editor.Text, TXml10nSymbol(Node.Data).SID);
  Editor.Modified := False;
end;

function TXml10nDesignerForm.CanDeleteOrRenameNode(Node: TTreeNode): Boolean;
begin
  Result := not (TXml10nCollectionItem(Node.Data) is TXml10nProperty);
end;

{ TXml10nDesigner }

procedure TXml10nDesigner.ExecuteVerb(Index: Integer);
var
  DesignerForm: TXml10nDesignerForm;
begin
  DesignerForm := TXml10nDesignerForm.Create(Application);
  try
    DesignerForm.L10n := TXml10n(Component);
    DesignerForm.ShowModal;
  finally
    DesignerForm.Free;
  end;
end;

function TXml10nDesigner.GetVerb(Index: Integer): string;
begin
  Result := 'Localization Designer...';
end;

function TXml10nDesigner.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
