unit Xml10nListDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TXml10nListDlgForm = class(TForm)
    List: TListBox;
    OKButton: TButton;
    CancelButton: TButton;
    procedure ListClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  end;

var
  Xml10nListDlgForm: TXml10nListDlgForm;

function ListDlg(const Title: string; Items: TStrings): Integer;

implementation

{$R *.dfm}

function ListDlg(const Title: string; Items: TStrings): Integer;
var
  Dlg: TXml10nListDlgForm;
begin
  Dlg := TXml10nListDlgForm.Create(Application);
  try
    Dlg.Caption := Title;
    Dlg.List.Items := Items;
    if Dlg.ShowModal = mrOk then
      Result := Dlg.List.ItemIndex
    else
      Result := -1;
  finally
    Dlg.Free;
  end;
end;

procedure TXml10nListDlgForm.ListClick(Sender: TObject);
begin
  OKButton.Enabled := List.ItemIndex <> -1;
end;

procedure TXml10nListDlgForm.ListDblClick(Sender: TObject);
begin
  OKButton.Click;
end;

end.
