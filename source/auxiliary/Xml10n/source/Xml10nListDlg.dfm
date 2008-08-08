object Xml10nListDlgForm: TXml10nListDlgForm
  Left = 191
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Xml10nListDlgForm'
  ClientHeight = 296
  ClientWidth = 221
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object List: TListBox
    Left = 12
    Top = 12
    Width = 197
    Height = 233
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListClick
    OnDblClick = ListDblClick
  end
  object OKButton: TButton
    Left = 33
    Top = 260
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 113
    Top = 260
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
