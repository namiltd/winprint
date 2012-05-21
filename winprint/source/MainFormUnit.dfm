object MainForm: TMainForm
  Left = 304
  Top = 207
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 179
  ClientWidth = 256
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu1: TPopupMenu
    Left = 112
    Top = 16
    object Konfiguracja1: TMenuItem
      Caption = 'PopupMenu1'
      Default = True
      Hint = 'PopupMenu1'
      OnClick = Konfiguracja1Click
    end
    object Info1: TMenuItem
      Caption = 'Info1'
      Hint = 'Info1'
      OnClick = Info1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Zakoncz1: TMenuItem
      Caption = 'Zakoncz1'
      Hint = 'Zakoncz1'
      OnClick = Zakoncz1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 192
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'tk'
    Filter = 'OpenDialog1'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 112
    Top = 64
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Device = fdPrinter
    Options = [fdFixedPitchOnly, fdForceFontExist]
    Left = 192
    Top = 64
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer2Timer
    Left = 32
    Top = 112
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'tk'
    Filter = 'SaveDialog1'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 112
    Top = 112
  end
end
