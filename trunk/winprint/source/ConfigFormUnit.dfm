object ConfigForm: TConfigForm
  Left = 576
  Top = 140
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  BorderWidth = 8
  Caption = 'ConfigForm'
  ClientHeight = 359
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    314
    359)
  PixelsPerInch = 96
  TextHeight = 13
  object Button3: TButton
    Left = 239
    Top = 334
    Width = 75
    Height = 25
    Hint = 'Button3'
    Anchors = [akRight, akBottom]
    Caption = 'Button3'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button2: TButton
    Left = 159
    Top = 334
    Width = 75
    Height = 25
    Hint = 'Button2'
    Anchors = [akRight, akBottom]
    Caption = 'Button2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 78
    Top = 334
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Button1'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 314
    Height = 326
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object TabSheet2: TTabSheet
      BorderWidth = 8
      Caption = 'PageControl1'
      ImageIndex = 1
      DesignSize = (
        290
        282)
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 290
        Height = 92
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GroupBox3'
        TabOrder = 0
        DesignSize = (
          290
          92)
        object Label5: TLabel
          Left = 12
          Top = 68
          Width = 32
          Height = 13
          Caption = 'Label5'
        end
        object Memo1: TMemo
          Left = 12
          Top = 20
          Width = 182
          Height = 45
          TabStop = False
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            #261#263#281#322#324#243#347#378#380
            #260#262#280#321#323#211#346#377#379)
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
        end
        object Button4: TButton
          Left = 205
          Top = 20
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Button4'
          TabOrder = 1
          OnClick = Button4Click
        end
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 94
        Width = 198
        Height = 93
        Caption = 'GroupBox4'
        TabOrder = 1
        object Label6: TLabel
          Left = 6
          Top = 28
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = 'Label6'
        end
        object Label7: TLabel
          Left = 98
          Top = 28
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = 'Label7'
        end
        object Label8: TLabel
          Left = 6
          Top = 60
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = 'Label8'
        end
        object Label9: TLabel
          Left = 96
          Top = 60
          Width = 33
          Height = 13
          Alignment = taRightJustify
          Caption = 'LAbel9'
        end
      end
      object RadioGroup1: TRadioGroup
        Left = 204
        Top = 94
        Width = 86
        Height = 93
        Anchors = [akLeft, akTop, akRight]
        Caption = 'RadioGroup1'
        Items.Strings = (
          'RadioGroup1_0'
          'RadioGroup1_1')
        TabOrder = 2
        OnClick = ConfigChanged
      end
      object Button6: TButton
        Left = 215
        Top = 257
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Button6'
        TabOrder = 4
        OnClick = Button6Click
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 190
        Width = 290
        Height = 60
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GroupBox5'
        TabOrder = 3
        object Label10: TLabel
          Left = 47
          Top = 26
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'Label10'
        end
        object Label14: TLabel
          Left = 168
          Top = 26
          Width = 38
          Height = 13
          Alignment = taRightJustify
          Caption = 'Label14'
        end
      end
    end
    object TabSheet1: TTabSheet
      BorderWidth = 8
      Caption = 'TabSheet1'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 290
        Height = 114
        Caption = 'GroupBox1'
        TabOrder = 0
        DesignSize = (
          290
          114)
        object SpeedButton1: TSpeedButton
          Left = 254
          Top = 36
          Width = 23
          Height = 22
          Anchors = [akTop, akRight]
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
            5555555555555555555555555555555555555555555555555555555555555555
            555555555555555555555555555555555555555FFFFFFFFFF555550000000000
            55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
            B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
            000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
            555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
            55555575FFF75555555555700007555555555557777555555555555555555555
            5555555555555555555555555555555555555555555555555555}
          NumGlyphs = 2
          OnClick = SpeedButton1Click
        end
        object Label1: TLabel
          Left = 12
          Top = 20
          Width = 32
          Height = 13
          Caption = 'Label1'
        end
        object Label2: TLabel
          Left = 12
          Top = 60
          Width = 32
          Height = 13
          Caption = 'Label2'
        end
        object Label11: TLabel
          Left = 116
          Top = 60
          Width = 38
          Height = 13
          Caption = 'Label11'
        end
        object Edit1: TEdit
          Left = 12
          Top = 36
          Width = 243
          Height = 21
          Hint = 'Edit1'
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnChange = ConfigChanged
        end
        object Edit2: TEdit
          Left = 12
          Top = 76
          Width = 88
          Height = 21
          Hint = 'Edit2'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnChange = ConfigChanged
        end
        object Edit3: TEdit
          Left = 116
          Top = 76
          Width = 88
          Height = 21
          Hint = 'Edit3'
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnChange = ConfigChanged
        end
        object CheckBox2: TCheckBox
          Left = 214
          Top = 82
          Width = 69
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'CheckBox2'
          TabOrder = 3
          OnClick = CheckBox2Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 117
        Width = 290
        Height = 134
        Caption = 'GroupBox2'
        TabOrder = 1
        DesignSize = (
          290
          134)
        object Label3: TLabel
          Left = 174
          Top = 20
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = 'Label3'
        end
        object Label4: TLabel
          Left = 174
          Top = 48
          Width = 32
          Height = 13
          Alignment = taRightJustify
          Caption = 'Label4'
        end
        object Label12: TLabel
          Left = 12
          Top = 78
          Width = 38
          Height = 13
          Caption = 'Label12'
        end
        object Label13: TLabel
          Left = 168
          Top = 78
          Width = 38
          Height = 13
          Caption = 'Label13'
        end
        object Edit6: TEdit
          Left = 220
          Top = 16
          Width = 40
          Height = 21
          Hint = 'Edit6'
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 0
          Text = '100'
        end
        object Edit7: TEdit
          Left = 220
          Top = 44
          Width = 40
          Height = 21
          Hint = 'Edit7'
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 1
          Text = '100'
        end
        object UpDown1: TUpDown
          Left = 260
          Top = 16
          Width = 17
          Height = 21
          Associate = Edit6
          Min = 100
          Max = 5000
          Increment = 100
          Position = 100
          TabOrder = 4
          Thousands = False
          OnChanging = ConfigChanging
        end
        object UpDown2: TUpDown
          Left = 260
          Top = 44
          Width = 17
          Height = 21
          Associate = Edit7
          Min = 100
          Max = 5000
          Increment = 100
          Position = 100
          TabOrder = 5
          Thousands = False
          OnChanging = ConfigChanging
        end
        object CheckBox1: TCheckBox
          Left = 12
          Top = 106
          Width = 165
          Height = 17
          Hint = 'CheckBox1'
          Caption = 'CheckBox1'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = ConfigChanged
        end
        object TrackBar1: TTrackBar
          Left = 56
          Top = 68
          Width = 105
          Height = 31
          Max = 3
          PageSize = 1
          Position = 1
          TabOrder = 2
          ThumbLength = 14
          TickMarks = tmBoth
          OnChange = TrackBar1Change
        end
      end
      object Button5: TButton
        Left = 215
        Top = 257
        Width = 75
        Height = 25
        Caption = 'Button5'
        TabOrder = 2
        OnClick = Button5Click
      end
    end
    object TabSheet3: TTabSheet
      BorderWidth = 8
      Caption = 'TabSheet3'
      ImageIndex = 2
      object GroupBox6: TGroupBox
        Left = 0
        Top = 0
        Width = 290
        Height = 76
        Caption = 'GroupBox6'
        TabOrder = 0
        object Label15: TLabel
          Left = 12
          Top = 20
          Width = 38
          Height = 13
          Caption = 'Label15'
          Transparent = True
        end
        object Edit4: TEdit
          Left = 146
          Top = 16
          Width = 129
          Height = 21
          TabOrder = 0
          OnChange = ConfigChanged
        end
        object CheckBox3: TCheckBox
          Left = 12
          Top = 46
          Width = 117
          Height = 17
          Caption = 'CheckBox3'
          TabOrder = 1
          OnClick = ConfigChanged
        end
        object CheckBox5: TCheckBox
          Left = 144
          Top = 46
          Width = 129
          Height = 17
          Hint = 'CheckBox5'
          Caption = 'CheckBox5'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = ConfigChanged
        end
      end
      object GroupBox7: TGroupBox
        Left = 0
        Top = 78
        Width = 290
        Height = 172
        Caption = 'GroupBox7'
        TabOrder = 1
        object Label16: TLabel
          Left = 12
          Top = 22
          Width = 38
          Height = 13
          Caption = 'Label16'
        end
        object SpeedButton2: TSpeedButton
          Left = 124
          Top = 76
          Width = 23
          Height = 22
          Hint = 'SpeedButton2'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333FF3333333333333003333
            3333333333773FF3333333333309003333333333337F773FF333333333099900
            33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
            99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
            33333333337F3F77333333333309003333333333337F77333333333333003333
            3333333333773333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = SpeedButton2Click
        end
        object SpeedButton3: TSpeedButton
          Left = 124
          Top = 104
          Width = 23
          Height = 22
          Hint = 'SpeedButton3'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333FF3333333333333003333333333333F77F33333333333009033
            333333333F7737F333333333009990333333333F773337FFFFFF330099999000
            00003F773333377777770099999999999990773FF33333FFFFF7330099999000
            000033773FF33777777733330099903333333333773FF7F33333333333009033
            33333333337737F3333333333333003333333333333377333333333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          OnClick = SpeedButton3Click
        end
        object Label17: TLabel
          Left = 12
          Top = 74
          Width = 38
          Height = 13
          Caption = 'Label17'
        end
        object Label18: TLabel
          Left = 12
          Top = 116
          Width = 38
          Height = 13
          Caption = 'Label18'
        end
        object ComboBox1: TComboBox
          Left = 90
          Top = 18
          Width = 185
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = ConfigChanged
          Items.Strings = (
            '')
        end
        object ListBox1: TListBox
          Left = 152
          Top = 50
          Width = 123
          Height = 82
          IntegralHeight = True
          ItemHeight = 13
          TabOrder = 1
          OnMouseMove = ListBox1MouseMove
        end
        object Edit5: TEdit
          Left = 12
          Top = 132
          Width = 106
          Height = 21
          TabOrder = 3
        end
        object CheckBox4: TCheckBox
          Left = 13
          Top = 51
          Width = 133
          Height = 17
          Caption = 'CheckBox4'
          TabOrder = 5
          OnClick = CheckBox4Click
        end
        object Button8: TButton
          Left = 146
          Top = 138
          Width = 64
          Height = 25
          Caption = 'Button8'
          TabOrder = 2
          OnClick = Button8Click
        end
        object Button9: TButton
          Left = 212
          Top = 138
          Width = 64
          Height = 25
          Caption = 'Button9'
          TabOrder = 4
          OnClick = Button9Click
        end
      end
      object Button7: TButton
        Left = 215
        Top = 257
        Width = 75
        Height = 25
        Caption = 'Button7'
        TabOrder = 2
        OnClick = Button7Click
      end
    end
    object TabSheet4: TTabSheet
      BorderWidth = 8
      Caption = 'TabSheet4'
      ImageIndex = 3
      DesignSize = (
        290
        282)
      object GroupBox8: TGroupBox
        Left = 0
        Top = 0
        Width = 290
        Height = 113
        Anchors = [akLeft, akTop, akRight]
        Caption = 'GroupBox8'
        TabOrder = 0
        DesignSize = (
          290
          113)
        object SpeedButton4: TSpeedButton
          Left = 254
          Top = 36
          Width = 23
          Height = 22
          Anchors = [akTop, akRight]
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000120B0000120B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
            5555555555555555555555555555555555555555555555555555555555555555
            555555555555555555555555555555555555555FFFFFFFFFF555550000000000
            55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
            B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
            000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
            555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
            55555575FFF75555555555700007555555555557777555555555555555555555
            5555555555555555555555555555555555555555555555555555}
          NumGlyphs = 2
          OnClick = SpeedButton4Click
        end
        object Label19: TLabel
          Left = 12
          Top = 20
          Width = 38
          Height = 13
          Caption = 'Label19'
        end
        object Label20: TLabel
          Left = 12
          Top = 63
          Width = 38
          Height = 13
          Caption = 'Label20'
        end
        object Label21: TLabel
          Left = 88
          Top = 63
          Width = 38
          Height = 13
          Caption = 'Label21'
        end
        object Edit8: TEdit
          Left = 12
          Top = 36
          Width = 243
          Height = 21
          Hint = 'Edit8'
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnChange = ConfigChanged
        end
        object CheckBox6: TCheckBox
          Left = 156
          Top = 82
          Width = 121
          Height = 17
          Hint = 'CheckBox6'
          Anchors = [akTop, akRight]
          Caption = 'CheckBox6'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = CheckBox2Click
        end
      end
      object Button10: TButton
        Left = 215
        Top = 257
        Width = 75
        Height = 25
        Caption = 'Button10'
        TabOrder = 1
        OnClick = Button10Click
      end
    end
  end
  object Button11: TButton
    Left = 6
    Top = 334
    Width = 25
    Height = 25
    Margins.Left = 1
    Margins.Top = 1
    Margins.Right = 1
    Margins.Bottom = 1
    Caption = 'Button11'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button11Click
  end
end
