{******************************************************************************}
{                                                                              }
{   WinPrint - Print Spooler for DOS Programs                                  }
{                                                                              }
{   Copyright (C) 2004 Przemyslaw Czerkas <przemekc@users.sourceforge.net>     }
{                 2008 Mieczyslaw Nalewaj <namiltd@users.sourceforge.net>      }
{   See GPL.TXT for copyright and license details.                             }
{                                                                              }
{******************************************************************************}

{******************************************************************************}
{                                                                              }
{   This file is part of WinPrint.                                             }
{                                                                              }
{   WinPrint is free software; you can redistribute it and/or modify           }
{   it under the terms of the GNU General Public License as published by       }
{   the Free Software Foundation; either version 2 of the License, or          }
{   (at your option) any later version.                                        }
{                                                                              }
{   WinPrint is distributed in the hope that it will be useful,                }
{   but WITHOUT ANY WARRANTY; without even the implied warranty of             }
{   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              }
{   GNU General Public License for more details.                               }
{                                                                              }
{   You should have received a copy of the GNU General Public License          }
{   along with WinPrint; if not, write to the Free Software                    }
{   Foundation, Inc., 59 Temple Place, Suite 330, Boston,                      }
{   MA  02111-1307  USA                                                        }
{                                                                              }
{******************************************************************************}

unit ConfigFormUnit;

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, ExtCtrls, NumEdit, Printers, ConversionUnit;

type
  TConfigData = record
    InputFilesDir: string;
    InputFilesMask: string;
    FormatFileExtension: string;
    EnableFormatting: boolean;
    TimerInterval: integer;
    MinFileAge: integer;
    Priority: integer;
    AutoStart: boolean;
    FontName: string;
    FontSize: integer;
    FontCharset: integer;
    FontStyles: TFontStyles;
    MarginLeft,MarginRight,MarginTop,MarginBottom: double;
    Orientation: TPrinterOrientation;
    LinesPerInch: double;
    LinesPerPage: integer;
    EOPCodes: TCharCodes;
    SkipEmptyPages: boolean;
    ClipperCompatible: boolean;
    CodePage: TCodePage;
    UseCustomConversionTable: boolean;
    ConversionItems: TConversionItems;
    Logo: string;
    LogoLeft: double;
    LogoTop: double;
    Logo1PageOnly: boolean;
  end;

  TConfigForm = class(TForm)
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    Memo1: TMemo;
    Button4: TButton;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    RadioGroup1: TRadioGroup;
//    FloatEdit1: TFloatEdit;
//    FloatEdit2: TFloatEdit;
//    FloatEdit3: TFloatEdit;
//    FloatEdit4: TFloatEdit;
    Edit3: TEdit;
    Label11: TLabel;
    Button5: TButton;
    Button6: TButton;
    Label12: TLabel;
    TrackBar1: TTrackBar;
    Label13: TLabel;
    GroupBox5: TGroupBox;
    Label10: TLabel;
//    FloatEdit5: TFloatEdit;
//    IntEdit1: TIntEdit;
    Label14: TLabel;
    CheckBox2: TCheckBox;
    TabSheet3: TTabSheet;
    GroupBox6: TGroupBox;
    Edit4: TEdit;
    Label15: TLabel;
    CheckBox3: TCheckBox;
    GroupBox7: TGroupBox;
    Button7: TButton;
    ComboBox1: TComboBox;
    Label16: TLabel;
    ListBox1: TListBox;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
//    IntEdit2: TIntEdit;
//    IntEdit3: TIntEdit;
    Edit5: TEdit;
    CheckBox4: TCheckBox;
    Label17: TLabel;
    Label18: TLabel;
    Button8: TButton;
    Button9: TButton;
    CheckBox5: TCheckBox;
    Edit6: TEdit;
    Edit7: TEdit;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    TabSheet4: TTabSheet;
    GroupBox8: TGroupBox;
    SpeedButton4: TSpeedButton;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Edit8: TEdit;
    Button10: TButton;
    CheckBox6: TCheckBox;
    Button11: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FloatEdit5Change(Sender: TObject);
    procedure IntEdit1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
    { Private declarations }
    ListBox1HintIndex: integer;
    IgnoreOnChange: boolean;
    TempConversionItems: TConversionItems;
    IntEdit1: TIntEdit;
    IntEdit2: TIntEdit;
    IntEdit3: TIntEdit;
    FloatEdit1: TFloatEdit;
    FloatEdit2: TFloatEdit;
    FloatEdit3: TFloatEdit;
    FloatEdit4: TFloatEdit;
    FloatEdit5: TFloatEdit;
    FloatEdit6: TFloatEdit;
    FloatEdit7: TFloatEdit;
    procedure ReadConfig;
    procedure WriteConfig;
  public
    { Public declarations }
    ConfigData: TConfigData;
    procedure LoadLang;
  published
    procedure ConfigChanged(Sender: TObject);
    procedure ConfigChanging(Sender: TObject; var AllowChange: Boolean);
    procedure ResolveEditValues1(Sender: TObject);
  end;

 function RString(ID: WORD):string;

var
  ConfigForm: TConfigForm;
  LANG: word = 61000;

implementation

uses
  MainFormUnit, FileCtrl, Registry, IniFiles,
  SetString; //dla SetToString

{$R *.DFM}

const
  DEFAULT_INPUT_FILES_DIR = '';
  DEFAULT_INPUT_FILES_MASK = '*.txt';
  DEFAULT_FORMAT_FILE_EXTENSION = 'frm';
  DEFAULT_ENABLE_FORMATTING = true;
  DEFAULT_TIMER_INTERVAL = 500;
  DEFAULT_MIN_FILE_AGE = 500;
  DEFAULT_PRIORITY = 2; //real-time priority
  DEFAULT_AUTO_START = false;
  DEFAULT_FONT_NAME = 'Courier New';
  DEFAULT_FONT_SIZE = 12;
  DEFAULT_FONT_CHARSET = 238; //Easter Europe
  DEFAULT_FONT_STYLES = [];
  DEFAULT_MARGIN_LEFT = 12.7;
  DEFAULT_MARGIN_RIGHT = 12.7;
  DEFAULT_MARGIN_TOP = 12.7;
  DEFAULT_MARGIN_BOTTOM = 12.7;
  DEFAULT_ORIENTATION = poPortrait;
  DEFAULT_LINES_PER_INCH = 6;
  DEFAULT_LINES_PER_PAGE = 0;
  DEFAULT_EOP_CODES = [12,26];
  DEFAULT_SKIP_EMPTY_PAGES = true;
  DEFAULT_CLIPPER_COMPATIBLE = TRUE;
  DEFAULT_CODE_PAGE = cp852;
  DEFAULT_USE_CUSTOM_CONVERSION_TABLE = false;
  DEFAULT_LOGO = '';
  DEFAULT_LOGO_LEFT = 12.7;
  DEFAULT_LOGO_TOP = 12.7;
  DEFAULT_LOGO_1PAGE_ONLY = false;

  MinPriorityClass = -1;
  MaxPriorityClass = 2;

  PriorityClassValues: array[MinPriorityClass..MaxPriorityClass] of cardinal =
        (IDLE_PRIORITY_CLASS,
         NORMAL_PRIORITY_CLASS,
         HIGH_PRIORITY_CLASS,
         REALTIME_PRIORITY_CLASS);
var
  PriorityClassNames: array[MinPriorityClass..MaxPriorityClass] of string;


function RString(ID: WORD):string;
var
  a : array[0..255] of char;
begin
 result:='';
 if ID<1000 then ID:=ID+LANG; //multilingual respurces
 if (LoadString(hInstance,ID,@a,sizeof(a)) <> 0)
   then result:=StrPas(a);
end;

procedure SaveCollectionToStream(Collection: TCollection; Stream: TStream);
begin
  with TWriter.Create(Stream, 4096) do
  try
    WriteCollection(Collection);
  finally
    Free;
  end;
end;

procedure LoadCollectionFromStream(Stream: TStream; Collection: TCollection);
begin
  with TReader.Create(Stream, 4096) do
  try
    CheckValue(vaCollection);
    ReadCollection(Collection);
  finally
    Free;
  end;
end;

{}

procedure TConfigForm.FormCreate(Sender: TObject);
begin
 Label8.Caption:=RString(111);
{komponenty dynamiczne}
  IntEdit1:=TIntEdit.Create(Self);
  with IntEdit1 do begin
          Left := 212;
          Top := 22;
          Width := 50;
          Height := 21;
//          TabOrder := 1;
          Text := '0';
          OnChange := IntEdit1Change;
          OnExit := ResolveEditValues1;
          Parent := GroupBox5;
          Visible := true;
  end;
  IntEdit2:=TIntEdit.Create(Self);
  with IntEdit2 do begin
          Left := 12;
          Top := 90;
          Width := 48;
          Height := 21;
//          TabOrder := 2;
          Text := '0';
          Parent := GroupBox7;
          Visible := true;
  end;
  IntEdit3:=TIntEdit.Create(Self);
  with IntEdit3 do begin
          Left := 70;
          Top := 90;
          Width := 48;
          Height := 21;
          Hint := 'IntEdit3';
          ParentShowHint := False;
          ShowHint := True;
//          TabOrder := 3;
          Text := '0';
          Parent := GroupBox7;
          Visible := true;
  end;
  FloatEdit1:=TFloatEdit.Create(Self);
  with FloatEdit1 do begin
          Left := 44;
          Top := 24;
          Width := 50;
          Height := 21;
//          TabOrder := 0;
          Text := '0';
          OnChange := ConfigChanged;
          Parent := GroupBox4;
          Visible := true;
  end;
  FloatEdit2:=TFloatEdit.Create(Self);
  with FloatEdit2 do begin
          Left := 136;
          Top := 24;
          Width := 50;
          Height := 21;
//          TabOrder := 1;
          Text := '0';
          OnChange := ConfigChanged;
          Parent := GroupBox4;
          Visible := true;
  end;
  FloatEdit3:=TFloatEdit.Create(Self);
  with FloatEdit3 do begin
          Left := 44;
          Top := 56;
          Width := 50;
          Height := 21;
//          TabOrder := 2;
          Text := '0';
          OnChange := ConfigChanged;
          Parent := GroupBox4;
          Visible := true;
  end;
  FloatEdit4:=TFloatEdit.Create(Self);
  with FloatEdit4 do begin
          Left := 136;
          Top := 56;
          Width := 50;
          Height := 21;
//          TabOrder := 3;
          Text := '0';
          OnChange := ConfigChanged;
          Parent := GroupBox4;
          Visible := true;
  end;
  FloatEdit5:=TFloatEdit.Create(Self);
  with FloatEdit5 do begin
          Left := 92;
          Top := 22;
          Width := 50;
          Height := 21;
//          TabOrder := 5;
          Text := '0';
          OnChange := FloatEdit5Change;
          OnExit := ResolveEditValues1;
          Parent := GroupBox5;
          Visible := true;
  end;
  FloatEdit6:=TFloatEdit.Create(Self);
  with FloatEdit6 do begin
          Left := 12;
          Top := 80;
          Width := 50;
          Height := 21;
//          TabOrder := 3;
          Text := '0';
          OnChange := ConfigChanged;
          Parent := GroupBox8;
          Visible := true;
  end;
  FloatEdit7:=TFloatEdit.Create(Self);
  with FloatEdit7 do begin
          Left := 88;
          Top := 80;
          Width := 50;
          Height := 21;
//          TabOrder := 0;
          Text := '0';
          OnChange := FloatEdit5Change;
          OnExit := ResolveEditValues1;
          Parent := GroupBox8;
          Visible := true;
  end;

  with MainForm.CEVersionInfo1 do
    Caption:=ProductName+' '+FileVersion;

  PageControl1.ActivePageIndex:=0;
  FloatEdit5.Color:=clBtnFace;
  IntEdit1.Color:=clWindow;
  TrackBar1.Min:=MinPriorityClass;
  TrackBar1.Max:=MaxPriorityClass;
  ListBox1HintIndex:=-1;
  Application.HintPause:=200;
  ConfigData.ConversionItems:=TConversionItems.Create;
  TempConversionItems:=TConversionItems.Create;
  ReadConfig;
  WriteConfig;
  MainForm.Timer1.Enabled:=true;
end;

procedure TConfigForm.FormDestroy(Sender: TObject);
begin
  TempConversionItems.Free;
  ConfigData.ConversionItems.Free;
end;

procedure TConfigForm.SpeedButton1Click(Sender: TObject);
var
  TempDir: string;
begin
  if SelectDirectory(RString(300),'\',TempDir) then
  begin
    if TempDir<>'' then TempDir:=IncludeTrailingBackslash(TempDir);
    If LowerCase(TempDir) = LowerCase(ExtractFilePath(ParamStr(0))) then TempDir := '';
    Edit1.Text:=TempDir;
  end;
end;

function IsFullPath(APath: string): boolean;
begin
  result:=(length(APath)>=2) and (APath[2]=':');
end;

procedure TConfigForm.ReadConfig;
var
  Registry: TRegistry;
  IniFile: TIniFile;
  i: integer;
  MemStream: TMemoryStream;
  section: string;
begin
  with ConfigData do
  begin
    if FileExists(ChangeFileExt(Application.ExeName,'.ini')) then
    begin
      IniFile:=TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
      with IniFile do
      try
        section:='Settings';
        InputFilesDir:=ReadString(section,'InputFilesDir',DEFAULT_INPUT_FILES_DIR);
        InputFilesMask:=ReadString(section,'InputFilesMask',DEFAULT_INPUT_FILES_MASK);
        FormatFileExtension:=ReadString(section,'FormatFileExtension',DEFAULT_FORMAT_FILE_EXTENSION);
        EnableFormatting:=ReadBool(section,'EnableFormatting',DEFAULT_ENABLE_FORMATTING);
        TimerInterval:=ReadInteger(section,'TimerInterval',DEFAULT_TIMER_INTERVAL);
        if (TimerInterval<UpDown1.Min) or (TimerInterval>UpDown1.Max) then TimerInterval:=DEFAULT_TIMER_INTERVAL;
        MinFileAge:=ReadInteger(section,'MinFileAge',DEFAULT_MIN_FILE_AGE);
        if (MinFileAge<UpDown2.Min) or (MinFileAge>UpDown2.Max) then MinFileAge:=DEFAULT_MIN_FILE_AGE;
        Priority:=ReadInteger(section,'Priority',DEFAULT_PRIORITY);
        if (Priority<MinPriorityClass) or (Priority>MaxPriorityClass) then Priority:=DEFAULT_PRIORITY;
        AutoStart:=ReadBool(section,'AutoStart',DEFAULT_AUTO_START);
        FontName:=ReadString(section,'FontName',DEFAULT_FONT_NAME);
        if (FontName='') then FontName:=DEFAULT_FONT_NAME;
        FontSize:=ReadInteger(section,'FontSize',DEFAULT_FONT_SIZE);
        if (FontSize<=0) then FontSize:=DEFAULT_FONT_SIZE;
        FontCharset:=ReadInteger(section,'FontCharset',DEFAULT_FONT_CHARSET);
        FontStyles:=DEFAULT_FONT_STYLES;
        StringToSet(ReadString(section,'FontStyles',SetToString(TypeInfo(TFontStyles),FontStyles)),TypeInfo(TFontStyles),FontStyles);
        MarginLeft:=ReadFloat(section,'MarginLeft',DEFAULT_MARGIN_LEFT);
        MarginRight:=ReadFloat(section,'MarginRight',DEFAULT_MARGIN_RIGHT);
        MarginTop:=ReadFloat(section,'MarginTop',DEFAULT_MARGIN_TOP);
        MarginBottom:=ReadFloat(section,'MarginBottom',DEFAULT_MARGIN_BOTTOM);
        Orientation:=TPrinterOrientation(ReadInteger(section,'Orientation',ord(DEFAULT_ORIENTATION)));
        if not (Orientation in [low(TPrinterOrientation)..high(TPrinterOrientation)]) then Orientation:=DEFAULT_ORIENTATION;
        LinesPerInch:=ReadFloat(section,'LinesPerInch',DEFAULT_LINES_PER_INCH);
        if (LinesPerInch<0) then LinesPerInch:=DEFAULT_LINES_PER_INCH;
        LinesPerPage:=ReadInteger(section,'LinesPerPage',DEFAULT_LINES_PER_PAGE);
        if (LinesPerPage<0) then LinesPerPage:=DEFAULT_LINES_PER_PAGE;
        EOPCodes:=DEFAULT_EOP_CODES;
        StringToSet(ReadString(section,'EOPCodes',SetToString(TypeInfo(TCharCodes),EOPCodes)),TypeInfo(TCharCodes),EOPCodes);
        SkipEmptyPages:=ReadBool(section,'SkipEmptyPages',DEFAULT_SKIP_EMPTY_PAGES);
        ClipperCompatible:=ReadBool(section,'ClipperCompatible',DEFAULT_CLIPPER_COMPATIBLE);
        CodePage:=TCodePage(StringToOrd(TypeInfo(TCodePage),ReadString(section,'CodePage',OrdToString(TypeInfo(TCodePage),ord(DEFAULT_CODE_PAGE)))));
        if not (CodePage in [CodePageLow..CodePageHigh]) then CodePage:=DEFAULT_CODE_PAGE;
        UseCustomConversionTable:=ReadBool(section,'UseCustomConversionTable',DEFAULT_USE_CUSTOM_CONVERSION_TABLE);
        try
          MemStream:=TMemoryStream.Create;
          try
            ReadBinaryStream(section,'CustomConversionTable',MemStream);
            MemStream.Seek(0,soFromBeginning);
            LoadCollectionFromStream(MemStream,ConfigData.ConversionItems);
          finally
            MemStream.Free;
          end;
        except
        end;
        Logo:=ReadString(section,'Logo',DEFAULT_LOGO);
        LogoLeft:=ReadFloat(section,'LogoLeft',DEFAULT_LOGO_LEFT);
        LogoTop:=ReadFloat(section,'LogoTop',DEFAULT_LOGO_TOP);
        Logo1PageOnly:=ReadBool(section,'Logo1PageOnly',DEFAULT_LOGO_1PAGE_ONLY);
      finally
        IniFile.Free;
      end;
    end
    else
    begin
      //backward compatibility for older version with registry-based config
      Registry:=TRegistry.Create;
      with Registry do
      try
        RootKey:=HKEY_LOCAL_MACHINE;
        try
          if OpenKey('Software\GNU\WinPrint',false) then
          begin
            try
              InputFilesDir:=ReadString('InputFilesDir');
            except
              InputFilesDir:=DEFAULT_INPUT_FILES_DIR;
            end;
            try
              InputFilesMask:=ReadString('InputFilesMask');
              if (InputFilesMask='') then InputFilesMask:=DEFAULT_INPUT_FILES_MASK;
            except
              InputFilesMask:=DEFAULT_INPUT_FILES_MASK;
            end;
            try
              FormatFileExtension:=ReadString('FormatFileExtension');
              if (FormatFileExtension='') then FormatFileExtension:=DEFAULT_FORMAT_FILE_EXTENSION;
            except
              FormatFileExtension:=DEFAULT_FORMAT_FILE_EXTENSION;
            end;
            try
              EnableFormatting:=ReadBool('EnableFormatting');
            except
              EnableFormatting:=DEFAULT_ENABLE_FORMATTING;
            end;
            try
              TimerInterval:=ReadInteger('TimerInterval');
              if (TimerInterval<UpDown1.Min) or (TimerInterval>UpDown1.Max) then TimerInterval:=DEFAULT_TIMER_INTERVAL;
            except
              TimerInterval:=DEFAULT_TIMER_INTERVAL;
            end;
            try
              MinFileAge:=ReadInteger('MinFileAge');
              if (MinFileAge<UpDown2.Min) or (MinFileAge>UpDown2.Max) then MinFileAge:=DEFAULT_MIN_FILE_AGE;
            except
              MinFileAge:=DEFAULT_MIN_FILE_AGE;
            end;
            try
              Priority:=ReadInteger('Priority');
              if (Priority<MinPriorityClass) or (Priority>MaxPriorityClass) then Priority:=DEFAULT_PRIORITY;
            except
              Priority:=DEFAULT_PRIORITY;
            end;
            try
              AutoStart:=ReadBool('AutoStart');
            except
              AutoStart:=DEFAULT_AUTO_START;
            end;
            try
              FontName:=ReadString('FontName');
              if (FontName='') then FontName:=DEFAULT_FONT_NAME;
            except
              FontName:=DEFAULT_FONT_NAME;
            end;
            try
              FontSize:=ReadInteger('FontSize');
              if (FontSize<=0) then FontSize:=DEFAULT_FONT_SIZE;
            except
              FontSize:=DEFAULT_FONT_SIZE;
            end;
            try
              FontCharset:=ReadInteger('FontCharset');
            except
              FontCharset:=DEFAULT_FONT_CHARSET;
            end;
            try
              StringToSet(ReadString('FontStyles'),TypeInfo(TFontStyles),FontStyles);
            except
              FontStyles:=DEFAULT_FONT_STYLES;
            end;
            try
              MarginLeft:=ReadFloat('MarginLeft');
            except
              MarginLeft:=DEFAULT_MARGIN_LEFT;
            end;
            try
              MarginRight:=ReadFloat('MarginRight');
            except
              MarginRight:=DEFAULT_MARGIN_RIGHT;
            end;
            try
              MarginTop:=ReadFloat('MarginTop');
            except
              MarginTop:=DEFAULT_MARGIN_TOP;
            end;
            try
              MarginBottom:=ReadFloat('MarginBottom');
            except
              MarginBottom:=DEFAULT_MARGIN_BOTTOM;
            end;
            try
              Orientation:=TPrinterOrientation(ReadInteger('Orientation'));
              if not (Orientation in [low(TPrinterOrientation)..high(TPrinterOrientation)]) then Orientation:=DEFAULT_ORIENTATION;
            except
              Orientation:=DEFAULT_ORIENTATION;
            end;
            try
              LinesPerInch:=ReadFloat('LinesPerInch');
              if (LinesPerInch<0) then LinesPerInch:=DEFAULT_LINES_PER_INCH;
            except
              LinesPerInch:=DEFAULT_LINES_PER_INCH;
            end;
            try
              LinesPerPage:=ReadInteger('LinesPerPage');
              if (LinesPerPage<0) then LinesPerPage:=DEFAULT_LINES_PER_PAGE;
            except
              LinesPerPage:=DEFAULT_LINES_PER_PAGE;
            end;
            try
              StringToSet(ReadString('EOPCodes'),TypeInfo(TCharCodes),EOPCodes);
            except
              EOPCodes:=DEFAULT_EOP_CODES;
            end;
            try
              SkipEmptyPages:=ReadBool('SkipEmptyPages');
            except
              SkipEmptyPages:=DEFAULT_SKIP_EMPTY_PAGES;
            end;
            try
              ClipperCompatible:=ReadBool('ClipperCompatible');
            except
              ClipperCompatible:=DEFAULT_CLIPPER_COMPATIBLE;
            end;
            try
              CodePage:=TCodePage(StringToOrd(TypeInfo(TCodePage),ReadString('CodePage')));
              if not (CodePage in [CodePageLow..CodePageHigh]) then CodePage:=DEFAULT_CODE_PAGE;
            except
              CodePage:=DEFAULT_CODE_PAGE;
            end;
            try
              UseCustomConversionTable:=ReadBool('UseCustomConversionTable');
            except
              UseCustomConversionTable:=DEFAULT_USE_CUSTOM_CONVERSION_TABLE;
            end;
            try
              MemStream:=TMemoryStream.Create;
              try
                MemStream.SetSize(Registry.GetDataSize('CustomConversionTable'));
                Registry.ReadBinaryData('CustomConversionTable',MemStream.Memory^,MemStream.Size);
                LoadCollectionFromStream(MemStream,ConfigData.ConversionItems);
              finally
                MemStream.Free;
              end;
            except
            end;
            try
              Logo:=ReadString('Logo');
            except
              Logo:=DEFAULT_LOGO;
            end;
            try
              LogoLeft:=ReadFloat('LogoLeft');
            except
              LogoLeft:=DEFAULT_LOGO_LEFT;
            end;
            try
              LogoTop:=ReadFloat('LogoTop');
            except
              LogoTop:=DEFAULT_LOGO_TOP;
            end;
            try
              Logo1PageOnly:=ReadBool('Logo1PageOnly');
            except
              Logo1PageOnly:=DEFAULT_LOGO_1PAGE_ONLY;
            end;
          end
          else
          begin
            InputFilesDir:=DEFAULT_INPUT_FILES_DIR;
            InputFilesMask:=DEFAULT_INPUT_FILES_MASK;
            FormatFileExtension:=DEFAULT_FORMAT_FILE_EXTENSION;
            EnableFormatting:=DEFAULT_ENABLE_FORMATTING;
            TimerInterval:=DEFAULT_TIMER_INTERVAL;
            MinFileAge:=DEFAULT_MIN_FILE_AGE;
            Priority:=DEFAULT_PRIORITY;
            AutoStart:=DEFAULT_AUTO_START;
            FontName:=DEFAULT_FONT_NAME;
            FontSize:=DEFAULT_FONT_SIZE;
            FontCharset:=DEFAULT_FONT_CHARSET;
            FontStyles:=DEFAULT_FONT_STYLES;
            MarginLeft:=DEFAULT_MARGIN_LEFT;
            MarginRight:=DEFAULT_MARGIN_RIGHT;
            MarginTop:=DEFAULT_MARGIN_TOP;
            MarginBottom:=DEFAULT_MARGIN_BOTTOM;
            Orientation:=DEFAULT_ORIENTATION;
            LinesPerInch:=DEFAULT_LINES_PER_INCH;
            LinesPerPage:=DEFAULT_LINES_PER_PAGE;
            EOPCodes:=DEFAULT_EOP_CODES;
            SkipEmptyPages:=DEFAULT_SKIP_EMPTY_PAGES;
            ClipperCompatible:=DEFAULT_CLIPPER_COMPATIBLE;
            CodePage:=DEFAULT_CODE_PAGE;
            UseCustomConversionTable:=DEFAULT_USE_CUSTOM_CONVERSION_TABLE;
            Logo:=DEFAULT_LOGO;
            LogoLeft:=DEFAULT_LOGO_LEFT;
            LogoTop:=DEFAULT_LOGO_TOP;
            Logo1PageOnly:=DEFAULT_LOGO_1PAGE_ONLY;
          end;
        finally
          CloseKey;
        end;
      finally
        Registry.Free;
      end;
    end;

    Edit1.Text:=InputFilesDir;
    Edit2.Text:=InputFilesMask;
    Edit3.Text:=FormatFileExtension;
    CheckBox2.Checked:=EnableFormatting;
    Edit8.Text:=Logo;
    FloatEdit6.Value:=LogoLeft;
    FloatEdit7.Value:=LogoTop;
    CheckBox6.Checked:=Logo1PageOnly;
    if EnableFormatting then
    begin
      Edit3.Enabled:=true;
      Edit3.Color:=clWindow;
    end
    else
    begin
      Edit3.Enabled:=false;
      Edit3.Color:=clBtnFace;
    end;
    Updown1.Position:=TimerInterval;
    Updown2.Position:=MinFileAge;
    TrackBar1.Position:=Priority;
    Label13.Caption:=PriorityClassNames[Priority];
    CheckBox1.Checked:=AutoStart;
    Memo1.Font.Name:=FontName;
    Memo1.Font.Size:=FontSize;
    Memo1.Font.Charset:=FontCharset;
    if Memo1.Font.Charset=238 then begin //East Europe
                                     Memo1.Lines.Strings[0]:=RString(50000);
                                     Memo1.Lines.Strings[1]:=RString(50001);
                                   end
                              else begin
                                     Memo1.Lines.Strings[0]:=RString(51000);
                                     Memo1.Lines.Strings[1]:=RString(51001);
                                   end;
    Memo1.Font.Style:=FontStyles;
    Label5.Caption:=Format('%s, '+RString(301)+': %d',[FontName,FontSize]);
    FloatEdit1.Value:=MarginLeft;
    FloatEdit2.Value:=MarginRight;
    FloatEdit3.Value:=MarginTop;
    FloatEdit4.Value:=MarginBottom;
    if (Orientation=poPortrait) then RadioGroup1.ItemIndex:=0 else RadioGroup1.ItemIndex:=1;
    FloatEdit5.Value:=LinesPerInch;
    IntEdit1.Value:=LinesPerPage;
    Edit4.Text:=SetToString(TypeInfo(TCharCodes),EOPCodes,',','','');
    CheckBox3.Checked:=SkipEmptyPages;
    CheckBox5.Checked:=ClipperCompatible;

    with ComboBox1 do
    begin
      Items.Clear;
      for i:=ord(CodePageLow) to ord(CodePageHigh) do
        Items.Append(CodePageInfo[TCodePage(i)].name);
      ItemIndex:=ord(CodePage);
    end;

    CheckBox4.Checked:=UseCustomConversionTable;
    if UseCustomConversionTable then
    begin
      Label17.Enabled:=true;
      IntEdit2.Enabled:=true;
      IntEdit2.Color:=clWindow;
      IntEdit3.Enabled:=true;
      IntEdit3.Color:=clWindow;
      Label18.Enabled:=true;
      Edit5.Enabled:=true;
      Edit5.Color:=clWindow;
      SpeedButton2.Enabled:=true;
      SpeedButton3.Enabled:=true;
      ListBox1.Enabled:=true;
      ListBox1.Color:=clWindow;
      Button8.Enabled:=true;
      Button9.Enabled:=true;
    end
    else
    begin
      Label17.Enabled:=false;
      IntEdit2.Enabled:=false;
      IntEdit2.Color:=clBtnFace;
      IntEdit3.Enabled:=false;
      IntEdit3.Color:=clBtnFace;
      Label18.Enabled:=false;
      Edit5.Enabled:=false;
      Edit5.Color:=clBtnFace;
      SpeedButton2.Enabled:=false;
      SpeedButton3.Enabled:=false;
      ListBox1.Enabled:=false;
      ListBox1.Color:=clBtnFace;
      Button8.Enabled:=false;
      Button9.Enabled:=false;
    end;

    TempConversionItems.Assign(ConversionItems);
    with ListBox1.Items do
    begin
      Clear;
      for i:=1 to TempConversionItems.Count do
        AddObject(TempConversionItems[i-1].DisplayText,TempConversionItems[i-1]);
    end;

    ResolveEditValues1(self); //Ustaw wartosci domyslne, gdy obie gestosci rowne 0
    if not IsFullPath(InputFilesDir) then   //normalizacja sciezek bezwzglednych i wzglednych
      InputFilesDir:=IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)))+InputFilesDir;
    InputFilesDir:=IncludeTrailingBackslash(ExpandFileName(InputFilesDir));
    MainForm.Timer1.Interval:=TimerInterval; //ustawienie Timera
    SetPriorityClass(GetCurrentProcess,PriorityClassValues[Priority]);
    Button3.Enabled:=false; //Klawisz Zastosuj
  end;
end;

procedure TConfigForm.WriteConfig;
var
  Registry: TRegistry;
  IniFile: TIniFile;
  Styles: TFontStyles;
  TempSet: TCharCodes;
  MemStream: TMemoryStream;
  section: string;
begin
  IniFile:=TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  with IniFile do
  try
    section:='Settings';
    WriteString(section,'InputFilesDir',Edit1.Text);
    WriteString(section,'InputFilesMask',Edit2.Text);
    WriteString(section,'FormatFileExtension',Edit3.Text);
    WriteBool(section,'EnableFormatting',CheckBox2.Checked);
    WriteInteger(section,'TimerInterval',UpDown1.Position);
    WriteInteger(section,'MinFileAge',UpDown2.Position);
    WriteInteger(section,'Priority',TrackBar1.Position);
    WriteBool(section,'AutoStart',CheckBox1.Checked);
    WriteString(section,'FontName',Memo1.Font.Name);
    WriteInteger(section,'FontSize',Memo1.Font.Size);
    WriteInteger(section,'FontCharset',Memo1.Font.Charset);
    Styles:=Memo1.Font.Style; WriteString(section,'FontStyles',SetToString(TypeInfo(TFontStyles),Styles));
    WriteFloat(section,'MarginLeft',FloatEdit1.Value);
    WriteFloat(section,'MarginRight',FloatEdit2.Value);
    WriteFloat(section,'MarginTop',FloatEdit3.Value);
    WriteFloat(section,'MarginBottom',FloatEdit4.Value);
    WriteInteger(section,'Orientation',RadioGroup1.ItemIndex);
    WriteFloat(section,'LinesPerInch',FloatEdit5.Value);
    WriteInteger(section,'LinesPerPage',IntEdit1.Value);
    StringToSet(Edit4.Text,TypeInfo(TCharCodes),TempSet); WriteString(section,'EOPCodes',SetToString(TypeInfo(TCharCodes),TempSet));
    WriteBool(section,'SkipEmptyPages',CheckBox3.Checked);
    WriteBool(section,'ClipperCompatible',CheckBox5.Checked);
    WriteString(section,'CodePage',OrdToString(TypeInfo(TCodePage),ComboBox1.ItemIndex));
    WriteBool(section,'UseCustomConversionTable',CheckBox4.Checked);
    ConfigData.ConversionItems.Assign(TempConversionItems);
    MemStream:=TMemoryStream.Create;
    try
      SaveCollectionToStream(ConfigData.ConversionItems,MemStream);
      MemStream.Seek(0,soFromBeginning);
      WriteBinaryStream(section,'CustomConversionTable',MemStream);
    finally
      MemStream.Free;
    end;
    WriteString(section,'Logo',Edit8.Text);
    WriteFloat(section,'LogoLeft',FloatEdit6.Value);
    WriteFloat(section,'LogoTop',FloatEdit7.Value);
    WriteBool(section,'Logo1PageOnly',CheckBox6.Checked);
  finally
    IniFile.Free;
  end;

  Registry:=TRegistry.Create;
  with Registry do
  try
    //user may not have delete rights in HKLM
    //so we move autostart to the HKCU root key
    RootKey:=HKEY_LOCAL_MACHINE;
    try
      if OpenKey('Software\Microsoft\Windows\CurrentVersion\Run',false) then
      try
        DeleteValue('WinPrint');
      except
      end;
    finally
      CloseKey;
    end;
    RootKey:=HKEY_CURRENT_USER;
    try
      if OpenKey('Software\Microsoft\Windows\CurrentVersion\Run',false) then
      if CheckBox1.Checked then
        WriteString('WinPrint',Application.ExeName)
      else
        DeleteValue('WinPrint');
    finally
      CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TConfigForm.ConfigChanged(Sender: TObject);
begin
  Button3.Enabled:=true;
end;

procedure TConfigForm.ConfigChanging(Sender: TObject; var AllowChange: Boolean);
begin
  ConfigChanged(Sender);
end;

//Klawisz jêzyka
procedure TConfigForm.Button11Click(Sender: TObject);
begin
  if LANG=61000 then LANG:=60000
                else LANG:=61000;
  LoadLang;
  MainForm.Loadlang;
end;

//Klawisz OK
procedure TConfigForm.Button1Click(Sender: TObject);
begin
  WriteConfig;
  ReadConfig;  
  Close;
end;

//Klawisz Anuluj
procedure TConfigForm.Button2Click(Sender: TObject);
begin
  Close;
end;

//Klawisz Zastosuj
procedure TConfigForm.Button3Click(Sender: TObject);
begin
  WriteConfig;
  ReadConfig;
end;

//Klawisz Wybierz czcionkê
procedure TConfigForm.Button4Click(Sender: TObject);
begin
  with MainForm do
  begin
    FontDialog1.Font:=Memo1.Font;
    if FontDialog1.Execute then
    begin
      Memo1.Font:=FontDialog1.Font;
      if Memo1.Font.Charset=238 then begin //East Europe
                                       Memo1.Lines.Strings[0]:=RString(50000);
                                       Memo1.Lines.Strings[1]:=RString(50001);
                                     end
                                else begin
                                       Memo1.Lines.Strings[0]:=RString(51000);
                                       Memo1.Lines.Strings[1]:=RString(51001);
                                     end;
      Label5.Caption:=Format('%s, '+RString(301)+': %d',[Memo1.Font.Name,Memo1.Font.Size]);
      ConfigChanged(Sender);
    end;
  end;
end;

//Karta Opcje u¿ytkowe - Klawisz domyœlne
procedure TConfigForm.Button5Click(Sender: TObject);
begin
  Edit1.Text:=DEFAULT_INPUT_FILES_DIR;
  Edit2.Text:=DEFAULT_INPUT_FILES_MASK;
  Edit3.Text:=DEFAULT_FORMAT_FILE_EXTENSION;
  CheckBox2.Checked:=DEFAULT_ENABLE_FORMATTING;
  UpDown1.Position:=DEFAULT_TIMER_INTERVAL;
  UpDown2.Position:=DEFAULT_MIN_FILE_AGE;
  TrackBar1.Position:=DEFAULT_PRIORITY;
  CheckBox1.Checked:=DEFAULT_AUTO_START;
  ConfigChanged(Sender);
end;

//Karta Ustawienia strony - Klawisz domyœlne
procedure TConfigForm.Button6Click(Sender: TObject);
begin
  Memo1.Font.Name:=DEFAULT_FONT_NAME;
  Memo1.Font.Size:=DEFAULT_FONT_SIZE;
  Memo1.Font.Charset:=DEFAULT_FONT_CHARSET;
  if Memo1.Font.Charset=238 then begin //East Europe
                                   Memo1.Lines.Strings[0]:=RString(50000);
                                   Memo1.Lines.Strings[1]:=RString(50001);
                                 end
                            else begin
                                   Memo1.Lines.Strings[0]:=RString(51000);
                                   Memo1.Lines.Strings[1]:=RString(51001);
                                 end;
  Memo1.Font.Style:=DEFAULT_FONT_STYLES;
  Label5.Caption:=Format('%s, '+RString(301)+': %d',[DEFAULT_FONT_NAME,DEFAULT_FONT_SIZE]);
  FloatEdit1.Value:=DEFAULT_MARGIN_LEFT;
  FloatEdit2.Value:=DEFAULT_MARGIN_RIGHT;
  FloatEdit3.Value:=DEFAULT_MARGIN_TOP;
  FloatEdit4.Value:=DEFAULT_MARGIN_BOTTOM;
  if (DEFAULT_ORIENTATION=poPortrait) then RadioGroup1.ItemIndex:=0 else RadioGroup1.ItemIndex:=1;
  FloatEdit5.Value:=DEFAULT_LINES_PER_INCH;
  IntEdit1.Value:=DEFAULT_LINES_PER_PAGE;
  ConfigChanged(Sender);
end;

//Karta Format danych - Klawisz domyœlne
procedure TConfigForm.Button7Click(Sender: TObject);
var
  TempSet: TCharCodes;
begin
  TempSet:=DEFAULT_EOP_CODES;
  Edit4.Text:=SetToString(TypeInfo(TCharCodes),TempSet,',','','');
  CheckBox3.Checked:=DEFAULT_SKIP_EMPTY_PAGES;
  CheckBox5.Checked:=DEFAULT_CLIPPER_COMPATIBLE;
  ComboBox1.ItemIndex:=ord(DEFAULT_CODE_PAGE);
  CheckBox4.Checked:=DEFAULT_USE_CUSTOM_CONVERSION_TABLE;
  ConfigChanged(Sender);
end;

//Karta Inne - Klawisz domyœlne
procedure TConfigForm.Button10Click(Sender: TObject);
begin
  Edit8.Text:=DEFAULT_LOGO;
  FloatEdit6.Value:=DEFAULT_LOGO_LEFT;
  FloatEdit7.Value:=DEFAULT_LOGO_TOP;
  CheckBox6.Checked:=DEFAULT_LOGO_1PAGE_ONLY;
  ConfigChanged(Sender);
end;

//Zmiana pozycji suwaka Priorytet
procedure TConfigForm.TrackBar1Change(Sender: TObject);
begin
  Label13.Caption:=PriorityClassNames[TrackBar1.Position];
  ConfigChanged(Sender);
end;

//Zmiana Iloœci linii na cal
procedure TConfigForm.FloatEdit5Change(Sender: TObject);
begin
  if IgnoreOnChange then exit;
  IgnoreOnChange:=true;
  try
    FloatEdit5.Color:=clWindow;
    IntEdit1.Color:=clBtnFace;
    IntEdit1.Value:=0;
    ConfigChanged(Sender);
  finally
    IgnoreOnChange:=false;
  end;
end;

//Zmiana Iloœci linii na stronê
procedure TConfigForm.IntEdit1Change(Sender: TObject);
begin
  if IgnoreOnChange then exit;
  IgnoreOnChange:=true;
  try
    IntEdit1.Color:=clWindow;
    FloatEdit5.Color:=clBtnFace;
    FloatEdit5.Value:=0;
    ConfigChanged(Sender);
  finally
    IgnoreOnChange:=false;
  end;
end;

//Ustaw wartosci domyslne, gdy obie gestosci rowne 0
procedure TConfigForm.ResolveEditValues1(Sender: TObject);
begin
  if (IntEdit1.Value<>0) or (FloatEdit5.Value<>0) then exit;
  FloatEdit5.Value:=DEFAULT_LINES_PER_INCH;
  IntEdit1.Value:=DEFAULT_LINES_PER_PAGE;
end;

//w³¹cz formatowanie
procedure TConfigForm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
  begin
    Edit3.Enabled:=true;
    Edit3.Color:=clWindow;
  end
  else
  begin
    Edit3.Enabled:=false;
    Edit3.Color:=clBtnFace;
  end;
  ConfigChanged(Sender);
end;

//w³¹cz dodatkowe przekodowywanie
procedure TConfigForm.CheckBox4Click(Sender: TObject);
begin
  if CheckBox4.Checked then
  begin
    Label17.Enabled:=true;
    IntEdit2.Enabled:=true;
    IntEdit2.Color:=clWindow;
    IntEdit3.Enabled:=true;
    IntEdit3.Color:=clWindow;
    Label18.Enabled:=true;
    Edit5.Enabled:=true;
    Edit5.Color:=clWindow;
    SpeedButton2.Enabled:=true;
    SpeedButton3.Enabled:=true;
    ListBox1.Enabled:=true;
    ListBox1.Color:=clWindow;
    Button8.Enabled:=true;
    Button9.Enabled:=true;
  end
  else
  begin
    Label17.Enabled:=false;
    IntEdit2.Enabled:=false;
    IntEdit2.Color:=clBtnFace;
    IntEdit3.Enabled:=false;
    IntEdit3.Color:=clBtnFace;
    Label18.Enabled:=false;
    Edit5.Enabled:=false;
    Edit5.Color:=clBtnFace;
    SpeedButton2.Enabled:=false;
    SpeedButton3.Enabled:=false;
    ListBox1.Enabled:=false;
    ListBox1.Color:=clBtnFace;
    Button8.Enabled:=false;
    Button9.Enabled:=false;
  end;
  ConfigChanged(Sender);
end;

//Dodaj pozycjê dodatkowego przekodowania
procedure TConfigForm.SpeedButton2Click(Sender: TObject);
var
  TempItem: TConversionItem;
begin
  TempItem:=TempConversionItems.Add;
  with TempItem do
  begin
    InCode:=IntEdit2.Value;
    OutCode:=IntEdit3.Value;
    Description:=Edit5.Text;
  end;
  ListBox1.Items.AddObject(TempItem.DisplayText,TempItem);
  IntEdit2.Value:=0;
  IntEdit3.Value:=0;
  Edit5.Text:='';
  ListBox1HintIndex:=-1; //reset hint
  ConfigChanged(Sender);
end;

//Usuñ pozycjê dodatkowego przekodowania
procedure TConfigForm.SpeedButton3Click(Sender: TObject);
begin
  with ListBox1 do
  if (ItemIndex<>-1) then
  begin
    with TempConversionItems[ItemIndex] do
    begin
      IntEdit2.Value:=InCode;
      IntEdit3.Value:=OutCode;
      Edit5.Text:=Description;
    end;
    TempConversionItems.Delete(ItemIndex);
    Items.Delete(ItemIndex);
    ListBox1HintIndex:=-1; //reset hint
    ConfigChanged(Sender);
  end;
end;


procedure TConfigForm.SpeedButton4Click(Sender: TObject);
var
  openDialog : TOpenDialog;    // Open dialog variable
begin
  openDialog := TOpenDialog.Create(self);

  if Edit8.Text = '' then openDialog.InitialDir := ExtractFilePath(paramstr(0))
                     else openDialog.InitialDir := ExtractFilePath(Edit8.Text);
  openDialog.Options := [ofFileMustExist];

  openDialog.Filter := RString(405);
  openDialog.FilterIndex := 1;
  if openDialog.Execute then
      Edit8.Text := openDialog.FileName;
  openDialog.Free;
end;

//Show ListBox1 hint
procedure TConfigForm.ListBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Index: integer;
  APoint: TPoint;
begin
  APoint:=Point(X,Y);
  Index:=ListBox1.ItemAtPos(APoint,true);
  if (Index<>-1) then
  begin
    if (ListBox1HintIndex<>Index) then
    begin
      ListBox1HintIndex:=Index;
      Application.CancelHint;
      ListBox1.Hint:=ListBox1.Items[Index];
    end;
  end
  else
    Application.CancelHint;
end;

//Zapisz tabelê konwersji
procedure TConfigForm.Button8Click(Sender: TObject);
var
  FileStream: TFileStream;
begin
  with MainForm do
  if SaveDialog1.Execute then
  begin
    FileStream:=TFileStream.Create(SaveDialog1.FileName,fmCreate or fmShareDenyNone);
    try
      SaveCollectionToStream(TempConversionItems,FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;

//Wczytaj tabelê konwersji
procedure TConfigForm.Button9Click(Sender: TObject);
var
  FileStream: TFileStream;
  i: integer;
begin
  with MainForm do
  if OpenDialog1.Execute then
  begin
    FileStream:=TFileStream.Create(OpenDialog1.FileName,fmOpenRead or fmShareDenyNone);
    try
      LoadCollectionFromStream(FileStream,TempConversionItems);
    finally
      FileStream.Free;
    end;
    with ListBox1.Items do
    begin
      Clear;
      for i:=1 to TempConversionItems.Count do
        AddObject(TempConversionItems[i-1].DisplayText,TempConversionItems[i-1]);
    end;
    ConfigChanged(Sender);
  end;
end;

procedure TConfigForm.LoadLang;
var i,j: integer;
    s: string;
begin
  Button3.Hint := RString(100);
  Button3.Caption := RString(101);
  Button2.Hint := RString(102);
  Button2.Caption := RString(103);
  Button1.Caption := RString(104);
  TabSheet2.Caption  := RString(105);
  GroupBox3.Caption := RString(106);
  Button4.Caption := RString(107);
  GroupBox4.Caption := RString(108);
  Label6.Caption := RString(109);
  Label7.Caption := RString(110);
  Label8.Caption := RString(111);
  Label9.Caption := RString(112);
  RadioGroup1.Caption := RString(113);
  RadioGroup1.Items.strings[0] := RString(114);
  RadioGroup1.Items.strings[1] := RString(115);
  Button6.Caption := RString(116);
  GroupBox5.Caption := RString(117);
  Label10.Caption := RString(118);
  Label14.Caption := RString(119);
  TabSheet1.Caption := RString(120);
  GroupBox1.Caption := RString(121);
  Label1.Caption := RString(122);
  Label2.Caption := RString(123);
  Label11.Caption := RString(124);
  Edit1.Hint := RString(125);
  Edit2.Hint := RString(126);
  Edit3.Hint := RString(127);
  CheckBox2.Caption := RString(128);
  GroupBox2.Caption := RString(129);
  Label3.Caption := RString(130);
  Label4.Caption := RString(131);
  Label12.Caption := RString(132);
  Edit6.Hint := RString(133);
  Edit7.Hint := RString(134);
  CheckBox1.Hint := RString(135);
  CheckBox1.Caption := RString(136);
  Button5.Caption := RString(116);
  TabSheet3.Caption := RString(138);
  GroupBox6.Caption := RString(139);
  Label15.Caption := RString(140);
  CheckBox3.Caption := RString(141);
  CheckBox5.Hint := RString(142);
  CheckBox5.Caption := RString(143);
  GroupBox7.Caption := RString(144);
  Label16.Caption := RString(145);
  SpeedButton2.Hint := RString(146);
  SpeedButton3.Hint := RString(147);
  Label17.Caption := RString(148);
  Label18.Caption := RString(149);
  CheckBox4.Caption := RString(150);
  Button8.Caption := RString(151);
  Button9.Caption := RString(152);
  Button7.Caption := RString(116);
  TabSheet4.Caption := RString(153);
  GroupBox8.Caption := RString(154);
  Label19.Caption := RString(155);
  Label20.Caption := RString(156);
  Label21.Caption := RString(157);
  Edit8.Hint := RString(158);
  CheckBox6.Hint := RString(159);
  CheckBox6.Caption := RString(160);
  Button10.Caption := RString(116);
  IntEdit3.Hint :=  RString(161);
  Button11.Caption := RString(000);

  PriorityClassNames[MinPriorityClass+0] := RString(200);
  PriorityClassNames[MinPriorityClass+1] := RString(201);
  PriorityClassNames[MinPriorityClass+2] := RString(202);
  PriorityClassNames[MinPriorityClass+3] := RString(203);
  Label13.Caption:=PriorityClassNames[ConfigData.Priority];

  s := Label5.Caption;
  i := Pos(',',s);
  j := Pos(':',s);
  if (i>0) and (j>0) and (j>(i+1)) then begin
     Delete(s, i+2, j-i-2);
     Insert(RString(301),s ,i+2);
     Label5.Caption:= s;
  end;

end;

end.

