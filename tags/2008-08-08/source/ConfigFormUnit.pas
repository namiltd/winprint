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

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, Spin, ExtCtrls, NumEdit, Printers, ConversionUnit;

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
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
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
    FloatEdit1: TFloatEdit;
    FloatEdit2: TFloatEdit;
    FloatEdit3: TFloatEdit;
    FloatEdit4: TFloatEdit;
    Edit3: TEdit;
    Label11: TLabel;
    Button5: TButton;
    Button6: TButton;
    Label12: TLabel;
    TrackBar1: TTrackBar;
    Label13: TLabel;
    GroupBox5: TGroupBox;
    Label10: TLabel;
    FloatEdit5: TFloatEdit;
    IntEdit1: TIntEdit;
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
    IntEdit2: TIntEdit;
    IntEdit3: TIntEdit;
    Edit5: TEdit;
    CheckBox4: TCheckBox;
    Label17: TLabel;
    Label18: TLabel;
    Button8: TButton;
    Button9: TButton;
    CheckBox5: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  private
    { Private declarations }
    ListBox1HintIndex: integer;
    IgnoreOnChange: boolean;
    TempConversionItems: TConversionItems;
    procedure ReadConfig;
    procedure WriteConfig;
  public
    { Public declarations }
    ConfigData: TConfigData;
  published
    procedure ConfigChanged(Sender: TObject);
    procedure ResolveEditValues1(Sender: TObject);
  end;

const
  CharCodeLow = low(TCharCode);
  CharCodeHigh = high(TCharCode);

var
  ConfigForm: TConfigForm;

implementation

uses
  MainFormUnit, FileCtrl, Registry,
  SetString //dla SetToString
  ;

{$R *.DFM}

const
  DEFAULT_INPUT_FILES_DIR = '';
  DEFAULT_INPUT_FILES_MASK = '*.txt';
  DEFAULT_FORMAT_FILE_EXTENSION = 'ini';
  DEFAULT_ENABLE_FORMATTING = true;
  DEFAULT_TIMER_INTERVAL = 500;
  DEFAULT_MIN_FILE_AGE = 500;
  DEFAULT_PRIORITY = 2; //real-time priority
  DEFAULT_AUTO_START = true;
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
  DEFAULT_CODE_PAGE = cpLAT;
  DEFAULT_USE_CUSTOM_CONVERSION_TABLE = false;

const
  MinPriorityClass = -1;
  MaxPriorityClass = 2;

  PriorityClassNames: array[MinPriorityClass..MaxPriorityClass] of string =
        ('bezczynny',
         'normalny',
         'wysoki',
         'czasu rzeczywistego');

  PriorityClassValues: array[MinPriorityClass..MaxPriorityClass] of cardinal =
        (IDLE_PRIORITY_CLASS,
         NORMAL_PRIORITY_CLASS,
         HIGH_PRIORITY_CLASS,
         REALTIME_PRIORITY_CLASS);


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
  with MainForm.CEVersionInfo1 do
    Caption:=ProductName+' '+FileVersion;

//  MainForm.Xml10n1.Load;
//  MainForm.Xml10n1.XmlToForm(ConfigForm);

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
  if SelectDirectory('Wska¿ folder plików.','\',TempDir) then
  begin
    TempDir:=IncludeTrailingBackslash(TempDir);
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
  i: integer;
  MemStream: TMemoryStream;
begin
  with ConfigData do
  begin
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
            if (TimerInterval<SpinEdit1.MinValue) or (TimerInterval>SpinEdit1.MaxValue) then TimerInterval:=DEFAULT_TIMER_INTERVAL;
          except
            TimerInterval:=DEFAULT_TIMER_INTERVAL;
          end;
          try
            MinFileAge:=ReadInteger('MinFileAge');
            if (MinFileAge<SpinEdit2.MinValue) or (MinFileAge>SpinEdit2.MaxValue) then MinFileAge:=DEFAULT_MIN_FILE_AGE;
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
        end;
      finally
        CloseKey;
      end;

    finally
      Registry.Free;
    end;

    Edit1.Text:=InputFilesDir;
    Edit2.Text:=InputFilesMask;
    Edit3.Text:=FormatFileExtension;
    CheckBox2.Checked:=EnableFormatting;
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
    SpinEdit1.Value:=TimerInterval;
    SpinEdit2.Value:=MinFileAge;
    TrackBar1.Position:=Priority;
    Label13.Caption:=PriorityClassNames[Priority];
    CheckBox1.Checked:=AutoStart;
    Memo1.Font.Name:=FontName;
    Memo1.Font.Size:=FontSize;
    Memo1.Font.Charset:=FontCharset;
    Memo1.Font.Style:=FontStyles;
    Label5.Caption:=Format('%s, rozmiar: %d',[FontName,FontSize]);
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
        Items.Append(CodePageNames[TCodePage(i)]);
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

    {**********************}
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
  Styles: TFontStyles;
  TempSet: TCharCodes;
  MemStream: TMemoryStream;
begin
  Registry:=TRegistry.Create;
  with Registry do
  try
    RootKey:=HKEY_LOCAL_MACHINE;
    try
      if OpenKey('Software\GNU\WinPrint',true) then
      begin
        WriteString('InputFilesDir',Edit1.Text);
        WriteString('InputFilesMask',Edit2.Text);
        WriteString('FormatFileExtension',Edit3.Text);
        WriteBool('EnableFormatting',CheckBox2.Checked);
        WriteInteger('TimerInterval',SpinEdit1.Value);
        WriteInteger('MinFileAge',SpinEdit2.Value);
        WriteInteger('Priority',TrackBar1.Position);
        WriteBool('AutoStart',CheckBox1.Checked);
        WriteString('FontName',Memo1.Font.Name);
        WriteInteger('FontSize',Memo1.Font.Size);
        WriteInteger('FontCharset',Memo1.Font.Charset);
        Styles:=Memo1.Font.Style; WriteString('FontStyles',SetToString(TypeInfo(TFontStyles),Styles));
        WriteFloat('MarginLeft',FloatEdit1.Value);
        WriteFloat('MarginRight',FloatEdit2.Value);
        WriteFloat('MarginTop',FloatEdit3.Value);
        WriteFloat('MarginBottom',FloatEdit4.Value);
        WriteInteger('Orientation',RadioGroup1.ItemIndex);
        WriteFloat('LinesPerInch',FloatEdit5.Value);
        WriteInteger('LinesPerPage',IntEdit1.Value);
        StringToSet(Edit4.Text,TypeInfo(TCharCodes),TempSet); WriteString('EOPCodes',SetToString(TypeInfo(TCharCodes),TempSet));
        WriteBool('SkipEmptyPages',CheckBox3.Checked);
        WriteBool('ClipperCompatible',CheckBox5.Checked);
        WriteString('CodePage',OrdToString(TypeInfo(TCodePage),ComboBox1.ItemIndex));
        WriteBool('UseCustomConversionTable',CheckBox4.Checked);
        ConfigData.ConversionItems.Assign(TempConversionItems);
        MemStream:=TMemoryStream.Create;
        try
          SaveCollectionToStream(ConfigData.ConversionItems,MemStream);
          Registry.WriteBinaryData('CustomConversionTable',MemStream.Memory^,MemStream.Size);
        finally
          MemStream.Free;
        end;
      end;
    finally
      CloseKey;
    end;
    RootKey:=HKEY_LOCAL_MACHINE;
    try
      if OpenKey('Software\Microsoft\Windows\CurrentVersion\Run',true) then
      if CheckBox1.Checked then WriteString('WinPrint',Application.ExeName) else WriteString('WinPrint','');
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

procedure TConfigForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ReadConfig;
end;


//Klawisz OK
procedure TConfigForm.Button1Click(Sender: TObject);
begin
  WriteConfig;
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
      Label5.Caption:=Format('%s, rozmiar: %d',[Memo1.Font.Name,Memo1.Font.Size]);
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
  SpinEdit1.Value:=DEFAULT_TIMER_INTERVAL;
  SpinEdit2.Value:=DEFAULT_MIN_FILE_AGE;
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
  Memo1.Font.Style:=DEFAULT_FONT_STYLES;
  Label5.Caption:=Format('%s, rozmiar: %d',[DEFAULT_FONT_NAME,DEFAULT_FONT_SIZE]);
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


end.

