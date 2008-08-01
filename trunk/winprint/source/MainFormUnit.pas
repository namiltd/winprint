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

unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Trayicon, ExtCtrls, ImgList, ConfigFormUnit, CEVersionInfo, Xml10n;

type
  TMainForm = class(TForm)
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    Konfiguracja1: TMenuItem;
    N1: TMenuItem;
    Zakocz1: TMenuItem;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    FontDialog1: TFontDialog;
    Timer2: TTimer;
    SaveDialog1: TSaveDialog;
    CEVersionInfo1: TCEVersionInfo;
    Xml10n1: TXml10n;
    procedure Zakocz1Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure Konfiguracja1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    SearchRec : TSearchRec;
    MustExit: boolean;
    TrayIconIndex: integer;
    function TestFile: boolean;
    procedure ProcessFile;
    procedure ProcessFormatFile(FileName: string;
      var ConfigData: TConfigData);
    procedure AppException(Sender: TObject; E: Exception);
  public
    { Public declarations }
    StopTrayIconTimer: boolean;
    ZeroTrayIconIndex: boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses
  FileCtrl, ShellAPI, Math, PrintStringsUnit, Printers, MyStrings, ConversionUnit;

{$R *.DFM}

var
  Atom1: TAtom;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Icon: TIcon;
begin
  Application.OnException:=AppException;
  Icon:=TIcon.Create;
  try
    Icon.Handle:=LoadIcon(hInstance,'tray_icon_0');
    TrayIcon1.Icon:=Icon;
  finally
    Icon.Free;
  end;
  with CEVersionInfo1 do
  begin
    Application.Title:=ProductName+' '+ProductVersion;
    TrayIcon1.ToolTip:=Application.Title;
    if (GlobalFindAtom(PChar(CompanyName+' '+ProductName))<>0) then
    if (Application.MessageBox(PChar(
      'Program '+ProductName+' jest ju� uruchomiony.'+#13#10+
      'Powt�rne uruchomienie programu mo�e spowodowa� b��dy w jego dzia�aniu.'+#13#10+
      #13#10+
      'Czy chcesz uruchomi� ten program ponownie?'),
      'Ostrze�enie',
      MB_YESNO+MB_ICONWARNING+MB_DEFBUTTON2+MB_SYSTEMMODAL)=IDNO) then
        Halt; //zako�cz program
    Atom1:=GlobalAddAtom(PChar(CompanyName+' '+ProductName));
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GlobalDeleteAtom(Atom1);
end;

procedure TMainForm.Zakocz1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.TrayIcon1DblClick(Sender: TObject);
begin
  if (ConfigForm=nil) then exit;
  if not ConfigForm.Showing then
  begin
    ConfigForm.Show;
    Application.BringToFront;
  end
  else
    ConfigForm.Hide;
end;

procedure TMainForm.Konfiguracja1Click(Sender: TObject);
begin
  if (ConfigForm=nil) then exit;
  ConfigForm.Show;
  Application.BringToFront;
end;

//////////////////////////////////////////////////////////////
//Funkcje konwersji r�nych format�w reprezentacji daty/czasu

function FileTimeToInt64(AFileTime : FILETIME) : Int64;
var
  TempResult : LARGE_INTEGER;
begin
  TempResult.LowPart:=AFileTime.dwLowDateTime;
  TempResult.HighPart:=AFileTime.dwHighDateTime;
  result:=TempResult.QuadPart;
end;

function SystemTimeToInt64(ASystemTime : SYSTEMTIME) : Int64;
var
  TempFileTime : FILETIME;
begin
  SystemTimeToFileTime(ASystemTime,TempFileTime);
  result:=FileTimeToInt64(TempFileTime);
end;

function Int64ToFileTime(AInt64 : Int64) : FILETIME;
var
  TempResult : LARGE_INTEGER;
begin
  TempResult.QuadPart:=AInt64;
  result.dwLowDateTime:=TempResult.LowPart;
  result.dwHighDateTime:=TempResult.HighPart;
end;

function Int64ToSystemTime(AInt64 : Int64) : SYSTEMTIME;
var
  TempFileTime : FILETIME;
begin
  TempFileTime:=Int64ToFileTime(AInt64);
  FileTimeToSystemTime(TempFileTime,result);
end;
//////////////////////////////////////////////////////////////


function TMainForm.TestFile;
var
  NowSystemTime : SYSTEMTIME;
begin
  GetSystemTime(NowSystemTime);

  //10*1000*1000 = 1 sekunda wyra�ona w setkach nanosekund
  with SearchRec.FindData do
    result:=((SystemTimeToInt64(NowSystemTime)-Int64(ConfigForm.ConfigData.MinFileAge)*10*1000)>max(FileTimeToInt64(ftCreationTime),FileTimeToInt64(ftLastWriteTime))) and
            ((dwFileAttributes and FILE_ATTRIBUTE_READONLY)=0);
end;


function ExchangeSubstring(AString: string;
                           AFrom: string;
                           ATo: string): string;
var
  TempString: string;
  TempPos: integer;
begin
  TempString:='';
  while length(AString)>0 do
  begin
    TempPos:=pos(AFrom,AString);
    if TempPos>0 then
    begin
      TempString:=concat(TempString,copy(AString,1,TempPos-1),ATo);
      AString:=copy(AString,TempPos+length(AFrom),length(AString));
    end
    else
    begin
      TempString:=concat(TempString,AString);
      AString:='';
    end;
  end;
  result:=TempString;
end;

procedure TMainForm.ProcessFormatFile(FileName: string; var ConfigData: TConfigData);
var
  TempFile: TextFile;
  TempLine,LeftString: string;
begin
  if not FileExists(FileName) then exit;

  TempLine:='';

  AssignFile(TempFile,FileName);
  reset(TempFile);
  try
    readln(TempFile); //pomin smieci z clippera w 1-ej linii
    readln(TempFile,TempLine);
  finally
    CloseFile(TempFile);
  end;

  TempLine:=trim(TempLine);
  if (length(TempLine)=0) then exit;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.FontSize:=StrToInt(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj�tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.Orientation:=TPrinterOrientation(StrToInt(trim(LeftString)));
  except
    on EConvertError do; //odrzuc wyj�tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginLeft:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj�tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginRight:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj�tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginTop:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj�tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginBottom:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj�tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.LinesPerPage:=StrToInt(trim(LeftString));
    ConfigData.LinesPerInch:=0;
  except
    on EConvertError do; //odrzuc wyj�tki konwersji
  end;
end;

procedure TMainForm.ProcessFile;
var
  InputFileName: string;
  FormatFileName: string;
  BadFileName: string;
  StringList: TStringList;
  TempFont: TFont;
  TempConfigData: TConfigData;
begin
  ZeroTrayIconIndex:=true;

  Timer2.Enabled:=true;
  try
    InputFileName:=ConfigForm.ConfigData.InputFilesDir+SearchRec.Name;
    StringList:=TStringList.Create;
    try

      ReadANDConvert(ConfigForm.ConfigData.CodePage,InputFileName,StringList,ConfigForm.ConfigData.UseCustomConversionTable,ConfigForm.ConfigData.ConversionItems); //Reads from file and change CodePage

      TempFont:=TFont.Create;
      try
        TempConfigData:=ConfigForm.ConfigData;

        if ConfigForm.ConfigData.ClipperCompatible and (StringList.Count>0) and (length(StringList.Strings[0])=0) then
          StringList.Delete(0);

        if ConfigForm.ConfigData.EnableFormatting then
        begin
          FormatFileName:=ChangeFileExt(InputFileName,'.'+ConfigForm.ConfigData.FormatFileExtension);
          ProcessFormatFile(FormatFileName,TempConfigData);
        end;

        TempFont.Name:=TempConfigData.FontName;
        TempFont.Charset:=TempConfigData.FontCharset;
        TempFont.Size:=TempConfigData.FontSize;
        TempFont.Style:=TempConfigData.FontStyles;

        try
          //procedure drukujaca StringList
          PrintStrings('Dokument programu WinPrint - '+SearchRec.Name,
                       StringList,
                       cMILTOINCH*TempConfigData.MarginLeft,
                       cMILTOINCH*TempConfigData.MarginRight,
                       cMILTOINCH*TempConfigData.MarginTop,
                       cMILTOINCH*TempConfigData.MarginBottom,
                       TempConfigData.Orientation,
                       TempConfigData.LinesPerInch,
                       TempConfigData.LinesPerPage,
                       TempConfigData.SkipEmptyPages,
                       TempFont,
                       false,
                       nil,nil);
        except
          //wyj�tek podczas drukowania plik nie wydrukowany - nie usuwaj pliku tylko
          //zmie� rozszerzenie na .bad lub skasuj
          BadFileName:=ChangeFileExt(InputFileName,'.bad');
          if FileExists(BadFileName) then DeleteFile(BadFileName);
          if not RenameFile(InputFileName,BadFileName) then //najpierw pr�buj zmienic rozszerzenie na .bad
          if not DeleteFile(InputFileName) then //na koniec probuj skasowac plik
          begin
            //krytyczny b��d podczas archiwizowania b��dnego pliku wydruku - zako�cz aplikacje
            MustExit:=true;
            raise EInOutError.Create('Wyst�pi� krytyczny b��d podczas archiwizowania b��dnego pliku wydruku'+#10#13+'program WinPrint zostanie zako�czony');
          end;
          //re-raise inne wyj�tki powstale przy wydruku
          raise;
        end;

        //plik zosta� wydrukowany pomy�lnie - probuj skasowac plik
        if not DeleteFile(InputFileName) then
        begin
          //krytyczny b��d podczas usuwania pliku z kolejki - zako�cz aplikacje
          MustExit:=true;
          raise EInOutError.Create('Wyst�pi� krytyczny b��d podczas usuwania pliku wydruku z kolejki'+#10#13+'program WinPrint zostanie zako�czony');
        end;

        //jezelio w��czono formatowanie pr�buj usun�� r�wnie� plik formatuj�cy
        if ConfigForm.ConfigData.EnableFormatting then
        if FileExists(FormatFileName) then
        if not DeleteFile(FormatFileName) then
        begin
          //krytyczny b��d podczas usuwania pliku formatuj�cego z kolejki - zako�cz aplikacje
          MustExit:=true;
          raise EInOutError.Create('Wyst�pi� krytyczny b��d podczas usuwania pliku formatuj�cego z kolejki'+#10#13+'program WinPrint zostanie zako�czony');
        end;

      finally
        TempFont.Free;
      end;

    finally
      StringList.Free;
    end;

  finally
    StopTrayIconTimer:=true;
  end;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  Processed: boolean;
begin
  Timer1.Enabled:=false;
  if not MustExit then
  try
    with ConfigForm.ConfigData do
    if (InputFilesDir<>'') and DirectoryExists(InputFilesDir) then
    try
      Processed:=false;
      if FindFirst(InputFilesDir+InputFilesMask,0,SearchRec)=0 then
      if TestFile then
      begin
        ProcessFile;
        Processed:=true;
      end;
      while (not Processed) and (FindNext(SearchRec)=0) do
      if TestFile then
      begin
        ProcessFile;
        Processed:=true;
      end;
    finally
      FindClose(SearchRec);
    end;
  finally
    Timer1.Enabled:=true;
  end;
end;

procedure TMainForm.AppException(Sender: TObject; E: Exception);
begin
  Application.ShowException(E);
  if MustExit then Application.Terminate;
end;

procedure TMainForm.Timer2Timer(Sender: TObject);
var
  Icon: TIcon;
begin
  Timer2.Enabled:=false;
  try
    Icon:=TIcon.Create;
    try
      if ((TrayIconIndex mod 16)=0) and ZeroTrayIconIndex then
      begin
        TrayIconIndex:=0;
        ZeroTrayIconIndex:=false;
      end;
      inc(TrayIconIndex);
      Icon.Handle:=LoadIcon(hInstance,PChar('tray_icon_'+IntToStr(TrayIconIndex mod 16)));
      SendMessage(ConfigForm.Handle, WM_SETICON, 1, Icon.Handle);
      TrayIcon1.Icon:=Icon;
    finally
      Icon.Free;
    end;
  finally
    if (TrayIconIndex<10*16) then
      Timer2.Enabled:=true
    else
    begin
      TrayIconIndex:=0;
      if not StopTrayIconTimer then
        Timer2.Enabled:=true
      else
        StopTrayIconTimer:=false;
    end;
  end;
end;

end.
