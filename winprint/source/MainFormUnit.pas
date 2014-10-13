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
  Menus, Trayicon, ExtCtrls, ImgList, ConfigFormUnit, CEVersionInfo, NumEdit;

type
  TMainForm = class(TForm)
    PopupMenu1: TPopupMenu;
    Konfiguracja1: TMenuItem;
    N1: TMenuItem;
    Zakoncz1: TMenuItem;
    Info1: TMenuItem;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    FontDialog1: TFontDialog;
    Timer2: TTimer;
    SaveDialog1: TSaveDialog;
    procedure Zakoncz1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
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
    TrayIcon1: TTrayIcon2;
    function TestFile: boolean;
    function ProcessFile: boolean;
    procedure ProcessFormatFile(FileName: string;
      var ConfigData: TConfigData);
    procedure AppException(Sender: TObject; E: Exception);
  public
    { Public declarations }
    StopTrayIconTimer: boolean;
    ZeroTrayIconIndex: boolean;
    CEVersionInfo1: TCEVersionInfo;
    procedure LoadLang;
  end;

var
  MainForm: TMainForm;

implementation

uses
  ShellAPI, Math, PrintStringsUnit, Printers, MyStrings, ConversionUnit;

{$R *.DFM}

//var
//  Atom1: TAtom;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Icon: TIcon;
  lpTargetPath :array[0..2048] of char;
  nazwa: string;
  osVerInfo: TOSVersionInfo;

begin

{info  for example: Winprint.exe /LPT1}

 if ParamCount>0 then begin
    osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    if GetVersionEx(osVerInfo) { XP or above }
      and (osVerInfo.dwPlatformId=VER_PLATFORM_WIN32_NT)
      and ( ((osVerInfo.dwMajorVersion=5) and (osVerInfo.dwMinorVersion>=1)) or (osVerInfo.dwMajorVersion>5) )
    then begin
    	nazwa:=UpperCase(ParamStr(1));
    	if ((Length(nazwa)=4)and(nazwa[1]='/')and(nazwa[2]='P')and(nazwa[3]='R')and(nazwa[4]='N'))
            or ((Length(nazwa)=5)and
            (((nazwa[1]='/')and(nazwa[2]='L')and(nazwa[3]='P')and(nazwa[4]='T')) or ((nazwa[1]='/')and(nazwa[2]='C')and(nazwa[3]='O')and(nazwa[4]='M'))) and
            ((nazwa[5]>='1')and(nazwa[5]<='9'))) then
            begin
                lpTargetPath[0] := #0;
                QueryDosDevice(PChar(nazwa)+1, @lpTargetPath[0], sizeof(lpTargetPath));
                Application.MessageBox(@lpTargetPath[0],PChar(nazwa)+1,MB_OK);
            end;
    end;
    Halt;
 end;

 if (GetUserDefaultLangID and $3ff)=LANG_POLISH
     then LANG := 60000
     else if (GetUserDefaultLangID and $3ff)=LANG_CZECH
               then LANG := 62000
               else LANG := 61000;


 If not LoadUnicode then begin
    Application.MessageBox(PChar(RString(509)),
      PChar(RString(508)),
      MB_ICONERROR+MB_SYSTEMMODAL);
    halt;
 end;


 {komponenty dynamiczne}
  CEVersionInfo1:=TCEVersionInfo.Create(self);
  TrayIcon1:=TTrayIcon2.Create(self);
  with TrayIcon1 do begin
          Active := True;
          ShowDesigning := False;
          ShowApp := False;
          OnDblClick := TrayIcon1DblClick;
          PopupMenu := PopupMenu1;
          //Left := 32;
          //Top := 16;
  end;
  PROGRAMNAME:=StringReplace(ExtractFileName(Paramstr(0)),'.exe','',[rfReplaceAll, rfIgnoreCase]);
  if PROGRAMNAME='' then PROGRAMNAME:='WinPrint';

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
    Application.Title:=PROGRAMNAME+' - '+ProductName+' '+ProductVersion;
    TrayIcon1.ToolTip:=Application.Title;
    CreateFileMapping(THANDLE($FFFFFFFF), nil, PAGE_READONLY, 0, 32, PChar(ProductName+'-'+PROGRAMNAME));
    if (GetLastError = ERROR_ALREADY_EXISTS) then
//    if (GlobalFindAtom(PChar(CompanyName+' '+ProductName))<>0) then
    if (Application.MessageBox(PChar(
      'Program '+PROGRAMNAME+' '+RString(500)),
      PChar(RString(501)),

      MB_YESNO+MB_ICONWARNING+MB_DEFBUTTON2+MB_SYSTEMMODAL)=IDNO) then
        Halt; //zakoñcz program
//    Atom1:=GlobalAddAtom(PChar(CompanyName+' '+ProductName));
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
//  GlobalDeleteAtom(Atom1);
end;

procedure TMainForm.Zakoncz1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Info1Click(Sender: TObject);
var lpTargetPath :array[0..2048] of char;
begin
  if ConfigForm.ConfigData.PortCapturing=1 then begin
    lpTargetPath[0] := #0;
    QueryDosDevice(PChar(ConfigForm.ConfigData.InputFilesMask), @lpTargetPath[0], sizeof(lpTargetPath));
    Application.MessageBox(PChar(RString(700)+ ': ' + ConfigForm.ConfigData.InputFilesMask +
    #13 + #13 + RString(701) + ': ' + String(PChar(@lpTargetPath[0])))
    ,PChar(ConfigForm.Caption),MB_OK);
  end else begin
    Application.MessageBox(PChar(RString(702) + ': ' + ConfigForm.ConfigData.InputFilesMask +
    #13 + #13+ RString(703) + ': ' + ConfigForm.ConfigData.InputFilesDir)
    ,PChar(ConfigForm.Caption),MB_OK);
  end;
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
//Funkcje konwersji ró¿nych formatów reprezentacji daty/czasu

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
  TestFS: TFileStream;
  TestFileName : string;
  TestResult : Boolean;
  FNLength : integer;
begin
  FNLength := Length(SearchRec.Name);
  if (FNLength<1) or (SearchRec.Name[FNLength]='~') then //Nazwa tymczasowa lub archiwalna po b³êdzie
  begin
    result:=false;
    exit;
  end;
  
  GetSystemTime(NowSystemTime);

  if ((not ConfigForm.ConfigData.IgnoreEmptyFiles) and (ConfigForm.ConfigData.PortCapturing<>1))//Not IgnoreEmptyFiles only if not port capturing
      or (SearchRec.FindData.nFileSizeHigh<>0)
      or (SearchRec.FindData.nFileSizeLow<>0) then begin
    TestResult := true;
    TestFileName:=ConfigForm.ConfigData.InputFilesDir+SearchRec.Name;
    try
    // Open for read test
       TestFS:=TFileStream.Create(TestFileName,fmOpenRead or fmShareDenyNone);
       TestFS.Free;
    except
       on EFOpenError do TestResult:= false;
    end;
  end else TestResult := false;

  //10*1000*1000 = 1 sekunda wyra¿ona w setkach nanosekund
  with SearchRec.FindData do
      result:=TestResult and ((SystemTimeToInt64(NowSystemTime)-Int64(ConfigForm.ConfigData.MinFileAge)*10*1000)>max(FileTimeToInt64(ftCreationTime),FileTimeToInt64(ftLastWriteTime))) and
            ((dwFileAttributes and FILE_ATTRIBUTE_READONLY)=0);
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
    on EConvertError do; //odrzuc wyj¹tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.Orientation:=TPrinterOrientation(StrToInt(trim(LeftString)));
  except
    on EConvertError do; //odrzuc wyj¹tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginLeft:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj¹tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginRight:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj¹tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginTop:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj¹tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.MarginBottom:=StrToFloat(trim(LeftString));
  except
    on EConvertError do; //odrzuc wyj¹tki konwersji
  end;

  SplitLeft(TempLine,' ',LeftString,TempLine);
  if (LeftString<>'') then
  try
    ConfigData.LinesPerPage:=StrToInt(trim(LeftString));
    ConfigData.LinesPerInch:=0;
  except
    on EConvertError do; //odrzuc wyj¹tki konwersji
  end;
end;

function TMainForm.ProcessFile;
var
  InputFileName: string;
  FormatFileName: string;
  BadFileName: string;
  TmpFileName: string;
  StringList: TStringList;
  TempFont: TFont;
  TempConfigData: TConfigData;
  Bitmap : TBitmap;
  kopii : Integer;
  Image : TmemoryStream;
  ImageSize : Cardinal;
  Info : PBitmapInfo;
  InfoSize : Cardinal;

  Header : Record
    FileHeader : tBitmapFileHeader;
    InfoHeader : tBitmapInfoHeader;
  end;
  Bmp : File;
  dpmok : Boolean;
  Count : Integer;
  xDPM : DWORD;
  yDPM : DWORD;
  HandleToFile: THandle;

begin
  result:=true;
  try
    InputFileName:=ConfigForm.ConfigData.InputFilesDir+SearchRec.Name;

    TmpFileName:=ChangeFileExt(InputFileName,'.tmp~');
    if FileExists(TmpFileName) then DeleteFile(TmpFileName);
    if not RenameFile(InputFileName,TmpFileName) then //próbuj zmienic rozszerzenie na .tmp~
    begin
        //krytyczny b³¹d podczas zmiany nazwy pliku wydruku na tymczasow¹ - zakoñcz aplikacje
        //MustExit:=true;
        //raise EInOutError.Create(RString(505));
        result:=false;
        exit;
    end
    else begin
        ZeroTrayIconIndex:=true;

        Timer2.Enabled:=true;

        if ConfigForm.ConfigData.PortCapturing=1 then begin
            HandleToFile:=CreateFile(PChar(InputFileName), GENERIC_WRITE, 0, NIL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
            if HandleToFile = INVALID_HANDLE_VALUE then
            begin
                //krytyczny b³¹d podczas tworzenia pliku spoolera - zakoñcz aplikacje
                MustExit:=true;
                raise EInOutError.Create(RString(506)); 
            end
            else CloseHandle(HandleToFile);
        end;
        InputFileName := TmpFileName;
    end;
 

    StringList:=TStringList.Create;
    try
      ReadANDConvert(ConfigForm.ConfigData.CodePage, InputFileName,StringList,ConfigForm.ConfigData.UseCustomConversionTable,ConfigForm.ConfigData.ConversionItems); //Reads from file and change CodePage
      TempFont:=TFont.Create;
      try
        TempConfigData:=ConfigForm.ConfigData;

        if ConfigForm.ConfigData.ClipperCompatible and (StringList.Count>0) and ((length(StringList.Strings[0])=0) or ((length(StringList.Strings[0])=1)and(StringList.Strings[0][1]=#13)))  then
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

        Bitmap:=TBitmap.Create;
        Image := nil;
        Info := nil;
        if TempConfigData.Logo<>'' then begin
          dpmok := True;
          xDPM:=0;
          yDPM:=0;
          AssignFile(Bmp, TempConfigData.Logo);
          try
          Reset(Bmp, 1);
          except
            dpmok := False;
          end;
          if dpmok then begin
            BlockRead(Bmp, Header, sizeof(Header), Count);
            if Count = sizeof(Header) then
            begin
              xDPM := Header.InfoHeader.biXPelsPerMeter;
              yDPM := Header.InfoHeader.biYPelsPerMeter;
              if (xDPM=0) or (yDPM=0) then begin
                dpmok:=false;
                xDPM:=0;
                yDPM:=0;
              end;
            end
            else dpmok:=false;
            CloseFile(Bmp);
          end;
          try
            Bitmap.LoadFromFile(TempConfigData.Logo);
            if Bitmap.Empty = false then begin
              GetDIBSizes(Bitmap.Handle, InfoSize, ImageSize);
              try
                Info := AllocMem(InfoSize);
                try
                  Image := Tmemorystream.Create;
                  try
                    Image.SetSize(Imagesize);
                    try
                      if not GetDIB(Bitmap.Handle, Bitmap.Palette, Info^, Image.Memory^)
                      then begin
                        FreeMem(Info, InfoSize);
                        Image.free;
                        Image := nil;
                        Info := nil;
                      end else
                      if dpmok then begin
                        Info^.bmiHeader.biXPelsPerMeter:=xDPM;
                        Info^.bmiHeader.biYPelsPerMeter:=yDPM;
                      end;
                    except
                      FreeMem(Info, InfoSize);
                      Image.free;
                      Image := nil;
                      Info := nil;
                    end;
                  except
                    FreeMem(Info, InfoSize);
                    Image.free;
                    Image := nil;
                    Info := nil;
                  end;
                except
                  FreeMem(Info, InfoSize);
                  Image := nil;
                  Info := nil;
                end;
              except
                Info := nil;
                Image := nil;
              end;
            end;
            except
          end;
        end;
        kopii := TempConfigData.NumberOfCopies;
        if kopii<1 then kopii:=1
        else if kopii>99 then kopii:=99;
        while (kopii>0) do begin
          try
            //procedure drukujaca StringList
            PrintStrings('Dokument programu '+PROGRAMNAME+' - '+SearchRec.Name,
                       StringList,
                       CodePageInfo[ConfigForm.ConfigData.CodePage].CpNr,
                       TempConfigData.PrinterId,
                       cMILTOINCH*TempConfigData.MarginLeft,
                       cMILTOINCH*TempConfigData.MarginRight,
                       cMILTOINCH*TempConfigData.MarginTop,
                       cMILTOINCH*TempConfigData.MarginBottom,
                       TempConfigData.Orientation,
                       TempConfigData.LinesPerInch,
                       TempConfigData.LinesPerPage,
                       TempConfigData.SkipEmptyPages,
                       TempFont,
                       Info,
                       Image,
                       cMILTOINCH*TempConfigData.LogoLeft,
                       cMILTOINCH*TempConfigData.LogoTop,
                       TempConfigData.Logo1PageOnly,
                       TempConfigData.EOPCodes,
                       false,
                       nil,nil);
          except
      		//wyj¹tek podczas drukowania plik nie wydrukowany
            //zmieñ rozszerzenie na .bad~ lub jesli nie chce zmienic skasuj plik
            BadFileName:=ChangeFileExt(InputFileName,'.bad~');
            if FileExists(BadFileName) then DeleteFile(BadFileName);
            if not RenameFile(InputFileName,BadFileName) then //najpierw próbuj zmienic rozszerzenie na .bad~
            if not DeleteFile(InputFileName) then //na koniec probuj skasowac plik
            begin
              //krytyczny b³¹d podczas archiwizowania b³êdnego pliku wydruku - zakoñcz aplikacje
              MustExit:=true;
              raise EInOutError.Create(RString(502));
            end;
            //re-raise inne wyj¹tki powstale przy wydruku
            raise;
            break;
          end;
          dec(kopii);
        end;
        if (Image<>nil) and (Info<>nil) then begin
          FreeMem(Info, InfoSize);
          Image.free;
//          Image := nil;
//          Info := nil;
        end;
        Bitmap.free;

        if not DeleteFile(InputFileName) then //plik zosta³ wydrukowany pomyœlnie - probuj skasowac plik
        begin
            //krytyczny b³¹d podczas usuwania pliku z kolejki - zakoñcz aplikacje
            MustExit:=true;
            raise EInOutError.Create(RString(503));
        end;

        //jezeli w³¹czono formatowanie próbuj usun¹æ plik formatuj¹cy
        if ConfigForm.ConfigData.EnableFormatting then
        if FileExists(FormatFileName) then
        if not DeleteFile(FormatFileName) then
        begin
            //krytyczny b³¹d podczas usuwania pliku formatuj¹cego z kolejki - zakoñcz aplikacje
            MustExit:=true;
            raise EInOutError.Create(RString(504));
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
  addtomask: string;
begin
  Timer1.Enabled:=false;
  if not MustExit then
  try
    with ConfigForm.ConfigData do
    if (InputFilesDir<>'') and DirectoryExists(InputFilesDir) then
    try
      Processed:=false;
      if PortCapturing=1 then addtomask:='spl.tmp'
                         else addtomask:='';
      if FindFirst(InputFilesDir+InputFilesMask+addtomask,0,SearchRec)=0 then
      if TestFile then
      begin
        if ProcessFile then Processed:=true;
      end;
      while (not Processed) and (FindNext(SearchRec)=0) do
      if TestFile then
      begin
        if ProcessFile then Processed:=true;
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

procedure TMainForm.LoadLang;
begin
  Konfiguracja1.Caption := RString(400);
  Konfiguracja1.Hint := RString(401);

  Zakoncz1.Caption := RString(402);
  Zakoncz1.Hint := RString(403);

  Info1.Caption := RString(406);
  Info1.Hint := RString(407);

  OpenDialog1.Filter := RString(404);
  SaveDialog1.Filter := RString(404);
end;

end.
