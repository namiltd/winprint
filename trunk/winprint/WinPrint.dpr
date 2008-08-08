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

program WinPrint;

{$R 'resources\icons32x32\icons.res' 'resources\icons32x32\icons.rc'}

uses
  Forms,
  Windows,

  //Misc Components sources
  MyStrings in 'source\auxiliary\MyStrings.pas',
  NumEdit in 'source\auxiliary\NumEdit.pas',
  TrayIcon in 'source\auxiliary\TrayIcon.pas',
  CEVersionInfo in 'source\auxiliary\CEVersionInfo.pas',
  SetString in 'source\auxiliary\SetString.pas',

  //WinPrint sources
  MainFormUnit in 'source\MainFormUnit.pas' {MainForm},
  ConfigFormUnit in 'source\ConfigFormUnit.pas' {ConfigForm},
  PrintStringsUnit in 'source\PrintStringsUnit.pas',
  ConversionUnit in 'source\ConversionUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  SetWindowLong(Application.Handle, GWL_EXSTYLE, WS_EX_TOOLWINDOW); //prevents flickering
  Application.ShowMainForm:=false;
  Application.Title := 'WinPrint';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConfigForm, ConfigForm);
  Application.Run;
end.
