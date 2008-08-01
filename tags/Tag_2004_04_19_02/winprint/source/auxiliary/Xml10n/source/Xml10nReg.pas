{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit Xml10nReg;

interface

procedure Register;

implementation

uses
  Classes, DesignIntf, Xml10n, Xml10nDesigner;

procedure Register;
begin
  RegisterComponents('Xml10n', [TXml10n]);
  RegisterComponentEditor(TXml10n, TXml10nDesigner);
end;

end.
