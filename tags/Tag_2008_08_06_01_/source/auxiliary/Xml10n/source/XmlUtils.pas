{******************************************************************************}
{                                                                              }
{   Xml10n - GUI Localization Components                                       }
{                                                                              }
{   Copyright (C) 2004 Maurizio Basaglia, all rights reserved.                 }
{   See LICENSE.TXT for copyright and license details.                         }
{                                                                              }
{******************************************************************************}

unit XmlUtils;

interface

uses
  SysUtils;

type
  EL10nXmlError = class(Exception);

function EncodeXmlString(const S: string): string;
function DecodeXmlString(const S: string): string;

implementation

function EncodeXmlString(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    case S[I] of
      '&': Result := Result + '&amp;';
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
      else Result := Result + S[I];
    end;
end;

function DecodeXmlString(const S: string): string;
var
  I: Integer;
begin
  I := 1;
  Result := '';
  while I <= Length(S) do
  begin
    if SameText(Copy(S, I, 5), '&amp;') then
    begin
      Result := Result + '&';
      Inc(I, 5);
    end
    else if SameText(Copy(S, I, 4), '&lt;') then
    begin
      Result := Result + '<';
      Inc(I, 4);
    end
    else if SameText(Copy(S, I, 4), '&gt;') then
    begin
      Result := Result + '>';
      Inc(I, 4);
    end
    else if SameText(Copy(S, I, 6), '&quot;') then
    begin
      Result := Result + '"';
      Inc(I, 6);
    end
    else if SameText(Copy(S, I, 6), '&apos;') then
    begin
      Result := Result + '''';
      Inc(I, 6);
    end
    else
    begin
      Result := Result + S[I];
      Inc(I);
    end;
  end;
end;

end.
