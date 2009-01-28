unit SetString;

interface

uses SysUtils, TypInfo;

// Convert a set to a string, e.g.,
// var
//   Styles: TFontStyles;
// begin
//   Styles := [fsBold, fsItalic];
//   WriteLn(SetToString(TypeInfo(TFontStyles), Styles));
// prints "fsBold,fsItalic"
//
// If Info is not a set type, raises an EConvertError exception.
//
// The second form lets you specify the string that separates
// the set elements. The default is ','.
// The third form lets you specify a string that comes before
// and after all the set elements. The defaults are '[' and ']'.
//
// The set type can be any type permitted by Delphi, of any
// size and with any component type.
function SetToString(Info: PTypeInfo; const Value): string; overload;
function SetToString(Info: PTypeInfo; const Value; const Separator: string): string; overload;
function SetToString(Info: PTypeInfo; const Value; const Separator, Prefix, Suffix: string): string; overload;

// Convert a string to a set, reversing the operation above.
// White space is permitted before and after each literal.
// Any punctuation character is permitted as a separator.
// A leading and trailing punctuation character is also allowed.
procedure StringToSet(const Str: string; Info: PTypeInfo; var Value);

// Convert an ordinal value (integer, character, enumeration)
// to a string. Like GetEnumName, but handles characters.
function OrdToString(Info: PTypeInfo; Value: Integer): string;
// Convert a string to an ordinal value. Like GetEnumValue,
// but it handles characters.
function StringToOrd(Info: PTypeInfo; const Value: string): Integer;

implementation

// Convert an ordinal value to a string. The ordinal value
// can be an integer, enumerated value, or a character.
function OrdToString(Info: PTypeInfo; Value: Integer): string;
resourcestring
  sCvtError = 'OrdToString: type kind must be ordinal, not %s';
const
  AsciiChars = [32..127]; // printable ASCII characters
begin
  case Info.Kind of
  // In D5, the Value must fit in an Integer, for tkInt64 is not
  // likely to occur. A future version of Delphi might define
  // Int64 so it fits in an Integer, so for future expansion,
  // handle the case of tkInt64.
  tkInteger, tkInt64:
    Result := IntToStr(Value);
  tkChar, tkWChar:
    if Value in AsciiChars then
      Result := '''' + Chr(Value) + ''''
    else
      Result := Format('#%d', [Value]);
  tkEnumeration:
    Result := GetEnumName(Info, Value);
  else
    raise EConvertError.CreateFmt(sCvtError, [GetEnumName(TypeInfo(TTypeKind), Ord(Info.Kind))]);
  end;
end;

// Convert a string to an ordinal value. The ordinal type can be
// integer, character, or enumeration.
function StringToOrd(Info: PTypeInfo; const Value: string): Integer;
resourcestring
  sCvtError = 'StringToOrd: type kind must be ordinal, not %s';
  sNoValue = 'StringToOrd: value string cannot be empty';
  SNotAChar = 'StringToOrd: Cannot convert invalid character, %s';
begin
  Assert(Length(Value) > 0, sNoValue);

  case Info.Kind of
  tkInteger:
    Result := StrToInt(Value);
  tkInt64:
    // In D5, the result type is Integer, which is smaller than Int64,
    // and so the conversion might result in a range check error.
    // Allow the tkInt64 type, though, to handle small numbers, and
    // to allow for the future, where Int64 might fit in an Integer.
    Result := StrToInt64(Value);
  tkChar, tkWChar:
    // A character can be an ordinal value (#27) or a quoted character.
    // If quoted, it might be a repeated quote, that is, ''''.
    if Value[1] = '#' then
      Result := StrToInt(Copy(Value, 2, MaxInt))
    else if (Value[1] = '''') and (Value[Length(Value)] = '''') and
            ((Length(Value) = 3) or (Value = ''''''''''))
    then
      Result := Ord(Value[2])
    else
      raise EConvertError.CreateFmt(sNotAChar, [Value]);
  tkEnumeration:
    Result := GetEnumValue(Info, Value);
  else
    raise EConvertError.CreateFmt(sCvtError, [GetEnumName(TypeInfo(TTypeKind), Ord(Info.Kind))]);
  end;
end;



{ SetToString }

resourcestring
  sNotASet = 'SetToString: argument must be a set type; %s not allowed';
const
  MaxSet = 255;       // Largest ordinal value in a Delphi set.
  BitsPerByte = 8;
  // Mask to force the minimum set value to be
  // a set element on a byte boundary.
  ByteBoundaryMask = not (BitsPerByte - 1);
type
  TSet = set of 0..MaxSet;

function SetToString(Info: PTypeInfo; const Value): string; overload;
begin
  Result := SetToString(Info, Value, ',');
end;

function SetToString(Info: PTypeInfo; const Value; const Separator: string): string; overload;
begin
  Result := SetToString(Info, Value, Separator, '[', ']');
end;

function SetToString(Info: PTypeInfo; const Value; const Separator, Prefix, Suffix: string): string; overload;
var
  CompInfo: PTypeInfo;           // Type info for the set's component type
  CompData: PTypeData;           // Type data for CompInfo.
  SetValue: TSet absolute Value; // The set value as a convenient set type.
  Element: 0..MaxSet;            // A member of the set.
  MinElement: 0..MaxSet;         // Minimum ordinal for the set's lower bound,
                                 // rounded to a byte boundary.
begin
  if Info.Kind <> tkSet then
    raise EConvertError.CreateFmt(sNotASet, [GetEnumName(TypeInfo(TTypeKind), Ord(Info.Kind))]);
  CompInfo := GetTypeData(Info)^.CompType^;
  CompData := GetTypeData(CompInfo);

  Result := '';
  MinElement := CompData.MinValue and ByteBoundaryMask;
  for Element := CompData.MinValue to CompData.MaxValue do
  begin
    if (Element - MinElement) in SetValue then
      if Result = '' then
        Result := Prefix + OrdToString(CompInfo, Element)
      else
        Result := Result + Separator + OrdToString(CompInfo, Element);
  end;
  if Result = '' then
    Result := Prefix + Suffix
  else
    Result := Result + Suffix;
end;


{ StringToSet }

const
  WhiteSpace = [#0..' '];
  Alphabetic = ['a'..'z', 'A'..'Z', '_'];
  Digits = ['0'..'9'];
  AlphaNumeric = Alphabetic + Digits;
  HexDigits = ['a'..'f', 'A'..'F'] + Digits;
  CharBegin = ['#', ''''];
  AsciiChars = [' '..'~']; // printable ASCII characters

resourcestring
  sInvalidSetString = 'StringToSet: %s is not a valid literal for the set type';
  sOutOfRange = 'StringToSet: %0:d is out of range [%1:d..%2:d]';

procedure SkipWhiteSpace(const Str: string; var I: Integer);
begin
  while (I <= Length(Str)) and (Str[I] in WhiteSpace) do
    Inc(I);
end;

// Convert a string to a set of enumerated or integer elements.
procedure StringToEnumSet(const Str: string; CompInfo: PTypeInfo; CompData: PTypeData; var Value: TSet);
var
  ElementName: string;      // String form of one set element.
  Element: Integer;         // Ordinal value of one set element.
  MinElement: Integer;      // Offset for the first set element.
  Start: Integer;           // Index in Str for the start of an element name.
  I: Integer;               // Current index in Str.
begin
  MinElement := CompData.MinValue and ByteBoundaryMask;
  I := 1;
  while I <= Length(Str) do
  begin
    SkipWhiteSpace(Str, I);
    // Skip the prefix, separator, or suffix.
    if (I <= Length(Str)) and not (Str[I] in AlphaNumeric) then
      Inc(I);
    SkipWhiteSpace(Str, I);

    // Remember the start of the set element, and collect the entire element name.
    Start := I;
    while (I <= Length(Str)) and (Str[I] in AlphaNumeric) do
      Inc(I);

    // No name, so skip to the next element.
    if I = Start then
      Continue;

    ElementName := Copy(Str, Start, I-Start);
    Element := StringToOrd(CompInfo, ElementName);
    if Element < 0 then
      raise EConvertError.CreateFmt(sInvalidSetString, [AnsiQuotedStr(ElementName, '''')]);
    if (Element < CompData.MinValue) or (Element > CompData.MaxValue) then
      raise EConvertError.CreateFmt(sOutOfRange, [Element, CompData.MinValue, CompData.MaxValue]);

    Include(Value, Element - MinElement);
  end;
end;

resourcestring
  sNotAChar = 'StringToSet: Not a valid character (%.10s)';
  sCharOutOfRange = 'StringToSet: Character #%0:d is out of range [#%1:d..#%2:d]';

// Convert a string to a set of character elements.
procedure StringToCharSet(const Str: string; CompData: PTypeData; var Value: TSet);
var
  ElementName: string;      // String form of one set element.
  Element: Integer;         // Ordinal value of one set element.
  MinElement: Integer;      // Offset for the first set element.
  Start: Integer;           // Index in Str for the start of an element name.
  I: Integer;               // Current index in Str.
begin
  MinElement := CompData.MinValue and ByteBoundaryMask;
  I := 1;
  while I <= Length(Str) do
  begin
    SkipWhiteSpace(Str, I);
    // Skip over one character, which might be the prefix,
    // a separator, or suffix.
    if (I <= Length(Str)) and not (Str[I] in CharBegin) then
      Inc(I);
    SkipWhiteSpace(Str, I);

    if I > Length(Str) then
      Break;

    case Str[I] of
    '#':
      begin
        // Character is specified by ordinal value, e.g.,
        // #31 or #$A2.
        Inc(I);
        Start := I;
        if (I < Length(Str)) and (Str[I] = '$') then
        begin
          Inc(I);
          while (I <= Length(Str)) and (Str[I] in HexDigits) do
            Inc(I);
        end
        else
        begin
          while (I <= Length(Str)) and (Str[I] in Digits) do
            Inc(I);
        end;
        ElementName := Copy(Str, Start, I-Start);
        Element := StrToInt(ElementName);
      end;
    '''':
      begin
        // Delphi cannot tell that Element is always set by the code below.
        // Turn off the warning that Element might not be initialized.
        Element := 0;
        Start := I; // Save the position for error messages.
        // Character is enclosed in quotes, e.g., 'A', ''''.
        Inc(I);
        if (I <= Length(Str)) then
        begin
          Element := Ord(Str[I]);
          if Str[I] = '''' then
            // Skip over a repeated quote character.
            Inc(I);
          // Skip to the closing quote.
          Inc(I);
        end;
        if (I <= Length(Str)) and (Str[I] = '''') then
          Inc(I)
        else
          raise EConvertError.CreateFmt(sNotAChar, [Copy(Str, Start, I-Start)]);
      end;
    else
      // The unknown character might be the suffix. Try skipping over
      // it and subsequent white space. Remember the original index
      // in case the suffix-test fails.
      Start := I;
      Inc(I);
      SkipWhiteSpace(Str, I);
      if I <= Length(Str) then
        raise EConvertError.CreateFmt(sNotAChar, [Copy(Str, Start, I-Start)])
      else
        Exit;
      // Delphi cannot tell that Element is always set by the code above.
      // Turn off the incorrect warning that Element might not be initialized.
      Element := 0;
    end;
    if (Element < CompData.MinValue) or (Element > CompData.MaxValue) then
      raise EConvertError.CreateFmt(sCharOutOfRange, [Element, CompData.MinValue, CompData.MaxValue]);

    Include(Value, Element - MinElement);
  end;
end;

// Convert a string to a set value. The set can have any component type:
// integer, character, or enumeration. The set can be any size that Delphi
// supports, that is, up to 256 members, with ordinal values in the range
// 0 to 255.
procedure StringToSet(const Str: string; Info: PTypeInfo; var Value);
var
  CompInfo: PTypeInfo;            // Type info for the component type.
  CompData: PTypeData;            // Type date for CompInfo.
  SetValue: TSet absolute Value;  // The actual set as a convenient set type.
  MinValue, MaxValue: Integer;    // Set limits, rounded off to byte boundaries.
begin
  if Info.Kind <> tkSet then
    raise EConvertError.CreateFmt(sNotASet, [GetEnumName(TypeInfo(TTypeKind), Ord(Info.Kind))]);
  CompInfo := GetTypeData(Info)^.CompType^;

  // Initialize SetValue to an empty set. Only initialize as many bytes
  // as are present in the set.
  CompData := GetTypeData(CompInfo);
  MinValue := CompData.MinValue and ByteBoundaryMask;
  MaxValue := (CompData.MaxValue + BitsPerByte - 1) and ByteBoundaryMask;
  FillChar(SetValue, (MaxValue - MinValue) div BitsPerByte, 0);

  if CompInfo.Kind in [tkChar, tkWChar] then
    StringToCharSet(Str, CompData, SetValue)
  else
    StringToEnumSet(Str, CompInfo, CompData, SetValue);
end;

end.