unit MyStrings;

interface


function PosLeft(AString: string;
                 ASubString: string): integer;

function PosRight(AString: string;
                  ASubString: string): integer;

function SplitLeft(AString: string;
                   ASubString: string;
                   var LeftPart: string;
                   var RightPart: string): integer;

function SplitRight(AString: string;
                    ASubString: string;
                    var LeftPart: string;
                    var RightPart: string): integer;

function IsRootPath(AString: string): boolean;

procedure CutFirstSlash(var AString: string);

procedure CutLastSlash(var AString: string);

function ExchangeSubstring(AString: string;
                           AFrom: string;
                           ATo: string): string;

implementation

function PosLeft(AString: string;
                 ASubString: string): integer;
begin
  result:=pos(ASubString,AString);
end;

function PosRight(AString: string;
                  ASubString: string): integer;
var
  TempPos: integer;
begin
  result:=0;
  repeat
    TempPos:=PosLeft(AString,ASubString);
    if TempPos>0 then
    begin
      delete(AString,1,TempPos);
      inc(result,TempPos);
    end;
  until TempPos=0;
end;

function SplitLeft(AString: string;
                   ASubString: string;
                   var LeftPart: string;
                   var RightPart: string): integer;
var
  TempPos: integer;                 
begin
  result:=PosLeft(AString,ASubString);
  if result=0 then
  begin
    LeftPart:=AString;
    RightPart:='';
  end
  else
  begin
    LeftPart:=copy(AString,1,result-1);
    TempPos:=result+length(ASubString);
    RightPart:=copy(AString,TempPos,length(AString)-TempPos+1);
  end;
end;

function SplitRight(AString: string;
                    ASubString: string;
                    var LeftPart: string;
                    var RightPart: string): integer;
var
  TempPos: integer;                  
begin
  result:=PosRight(AString,ASubString);
  if result=0 then
  begin
    LeftPart:='';
    RightPart:=AString;
  end
  else
  begin
    LeftPart:=copy(AString,1,result-1);
    TempPos:=result+length(ASubString);
    RightPart:=copy(AString,TempPos,length(AString)-TempPos+1);
  end;
end;

function IsRootPath(AString: string): boolean;
begin
  result:=false;
  if (length(AString)>0) and (AString[1] in ['\','/']) then
    result:=true;
end;

procedure CutFirstSlash(var AString: string);
begin
  if (length(AString)>0) and (AString[1] in ['\','/']) then
    delete(AString,1,1);
end;

procedure CutLastSlash(var AString: string);
begin
  if (length(AString)>0) and (AString[length(AString)] in ['\','/']) then
    delete(AString,length(AString),1);
end;


function ExchangeSubstring(AString: string;
                           AFrom: string;
                           ATo: string): string;
var
  TempPos: integer;
begin
  result:='';
  while length(AString)>0 do
  begin
    TempPos:=pos(AFrom,AString);
    if TempPos>0 then
    begin
      result:=concat(result,copy(AString,1,TempPos-1),ATo);
      AString:=copy(AString,TempPos+length(AFrom),length(AString));
    end
    else
    begin
      result:=concat(result,AString);
      AString:='';
    end;
  end;
end;

end.
