unit QXml;

interface
uses classes,sysutils;

type


TParsePrimative = class(TObject)
 private
   ftext:String;
 public
   property Text:String read ftext write ftext;
 end;

TTagPrimative = class(TParsePrimative);
TDataPrimative = class(TParsePrimative);   

TParseLeaf = class(TCollectionItem)
 private
   ftag,ftext:String; fchildren:TCollection; fparams:TStringlist;
 public
   constructor create(Collection:TCollection);override;
   destructor destroy;override; 
 published
   property Tag:String read ftag write ftag;
   property Parameters:TStringlist read fparams write fparams;
   property Text:String read ftext write ftext;
   property Children:TCollection read fchildren write fchildren;
 end;



TMiddleXNode = class(TPersistent)
  private
    ftag,fns,fbody:string;
    fproperties:tstringlist;
    fchildren:TList; fshort,fbuild:boolean;
    fdata:TObject;
    fparent:TMiddleXNode;
    Primatives:TList;
    function  getXML:String;
    procedure setXML(sXML:String);
    function  GetSTored:String;
  public
{    function  getBody:String;
    procedure setBody(sXML:String);}
    constructor create;
    destructor destroy;override;
    function  ChildNode(index:integer):TMiddleXNode;
    function  AddChildNode :TMiddleXNode;
    function  IndexByName (tag:String):integer;
    procedure DeleteChildNode (index:integer);
    function  ChildNodeCount:integer;
    property  Data:TObject read fdata write fdata; {for any associated data.. like a QuickRTTIEnabler!}
  published
    property  BuildChildren:boolean read fbuild write fbuild;
    property  ShortTags:boolean read fshort write fshort;
    property  XML:String read getXML write setXML;
    property  StoredValue:String read GetStored;
    property  Body:String read {getBody} fbody write {setBody} fbody;
    property  Tag:String read ftag write ftag;
    property  NameSpace:String read fns write fns;
    property  Properties:tstringlist read fproperties write fproperties;
    property  Parent:TMiddleXNode read fparent write fparent;
  end;


procedure BuildNodes (Root:TMiddleXNode;sXML:String);
procedure TagProps (props:String; strings:TStrings);

implementation

procedure TagProps (props:String; strings:TStrings);
var hold,seg,n,v,prop:String;spos,epos,qpos:integer;
begin
hold:=trim(props)+' ';
spos:=pos(' ',hold);
if length(hold)>1 then
while spos>0 do
 begin
   seg:=copy(hold,1,spos-1);
  //   NAME="VALUE"
   epos:=pos('=',seg);
   prop:=copy(seg,1,epos-1);
 // "VALUE"|VALUE
   delete(seg,1,epos);
    if length(seg)>=1 then
   begin
    if seg[1]='"' then delete(seg,1,1);
    if seg[length(seg)]='"' then delete(seg,length(seg),1);
    end;
   if not(prop='') then  {Make sure we have a prop to add! Thanks again ,Dean.} 
     strings.add(prop+'='+seg);
   delete(hold,1,spos);
   spos:=pos(' ',hold);
 end;

end;

function BuildPrimativesList (XML:String):TList;
var i,max:integer;  intag:boolean; seg:String;TP:TTagPrimative;DP:TDataPrimative;
begin
  max:=length(XML);
  intag:=false;
  result:=Tlist.create;
  i:=1;
//  run thru and build a list, each tag one type, each bit of data another
  while i<=max do
  begin
   if XML[i]='<' then
     begin
      intag:=true;
      if seg<>'' then
       begin
          DP:=TDataPrimative.create;
          DP.text:=seg;
          result.add(DP);
       end;
      seg:='';
     end
     else
    if XML[i]='>' then
     begin
      intag:=false;
      if seg<>'' then
       begin
          TP:=TTagPrimative.create;
          TP.text:=seg;
          result.add(TP);
       end;
      seg:='';
     end else seg:=seg+XML[i];
   inc(i);
  end;
//  any left over data should get tacked on..
  if seg<>'' then
   begin
     if intag then
      begin
          TP:=TTagPrimative.create;
          TP.text:=seg;
          result.add(TP);
      end else
       begin
          DP:=TDataPrimative.create;
          DP.text:=seg;
          result.add(DP);
       end;
   end;
end;


procedure BuildNodes (Root:TMiddleXNode;sXML:String);
var Leaves:TList;
    i,max:integer; MN,Parent:TMIddlexNode; PP:TParsePrimative;
    spos,starttag,endtag:integer; body:String;
begin
 Leaves:= BuildPrimativesList(sXML);
 {run thru the leaves.}
 max:= Leaves.count-1;
 MN:=Root;
 for i:= 0 to max do
 begin
   {If we find a <tag, MN becomes new node,
    oldMN:=Parent of new node end of that tag
    and we move back up to the Parent. all other
    content goes into the tag's body}
   PP:=TParsePrimative(Leaves[i]);   
   if PP is TTagPrimative then
    begin
      if ((pos('/',PP.Text)=1) OR (pos('/',PP.Text)=length(pp.text)))  then
       begin
         if (pos('/',PP.Text)=length(pp.text)) then
          begin
            Parent:=MN;
            MN:=Parent.AddChildNode ;
            MN.Parent:=Parent;
            MN.BuildChildren := false;
              spos:=pos(' ',PP.text);
              MN.Tag := copy(PP.text,1, spos-1);
              TagProps(copy(PP.text,spos,length(pp.text)-spos),mn.Properties );
          end else  MN.body:= trim(body) ;
         body:='';
         if assigned(MN.Parent) then MN:=MN.Parent;{go back up one}
       end
       else
       begin
         if pos('?',PP.text)<>1 then
         begin
         if mn.tag<>'' then
           begin
            Parent:=MN;
            MN:=Parent.AddChildNode ;
            MN.Parent:=Parent;
            MN.BuildChildren := false;
           end;
         if pos(' ',PP.text)=0 then
              MN.Tag:=PP.text
              else
              begin
              spos:=pos(' ',PP.text);
              MN.Tag := copy(PP.text,1, spos-1);
              TagProps(copy(PP.text,spos,length(pp.text)-spos),mn.Properties );
              end;
         end else begin
             {Ignore.. ?XML tags}
          end;
       end;
    end else
      body:=  Body + PP.text;
 end;
 {Cleanup.. Thanks Dean!}
 for i:= 0 to max do TParsePrimative(Leaves.items[i]).free;
 leaves.free;
end;

constructor TParseLeaf.create(Collection:TCollection);
begin
   inherited create(COllection);
   fchildren:=tcollection.create(TParseLeaf);
   fparams:=tstringlist.create;
end;

destructor TParseLeaf.destroy;
begin
  try
    fchildren.free;
    fparams.free;
  finally
    inherited destroy;
  end;
end; 
 

function  TMiddleXNode.getXML:String;
var hold,n,v,holdtag,holdprop:String;i,max:integer;
begin
     holdtag:=Tag;
     max:= fproperties.count-1;
     if max>-1 then
      begin
       holdprop:=holdprop+' ';
      for i:= 0 to max do
       begin
        n:= fproperties.names[i];
        v:= fproperties.Values[n];
        holdprop:=holdprop+n+'="'+v+'"';
        if i<max then holdprop:=holdprop+' ';
       end;
      end ;
 max:=ChildNodeCount-1;
  if (( max>-1) OR (fbody<>'')) then
   begin
     hold:='<'+holdtag+holdprop+'>';
      hold:=hold+fbody;
     for i:= 0 to max do
     if assigned(ChildNode(i)) then
      begin
        hold:=hold+ChildNode(i).xml;
      end;
     hold:=hold+'</'+holdtag+'>'+#13+#10;    
   end
   else
   begin
      if fshort then
       begin
        if trim(holdtag)<>'' then hold:= '<'+holdtag+holdprop+' />'+#13+#10;
       end else hold:=  '<'+holdtag+holdprop+'></'+holdtag+'>'+#13+#10;

   end;
  result:=hold;
end;

function  TMiddleXNode.getStored:String;
var hold,n,v,holdtag,holdprop:String;i,max:integer;
begin
 max:=ChildNodeCount-1;
  if ( (max>-1) or (fbody<>'')) then
   begin
      hold:=hold+fbody;
     for i:= 0 to max do
     if assigned(ChildNode(i)) then
      begin
        hold:=hold+ChildNode(i).xml;
      end;
   end;
  result:=hold;
end;


constructor TMiddleXNode.create;
begin
 fchildren:=tlist.create;
 fproperties:=tstringlist.create;
 fshort:=true;
 fbuild:=true;
end;

destructor TMiddleXNode.destroy;
begin
 try
   fproperties.Free;
   while fchildren.count>0 do
    begin
      TMiddleXNode(fchildren[0]).free;
      fchildren.Delete (0);
    end;
 finally
   fchildren.free;
 end;
end;

function  TmiddleXNode.indexByName (tag:String):integer;
var MN:TMiddleXNode;max,i:integer; found:boolean; utag:String;
begin
 found:=false;
 i:=0;
 max:=fchildren.count-1;
 utag:=UPPERCASE(tag);
 result:=-1;
 while ((i<=max) and (not(found))) do
  begin
    MN:=TmiddleXNode(fchildren[i]);
    if UPPERCASE(MN.tag)=Utag then
      begin
        result:=i;
        found:=true;
      end;
    inc(i);
  end;
end;

function  TMiddleXNode.ChildNode(index:integer):TMiddleXNode;
begin
 result:=TmiddleXnode(fchildren[index]);
end;

function TMiddleXNode.AddChildNode :TMiddleXNode;
begin
  result:=TMiddleXNode.create;
  result.ShortTags := fshort;
  fchildren.add(result);
end;

procedure TMiddleXNode.DeleteChildNode (index:integer);
begin

  if index<=fchildren.count-1 then  TMiddleXNode(fchildren[index]).free;
  fchildren.delete(index);
end;

function  TMiddleXNode.ChildNodeCount:integer;
begin
 result:=fchildren.count;
end;

procedure TMiddleXNode.setXML(sXML:String);
var Primatives:TList; inchild:boolean; i,j,max,starttag,endtag,nest,spos:integer;
    holdbody,lookfor:String;  PP:TParsePrimative;  TP:TTagPrimative;
begin


   if fbuild then
    begin
   fbody:='';
   while fchildren.count>0 do
   begin
     TmiddlexNode(fchildren[0]).free;
     fchildren.delete(0);
   end;
   fproperties.clear;
   tag:='';
    {If we are supposed to build our own children, then do so.}
    BuildNodes(Self,sXML);
    end;
end;

end.
