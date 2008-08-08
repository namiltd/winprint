unit QXmlRTTI;

interface
uses classes,controls,typinfo,sysutils,QXml,QRTTI;

type

TMiddleXQuickRTTI = class (TCustomXMLRTTI)
 private
   fNode:TmiddleXNode; 
 protected
   function outputXML :String; override;
   function outputSchema :String; override;
   procedure inputXML (sXML:String); override;
   procedure inputSchema (sSchema:String); override;
 public
    property Node:TMiddleXNode read fnode write fnode;
    property Value;
    constructor create;override;
    destructor destroy;override;
 published
   property RTTIObject;
   property InTagProperties;
   property ShowType;
   property ObjectID;
   property XML:String read outputXML write inputXML;
   property TagName;
   property TagClassType;
   property FindTypes;           
 end;

implementation

constructor TMiddleXQuickRTTI.create;
begin
  inherited create;
     Node:=TmiddleXNode.create;
end;

destructor TMiddleXQuickRTTI.destroy;
begin
  try
   {}
    Node.Free; 
  finally
    inherited destroy;
  end;
end;

function TMiddleXQuickRTTI.outputSchema :String;
begin
 result:='<Not Supported/>'
end;

procedure TMiddleXQuickRTTI.inputSchema (sSchema:String);
begin
 {Ignored}
end;


function TMiddleXQuickRTTI.outputXML :String;
var i,{j,}k,max:integer; typname:ttypekind; { q,q2:TMiddleXQuickRTTI; holdstream:tmemorystream;}
  s:TStrings;{L:TList;} C:TCollection;holdtags,{cname,}thisprop,outhold,holdtag:String; ftags:TStringlist;
 { clname:String;} compobj{,compchild}:TComponent;{  cnode:TMIddlexnode;}
begin
 ftags:=nil;
 outhold:='';
 uniquestring(outhold);
 {Thanks to for catching to error. I added a property "TagClassType" if the item is unassigned}

 if showtype then
   outhold:='<'+tagname+' TYPE="'+TagClassType+'"'
   else
   outhold:='<'+tagname;


 if objectid<>'' then outhold:=outhold+' ID="'+objectid+'"';
   if intagproperties<>'' then
   begin
    ftags:=tstringlist.create;
     holdtags:=intagproperties;
     while pos(',',holdtags)<>0 do
     begin
       ftags.add (trim(copy(holdtags,1,pos(',',holdtags)-1)));
       delete(holdtags,1,pos(',',holdtags));
     end;
     if holdtags<>'' then ftags.add(trim(holdtags));
   end;

 if intagproperties<>'' then
 begin
  for i:= 0 to propertycount-1 do
  if ftags.indexof(propertynames(i))>-1 then
  begin
  thisprop:=propertynames(i);
  typname :=  self.propertyTypes (i);
  if typname<>tkclass then
   outhold:=outhold+' '+thisprop+'="'+GetValue(thisprop)+'" ';
  end;
 end;

 // The above line allows us to have collections of items.. like tlist
 outhold:=outhold+'>'+#13+#10;
 for i:= 0 to propertycount-1 do
 begin
 thisprop:=propertynames(i);
 typname :=  self.propertyTypes (i);
  if typname<>tkclass then
    begin
    if intagproperties='' then
     outhold:=outhold+' <'+thisprop+'>'+GetValue(thisprop)+'</'+thisprop+'>'+#13+#10
     else
     if ftags.indexof(propertynames(i))=-1 then
            outhold:=outhold+' <'+thisprop+'>'+GetValue(thisprop)+'</'+thisprop+'>'+#13+#10
    end
   else
   begin
    if Childobject(thisprop) is TPersistent then
    try
       Cache.rttiobject:=TPersistent(Childobject(thisprop));
     if assigned(Cache.rttiObject) then
      begin
        Cache.TagName := thisprop;
        outhold:=outhold + TMiddleXQuickRTTI(Cache).XML;
      end;
    except on E:Exception do
     begin
        E.message:='Error Outputting for property ['+thisprop+']';
        raise(e);
     end;
    end;
   end;

 end;

   if  RTTIObject is TStrings then
   begin
    s:=TStrings(self.rttiobject);
    for k:= 0 to s.Count-1 do
    outhold:=outhold+'<LINE INDEX="'+inttostr(k)+'">' + s[k] + '</LINE>';
   end;

   if  RTTIObject is TCollection then
   begin
    c:=Tcollection(self.rttiobject);
    max:= c.count-1;
      holdtag:=c.itemclass.classname;
      delete(holdtag,1,1);
    for k:= 0 to max do
     begin
      {create and output any internal items}
      if C.Items[k] is TPersistent then
        begin
          cache.rttiobject:=TPersistent(C.Items[k]) ;
          cache.tagname:= holdtag;
          outhold:=outhold+ TMiddleXQuickRTTI(Cache).xml;
        end;
     end;
   end;
   if  RTTIObject is TComponent then
   begin
    compobj:=Tcomponent(self.rttiobject);
    max:= compobj.componentcount-1;
    if max>-1 then outhold:=outhold+'<_COMPONENTS>';
    for k:= 0 to max do
     begin
          cache.rttiobject:=Compobj.components[k] ;
          cache.tagname:= trim(Compobj.components[k].name);
          outhold:=outhold+ TMiddleXQuickRTTI(Cache).xml;
     end;
    if max>-1 then  outhold:=outhold+'</_COMPONENTS>';
    end;
 outhold:=outhold+'</'+tagname+'>'+#13+#10;  
 result:= outhold;
end;


procedure TMiddleXQuickRTTI.inputXML (sXML:String);
var max,i,j,k,cmax:integer;ftags,s:TStringlist;
    holdclassname,holdtags,thisprop,holdtag:String; Child,cnode:TMiddlexNode;
    c:TCollection; ci:TCollectionItem; holdchildXML:String;
    idx:integer; compobj,compchild:TComponent; cidx:integer;

begin
 ftags:=tstringlist.create;
                                 
 Node.ShortTags := true;
 Node.XML:=sXML;
 if assigned(RTTIObject) then
  begin
   {find any properties that might be "intag"}
   if intagproperties<>'' then
   begin
     holdtags:=intagproperties;
     while pos(',',holdtags)<>0 do
     begin
       ftags.add (trim(copy(holdtags,1,pos(',',holdtags)-1)));
       delete(holdtags,1,pos(',',holdtags));
     end;
     if holdtags<>'' then ftags.add(trim(holdtags));
   end;

   { read through properties and pull appropriate values}
   max:=propertyCount-1;
for i:= 0 to max do
 begin
   thisprop:= propertynames(i);

   idx:=Node.IndexByName(thisprop);
 if idx>-1 then
 if assigned(RTTIObject) then
  begin
    Child:= Node.ChildNode(idx) ;
    if self.propertyTypes(i) <> tkClass then
   begin
      if intagproperties='' then
        SetValue(thisprop,Child.storedvalue )
      else
        begin
        if ftags.indexof(thisprop)=-1 then
          SetValue(thisprop,Child.storedvalue)
          else
          SetValue(thisprop,Child.Properties.Values[thisprop])
        end;
   end
   else
    if assigned(TPersistent(childobject(thisprop))) then
    begin
       {class property}
     Cache.RTTIObject:= TPersistent(childobject(thisprop));
     Cache.TagName := thisprop;
     idx:=Node.IndexByName(thisprop);

     if assigned(Cache.rttiobject) then
      if idx>-1 then
       begin
        Child:=Node.childnode(idx);
        holdchildXML:= Child.xml;
        TMiddleXQuickRTTI(Cache).xml:=holdchildxml;
       end;
    end;
  end;
end; {cycle thru properties}

if assigned(RTTIOBJECT) then
 if  RTTIObject is TStrings then
   begin
    TStrings(RTTIObject).clear;
    s:=tstringlist.create;
    for j:= 0 to node.ChildNodeCount -1 do
    if assigned(node.childnode(j)) then
     begin
        s.add(node.childnode(j).body);
     end;
    TStrings(RTTIObject).assign (s);
    s.free;
   end;

   if  RTTIObject is TCollection then
   begin
    c:=Tcollection(self.rttiobject);
    c.Clear;
     holdtag:=c.itemclass.classname;
     delete(holdtag,1,1);
    cmax:=node.childnodecount-1;
    for k:= 0 to cmax do
     if assigned(node.childnode(k)) then
       if node.childnode(k).ChildNodeCount>0 then
       begin
        try
           Child:=node.childnode(k);
           holdclassname:=Child.properties.values['TYPE'];
           //ci:= NewRegisteredCollectionItem(holdclassname,c);
           ci:=TCollectionItem(GetClass(holdclassname).newinstance).create(c); 
           cache.rttiobject:=ci ;
           cache.tagname:=holdtag;
           TMiddleXQuickRTTI(Cache).XML:=node.childnode(k).xml ;
        except on E:Exception do
         begin
           E.message := 'Error Handling class ['+holdclassname+']';
           raise(E);
         end;
         end;
       end;
   end;

   if  RTTIObject is TComponent then
   begin
    compobj:=Tcomponent(self.rttiobject);
    {components}
    cidx:=node.IndexByName ('_COMPONENTS');
    if cidx>-1 then
    begin
       cnode:=node.ChildNode(cidx);
       cmax:=cnode.childnodecount-1;
    for k:= 0 to cmax do
     if assigned(cnode.childnode(k)) then
       begin
           Child:=cnode.childnode(k);
           holdclassname:=Child.properties.values['TYPE'];
//           compchild:= NewRegisteredComponent(holdclassname,compobj);
           compchild:= TComponent(GetClass(holdclassname).newinstance).create(compobj); 
           if (compchild is TCOntrol) and (compobj is TWinCOntrol) then
             begin
                TControl(compchild).parent:=TWincontrol(compobj);
             end;
           compchild.name:=child.tag;
           holdtag:=CHild.Tag ;
           cache.rttiobject:=compchild;
           cache.tagname:=child.tag;
           TMiddleXQuickRTTI(Cache).XML:=cnode.childnode(k).xml ;
           {Grab those define props}
       end;
     end{ cidx>-1};
   end;
  end;
 ftags.free;
end;

end.
