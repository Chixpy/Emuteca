{
[Info]
This script test functions from Units/uETKXMLUtils.pas unit.

It needs XMLExample.xml in the same folders 
[Data]
Name=Chixpy
Version=0.01
Date=20230115
[Changes]
0.01 - 20230115
  + Initial version
[EndInfo]
}
program TestETKXMLUtils;

//uses uETKXMLUtils;

{$I '../Units/uETKXMLUtils.pas'}

var
  cLine, cChar: integer;
  aXML, aContent, aFileName: string;
  aXMLFile: TStringList;

begin

  aFileName := AskFile('Select tests/XMLExample.xml',
    'XMLExample.xml|XMLExample.xml', '');

  aXMLFile := CreateStringList;
  aXMLFile.LoadFromFile(aFileName);
  
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test some functions from uETKXMLUtils.');
  WriteLn('');
  
  cLine := 0;
  cChar := 0;
  
  while cLine < aXMLFile.Count do
  begin
    aXML := XMLExtractTag(aXMLFile, cLine, cChar, 'parent');

    if aXML = '' then
       WriteLn('Parent Tag not found. (End of file)')
    else
    begin
      aContent := XMLExtractContent(aXML, 'Tag1');
      if aContent = '' then
        WriteLn('Parent/Tag1 content is empty')
      else
        WriteLn('Parent/Tag1 content: ' + XMLRemoveEntities(aContent));
    
      aContent := XMLExtractAttrib(aXML, 'Tag1', 'attrib');
      if aContent = '' then
        WriteLn('Parent/Tag1 attrib is empty')
      else
        WriteLn('Parent/Tag1 attrib: ' + XMLRemoveEntities(aContent));
    
      WriteLn('');
    end;
  end;

  aXMLFile.Free;

end.
