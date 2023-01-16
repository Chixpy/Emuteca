{ Emuteca Script unit
[Info]
TODO: Import LineEnding and don't use #13#10 by default.

Some common functions for XML handling in a very dirty way.

* XMLExtractTag: Search for a tag in a file and returns it with its atributes
  and its content in a string.
  
* XMLExtractAttrib: Returns the attribute value from a tag string.

* XMLExtractContent: Returns the content of a tag.
[Data]
Name=Chixpy
Version=0.02
Date=20230115
[Changes]
0.02 - 20230115
  m Faster XMLExtractTag and non-destructive.
0.01 - 20221211
  + XMLExtractTag
  + XMLExtractAttrib
  + XMLExtractContent
[EndInfo]
}
//uses uETKStrUtils;
{$I 'uETKStrUtils.pas'}

function XMLRemoveEntities(const aText: string): string;
begin
  Result := aText;
  
  // If there is not '&' then we don't need to search and replace.
  if pos('&', Result) < 1 then Exit;
  
  Result := UTF8TextReplace(Result, '&amp;', '&', '');
  Result := UTF8TextReplace(Result, '&apos;', '''', '');
  Result := UTF8TextReplace(Result, '&gt;', '>', '');
  Result := UTF8TextReplace(Result, '&lt;', '<', '');
  Result := UTF8TextReplace(Result, '&nbsp;', ' ', ''); // actually non-breaking space
  Result := UTF8TextReplace(Result, '&quot;', '"', '');
end;

function XMLExtractTag(XMLFile: TStrings; var cLine: integer; 
  var cChar: integer; TagName: string): string;
{ Extract a Tag and its content, and returns in a string.

  * cLine: Line of the to start searching the tag, returns the line where the 
    tag ends (useful to search consecutive tags).
    TStrings begins with line number 0.
  * cChar: Position in the file line where start searching the tag, returns the
    position where the tag ends.
    A lines begins with character position 1.
}
var
  aSearch, aLine: string;
  aPos: integer;
begin
  Result := '';
  
  // End of file...
  if cLine >= XMLFile.Count then Exit;
  
  // Read line
  aLine := XMLFile[cLine];
  Inc(cLine);
  
  if cChar < 1 then cChar := 1;
  
  // End of line... and skipping empty lines
  while cChar > Length(aLine) do
  begin
    // End of file...
    if cLine >= XMLFile.Count then Exit;
    
    aLine := XMLFile[cLine];
    Inc(cLine);
    cChar := 1;
  end;  
  if cChar > 1 then 
    aLine := ETKCopyFrom(aLine, cChar);
  
  // Searching openning tag
  aSearch := '<' + Trim(TagName);
  aPos := Pos(aSearch, aLine);
  while (aPos < 1) and (XMLFile.Count > cLine) do
  begin
    aLine := XMLFile[cLine];
    Inc(cLine);
    aPos := Pos(aSearch, aLine);
  end;
  if  aPos > 1 then 
    aLine := ETKCopyFrom(aLine, aPos); 
    
  // Searching end of openning tag ('>')  
  aSearch := '>';
  aPos := Pos(aSearch, aLine);
  while (aPos < 1) and (XMLFile.Count > cLine) do
  begin  
    Result:= Trim(Result + ' ' + Trim(aLine));
    aLine := XMLFile[cLine];
    Inc(cLine);
    aPos := Pos(aSearch, aLine);
  end;
  if aPos > 1 then 
  begin
    // Is an empty tag? <Tag blah='blah' />
    if aLine[aPos - 1] = '/' then
    begin
      if Result <> '' then
        Dec(cLine);
      cChar := aPos + 1;
      Result := Trim(Result + ' ' + Trim(Copy(aLine, 1, aPos)));
      Exit;
    end;
    // else continue.
  end
  else
  begin
    // No '>' ?
    Exit;
  end;
  
  // Searching clossing tag
  aSearch := '</' + Trim(TagName);
  aPos := Pos(aSearch, aLine);
  while (aPos < 1) and (XMLFile.Count > cLine) do
  begin
    Result:= Trim(Result + #13#10 + Trim(aLine));
    aLine := XMLFile[cLine];
    Inc(cLine);
    aPos := Pos(aSearch, aLine);
  end;   
  if aPos > 1 then
  begin
    if Result <> '' then
        Dec(cLine);
    cChar := aPos + Length(aSearch) + 1; // +1 -> '>'
    Result:= Trim(Result + #13#10 + Trim(Copy(aLine, 1, cChar - 1)));
  end;
end;

function XMLExtractAttrib(const TagLine, TagName, TagAttrib: string): string;
var
  aTagHeader: string;
begin
  Result := '';

  // Extracting Header  
  aTagHeader := ETKExtractBetween(TagLine, '<' + Trim(TagName), '>');
  if aTagHeader = '' then Exit; // Not Found
    
  // Searching Attrib
  Result := ETKExtractBetween(aTagHeader, Trim(TagAttrib) + '="', '"');
end;

function XMLExtractContent(const TagLine, TagName: string): string;
var
  aSearch, aTagContent: string;
  aPos: integer;
begin
  Result := '';

  // HACK: Removing text from ending tag first. It can be faster
  aTagContent := TagLine
  aSearch := '</' + Trim(TagName);  
  aPos := Pos(aSearch, aTagContent);
  if aPos > 0 then
    aTagContent := Trim(Copy(aTagContent, 1, aPos - 1));  

  // Searching Tag
  aSearch := '<' + Trim(TagName);    
  aPos := Pos(aSearch, aTagContent);  
  if aPos < 1 then Exit; // Not Found
  // Removing text before the tag
  aTagContent := ETKCopyFrom(aTagContent, aPos);  
  
  aPos := Pos('>', aTagContent);
  if aPos < 1 then Exit; // Not Found
  if aTagContent[aPos - 1] = '/' then Exit; // Testing empty tag: <Tag/>
  
  Result := ETKCopyFrom(aTagContent, aPos + 1);     
end;