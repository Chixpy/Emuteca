{ Emuteca Script unit
[Info]
Some common functions and data for MAME.

It's far from perfect.

Only to include in other programs.
[Data]
Name=Chixpy
Version=0.01
Date=20221211
[Changes]
0.02 20230106
  f XMLMAMEExtractTag: Not all machines have year, manufacturer or driver
    tags...
0.01 20221211
  + MAMEReadXMLFile
  + XMLMAMEExtractTag
  + MAMEExtractInfo
[EndInfo]
}

//uses uETKXMLUtils;
{$I 'uETKXMLUtils.pas'}

function XMLMAMEExtractTag(XMLFile: TStrings; var cLine: integer): string;
// Trying to do a faster version than the generic XML version in uETKXMLUtils.
//   - It supposes that <machine> and </machine> are in different lines.
//   - It supposes that there is not a <machine/> tag in the file.
//   - LineEndings are replaced by spaces in content
// 
// XMLFile: TStrings; 
//   - String List with XML data.
// var cLine: integer;
//   - IN: Line to start searching.
//   - OUT: Last line readed.
var
  aSearch, aLine: string;
  aPos: integer;
begin
  Result := '';

  // Searching <machine> tag
  aSearch := '<machine';
  aPos := 0;
  while (aPos < 1) and (XMLFile.Count > cLine) do
  begin
    aLine := XMLFile[cLine];
    aPos := Pos(aSearch, aLine);
    Inc(cLine);
  end;
  if aPos > 0 then
    Result := Trim(ETKCopyFrom(aLine, aPos));
    
  if not (XMLFile.Count > cLine) then Exit;
    
  // Searching </machine> tag
  aSearch := '</machine';
  aPos := 0;
  while (aPos < 1) and (XMLFile.Count > cLine) do
  begin
    aLine := XMLFile[cLine];
    aPos := Pos(aSearch, aLine)       
    Result := Result + ' ' + Trim(aLine);
    Inc(cLine);   
  end;  
end;

procedure MAMEReadXMLFile(XMLFile, VersionList: TStrings);
// Add the versions found in the XML file and their properties in CSV format.
//   "ID","Parent ID","Title","Year","Company"
var
  aTag: string;
  aSL: TStringList;
  i, ShowLog: integer;
begin
  aSL := CreateStringList;
  // 0.- ID
  // 1.- Parent ID
  // 2.- Title
  // 3.- Year
  // 4.- Publisher
  // 5.- Status
  try
    i := 0;
    ShowLog := 0;
    while i < XMLFile.Count do
    begin
      aSL.Clear;      
      
      if i > ShowLog then
      begin
        WriteLn(IntToStr(i) + ' lines readed...');
        ShowLog := ShowLog + 8192;
      end;
    
      // Searching "machine" tag
      aTag := XMLMAMEExtractTag(XMLFile, i);
      
      if aTag <> '' then
      begin       
        // ID {obl}
        // <machine [...] name="{ID}" [...]>
        aSL.Add(XMLRemoveEntities(XMLExtractAttrib(aTag, 'machine', 'name')));
          
        // Parent ID {opc}  
        // <machine [...] cloneof="{ParentID}" [...]>    
        aSL.Add(XMLRemoveEntities(XMLExtractAttrib(aTag, 'machine', 'cloneof'))); 
        
        // Game title {obl}
        // <machine>[...]<description>{GameTitle}</description>[...]</machine>
        aSL.Add(XMLRemoveEntities(XMLExtractContent(aTag, 'description')));
        
        // Year {obl}
        // <machine>[...]<year>{Year}</year>[...]</machine>
        aSL.Add(XMLRemoveEntities(XMLExtractContent(aTag, 'year')));
        
        // Publisher {obl}
        // <machine>[...]<manufacturer>{Publisher}</manufacturer>[...]</machine>
        aSL.Add(XMLRemoveEntities(XMLExtractContent(aTag, 'manufacturer')));
        
        // Status {obl}
        // <machine>[...]
        //   <driver [...] status="{good|imperfect|preliminary}" [...]/>
        // [...]</machine>
        aSL.Add(XMLExtractAttrib(aTag, 'driver', 'status'));
        
        if (aSL[0] <> '') and (aSL[2] <> '') then
          VersionList.Add(aSL.CommaText);
      end;  
    end;  
  finally
    aSL.Free;
  end;
end;

procedure MAMEExtractInfo(aVersionLine: TStrings; out VersionStr: string;
  out ParentStr: string);
// Reads a stringlist (usually created from a line of the StringList returned
//   with MAMEReadXMLFile), tries to extract more info of the version from
//   the title.
// Returns the version and parent string to be added to Emuteca's database file.
var
  aSL: TStringList;
  aPos: integer;
  DBTitle, DBSortTitle, DBVersion, DBZone, DBStatus,
    DBPirate, DBHack: string;
begin
  VersionStr := '';
  ParentStr := '';
  
  while aVersionLine.Count < 6 do
    aVersionLine.Add('');

  // Spliting Name / Version
  DBTitle := aVersionLine[2]
  DBVersion := '';
  aPos := Pos('(', DBTitle);
  if aPos >= 1 then
  begin
    DBVersion := Trim(ETKCopyFrom(DBTitle, aPos));
    DBTitle := Trim(Copy(DBTitle, 1, aPos - 1));
  end;

    
  ETKFixTitle(DBTitle, DBSortTitle);
    
  // Trying to extract version info
  if Length(DBVersion) > 2 then
  begin 
    if DBVersion[1] = '(' then
      DBVersion := ETKCopyFrom(DBVersion, 2);
    if DBVersion[Length(DBVersion)] = ')' then
      DBVersion := Copy(DBVersion, 1, Length(DBVersion) - 1);
    DBVersion := UTF8TextReplace(DBVersion, ',', ';', '');
  end;
  
  // Bootlegs / Pirate
  DBPirate := '';
  if Pos('bootleg', DBVersion) > 0 then
    DBPirate := 'Bootleg';
  
  // Hacked 
  DBHack := krsImportKeepValueKey        
  if Pos('hack', DBVersion) > 0 then
    DBHack := 'Hack';
    
  // Zone
  DBZone := krsImportKeepValueKey;
  // Commented ones are useless because there is another more generic rule
  //   with the same zone.
  //   'USA' is matched with 'US'; 'Korean' with 'Korea'
  if Pos('Asia', DBVersion) > 0 then DBZone := 'xa'
  else if Pos('Austrian', DBVersion) > 0 then DBZone := 'at'
  else if Pos('Belgian', DBVersion) > 0 then DBZone := 'be'
  else if Pos('Brazil', DBVersion) > 0 then DBZone := 'br'
  else if Pos('Canadian', DBVersion) > 0 then DBZone := 'ca'
  else if Pos('China', DBVersion) > 0 then DBZone := 'cn'
  else if Pos('Chinese', DBVersion) > 0 then DBZone := 'cn'
  else if Pos('Dutch', DBVersion) > 0 then DBZone := 'nl'
  else if Pos('Euro', DBVersion) > 0 then DBZone := 'eu'
  else if Pos('Finland', DBVersion) > 0 then DBZone := 'fi'
  else if Pos('France', DBVersion) > 0 then DBZone := 'fr'
  else if Pos('French', DBVersion) > 0 then DBZone := 'fr'
  else if Pos('German', DBVersion) > 0 then DBZone := 'de'
  // else if Pos('Germany', DBVersion) > 0 then DBZone := 'de'
  else if Pos('Hispanic', DBVersion) > 0 then DBZone := 'mx'
  else if Pos('Hong Kong', DBVersion) > 0 then DBZone := 'hk'
  else if Pos('Italian', DBVersion) > 0 then DBZone := 'it'
  else if Pos('Italy', DBVersion) > 0 then DBZone := 'it'
  else if Pos('JUE', DBVersion) > 0 then DBZone := 'xw'
  else if Pos('Japan', DBVersion) > 0 then DBZone := 'jp'
  else if Pos('Korea', DBVersion) > 0 then DBZone := 'kr'
  // else if Pos('Korean', DBVersion) > 0 then DBZone := 'kr'
  else if Pos('Malaysia', DBVersion) > 0 then DBZone := 'my'
  else if Pos('New Zealand', DBVersion) > 0 then DBZone := 'nz'
  else if Pos('Norwegian', DBVersion) > 0 then DBZone := 'no'
  else if Pos('Russia', DBVersion) > 0 then DBZone := 'ru'
  else if Pos('Spain', DBVersion) > 0 then DBZone := 'es'
  else if Pos('Spanish', DBVersion) > 0 then DBZone := 'es'
  else if Pos('Swedish', DBVersion) > 0 then DBZone := 'se'
  else if Pos('Swiss', DBVersion) > 0 then DBZone := 'ch'
  else if Pos('Taiwan', DBVersion) > 0 then DBZone := 'tw'
  else if Pos('UK', DBVersion) > 0 then DBZone := 'gb'
  else if Pos('US', DBVersion) > 0 then DBZone := 'us'
  // else if Pos('USA', DBVersion) > 0 then DBZone := 'us'
  else if Pos('Ukraine', DBVersion) > 0 then DBZone := 'ua'
  else if Pos('World', DBVersion) > 0 then DBZone := 'xw';


  // Status
  DBStatus := krsImportKeepValueKey;
  // We only need to check the first character
  if Length(aVersionLine[5]) > 0 then
  begin
    case aVersionLine[5][1] of
      'g': DBStatus := DumpSt2Key(edsVerified);
      'i': DBStatus := DumpSt2Key(edsGood);
      'p': DBStatus := DumpSt2Key(edsBadDump);
    end;
  end;
  
  aSL := CreateStringList;
  try
    // Creating version string
    if aVersionLine[1] <> '' then
      aSL.Add(aVersionLine[1])  // Parent ID
    else
      aSL.Add(aVersionLine[0]); // else use ID
    aSL.Add(''); // SHA1
    aSL.Add(aVersionLine[0]); // ID
    aSL.Add(krsImportKeepValueKey); // Folder
    aSL.Add(krsImportKeepValueKey); // FileName
    aSL.Add(DBTitle); // Title
    aSL.Add(''); // [Removed]
    aSL.Add(DBSortTitle); // SortTitle
    aSL.Add(DBVersion); // Version
    aSL.Add(aVersionLine[3]); // Year
    aSL.Add(aVersionLine[4]); // Publisher
    aSL.Add(DBZone); // Zone
    aSL.Add(DBStatus); // DumpStatus
    aSL.Add(krsImportKeepValueKey); // DumpInfo
    aSL.Add(krsImportKeepValueKey); // Fixed
    aSL.Add(krsImportKeepValueKey); // Trainer
    aSL.Add(krsImportKeepValueKey); // Translation
    aSL.Add(DBPirate); // Pirate
    aSL.Add(krsImportKeepValueKey); // Cracked
    aSL.Add(krsImportKeepValueKey); // Modified
    aSL.Add(DBHack); // Hack
    
    VersionStr := aSL.CommaText;
    
    // Creating parent string
    if aVersionLine[1] = '' then
    begin
     aSL.Clear;       
     aSL.Add(aVersionLine[0]); // ID
     aSL.Add(DBTitle); // Title
     aSL.Add(DBSortTitle); // Sort title
     aSL.Add(aVersionLine[3]); // Year
     aSL.Add(aVersionLine[4]); // Developer
     aSL.Add(''); // [Removed]
     
     ParentStr := aSL.CommaText;
    end;
  finally
    aSL.Free
  end;
  
end;



