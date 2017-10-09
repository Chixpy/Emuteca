{
[Info]
Some common functions and data for TOSEC.
[Author]
Name=Chixpy
Date=20170922
[EndInfo]
}
const
  TOSECIDSep = '|';
  TOSECMaxCopy = 512;
  
var
  TOSECVideo: Array of string;
  TOSECSystem: Array of string;
  TOSECCopyright: Array of string;
  TOSECCopyrightStr: Array of string;
  
procedure TOSECInit;
begin
  TOSECVideo := ['CGA','EGA','HGC','MCGA','MDA','NTSC','NTSC-PAL',
    'PAL','PAL-60','PAL-NTSC','SVGA','VGA','XGA'];
  TOSECSystem := ['+2','+2a','+3','130XE','A1000','A1200','A1200-A4000','A2000',
    'A2000-A3000','A2024','A2500-A3000UX','A3000','A4000','A4000T','A500',
    'A500+','A500-A1000-A2000','A500-A1000-A2000-CDTV','A500-A1200',
    'A500-A1200-A2000-A4000','A500-A2000','A500-A600-A2000','A570','A600',
    'A600HD','AGA','AGA-CD32','Aladdin Deck Enhancer','CD32','CDTV',
    'Computrainer','Doctor PC Jr.','ECS','ECS-AGA','Executive','Mega ST',
    'Mega-STE','OCS','OCS-AGA','ORCH80','Osbourne 1','PIANO90','PlayChoice-10',
    'Plus4','Primo-A','Primo-A64','Primo-B','Primo-B64','Pro-Primo','ST','STE',
    'STE-Falcon','TT','TURBO-R GT','TURBO-R ST','VS DualSystem','VS UniSystem'];
  TOSECCopyright := ['CW','CW-R','FW','GW','GW-R','LW','PD','SW','SW-R'];
  TOSECCopyrightStr := ['Cardware','Cardware (registered)','Freeware','Giftware',
    'Giftware (registered)','Licenceware','Public Domain','Shareware',
    'Shareware (registered)',
    // Not in CopyrightStr, but used if none found
    'Commercial'];    
end;

procedure TOSECError(const aFile: string; aError: string);
begin
  WriteLn('TOSEC ERROR: ' + aFile);
  WriteLn('  ' + aError);
end;

procedure TOSECAddStr(var aFlag: string; const aStr: string);
begin
  if aFlag = '' then 
   aFlag := Trim(aStr)
  else
    aFlag := Trim(aFlag) + '; ' + Trim(aStr);
end;
  
function TOSECExtractTag(var Tags: String; Open, Close: String): String;
var
  oPos, cPos: Integer;
begin
  Result := '';
  oPos := Pos(Open, Tags);
  if oPos = 0 then Exit;

  cPos := PosEx(Close, Tags, oPos + Length(Open));
  if cPos = 0 then Exit;

  Result := Trim(Copy(Tags, oPos + Length(Open), cPos - oPos - Length(Open)));
  if Result = '' then Result := '1';

  Tags := Trim(Copy(Tags, 1, oPos - 1) +
    Copy(Tags, cPos + Length(Close), TOSECMaxCopy));
end;

function TOSECExtractSoftLine(aStr, aIDKey: string): string;
var
  SHA1: string;
  Title: string;
  aPos: integer;
begin
  Result := '';

  // <rom name="!Mario (2000-[..]
  aStr := Trim(aStr);
  if Pos('<rom', aStr) <> 1 then
    Exit;

  // Title - name="<Title.ext>"
  aPos := Pos('name="',aStr);
  if aPos < 1 then
    Exit;

  aStr := Copy(aStr, aPos + 6, TOSECMaxCopy);
  aPos := Pos('"', aStr);
  if aPos < 1 then
    Exit;
  Title := ExtractFilenameOnly(Copy(aStr, 1, aPos - 1));

  aStr := Copy(aStr, aPos + 1, TOSECMaxCopy);

  // SHA1 - sha1="<sha1>"
  // md5=, crc32=, etc
  aPos := Pos(LowerCase(aIDKey) + '="',aStr);
  if aPos < 1 then
    Exit;
  aStr := Copy(aStr, aPos + Length(aIDKey) + 2, TOSECMaxCopy);
  aPos := Pos('"', aStr);
  if aPos < 1 then
    Exit;
  SHA1 := Copy(aStr, 1, aPos - 1);

  Result := SHA1 + TOSECIDSep + Title;
end;

function TOSECExtractInfo(aSoftLine: string): string;
var
  DBID, DBTitle, DBSortTitle, DBVersion, DBYear, DBPublisher,
  DBZone, DBDumpStatus, DBDumpInfo, DBFixed, DBTrainer, DBTranslation,
  DBPirate, DBCracked, DBModified, DBHack: string;
  TempStr, SoftStr, TempDemo: string;
  i, aPos: Integer;
begin
  Result := '';
  DBID := '';
  DBTitle := '';
  DBSortTitle := '';
  DBVersion := '';
  DBYear := '';
  DBPublisher := '';
  DBZone := '@'; // Keep zone
  DBDumpStatus := '';
  DBDumpInfo := '';
  DBFixed := '';
  DBTrainer := '';
  DBTranslation := '';
  DBPirate := '';
  DBCracked := '';
  DBModified := '';
  DBHack := '';
  TempDemo := '';
  SoftStr := aSoftLine
  
  // XML Entities...
  SoftStr := AnsiReplaceText(SoftStr, '&amp;', '&');
  SoftStr := AnsiReplaceText(SoftStr, '&apos;', '''');
  
  if Length(TOSECVideo) = 0 then
    TOSECInit;
    
  // Complete TOSEC info and order:
  //   Title version, The v 1 (demo) (Year)(Publisher)(System)(Video)(Country)
  //  (Language)(Copyright)(Devstatus)(Media Type)(Media Label)[cr][f][h][m]
  //  [p][t][tr][o][u][v][b][a][!][more info].ext  
  
  // ID
  // --
  aPos := Pos(TOSECIDSep, SoftStr);
  if aPos < 1 then Exit;
  DBID := Copy(SoftStr, 1, aPos -1);
  
  SoftStr := Copy(SoftStr, aPos + 1, TOSECMaxCopy);
  
  // Title (+ Version) (+ Demo)
  // -----------------
  // Anything before a flag
  // FIX: Searching mandatory space, some game have '(' in Title
  //   and ' (' too.. 
  // FIX2: Searching ' (' from right, then '(demo)' will be in DBTitle.
  
  aPos := RPos(' (', SoftStr);
  if aPos < 1 then
  begin
    TOSECError(aSoftLine, 'No flags found.');
    Exit;
  end;
  
  DBTitle := Trim(Copy(SoftStr, 1, aPos - 1)); // Title [+ version] [+ Demo]
  SoftStr := Trim(Copy(SoftStr, aPos, TOSECMaxCopy)); // Flags 
     
  // Demo (opt)
  // -----------------
  // ' (demo' + ['-' + KindOfDemo] + ') '
  // Extracting from DBTitle, and keep in TempDemo
  TempDemo := TOSECExtractTag(DBTitle, '(demo', ')');
  if TempDemo <> '' then
  begin
    if Pos('-', TempDemo) = 1 then
      TempDemo := Trim(Copy(TempDemo, 2, TOSECMaxCopy));
  end;  

  // Playing with Title [+ version]
  // Too many problems
  // * ', The' / ', A' / ', El' / etc. 
  //   Some games have "Game, The v 1", others "Game v 1, The " (wrong)
  // * Version
  //   Game v1
  //   Game v 1
  //   Game Rev1
  //   Game Rev 1
  //   Game set 1
  //   Game PRG 1
  //   Game Whatever
  //   Too many cases...  
  aPos := RPos(' ', DBTitle);
  if (aPos > 1) then
  begin
    TempStr := Trim(Copy(DBTitle, aPos + 1, TOSECMaxCopy)); // Last word
    DBTitle := Trim(Copy(DBTitle, 1, aPos - 1)); 
           
    // FIX: Checking wrong article position after version
    // As security check, article must have less than 5 chars (trimmed)  
    if (Length(TempStr) < 5) and
      (CompareText(DBTitle[Length(DBTitle)], ',') = 0) then
    begin 
      DBSortTitle := DBTitle + ' ' + TempStr // Keep as sort title
      DBTitle := Trim(Copy(DBTitle, 1, Length(DBTitle) - 1)); // Removing ','
      DBTitle := TempStr + ' ' + DBTitle; // Moving 'The' at beginning 
      
      aPos := RPos(' ', DBTitle); // Always aPos > 1
      TempStr := Trim(Copy(DBTitle, aPos + 1, TOSECMaxCopy)); // Last word
      DBTitle := Trim(Copy(DBTitle, 1, aPos - 1)); 
    end;    
    
    // Trying to extract version...
    // Length(TempStr) > 0 always
    
    // if Length(TempStr) = 1 then "V" => "5" Roman 
    if Length(TempStr) > 1 then
    begin
      if CompareText(TempStr[1], 'v') = 0 then
      begin // try vXXXXXX
        // A testing to diferenciate version with any other word...
        //   if TempStr[2] = number we can be sure that is a version.
        if (TempStr[2] <= '9') and (TempStr[2] >= '0') then
          TOSECAddStr(DBVersion, 'v ' + Trim(Copy(TempStr, 2, TOSECMaxCopy)));
      end
      else if (Length(TempStr) > 4) and 
        (Pos('rev', LowerCase(TempStr)) = 1) then
      begin // try revXXXXXX
         if (TempStr[4] <= '9') and (TempStr[4] >= '0') then
          TOSECAddStr(DBVersion, 'v rev' + Trim(Copy(TempStr, 4, TOSECMaxCopy)));
      end;
    end;
    
    // if not version found, try with last 2 words
    if DBVersion = '' then
    begin
      aPos := RPos(' ', DBTitle);
      
      if aPos > 1 then
      begin
        TempStr := Trim(Copy(DBTitle, aPos + 1, TOSECMaxCopy)) + 
          ' ' + TempStr; // Last 2 words
        DBTitle := Trim(Copy(DBTitle, 1, aPos - 1));         
         
        if Pos('v ', LowerCase(TempStr)) = 1 then
        begin // try v XXXXXX
          TOSECAddStr(DBVersion, 'v ' + Trim(Copy(TempStr, 2, TOSECMaxCopy)));
        end
        else if Pos('rev ', LowerCase(TempStr)) = 1 then
        begin // try rev XXXXXX
          TOSECAddStr(DBVersion, 'v rev' + Trim(Copy(TempStr, 4, TOSECMaxCopy)));
        end;
      end;     
    end;
    

    if DBVersion = '' then
      // Restoring Title
      DBTitle := DBTitle + ' ' + TempStr;     
    
    // Checking well placed article before version
    // If there is no version, it's already checked, so we will do that if
    //   DBSortTitle = '';
    // If title has two ',' (one for article), this will mess the title
    if DBSortTitle = '' then
    begin
      aPos := RPos(' ', DBTitle); 
      if aPos > 1 then
      begin     
        // As security artique must have less than 6 chars (5 + space)
        if ((Length(DBTitle) - aPos) < 6) and 
          (DBTitle[aPos - 1] = ',') then
        begin
          DBSortTitle := DBTitle; // Keep as sort title
          TempStr := Trim(Copy(DBTitle, aPos + 1, TOSECMaxCopy)); // Last word
          DBTitle := Trim(Copy(DBTitle, 1, aPos - 2)); // Removing ','          
          DBTitle := TempStr + ' ' + DBTitle; // Moving 'The' at beginning 
        end;
      end; 
    end;  

    WriteLn(DBTitle);
    WriteLn(DBVersion);    
    
  end;  
  
  // Adding Demo in Version
  if TempDemo <> '' then
    TOSECAddStr(DBVersion, 'Demo ' + TempDemo);
    
  // Year (obl)
  // -----------------
  // '(' + YYYY [+ '-' + MM [+ '-' + DD]] + ')'
  if Pos('(', SoftStr) <> 1 then
  begin
    TOSECError(aSoftLine, 'No YEAR found.');
    Exit;
  end;

  aPos := Pos(')', SoftStr);
  if aPos = 0 then
  begin
    TOSECError(aSoftLine, 'No ")" found at YEAR flag.');
    Exit;
  end;

  TempStr := Trim(Copy(SoftStr, 2, aPos - 2)); 
  DBYear := AnsiReplaceText(TempStr, '-', '/');
 
  SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy)); // Next flags
  
  
  // Publisher (obl)
  // -----------------
  // '(' + PublisherName + ')'
  if Pos('(', SoftStr) <> 1 then
  begin
    TOSECError(aSoftLine, 'No PUBLISHER found.');
    Exit;
  end;

  aPos := Pos(')', SoftStr);
  if aPos = 0 then
  begin
    TOSECError(aSoftLine, 'No ")" found at PUBLISHER flag.');
    Exit;
  end;

  DBPublisher := Trim(Copy(SoftStr, 2, aPos - 2)); // Publisher
  if DBPublisher = '-' then
    DBPublisher := krsImportKeepValue;

  SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy)); // Next flags
  
  
  // System (opt)
  // -----------------
  // '(' + System + ')'
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at SYSTEM flag.');
      Exit;
    end;

    TempStr := Copy(SoftStr, 2, aPos - 2);

    i := Low(TOSECSystem);
    while i <= High(TOSECSystem) do
    begin
      if TempStr = TOSECSystem[i] then
      begin
        TOSECAddStr(DBVersion, 'System ' + TempStr);
        SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy));
        i := High(TOSECSystem); // Break;
      end;
      Inc(i);
    end;
  end;

  // Video
  // -----------------
  // '(' + ('PAL'|'NTSC'|etc.) + ')'
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at VIDEO flag.');
      Exit;
    end;

    TempStr := Copy(SoftStr, 2, aPos - 2);

    i := Low(TOSECVideo);
    while i <= High(TOSECVideo) do
    begin
      if TempStr = TOSECVideo[i] then
      begin
        TOSECAddStr(DBVersion, 'Video ' + TempStr);
        SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy));
        i := High(TOSECVideo); // Break;
      end;
      Inc(i);
    end;
  end;

  // Country
  // -----------------
  // '(' + Country + ')' -> US, EU, JP, ... US-EU
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at COUNTRY flag.');
      Exit;
    end;
        
    TempStr := Copy(SoftStr, 2, aPos - 2);
    
    // Check if no its a Copyright tag...
    // GW = Guinea-Bisau and CW = Curaçao ... :-/
    i := Low(TOSECCopyright);
    while (i <= High(TOSECCopyright)) and (TempStr <> '') do
    begin
      if TOSECCopyright[i] = TempStr then TempStr := '';
      Inc(i);
    end;    

    if ((Length(TempStr) + 1) mod 3 = 0) and (TempStr = AnsiUpperCase(TempStr))
    then
    begin
      // 'AS' = American Samoa -> 'XA' = asia
      TempStr := AnsiReplaceText(TempStr, 'as', 'xa');
            
      DBZone := LowerCase(AnsiReplaceText(TempStr, '-', ','));
      SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy));
    end;    
  end;
  
  // Language
  // --------
  
  // Removing (M) flag, it don't actually says wich languages have.
  // If (M) found then don't try to search language
  TempStr := TOSECExtractTag(SoftStr, '(M', ')');
  if TempStr = '' then
  begin
    // TODO: Search language(s)
    // After searching... Emuteca don't store it in Soft Data, but it can be
    //   used for a "future" Tag tree
  
{
  // Language
  // -----------------
  // '(' + Language + ')' -> es, pt, fr, ... es-pt
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at LANGUAGE flag.');
      Exit;
    end;
    
    TempStr := Copy(SoftStr, 2, aPos - 2);
    
    if ((Length(TempStr) + 1) mod 3 = 0) and (TempStr = AnsiLowerCase(TempStr))
    then
    begin
      TempStr := '"' + AnsiReplaceText(TempStr, '-', '","') + '"';
      aGame.Languages.CommaText := TempStr;
      aGame.Languages.Sort;
      SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy)); 
    end;    
  end;
  // if no languages found...
  if (aGame.Languages.Count = 0) and (aGame.Zones.Count <> 0) then
  begin
    TempStr := 
    aPos := StrToIntDef(TempStr, 0);
    // We add zones to languajes...
    aGame.Languages.CommaText := aGame.Zones.CommaText;

    // if there are less zones that the number in M flag
    //   we adds undefined lenguages.
    while aPos > aGame.Languages.Count do
    begin
      aGame.Languages.Add('Undefined lang ' + IntToStr(
        aGame.Languages.Count));
    end;
  end;
}
  end;

{
  // Copyright
  // -----------------
  // (Copyright)
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at COPYRIGHT flag.');
      Exit;
    end;
    
    TempStr := Copy(SoftStr, 2, aPos - 2);

    i := Low(CopyrightStr);
    while i <= High(CopyrightStr) do
    begin
      if TempStr = CopyrightStr[i] then
      begin
        aGame.License := CopyrightEmu[i];
        SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy));
        i := High(CopyrightStr);
      end;
      Inc(i);
    end;
  end;
  if aGame.License = '' then aGame.License := CopyrightEmu[High(CopyrightEmu)];
}
  // Development status
  // -----------------
  // (Devstatus)
  
  // Automatically added at the end.
  // TODO 2: Maybe add as flag
  
  // Media type
  // -----------------
  // (Media Type)

  // Automatically added at the end.
  // TODO 2: Maybe add as flag

  // Media label
  // -----------------
  // (Media Label)

  // Automatically added at the end.  

  // Cracked
  // -----------------
  // '[cr' [+ ' ' + Cracker] + ']'
  DBCracked := TOSECExtractTag(SoftStr, '[cr', ']');

  // Fixed
  // -----------------
  // [f]
  DBFixed := TOSECExtractTag(SoftStr, '[f', ']');

  // Hacked
  // -----------------
  // [h]
  DBHack := TOSECExtractTag(SoftStr, '[h', ']');

  // Modified
  // -----------------
  // [m]
  DBModified := TOSECExtractTag(SoftStr, '[m', ']');
  // Restoring missidentified NES '[mapper 34]'
  if Pos('apper', DBModified) = 1 then
  begin
    SoftStr := SoftStr + '[m' + DBModified + ']';
    DBModified := '';
  end;

  // Pirated
  // -----------------
  // [p]
  DBPirate := TOSECExtractTag(SoftStr, '[p', ']');

  // Traslated
  // -----------------
  // It must be searched before [t]
  // [tr]
  DBTranslation := TOSECExtractTag(SoftStr, '[tr', ']');

  // Trained
  // -----------------
  // [t]
  DBTrainer := TOSECExtractTag(SoftStr, '[t', ']');

  // Verified, Good, Alternate, OverDump, BadDump, UnderDump
  // -------------------------------------------------------
  // [!], '', [a], [o], [b], [u]
  // Although some flags can coexist: [a][o]
  // We only keep the worst one
  
  DBDumpStatus := DumpSt2Key(edsGood);
  
  TempStr := TOSECExtractTag(SoftStr, '[!', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsVerified);
  end;
  
  TempStr := TOSECExtractTag(SoftStr, '[a', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsAlternate);
    TOSECAddStr(DBDumpInfo, 'a ' + TempStr)
  end;  
  
  TempStr := TOSECExtractTag(SoftStr, '[o', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsOverDump);
    TOSECAddStr(DBDumpInfo, 'o ' + TempStr)
  end;  
  
  TempStr := TOSECExtractTag(SoftStr, '[b', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsBadDump);
    TOSECAddStr(DBDumpInfo, 'b ' + TempStr)
  end; 

  TempStr := TOSECExtractTag(SoftStr, '[u', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsUnderDump);
    TOSECAddStr(DBDumpInfo, 'u ' + TempStr)
  end;    
  
  // Extra data
  // -----------------
  // [more info]
  // Unhandled flags...
  TempStr := TOSECExtractTag(SoftStr, '(', ')');
  while TempStr <> '' do
  begin
    TOSECAddStr(DBVersion, '(' + TempStr + ')');
    TempStr := TOSECExtractTag(SoftStr, '(', ')');
  end;

  TempStr := TOSECExtractTag(SoftStr, '[', ']');
  while TempStr <> '' do
  begin
    TOSECAddStr(DBDumpInfo, '[' + TempStr + ']');
    TempStr := TOSECExtractTag(SoftStr, '[', ']');
  end;

  if SoftStr <> '' then
  begin
    TOSECAddStr(DBDumpInfo, 'x ' + SoftStr);
  end;
 
  //"Group","SHA1","ID","Folder","FileName","Title","TransliteratedName",
  //"SortTitle","Version","Year","Publisher","Zone","DumpStatus","DumpInfo",
  //"Fixed","Trainer","Translation","Pirate","Cracked","Modified","Hack"
  Result := krsImportKeepValue + ',,' + DBID + ','
    + krsImportKeepValue + ',' + krsImportKeepValue + ',"' + DBTitle + '",'
    + krsImportKeepValue + ',"' + DBSortTitle + '","' + DBVersion + '","'
    + DBYear + '","' + DBPublisher + '","' + DBZone + '","'
    + DBDumpStatus + '","' + DBDumpInfo + '","' + DBFixed + '","'
    + DBTrainer + '","' + DBTranslation + '","' + DBPirate + '","'
    + DBCracked + '","' + DBModified + '","' + DBHack + '"'
end;
