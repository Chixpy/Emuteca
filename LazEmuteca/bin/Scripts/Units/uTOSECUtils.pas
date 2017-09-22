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
  // md5, crc32, etc
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

function TOSECExtractInfo(aSoftLine: string; IsSHA1: Boolean): string;
var
  DBID, DBTitle, DBSortTitle, DBVersion, DBYear, DBPublisher,
  DBZone, DBDumpStatus, DBDumpInfo, DBFixed, DBTrainer, DBTranslation,
  DBPirate, DBCracked, DBModified, DBHack: string;
  TempStr, SoftStr: string;
  i, aPos: Integer;
begin
  Result := '';
  DBID := '';
  DBTitle := '';
  DBSortTitle := '';
  DBVersion := '';
  DBYear := '';
  DBPublisher := '';
  DBZone := '';
  DBDumpStatus := '';
  DBDumpInfo := '';
  DBFixed := '';
  DBTrainer := '';
  DBTranslation := '';
  DBPirate := '';
  DBCracked := '';
  DBModified := '';
  DBHack := '';
  SoftStr := aSoftLine
  
  // XML Entities...
  SoftStr := AnsiReplaceText(SoftStr, '&amp;', ',');
  
  if Length(TOSECVideo) = 0 then
    TOSECInit;
    
  // Complete TOSEC info and order:
  //   Title version, The (demo) (Year)(Publisher)(System)(Video)(Country)
  //  (Language)(Copyright)(Devstatus)(Media Type)(Media Label)[cr][f][h][m]
  //  [p][t][tr][o][u][v][b][a][!][more info].ext  
  
  // SHA1 / ID
  aPos := Pos(TOSECIDSep, SoftStr);
  if aPos < 1 then Exit;
  DBID := Copy(SoftStr, 1, aPos -1);
  
  SoftStr := Copy(SoftStr, aPos + 1, TOSECMaxCopy);
  
  // Title (and Version)
  // -----------------
  // Anything before a flag
  
  aPos := Pos('(', SoftStr);
  if aPos = 0 then
  begin
    WriteLn(SoftStr + ': No flags found.');
    Exit;
  end;
  
  DBTitle := Trim(Copy(SoftStr, 1, aPos - 1)); // Title [+ version]
  SoftStr := Trim(Copy(SoftStr, aPos, TOSECMaxCopy)); // Flags  
  
  // Searching for version...
  // ------------------------
  // * Game Name vVersion
  // * Game Name v Version
  // * Game Name RevVersion
  // * Game Name Rev Version
  // * Game Name set 1
  // Too many cases...
  // We don't search ' v' or ' Rev', because 'Foo1 vs. Foo2' or
  //   'Foo 3 - Revenge of Fooing' are false positives.
  // aGame is already trimmed at the beginning of this script.
{ TODO: VERSION 

  aPos := RPos(' ', DBTitle);
  if (aPos > 1) then
  begin
    TempStr := Trim(Copy(DBTitle, aPos + 1, TOSECMaxCopy));
    DBTitle := Trim(Copy(DBTitle, 1, aPos - 1));
    
    // ', The' / ', A' / ', El' ? For example:
    //   'Hunt for Red October Rev 0, The (1991-01)(Hi-Tech Expressions)(US)'
    //   Changing to: 'The Hunt for Red October Rev 0'
    // Setting SortTitle then too
    // TODO: Remove version in SortTitle
    if DBTitle[Length(DBTitle)] = ',' then
    begin
      DBSortTitle := DBTitle + ' ' + TempStr
      DBTitle := Trim(Copy(DBTitle, 1, Length(DBTitle) - 1));
      DBTitle := TempStr + ' ' + DBTitle;
      aPos := RPos(' ', DBTitle);
      TempStr := Trim(Copy(DBTitle, aPos + 1, TOSECMaxCopy));
      DBTitle := Trim(Copy(DBTitle, 1, aPos - 1));
    end;
    
    
    // vVersion
    //   'v' seems to be lowcase... but in many languages, i.e. Spanish, it can
    //     fail because titles must be in lowercase...
    //   Warning: Some TOSEC titles had 'V' too :-(
    // RevVersion
    //   'Revenge'...
    // So we check the next character after de 'v' or 'Rev' is lowcase.
    // TODO: 'á', 'é', 'í', 'ó', etc.
    if // vVersion
      ((Length(TempStr) >= 2) and (TempStr[1] = 'v') and
      ((TempStr[2] < 'a') or (TempStr[2] > 'z'))) 
      or // RevVersion
      ((Length(TempStr) >= 4) and (Pos('Rev', TempStr) = 1) and
      ((TempStr[4] < 'a') or (TempStr[4] > 'z'))) then
        DBVersion := TempStr
    else // Trying with second space from te right
    begin    
      aPos := RPos(' ', DBTitle);
      if (aPos > 1) then
      begin
        // v Version and Rev Version
        TempStr := Trim(Copy(DBTitle, aPos + 1, TOSECMaxCopy)) + ' ' + TempStr;
        DBTitle := Trim(Copy(DBTitle, 1, aPos - 1));
        if (Pos('Rev ', TempStr) = 1) or (Pos('v ', TempStr) = 1) then
          DBVersion := TempStr
        else // No version found (no 'Rev' nor 'v')
          DBTitle := DBTitle + ' ' + TempStr;      
      end
      else // No version found (at last space)
        DBTitle := DBTitle + ' ' + TempStr;
    end;  
    
  end;
} 
  // Demo (opt)
  // -----------------
  // ' (demo' + ['-' + KindOfDemo] + ') '
  if Pos('(demo', SoftStr) = 1 then // SoftStr is trimmed...
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      WriteLn(aSoftLine + ': No ")" found at DEMO flag.');
      Exit;
    end;

    TempStr := AnsiReplaceText(Trim(Copy(SoftStr, 2, aPos - 2)), '-', '/');
    
    // TODO: Save tags
    //if aGame.Tags.IndexOf(TempStr) = -1 then
    //  aGame.Tags.Add(TempStr);
    
    TOSECAddStr(DBVersion, 'Demo ' + TempStr);
    
    SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy)); // Next flags
  end;
 
  // Year (obl)
  // -----------------
  // '(' + YYYY [+ '-' + MM [+ '-' + DD]] + ')'
  if Pos('(', SoftStr) <> 1 then
  begin
    WriteLn(aSoftLine + ': No YEAR found.');
    Exit;
  end;

  aPos := Pos(')', SoftStr);
  if aPos = 0 then
  begin
    WriteLn(aSoftLine + ': No ")" found at YEAR flag.');
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
    WriteLn(aSoftLine + ': No PUBLISHER found.');
    Exit;
  end;

  aPos := Pos(')', SoftStr);
  if aPos = 0 then
  begin
    WriteLn(aSoftLine + ': No ")" found at PUBLISHER flag.');
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
      WriteLn(aSoftLine + ': No ")" found at SYSTEM flag.');
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
      WriteLn(aSoftLine + ': No ")" found at VIDEO flag.');
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
      WriteLn(aSoftLine + ': No ")" found at COUNTRY flag.');
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
      // 'AS' = American Samoa
      TempStr := AnsiReplaceText(TempStr, 'as', 'asia');
            
      DBZone := LowerCase(AnsiReplaceText(TempStr, '-', ','));
      SoftStr := Trim(Copy(SoftStr, aPos + 1, TOSECMaxCopy));
    end;    
  end;

{
  // Language
  // -----------------
  // '(' + Language + ')' -> es, pt, fr, ... es-pt
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      WriteLn('"' + aSoftLine + '": No ")" found at LANGUAGE flag.');
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
    TempStr := ExtractTag(SoftStr, '(M', ')');
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

  // Copyright
  // -----------------
  // (Copyright)
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      WriteLn(aSoftLine + ': No ")" found at COPYRIGHT flag.');
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
    DBDumpStatus := DumpSt2Key(edsAlternate);
    TOSECAddStr(DBDumpInfo, 'o ' + TempStr)
  end;  
  
  TempStr := TOSECExtractTag(SoftStr, '[b', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsAlternate);
    TOSECAddStr(DBDumpInfo, 'b ' + TempStr)
  end; 

  TempStr := TOSECExtractTag(SoftStr, '[u', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsAlternate);
    TOSECAddStr(DBDumpInfo, 'u ' + TempStr)
  end;    
  
  // Extra data
  // -----------------
  // [more info]
  // Unhandled flags...
  SoftStr := Trim(SoftStr);
  if SoftStr <> '' then
  TOSECAddStr(DBDumpInfo, 'x ' + SoftStr);
 
  //"Group","SHA1","ID","Folder","FileName","Title","TransliteratedName",
  //"SortTitle","Version","Year","Publisher","Zone","DumpStatus","DumpInfo",
  //"Fixed","Trainer","Translation","Pirate","Cracked","Modified","Hack"
  if IsSHA1 then
    Result := krsImportKeepValue + ',' + DBID + ',,'
      + krsImportKeepValue + ',' + krsImportKeepValue + ',"' + DBTitle + '",'
      + krsImportKeepValue + ',"' + DBSortTitle + '","' + DBVersion + '","'
      + DBYear + '","' + DBPublisher + '","' + DBZone + '","'
      + DBDumpStatus + '","' + DBDumpInfo + '","' + DBFixed + '","'
      + DBTrainer + '","' + DBTranslation + '","' + DBPirate + '","'
      + DBCracked + '","' + DBModified + '","' + DBHack + '"'
  else
    Result := krsImportKeepValue + ',' + krsImportKeepValue + ',' + DBID + ','
      + krsImportKeepValue + ',' + krsImportKeepValue + ',"' + DBTitle + '",'
      + krsImportKeepValue + ',"' + DBSortTitle + '","' + DBVersion + '","'
      + DBYear + '","' + DBPublisher + '","' + DBZone + '","'
      + DBDumpStatus + '","' + DBDumpInfo + '","' + DBFixed + '","'
      + DBTrainer + '","' + DBTranslation + '","' + DBPirate + '","'
      + DBCracked + '","' + DBModified + '","' + DBHack + '"'
end;
