{
[Info]
Some common functions and data for TOSEC.

It's far from perfect.

Only to include in other programs.
[Data]
Name=Chixpy
Version=0.08
Date=20171212
[Changes]
0.08 20171212
  f TOSECExtractSoftLine: Better flags search.
0.07
  + TOSECExtractSoftLine: Lenguajes searched, so Copyright may be work better.
  - TOSECExtractSoftLine: Don't Group.
0.06
  m TOSECExtractSoftLine: Group by (Sort)Title
0.05
  + TOSECExtractSoftLine: Using uETKStrUtils to extract articles
[EndInfo]
}

//uses uETKStrUtils;
{$I 'uETKStrUtils.pas'}

const
  TOSECIDSep = '|';
  TOSECValueSepBegin = '; ';
  TOSECValueSepEnd = '';
  
var
  TOSECVideo: Array of string;
  TOSECSystem: Array of string;
  TOSECCopyright: Array of string;
  TOSECCopyrightStr: Array of string;
  
procedure TOSECInit;
begin
  // TOSEC Naming Convention v4 (2015-03-23)
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
// Internal procedure to log Errors
begin
  WriteLn('TOSEC ERROR: ' + aFile);
  WriteLn('  ' + aError);
end;

procedure TOSECAddStr(var aTag: string; const aStr: string);
// Internal procedure for adding values to a Emuteca tag
begin
  if aTag = '' then 
   aTag := Trim(aStr)
  else
    aTag := Trim(aTag) + TOSECValueSepBegin + Trim(aStr) + TOSECValueSepEnd;
end;
  
function TOSECExtractTag(var Tags: String; Open, Close: String): String;
// Internal procedure for extracting a TOSEC tag
var
  oPos, oLength, cPos: Integer;
begin
  Result := '';
  oPos := Pos(Open, Tags);
  if oPos = 0 then Exit; // Not found

  oLength := Length(Open);
  cPos := PosEx(Close, Tags, oPos + oLength);
  if cPos = 0 then Exit; // Not closed?

  Result := Trim(Copy(Tags, oPos + oLength, cPos - oPos - oLength));
  if Result = '' then Result := '1'; // [a]

  // Removing readed tag
  Tags := Trim(AnsiLeftStr(Tags, oPos - 1) + ETKCopyFrom(Tags, 
    cPos + Length(Close)));
end;

procedure TOSECReadDatFile(aTOSECFile, aSoftList: TStringList;
  aIDKey: string);
// Extracts all roms names from a TOSEC dat file and saves in a aSoftList
//   with 'IDKeyValue + TOSECIDSep + ROMName' format.
// * aTOSECFile: File of TOSEC dat
// * aSoftList: Output StringList
// * aIDKey: value key - 'sha1'/'crc'/'md5'
var
  i, aPos: integer;
  aLine, aTitle, aID: string;
begin
  aIDKey := LowerCase(aIDKey);
  
  aSoftList.BeginUpdate;
  i := 0;
  while i < aTOSECFile.Count do
  begin
    aLine := aTOSECFile[i];
    
    if Pos('<rom', aLine) > 1 then
    begin
      // Searching "name=" key for Title
      // and joning next lines if not found
      aPos := Pos('name="', aLine);
      
      while (aPos < 1) and ((i + 1) < aTOSECFile.Count) do
      begin
        Inc(i);
        aLine := aLine + aTOSECFile[i];
        aPos := Pos('name="', aLine);
      end;
      
      aLine := Trim(ETKCopyFrom(aLine, aPos + 6)); // Removing beginning
      
      aPos := Pos('"', aLine);      
      // TODO: Check file end.
      while (aPos < 1) do
      begin
        Inc(i);
        aLine := aLine + aTOSECFile[i];
        aPos := Pos('"', aLine);
      end;
      
      aTitle := ExtractFilenameOnly(AnsiLeftStr(aLine, aPos - 1));
      aLine := ETKCopyFrom(aLine, aPos + 1);
      
      // Searching "<aIDKey>=" key for ID
      // SHA1 - sha1="<sha1>"
      // md5=, crc32=, etc
      aPos := Pos(aIDKey + '="', aLine);
      
      // TODO: Check file end.
      while (aPos < 1) do
      begin
        Inc(i);
        aLine := aLine + aTOSECFile[i];
        aPos := Pos(aIDKey + '="', aLine);
      end;     
      
      aLine := Trim(ETKCopyFrom(aLine, aPos + Length(aIDKey) + 2));
      
      aPos := Pos('"', aLine);      
      // TODO: Check file end.
      while (aPos < 1) do
      begin
        Inc(i);
        aLine := aLine + aTOSECFile[i];
        aPos := Pos('"', aLine);
      end;
      
      aID := ExtractFilenameOnly(AnsiLeftStr(aLine, aPos - 1));      
      
      aSoftList.Add(aID + TOSECIDSep + aTitle);
    end;   
    
    if (i and 1023) = 1023 then
      WriteLn(IntToStr(i) + ' lines analized.');
    Inc(i);
  end;
  aSoftList.EndUpdate;
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
  // ID + TOSECIDSep + TOSEC Title
  aPos := Pos(TOSECIDSep, SoftStr);
  if aPos < 2 then
    Exit;
    
  DBID := AnsiLeftStr(SoftStr, aPos - 1);
  SoftStr := ETKCopyFrom(SoftStr, aPos + 1);
  
  // Title (+ Version) (+ Demo)
  // -----------------
  // Anything before a flag
  // FIX: Searching mandatory space, some game have '(' in Title
  //   and ' (' too.. 
  // FIX2: Searching ' (' from right, then '(demo)' will be in DBTitle.
  // FIX3: Searching from first ')[', this will fix:
  //   Metroid - Wall Jump (2004)(VL-Tone)[h][Metroid (Eu)]
  
  aPos := Pos(')[', SoftStr);
  if aPos < 1 then
    aPos := Length(SoftStr);
  aPos := RPosEx(' (', SoftStr, aPos);
  if aPos < 1 then
  begin
    TOSECError(aSoftLine, 'No flags found.');
    Exit;
  end;
  
  DBTitle := Trim(AnsiLeftStr(SoftStr, aPos - 1)); // Title [+ version] [+ Demo]
  SoftStr := Trim(ETKCopyFrom(SoftStr, aPos)); // Flags 
     
  // Demo (opt)
  // -----------------
  // ' (demo' + ['-' + KindOfDemo] + ') '
  // Extracting from DBTitle, and keep in TempDemo for adding it later
  TempDemo := TOSECExtractTag(DBTitle, '(demo', ')');
  if TempDemo <> '' then
  begin
    if Pos('-', TempDemo) = 1 then
      TempDemo := Trim(ETKCopyFrom(TempDemo, 2));
  end;  

  // Playing with Title [+ version]
  // Too many problems
  // * ', The' / ', A' / ', El' / etc. 
  // * Some games have "Game, The v 1", others "Game v 1, The" (wrong)
  // * Version tags:
  //   Game v1
  //   Game v 1
  //   Game Rev1
  //   Game Rev 1
  //   Game set 1
  //   Game PRG 1
  //   Game Whatever
  //   Too many cases...  
  
  // FIX: Checking wrong article position after version
  // "Game v 1, The"
  ETKFixTitle(DBTitle, DBSortTitle, TempStr); //DBSortTitle and TempStr unused
  
  aPos := RPos(' ', DBTitle);
  if (aPos > 1) then
  begin  
    TempStr := Trim(ETKCopyFrom(DBTitle, aPos + 1)); // Last word
    DBTitle := Trim(AnsiLeftStr(DBTitle, aPos - 1));     
    
    // Trying to extract version...    
    // if Length(TempStr) = 1 then "V" => "5" Roman 
    if Length(TempStr) > 1 then
    begin
      if CompareText(TempStr[1], 'v') = 0 then
      begin // try vXXXXXX
        // A testing to diferenciate version with any other word...
        //   if TempStr[2] = number we can be sure that is a version.
        if (TempStr[2] <= '9') and (TempStr[2] >= '0') then
          TOSECAddStr(DBVersion, 'v ' + Trim(ETKCopyFrom(TempStr, 2)));
      end
      else if (Length(TempStr) > 4) then
      begin
        if (Pos('rev', LowerCase(TempStr)) = 1) then
        begin // try revXXXXXX
          if (TempStr[4] <= '9') and (TempStr[4] >= '0') then
            TOSECAddStr(DBVersion, 'v rev' + Trim(ETKCopyFrom(TempStr, 4)));
        end
        else if (Pos('prg', LowerCase(TempStr)) = 1) then
        begin // try PRGXXXXXX
          if (TempStr[4] <= '9') and (TempStr[4] >= '0') then
            TOSECAddStr(DBVersion, 'v PRG' + Trim(ETKCopyFrom(TempStr, 4)));
        end;
      end;
    end;
    
    // if not version found, try with last 2 words
    if DBVersion = '' then
    begin
      aPos := RPos(' ', DBTitle);
      
      if aPos > 1 then
      begin
        TempStr := Trim(ETKCopyFrom(DBTitle, aPos + 1)) + ' ' + TempStr; // Last 2 words
        DBTitle := Trim(AnsiLeftStr(DBTitle, aPos - 1));         
         
        if Pos('v ', LowerCase(TempStr)) = 1 then
        begin // try v XXXXXX
          TOSECAddStr(DBVersion, 'v ' + Trim(ETKCopyFrom(TempStr, 2)));
        end
        else if Pos('rev ', LowerCase(TempStr)) = 1 then
        begin // try rev XXXXXX
          TOSECAddStr(DBVersion, 'v rev' + Trim(ETKCopyFrom(TempStr, 4)));
        end
        else if Pos('prg ', LowerCase(TempStr)) = 1 then
        begin // try PRG XXXXXX
          TOSECAddStr(DBVersion, 'v PRG' + Trim(ETKCopyFrom(TempStr, 4)));
        end;
      end;     
    end;    

    if DBVersion = '' then
      // Version not found: Restoring Title
      DBTitle := DBTitle + ' ' + TempStr;  
  end;  
  
  // Checking well placed article before version
  //  and setting DBSortTitle
  ETKFixTitle(DBTitle, DBSortTitle, TempStr); //TempStr unused

  
  // Adding Demo in Version
  if TempDemo <> '' then
    TOSECAddStr(DBVersion, 'Demo ' + TempDemo);
    
  // TAGS
  // ====

  // Year (obl)
  // -----------------
  // '(' + YYYY [+ '-' + MM [+ '-' + DD]] + ')'
  TempStr := TOSECExtractTag(SoftStr, '(', ')');
  if TempStr = '' then
  begin
    TOSECError(aSoftLine, 'No YEAR found.');
    Exit;
  end;
  if TempStr = '-' then
    DBYear := krsImportKeepValueKey
  else
    DBYear := AnsiReplaceText(TempStr, '-', '/');
    
  // Publisher (obl)
  // -----------------
  // '(' + PublisherName + ')'
  TempStr := TOSECExtractTag(SoftStr, '(', ')');
  if TempStr = '' then
  begin
    TOSECError(aSoftLine, 'No YEAR or PUBLISHER found.');
    Exit;
  end;  
  if TempStr = '-' then
    DBPublisher := krsImportKeepValueKey
  else
    DBPublisher := TempStr;  
   
   
  // Next flag ()
  //TempStr := TOSECExtractTag(SoftStr, '(', ')');
  
  // System (opt)
  // -----------------
  // '(' + System + ')'
  
  
  i := 0;
  while i <= High(TOSECSystem) do
  begin
    if TempStr = TOSECSystem[i] then
    begin
      TOSECAddStr(DBVersion, 'System ' + TempStr);
      SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
      i := High(TOSECSystem); // Break;
    end;
    Inc(i);
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
        SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
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
      SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
    end;    
  end;
  
  // Language
  // --------
  // // '(' + Language + ')' -> es, pt, fr, ... es-pt
  
  // Removing (M) flag, it don't actually says wich languages has.
  // If (M) found then don't try to search language
  TempStr := TOSECExtractTag(SoftStr, '(M', ')');
  if TempStr = '' then
  begin  
  
    // Actual search
    // TODO: It's added to version, but this must go as tag...
    if Pos('(', SoftStr) = 1 then
    begin
      aPos := Pos(')', SoftStr);
      if aPos = 0 then
      begin
        TOSECError(aSoftLine, 'No ")" found at LANGUAGE flag.');
        Exit;
      end;
          
      TempStr := Copy(SoftStr, 2, aPos - 2);
      
      // We don't need to check Copyright tag because is lowercase...

      if ((Length(TempStr) + 1) mod 3 = 0) and (TempStr = AnsiLowerCase(TempStr))
      then
      begin
        TempStr := AnsiReplaceText(TempStr, '-', ',');
        TOSECAddStr(DBVersion, 'Language ' + TempStr);
        SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
      end;    
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
      TOSECError(aSoftLine, 'No ")" found at COPYRIGHT flag.');
      Exit;
    end;
    
    TempStr := Copy(SoftStr, 2, aPos - 2);

    i := Low(TOSECCopyright);
    while i <= High(TOSECCopyright) do
    begin
      if TempStr = TOSECCopyright[i] then
      begin
        TOSECAddStr(DBVersion, 'Copyright ' + TOSECCopyrightStr[i]);
        SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
        i := High(TOSECCopyright); // Break;
      end;
      Inc(i);
    end;
  end;
  
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
 
  //"Group","SHA1","ID","Folder","FileName","Title","[Removed]",
  //"SortTitle","Version","Year","Publisher","Zone","DumpStatus","DumpInfo",
  //"Fixed","Trainer","Translation","Pirate","Cracked","Modified","Hack"
  Result := '"' + krsImportKeepValueKey + '",,"' + DBID + '","'
    + krsImportKeepValueKey + '","' + krsImportKeepValueKey + '","' + DBTitle
    + '","' + krsImportKeepValueKey + '","' + DBSortTitle + '","' + DBVersion
    + '","' + DBYear + '","' + DBPublisher + '","' + DBZone + '","'
    + DBDumpStatus + '","' + DBDumpInfo + '","' + DBFixed + '","'
    + DBTrainer + '","' + DBTranslation + '","' + DBPirate + '","'
    + DBCracked + '","' + DBModified + '","' + DBHack + '"'
end;
