{ Emuteca Script unit
[Info]
Some common functions and data for TOSEC. It's far from perfect.

Only to include in other programs. Remember call TOSECFinish at the end of
  main program.
[Data]
Name=Chixpy
Version=0.19
Date=20230529
[Changes]
0.19 20230529
  c ¡¡AAAHHH!! Some TOSEC dats actually have hacker in publisher field, so we
    keep the original again when a hack (and trainer) is found). 
    Actually this script is used only to create an initial "official", but...
0.18 20230527
  c Translation version and author moved to "Version" and "Publisher" fields.
  - Removing original Publisher and Year from translated, hacked or trained 
    software (Cracked, Modified, Pirated or Fixed keep original).
0.17 20230513
  - Removed "Registered"ware in TOSECCRStr.txt, as they are "full" versions,
    unless otherwise stated ("demo, promo"). "Public Domain" removed too.
  + Trying to get translation version in "[v..]" tag
0.16 20230509
  - Removed Alternate DumpStatus in Emuteca.
  c "Verified" -> "Favorite" DumpStatus Emuteca's change.
  c  "/" -> " / "
0.15 20230503
  c Formating translation to "languaje/Translator/version", actually
    translation version is not extracted.
  c Multiple valued tags separated separte them with "/"
0.14 20221211
  f Better return string for TOSECExtractInfo using TStringList.CommaText
0.13 20221023
  f It seems that can be many [aka] flags... ignoring all.
0.12 20221021
  c System, Video arrays changed to file loaded TStringLists.
  f Faster? searching of System and Video, thanks to sorted TStringLists.
0.11 20221018
  f Ignoring "[aka XXX]" flag, so it's not marked as alternate dump [a].
    We don't need to know alternate names in tags, it's the purpourse of
    Emuteca groups...
  f Better zone and language testing
  f Reverting to GoodDump, if DumpStatus it not found.
  c Zone is no keeped by default.
0.10 20221016
  f ¡¡WOOOPS!! System flag was not readed correctly. So sometimes was added to
    zone tag...
  f Little fix in version tag.
  c Removed "()[]" from Version and DumpInfo.
  c Removed some pseudo-flags added in version tag: "System". "Video", etc.
  c Languaje flag ignored.
0.09 20220901
  f TOSECExtractInfo: Some system don't use "[!]" flag, so keep current
      DumpStatus if none is found.
0.08 20171212
  f TOSECExtractSoftLine: Better flags search.
0.07
  + TOSECExtractSoftLine: Languages searched, so Copyright may be work better.
  - TOSECExtractSoftLine: Don't change current groups.
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
  TOSECValueSep = ' / ';
  
var
  TOSECVideoList: TStringList;
  TOSECSystemList: TStringList;
  TOSECCopyrightList: TStringList;
  TOSECCopyrightStrList: TStringList;

procedure TOSECInit;
begin
  TOSECVideoList := CreateStringList;
  TOSECVideoList.LoadFromFile('Scripts\Units\TOSECVideo.txt');
  TOSECVideoList.CaseSensitive := True;
  TOSECVideoList.Sorted := True;

  TOSECSystemList := CreateStringList;
  TOSECSystemList.LoadFromFile('Scripts\Units\TOSECSystem.txt');
  TOSECSystemList.CaseSensitive := True;
  TOSECSystemList.Sorted := True;

  TOSECCopyrightList := CreateStringList;
  TOSECCopyrightList.LoadFromFile('Scripts\Units\TOSECCRKey.txt');
  TOSECCopyrightList.CaseSensitive := True;
  TOSECCopyrightList.Sorted := True;

  TOSECCopyrightStrList := CreateStringList;
  TOSECCopyrightStrList.CaseSensitive := False;
  TOSECCopyrightStrList.Sorted := False;
  TOSECCopyrightStrList.LoadFromFile('Scripts\Units\TOSECCRStr.txt');

end;

procedure TOSECFinish;
begin
  // TOSECVideoList.SaveToFile('Scripts\Tests\TOSECVideo.txt');
  TOSECVideoList.Free;
  // TOSECSystemList.SaveToFile('Scripts\Tests\TOSECSystem.txt');
  TOSECSystemList.Free;
  // TOSECCopyrightList.SaveToFile('Scripts\Tests\TOSECCRKey.txt');
  TOSECCopyrightList.Free;
  // TOSECCopyrightStrList.SaveToFile('Scripts\Tests\TOSECCRStr.txt');
  TOSECCopyrightStrList.Free;
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
    aTag := Trim(aTag) + TOSECValueSep + Trim(aStr);
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
  if Result = '' then Result := '1'; // [b]

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
      // XML Entities...
      aTitle := AnsiReplaceText(aTitle, '&amp;', '&');
      aTitle := AnsiReplaceText(aTitle, '&apos;', '''');
      // Trying to change ' & ' if used to separate multiple games...
      aTitle := AnsiReplaceText(aTitle, ') & ', ') + ');
      aTitle := AnsiReplaceText(aTitle, '] & ', '] + ');
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
  aSL: TStringList;
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
  TempDemo := '';
  SoftStr := aSoftLine
   
  if not assigned(TOSECCopyrightStrList) then
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
  begin
    TOSECError(aSoftLine, 'No "ID|Title" found.');
    Exit;
  end;
    
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
    // No flags found, but has a title...
    DBTitle := SoftStr
    SoftStr := '';
  end
  else
  begin  
    DBTitle := Trim(AnsiLeftStr(SoftStr, aPos - 1)); // Title [+ version] [+ Demo]
    SoftStr := Trim(ETKCopyFrom(SoftStr, aPos)); // Flags 
  end;
     
  // Demo (opt)
  // -----------------
  // " (demo[-KindOfDemo]) "  <- It have spaces arround it but I don't care
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
  //   Game v1    <--- OK, if version is a number
  //   Game v 1   <--- OK, unless "v" is part of the title itself
  //   Game Rev1  <--- OK, if version is a number
  //   Game Rev 1 <--- OK, unless "v" is part of the title itself
  //   Game set 1 <--- Not searched in title
  //   Game PRG1  <--- 2022: Seems that they are "rev PRGX" now
  //   Game Whatever <--- It will with title
  //   Too many cases...
  
  // FIX: Checking wrong article position after version
  // "Game v 1, The"
  ETKFixTitle(DBTitle, DBSortTitle); //DBSortTitle is unused
  
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
          TOSECAddStr(DBVersion, 'v' + Trim(ETKCopyFrom(TempStr, 2)));
      end
      else if (Length(TempStr) > 4) then
      begin
        if (Pos('rev', LowerCase(TempStr)) = 1) then
        begin // try revXXXXXX
          if (TempStr[4] <= '9') and (TempStr[4] >= '0') then
            TOSECAddStr(DBVersion, 'v' + Trim(ETKCopyFrom(TempStr, 4)));
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
          TOSECAddStr(DBVersion, 'v' + Trim(ETKCopyFrom(TempStr, 3)));
        end
        else if Pos('rev ', LowerCase(TempStr)) = 1 then
        begin // try rev XXXXXX
          TOSECAddStr(DBVersion, 'v' + Trim(ETKCopyFrom(TempStr, 5)));
        end;
      end;     
    end;    

    if DBVersion = '' then
      // Version not found: Restoring Title
      DBTitle := DBTitle + ' ' + TempStr;  
  end;  
  
  // Checking well placed article before version
  //  and setting DBSortTitle
  ETKFixTitle(DBTitle, DBSortTitle);

  
  // Adding Demo in Version
  if TempDemo <> '' then
    TOSECAddStr(DBVersion, Trim('Demo ' + TempDemo));
    
  // TAGS
  // ====

  // Year (obl)
  // -----------------
  // "(YYYY[-MM[-DD]])"
  TempStr := TOSECExtractTag(SoftStr, '(', ')');
  if TempStr = '' then
  begin
    TOSECError(aSoftLine, 'No YEAR found.');
    Exit;
  end;
  if TempStr = '-' then
    DBYear := krsImportKeepValueKey
  else
    DBYear := AnsiReplaceText(TempStr, '-', '/'); // Don't use TOSECValueSep
    
  // Publisher (obl)
  // -----------------
  // "(Publisher[ - Publisher])"
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
  DBPublisher := AnsiReplaceText(DBPublisher, ' - ', TOSECValueSep);

  // Next flag (), But is used at the end because "()" can be inside "[]"
  //TempStr := TOSECExtractTag(SoftStr, '(', ')');


  // System (opt)
  // -----------------
  // "(System)" -> Fixed list of systems
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at SYSTEM flag.');
      Exit;
    end;

    TempStr := Copy(SoftStr, 2, aPos - 2);

    i := 0;
    while i < TOSECSystemList.Count do
    begin
      if TOSECSystemList[i] > TempStr then
      // TOSECSystemList is sorted, so we can skip searching
      begin
        i := TOSECSystemList.Count; // Break;
      end
      else
      begin
        if TempStr = TOSECSystemList[i] then
        begin
          TOSECAddStr(DBVersion, TempStr);
          SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
          i := TOSECSystemList.Count; // Break;
        end;
      end;

      Inc(i);
    end;
  end;
  

  // Video (opt)
  // -----------------
  // "(Video)" -> Fixed list of video
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at VIDEO flag.');
      Exit;
    end;

    TempStr := Copy(SoftStr, 2, aPos - 2);

    i := 0;
    while i < TOSECVideoList.Count do
    begin
      if TOSECVideoList[i] > TempStr then
      // TOSECVideoList is sorted, so we can skip searching
      begin
        i := TOSECVideoList.Count; // Break;
      end
      else
      begin
        if TempStr = TOSECVideoList[i] then
        begin
          TOSECAddStr(DBVersion, TempStr);
          SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
          i := TOSECVideoList.Count; // Break;
        end;
      end;

      Inc(i);
    end;
  end;

  // Country
  // -------
  // "(Country)" -> US, EU, JP, ... US-EU
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at COUNTRY flag.');
      Exit;
    end;
        
    TempStr := Copy(SoftStr, 2, aPos - 2);
    
    // Dirty check of development status or system
    //   and testing uppercase
    i := 1;
    while i <= Length(TempStr) do
    begin
      case (i mod 3) of
        0: begin
          if TempStr[i] <> '-' then
            TempStr := '';
        end;
        else
        begin
          if (TempStr[i] < 'A') or  (TempStr[i] > 'Z') then
            TempStr := '';
        end;
      end;

      Inc(i);
    end;

    // Dirty check if not is a Copyright tag...
    // GW = Guinea-Bisau and CW = Curaçao ... :-/
    if TempStr <> '' then
    begin
      i := 0;
      while i < TOSECCopyrightList.Count do
      begin
        if TOSECCopyrightList[i] > TempStr then
        // TOSECCopyrightList is sorted, so we can skip searching
        begin
          i := TOSECCopyrightList.Count; // Break;
        end
        else
        begin
          if TempStr = TOSECCopyrightList[i] then
          begin
            TempStr := '';
            i := TOSECCopyrightList.Count; // Break;
          end;
        end;

        Inc(i);
      end;
    end;

    if ((Length(TempStr) + 1) mod 3) = 0 then
    begin
      // 'AS' = American Samoa -> 'XA' = asia
      TempStr := AnsiReplaceText(TempStr, 'as', 'xa');
            
      DBZone := LowerCase(AnsiReplaceText(TempStr, '-', ','));
      SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
    end;    
  end;
  
  // Language
  // --------
  // "(Language)" -> es, pt, fr, ... es-pt
  
  // Removing (M) flag, it don't actually says wich languages.
  // If (M) found then don't try to search language
  TempStr := TOSECExtractTag(SoftStr, '(M', ')');
  if TempStr = '' then
  begin
    // Actual search
    if Pos('(', SoftStr) = 1 then
    begin
      aPos := Pos(')', SoftStr);
      if aPos = 0 then
      begin
        TOSECError(aSoftLine, 'No ")" found at LANGUAGE flag.');
        Exit;
      end;
          
      TempStr := Copy(SoftStr, 2, aPos - 2);
      
      // Dirty check of well formated language
      //   and testing lowercase
      i := 1;
      while i <= Length(TempStr) do
      begin
        case (i mod 3) of
          0: begin
            if TempStr[i] <> '-' then
              TempStr := '';
          end;
          else
          begin
            if (TempStr[i] < 'a') or (TempStr[i] > 'z') then
              TempStr := '';
          end;
        end;
        Inc(i);
      end;

      // We don't need to check Copyright tag because it is lowercase...

      if ((Length(TempStr) + 1) mod 3) = 0 then
      begin
        // Ignoring language tag after all XD
        // TempStr := AnsiReplaceText(TempStr, '-', ',');
        // TOSECAddStr(DBVersion, TempStr);
        SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
      end;    
    end;
  end;

  // Copyright
  // -----------------
  // "(Copyright)" -> Fixed list
  
  if Pos('(', SoftStr) = 1 then
  begin
    aPos := Pos(')', SoftStr);
    if aPos = 0 then
    begin
      TOSECError(aSoftLine, 'No ")" found at COPYRIGHT flag.');
      Exit;
    end;
    
    TempStr := Copy(SoftStr, 2, aPos - 2);

    i := 0;
    while i < TOSECCopyrightList.Count do
    begin
      if TOSECCopyrightList[i] > TempStr then
      // TOSECCopyrightList is sorted, so we can skip searching
      begin
        i := TOSECCopyrightList.Count; // Break;
      end
      else
      begin
        if TempStr = TOSECCopyrightList[i] then
        begin
          TOSECAddStr(DBVersion, TOSECCopyrightStrList[i]);
          SoftStr := Trim(ETKCopyFrom(SoftStr, aPos + 1));
          i := TOSECCopyrightList.Count; // Break;
        end;
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

  // Dump flags: "[]"

  // Cracked
  // -----------------
  // "[cr Cracker]"
  DBCracked := TOSECExtractTag(SoftStr, '[cr', ']');
  DBCracked := AnsiReplaceText(DBCracked, ' - ', TOSECValueSep);

  
  // Fixed
  // -----------------
  // "[f Fix Fixer]"
  DBFixed := TOSECExtractTag(SoftStr, '[f', ']');

  // Hacked
  // -----------------
  // "[h Hack Hacker]"
  DBHack := TOSECExtractTag(SoftStr, '[h', ']');

  // Modified
  // -----------------
  // "[m Modification]"

  // Removing NES '[mapper XX]'
  TempStr := TOSECExtractTag(SoftStr, '[mapper', ']');

  DBModified := TOSECExtractTag(SoftStr, '[m', ']');

  // Restoring '[mapper XX]'
  if TempStr <> '' then
    SoftStr := '[mapper ' + TempStr + ']' + SoftStr;

  // Pirated
  // -----------------
  // "[p Pirate]"
  DBPirate := TOSECExtractTag(SoftStr, '[p', ']');

  // Traslated
  // -----------------
  // It must be searched before [t]
  // "[tr Language Translator]"
  DBTranslation := TOSECExtractTag(SoftStr, '[tr', ']');

  // Posible formats... :-( 
  //   "[tr Language Translator]"
  //   "[tr Language Version Translator]"
  //   "[tr Language Translator][vXXXX, Other, dump, tags]"
  //   "[tr Language Translator][v.XXXX, Other, dump, tags]"
  // And... [v] [v Virus],  
  // Sometimes it doesn't have "v" or it's only a percentage... "100%"
  
  TempStr := ''; // Used to store version
  if Length(DBTranslation) > 3 then
  begin
    // Searching for Version and Translator
    aPos := Pos(' ', DBTranslation);
    if aPos > 0 then
    begin
      // "[tr Language Translator]"
      DBPublisher := Trim(ETKCopyFrom(DBTranslation, aPos + 1));
      DBTranslation := Trim(Copy(DBTranslation, 1, aPos - 1));  
    
      // "[tr Language Version Translator]"
      if (DBPublisher <> '') and (DBPublisher[1] = 'v') then
      begin
        aPos := Pos(' ', DBPublisher);
   
        if aPos > 0 then
        begin  
          TempStr := Trim(Copy(DBPublisher, 1, aPos - 1));
          DBPublisher := Trim(ETKCopyFrom(DBPublisher, aPos + 1));
        end; 
      end;
      // TODO: Test percentaje "45%" 
    end;
    
    // Trying [tr ...][vXXXX, Other, dump, tags]
    if TempStr = '' then
    begin
      TempStr := TOSECExtractTag(SoftStr, '[v', ']');
      if TempStr <> '' then
      begin
        // Is it a virus tag "[v Virus]"?
        if (TempStr = '1') or (TempStr[1] = ' ') then
        begin
          // Restoring it
          SoftStr := SoftStr + '[v ' + TempStr + ']';
          TempStr := '';
        end
        else
        begin    
          // Sometimes it is "vXXXXX" others "v.XXXX"...
          if TempStr[1] = '.' then
            TempStr := ETKCopyFrom(TempStr, 2);

          TempStr := 'v' + TempStr;
            
          // Sometimes it is "[v1.0, other tag, other...]" :-(
          // Sometimes it is "[other tag, other, trans v, ...]" :-(
          // Sometimes it is "[v1.0, other tag, other...]" :-(
          aPos := Pos(',', TempStr);
          if aPos > 1 then
          begin
            // Restoring other DumpStatus tags
            SoftStr := SoftStr + '[' + 
              AnsiReplaceText(ETKCopyFrom(TempStr, aPos + 1), ',', '][') + ']';
            TempStr := Copy(TempStr, 1, aPos - 1);
          end;
        end;
      end;      
    end;
  end;
  // Translation version found.
  if TempStr <> '' then
    TOSECAddStr(DBVersion, TempStr);

  // Trained
  // -------
  // "[t +x Trainer]"
  DBTrainer := TOSECExtractTag(SoftStr, '[t', ']');
  DBTrainer := AnsiReplaceText(DBTrainer, ' - ', TOSECValueSep);
  // Translation author and version have preference
  if (DBTranslation = '') and (DBTrainer <> '') then
  begin
    if DBTrainer[1] = '+' then
    begin    
      aPos := Pos(' ', DBTrainer);
      if aPos > 0 then
      begin
        DBPublisher := Trim(ETKCopyFrom(DBTrainer, aPos + 1));
        DBTrainer := Trim(Copy(DBTrainer, 1, aPos - 1));
      end;
    end
    else if (DBTrainer[1] < '0') or (DBTrainer[1] > '9') then
    begin
      DBPublisher := DBTrainer;
      DBTrainer := '1';
    end;
  end;

  // Removing [aka ] flags...
  TempStr := 'x';
  while TempStr <> '' do
    TempStr := TOSECExtractTag(SoftStr, '[aka', ']');

  // Verified, Good, Alternate, OverDump, BadDump, UnderDump
  // -------------------------------------------------------
  // [!], '', [a], [o], [b], [u]
  // Although some flags can coexist: [a][o]
  // We only keep the worst one
  
  // 2023/02/09: "a" flag removed from Emuteca
  TempStr := TOSECExtractTag(SoftStr, '[a', ']');
//  if TempStr <> '' then
//  begin
//    DBDumpStatus := DumpSt2Key(edsAlternate);
//    TOSECAddStr(DBDumpInfo, 'a ' + TempStr)
//  end;  
  
  TempStr := TOSECExtractTag(SoftStr, '[!', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsFavorite);
  end;

  TempStr := TOSECExtractTag(SoftStr, '[o', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsOverDump);
  end;  
  
  TempStr := TOSECExtractTag(SoftStr, '[b', ']');
  if TempStr <> '' then
  begin
    DBDumpStatus := DumpSt2Key(edsBadDump);
  end; 

  TempStr := TOSECExtractTag(SoftStr, '[u', ']');
  if TempStr <> '' then
  begin    
    DBDumpStatus := DumpSt2Key(edsUnderDump);
  end;

  if DBDumpStatus = '' then
    // DBDumpStatus := krsImportKeepValueKey;
    DBDumpStatus := DumpSt2Key(edsGood);
    
  // Extra data
  // -----------------
  // [v][more info]
  // Unhandled flags...
  TempStr := TOSECExtractTag(SoftStr, '[', ']');
  while TempStr <> '' do
  begin
    TOSECAddStr(DBDumpInfo, TempStr);
    TempStr := TOSECExtractTag(SoftStr, '[', ']');
  end;

  TempStr := TOSECExtractTag(SoftStr, '(', ')');
  while TempStr <> '' do
  begin
    TOSECAddStr(DBVersion, TempStr);
    TempStr := TOSECExtractTag(SoftStr, '(', ')');
  end;

  if SoftStr <> '' then
  begin
    TOSECAddStr(DBDumpInfo, SoftStr);
  end;
  
  aSL := CreateStringList;
  try
    aSL.Add(krsImportKeepValueKey); // Group
    aSL.Add(''); // SHA1
    aSL.Add(DBID); // ID
    aSL.Add(krsImportKeepValueKey); // Folder
    aSL.Add(krsImportKeepValueKey); // FileName
    aSL.Add(DBTitle); // Title
    aSL.Add(''); // [Removed]
    aSL.Add(DBSortTitle); // SortTitle
    aSL.Add(DBVersion); // Version
    aSL.Add(DBYear); // Year
    aSL.Add(DBPublisher); // Publisher
    aSL.Add(DBZone); // Zone
    aSL.Add(DBDumpStatus); // DumpStatus
    aSL.Add(DBDumpInfo); // DumpInfo
    aSL.Add(DBFixed); // Fixed
    aSL.Add(DBTrainer); // Trainer
    aSL.Add(DBTranslation); // Translation
    aSL.Add(DBPirate); // Pirate
    aSL.Add(DBCracked); // Cracked
    aSL.Add(DBModified); // Modified
    aSL.Add(DBHack); // Hack
    
    Result := aSL.CommaText;
  finally
    aSL.Free
  end;
end;
