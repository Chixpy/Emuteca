{ Emuteca Script unit
[Info]
Some common functions and data for TOSEC. It's far from perfect.

Only to include in other programs. Remember call TOSECFinish at the end of
  main program.
[Data]
Name=Chixpy
Version=0.20
Date=20231101
[Changes]
0.20 20231101
  c Don't remove not empty fields.
  c Reworked a little all the code.
  f Some single value tags added ' /' at the end.
  c Changed:
    - Multivalue flags separator: " / " -> " | ".
    - Tags without a value: "1" -> "*".
    - Zone: "," -> "-"
  + Added a const for debuging in TOSECExtractInfo
0.19 20230529
  c ¡¡AAAHHH!! Some TOSEC dats actually have hacker in publisher field, so we
    keep the original again when a hack (and trainer) is found). 
    Actually this script is used only to create an initial Emuteca DB, but...
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
  TOSECValueSep = ' | ';
  TOSECTagWOVal = '*';
  
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

procedure TOSECAddStr(var aTag: string; aStr: string);
// Internal procedure for adding values to a Emuteca tag
begin
  aStr := Trim(aStr);
  aTag := Trim(aTag);
  if aStr = '' then Exit;

  if aTag = '' then 
   aTag := aStr
  else
    aTag := aTag + TOSECValueSep + aStr;
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
  if Result = '' then Result := TOSECTagWOVal; // Tag without value "[!]"

  // Removing readed tag
  Tags := Trim(Copy(Tags, 1, oPos - 1) + ETKCopyFrom(Tags,
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
  VerStr, DIStr, TempStr, TempDemo: string;
  aSL: TStringList;
  i, aPos: Integer;
  CorrectFlag, DEBUG: Boolean;
begin
  DEBUG := False;
  
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
  DIStr := '';
  
  VerStr := aSoftLine
   
  if not assigned(TOSECCopyrightStrList) then
    TOSECInit;
      
  // ID
  // --
  // VerStr: ID + TOSECIDSep + TOSEC Filename
  aPos := Pos(TOSECIDSep, VerStr);
  if aPos < 2 then
  begin
    TOSECError(aSoftLine, 'No "ID' + TOSECIDSep + 'Title" found.');
    Exit;
  end;
    
  DBID := Trim(AnsiLeftStr(VerStr, aPos - 1));
  VerStr := Trim(ETKCopyFrom(VerStr, aPos + Length(TOSECIDSep)));

  if DBID = '' then
  begin
    TOSECError(aSoftLine, 'No "ID" found.');
    Exit;
  end;

  if VerStr = '' then
  begin
    TOSECError(aSoftLine, 'No "Title" found.');
    Exit;
  end;
  
  // Complete TOSEC filename info and order:
  //  Title, The v1 (demo) (Year)(Publisher)(System)(Video)(Country)
  //  (Language)(Copyright)(Devstatus)(Media Type)(Media Label)[cr][f][h][m]
  //  [p][t][tr][o][u][v][b][a][!][more info].ext  

  // FIX:
  // Splitting version and dump info because "(" can be inside "[]" tags.
  // For example: Metroid - Wall Jump (2004)(VL-Tone)[h][Metroid (Eu)] 
  aPos := Pos(')[', VerStr);
  if aPos > 1 then
  begin
    DIStr := ETKCopyFrom(VerStr, aPos + 1); // Dump Info
    VerStr := Copy(VerStr, 1, aPos); // Title + Version
  end;

  // Title (+ Version) (+ Demo)
  // -----------------
  // Anything before a flag
  // FIX: Searching mandatory space, some game have '(' in Title
  //   and ' (' too.. so we will search ' (' from right, 
  //   then '(demo)' will be in DBTitle to extract it later.
  
  aPos := RPos(' (', VerStr);
  if aPos < 1 then
  begin
    // No flags found, but has a title...
    DBTitle := VerStr
    VerStr := '';
  end
  else
  begin  
    DBTitle := Trim(AnsiLeftStr(VerStr, aPos - 1)); // Title [+ version] [+ Demo]
    VerStr := Trim(ETKCopyFrom(VerStr, aPos)); // Flags 
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
  ETKFixSortTitle(DBTitle, DBSortTitle); //DBSortTitle is unused
  
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
        if (TempStr[2] >= '0') and (TempStr[2] <= '9') then
          TOSECAddStr(DBVersion, 'v' + Trim(ETKCopyFrom(TempStr, 2)));
      end
      else if (Length(TempStr) > 4) then
      begin
        if (Pos('rev', LowerCase(TempStr)) = 1) then
        begin // try revXXXXXX
          if (TempStr[2] >= '0') and (TempStr[2] <= '9') then
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
  ETKFixSortTitle(DBTitle, DBSortTitle);
   
  // Adding Demo in Version
  if TempDemo <> '' then
  begin
    if TempDemo = TOSECTagWOVal then TempDemo := '';
    TOSECAddStr(DBVersion, 'Demo ' + TempDemo);
  end;
  
  // =============
  // REQUIRED TAGS
  // =============

  // Year (obl)
  // ----------
  // "(YYYY[-MM[-DD]])"
  TempStr := TOSECExtractTag(VerStr, '(', ')');
  if TempStr = '' then
  begin
    TOSECError(aSoftLine, 'No YEAR found.');
    Exit;
  end;
  if TempStr <> '-' then
    DBYear := TempStr; // '-' is automatically changed to '/' by Emuteca.

  // Publisher (obl)
  // ---------------
  // "(Publisher[ - Publisher])"
  TempStr := TOSECExtractTag(VerStr, '(', ')');
  if TempStr = '' then
  begin
    TOSECError(aSoftLine, 'No YEAR or PUBLISHER found.');
    Exit;
  end;  
  if TempStr = '-' then
    DBPublisher := ''
  else
    DBPublisher := TempStr;
  DBPublisher := AnsiReplaceText(DBPublisher, ' - ', TOSECValueSep); 

  // ===================
  // OTHER VERSION FLAGS
  // ===================
  
  // Next version flag...
  TempStr := TOSECExtractTag(VerStr, '(', ')');
  
  // System (opt)
  // -----------------
  // "(System)" -> Fixed list of systems  
  if TempStr <> '' then
  begin
    aPos := TOSECSystemList.IndexOf(TempStr);
    
    if aPos >= 0 then // Found
    begin
      TOSECAddStr(DBVersion, TempStr);
      // Next version flag...
      TempStr := TOSECExtractTag(VerStr, '(', ')');
    end;
  end;  

  // Video (opt)
  // -----------------
  // "(Video)" -> Fixed list of video flag
  if TempStr <> '' then
  begin
    aPos := TOSECVideoList.IndexOf(TempStr);

    if aPos >= 0 then // Found
    begin
      TOSECAddStr(DBVersion, TempStr);
      // Next version flag...
      TempStr := TOSECExtractTag(VerStr, '(', ')');
    end;
  end;

  // Country
  // -------
  // "(Country)" -> US, EU, JP, ... US-EU
  if TempStr <> '' then
  begin     
    // Checking string lenght: 2, 5, 8, 11...
    CorrectFlag := ((Length(TempStr) + 1) mod 3) = 0; 

    // Check if not is a Copyright tag...
    // GW = Guinea-Bisau and CW = Curaçao ... :-/
    // But it will be added later.
    if CorrectFlag and (TOSECCopyrightList.IndexOf(TempStr) >= 0) then
      CorrectFlag := False;    
    
    // Testing string format.
    i := 1;
    while CorrectFlag and (i <= Length(TempStr)) do
    begin
      case (i mod 3) of
        0: begin // Country separator
          if TempStr[i] <> '-' then
            CorrectFlag := False;
        end;
        else
        begin // Uppercase letter
          if (TempStr[i] < 'A') or (TempStr[i] > 'Z') then
            CorrectFlag := False;
        end;
      end;

      Inc(i);
    end;

    if CorrectFlag then
    begin
      // 'AS' = American Samoa -> 'XA' = asia
      TempStr := AnsiReplaceText(TempStr, 'as', 'xa');
      
      DBZone := LowerCase(TempStr);
      // Next version flag...
      TempStr := TOSECExtractTag(VerStr, '(', ')');
    end;    
  end;

  // Language
  // --------
  // "(Language)" -> es, pt, fr, ... es-pt and "(M)" flag 
  // Actually we want to remove both to extract copyright flag.  
  if TempStr <> '' then
  begin
    if TempStr[1] = 'M' then
    begin // '(Mx)'
      if Length(TempStr) > 1 then
      begin
        if (TempStr[2] >= '0') or (TempStr[2] <= '9') then
          TempStr := TOSECExtractTag(VerStr, '(', ')');
      end
      else
      begin
        TempStr := TOSECExtractTag(VerStr, '(', ')');
      end;
    end
    else
    begin // '(es-en)'
      // Checking string lenght: 2, 5, 8, 11...
      CorrectFlag := ((Length(TempStr) + 1) mod 3) = 0; 
      
      // We don't need to check Copyright tag because it is lowercase...
 
      // Testing string format.
      i := 1;
      while CorrectFlag and (i <= Length(TempStr)) do
      begin
        case (i mod 3) of
          0: begin // Language separator
            if TempStr[i] <> '-' then
              CorrectFlag := False;
          end;
          else
          begin // Lowercase letter
            if (TempStr[i] < 'a') or (TempStr[i] > 'z') then
              CorrectFlag := False;
          end;
        end;

        Inc(i);
      end;

      if CorrectFlag then
        // Next version flag...
        TempStr := TOSECExtractTag(VerStr, '(', ')');      
    end;
  end;

  // Copyright
  // -----------------
  // "(Copyright)" -> Fixed list
  if TempStr <> '' then
  begin
     aPos := TOSECCopyrightList.IndexOf(TempStr);

    if aPos >= 0 then // Found
    begin
      TOSECAddStr(DBVersion, TOSECCopyrightStrList[aPos]);
      // Next version flag...
      TempStr := TOSECExtractTag(VerStr, '(', ')');
    end;
  end; 
    
  // Extra version data
  // ------------------
  // (Devstatus)
  // (Media Type)
  // (Media Label)
  // Another unhandled flags  
  while TempStr <> '' do
  begin
    TOSECAddStr(DBVersion, TempStr);
    TempStr := TOSECExtractTag(VerStr, '(', ')');
  end;

  if VerStr <> '' then
  begin
    TOSECAddStr(DBVersion, VerStr);
  end;

  // ================
  // Dump flags: "[]"
  // ================

  // Cracked
  // -----------------
  // "[cr Cracker]"
  TempStr := TOSECExtractTag(DIStr, '[cr', ']');
  TempStr := AnsiReplaceText(TempStr, ' - ', TOSECValueSep);
  if TempStr <> '' then
    DBCracked := TempStr;
  
  // Fixed
  // -----------------
  // "[f Fix Fixer]"
  TempStr := TOSECExtractTag(DIStr, '[f', ']');
  if TempStr <> '' then
    DBFixed := TempStr;

  // Hacked
  // -----------------
  // "[h Hack Hacker]"
  TempStr := TOSECExtractTag(DIStr, '[h', ']');
  if TempStr <> '' then
    DBHack := TempStr;

  // Modified
  // -----------------
  // "[m Modification]"

  // Removing NES '[mapper XX]'
  TempDemo := TOSECExtractTag(DIStr, '[mapper', ']');

  TempStr := TOSECExtractTag(DIStr, '[m', ']');
  if TempStr <> '' then
    DBModified := TempStr;

  // Restoring '[mapper XX]'
  if TempDemo <> '' then
    DIStr := DIStr + '[Mapper ' + TempDemo + ']';

  // Pirated
  // -----------------
  // "[p Pirate]"
  TempStr := TOSECExtractTag(DIStr, '[p', ']');
  if TempStr <> '' then
    DBPirate := TempStr;

  // Traslated
  // -----------------
  // It must be searched before [t]
  // "[tr Language Translator]"
  DBTranslation := TOSECExtractTag(DIStr, '[tr', ']');

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
    // If soft is translated then Publisher is the translator
    DBPublisher := krsImportKeepValueKey; 
    
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
      TempStr := TOSECExtractTag(DIStr, '[v', ']');
      if TempStr <> '' then
      begin
        // Is it a virus tag "[v Virus]"?
        if (TempStr = '1') or (TempStr[1] = ' ') then
        begin
          // Restoring it
          DIStr := DIStr + '[v ' + TempStr + ']';
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
            DIStr := DIStr + '[' + 
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
  DBTrainer := TOSECExtractTag(DIStr, '[t', ']');
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
    else if (DBTrainer[1] > '9') and (DBTrainer[1] <> TOSECTagWOVal) then
    begin
      DBPublisher := DBTrainer;
      DBTrainer := TOSECTagWOVal;
    end;
  end;

  // Removing [aka ] flags...
  TempStr := 'x';
  while TempStr <> '' do
    TempStr := TOSECExtractTag(DIStr, '[aka ', ']');

  // Verified, Good, Alternate, OverDump, BadDump, UnderDump
  // -------------------------------------------------------
  // [!], '', [a], [o], [b], [u]
  // Although some flags can coexist: [a][o]
  // We only keep the worst one
  
  // 2023/02/09: "a" flag removed from Emuteca
  TempStr := TOSECExtractTag(DIStr, '[a', ']');
  if TempStr <> '' then
  begin
    if (TempStr[1] > '9') and (TempStr[1] <> TOSECTagWOVal) then
      DIStr := DIStr + '[A' + TempStr  + ']';
  end;
  
  TempStr := TOSECExtractTag(DIStr, '[!', ']');
  if TempStr <> '' then
  begin
    if (TempStr[1] > '9') and (TempStr[1] <> TOSECTagWOVal) then
      DIStr := DIStr + '[!' + TempStr + ']'
    else
      DBDumpStatus := DumpSt2Key(edsFavorite);
  end;

  TempStr := TOSECExtractTag(DIStr, '[o', ']');
  if TempStr <> '' then
  begin
    if (TempStr[1] > '9') and (TempStr[1] <> TOSECTagWOVal) then
      DIStr := DIStr + '[O' + TempStr + ']'
    else
      DBDumpStatus := DumpSt2Key(edsOverDump);
  end;
  
  TempStr := TOSECExtractTag(DIStr, '[b', ']');
  if TempStr <> '' then
  begin
    if (TempStr[1] > '9') and (TempStr[1] <> TOSECTagWOVal) then
      DIStr := DIStr + '[B' + TempStr + ']'
    else
      DBDumpStatus := DumpSt2Key(edsBadDump);
  end;

  TempStr := TOSECExtractTag(DIStr, '[u', ']');
  if TempStr <> '' then
  begin
    if (TempStr[1] > '9') and (TempStr[1] <> TOSECTagWOVal) then
      DIStr := DIStr + '[U' + TempStr + ']'
    else
      DBDumpStatus := DumpSt2Key(edsUnderDump);
  end;

  // Extra data
  // -----------------
  // [v][more info]
  // Unhandled flags...
  TempStr := TOSECExtractTag(DIStr, '[', ']');
  while TempStr <> '' do
  begin
    TOSECAddStr(DBDumpInfo, TempStr);
    TempStr := TOSECExtractTag(DIStr, '[', ']');
  end;

  if DIStr <> '' then
  begin
    TOSECAddStr(DBDumpInfo, DIStr);
  end;
  
  // Keep values not extracted
  if DBVersion = '' then DBVersion := krsImportKeepValueKey;
  if DBYear = '' then DBYear := krsImportKeepValueKey;
  if DBPublisher = '' then DBPublisher := krsImportKeepValueKey;
  if DBZone = '' then DBZone := krsImportKeepValueKey;
  if DBDumpStatus = '' then DBDumpStatus := krsImportKeepValueKey;
  if DBDumpInfo = '' then DBDumpInfo := krsImportKeepValueKey;
  if DBFixed = '' then DBFixed := krsImportKeepValueKey;
  if DBTrainer = '' then DBTrainer := krsImportKeepValueKey;
  if DBTranslation = '' then DBTranslation := krsImportKeepValueKey;
  if DBPirate = '' then DBPirate := krsImportKeepValueKey;
  if DBCracked = '' then DBCracked := krsImportKeepValueKey;
  if DBModified = '' then DBModified := krsImportKeepValueKey;
  if DBHack = '' then DBHack := krsImportKeepValueKey;    
    
// For testing purpouses
if DEBUG then WriteLn('');
if DEBUG then WriteLn(aSoftLine);
if DEBUG then WriteLn('         DBID: ' + DBID); // ID
if DEBUG then WriteLn('      DBTitle: ' + DBTitle); // Title
if DEBUG then WriteLn('  DBSortTitle: ' + DBSortTitle); // SortTitle
if DEBUG then WriteLn('    DBVersion: ' + DBVersion); // Version
if DEBUG then WriteLn('       DBYear: ' + DBYear); // Year
if DEBUG then WriteLn('  DBPublisher: ' + DBPublisher); // Publisher
if DEBUG then WriteLn('       DBZone: ' + DBZone); // Zone
if DEBUG then WriteLn(' DBDumpStatus: ' + DBDumpStatus); // DumpStatus
if DEBUG then WriteLn('   DBDumpInfo: ' + DBDumpInfo); // DumpInfo
if DEBUG then WriteLn('      DBFixed: ' + DBFixed); // Fixed
if DEBUG then WriteLn('    DBTrainer: ' + DBTrainer); // Trainer
if DEBUG then WriteLn('DBTranslation: ' + DBTranslation); // Translation
if DEBUG then WriteLn('     DBPirate: ' + DBPirate); // Pirate
if DEBUG then WriteLn('    DBCracked: ' + DBCracked); // Cracked
if DEBUG then WriteLn('   DBModified: ' + DBModified); // Modified
if DEBUG then WriteLn('       DBHack: ' + DBHack); // Hack

  // Finishing
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
