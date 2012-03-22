{
[SCRIPTDATA]
Author = Chixpy
Version = 0.1
Date = 20110121
Description =
~begin~
  Extracts data from filename cowered with TOSEC
~end~
Changes =
~begin~
  0.2 - 20111128:
    + More flags handled
    m Better version extraction
  0.1 - 20110121:
    + Initial version
~end~
}
program TOSECFilenames;

var
  VideoStr: Array of String;
  SystemStr: Array of String;
  CopyrightStr: Array of String;
  CopyrightEmu: Array of String;

function ExtractTag(var Tags: String; Open, Close: String): String;
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

  Tags := Trim(Copy(Tags, 1, oPos - 1) + Copy(Tags, cPos + Length(Close), 512));
end;



procedure ExtractTOSEC(aGame: cGame);
var
  GameFN: String;
  TempStr: String;
  TempStrList: TStringList;
  i, aPos: Integer;
begin
  // Complete TOSEC info and order:
  //   Title version (demo) (Year)(Publisher)(System)(Video)(Country)(Language)
  //  (Copyright)(Devstatus)(Media Type)(Media Label)[cr][f][h][m][p][t][tr][o]
  //  [u][v][b][a][!][more info].ext

  GameFN := ChangeFileExt(aGame.FileName, '');

  // Title (and Version)
  // -----------------
  // Anything before a flag
  aPos := Pos('(', GameFN);
  if aPos = 0 then
  begin
    WriteLn(aGame.FileName + ': No flags found.');
    Exit;
  end;

  aGame.Name := Trim(Copy(GameFN, 1, aPos - 1)); // Title [+ version]
  aGame.Version := '';
  GameFN := Trim(Copy(GameFN, aPos, 512)); // Flags

  // Searching for version...
  // ------------------------
  // * Game Name vVersion
  // * Game Name v Version
  // * Game Name RevVersion
  // * Game Name Rev Version

  // We don't search ' v' or ' Rev', because 'Foo1 vs. Foo2' or
  //   'Foo 3 - Revenge of Fooing' are false positives.
  // aGame is already trimmed at the beginning of this script.
  aPos := RPos(' ', aGame.Name);
  if (aPos > 1) then
  begin
    TempStr := Trim(Copy(aGame.Name, aPos + 1, 512));
    aGame.Name := Trim(Copy(aGame.Name, 1, aPos - 1));
    
    if aGame.Name[Length(aGame.Name)] = ',' then
    begin
      // Argh, some wrong named TOSEC (2011-11-11), for example:
      //   'Hunt for Red October Rev 0, The (1991-01)(Hi-Tech Expressions)(US)'
      // Changing to: 'The Hunt for Red October Rev 0'
      // Anyway, Emuteca prefers 'The Emuteca' over 'Emuteca, The'
      //   (sorting is done with cGame.SortKey property wich holds the later)
      aGame.Name := Trim(Copy(aGame.Name, 1, Length(aGame.Name) - 1));
      aGame.Name := TempStr + ' ' + aGame.Name;
      aPos := RPos(' ', aGame.Name);
      TempStr := Trim(Copy(aGame.Name, aPos + 1, 512));
      aGame.Name := Trim(Copy(aGame.Name, 1, aPos - 1));
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
        aGame.Version := TempStr
    else // Trying with secon space from te right
    begin    
      aPos := RPos(' ', aGame.Name);
      if (aPos > 1) then
      begin
        // v Version and Rev Version
        TempStr := Trim(Copy(aGame.Name, aPos + 1, 512)) + ' ' + TempStr;
        aGame.Name := Trim(Copy(aGame.Name, 1, aPos - 1));
        if (Pos('Rev ', TempStr) = 1) or (Pos('v ', TempStr) = 1) then
          aGame.Version := TempStr
        else // No version found (no 'Rev' nor 'v')
          aGame.Name := aGame.Name + ' ' + TempStr;      
      end
      else // No version found (at last space)
        aGame.Name := aGame.Name + ' ' + TempStr;
    end;    
  end;

  // Demo (opt)
  // -----------------
  // '(demo' + ['-' + KindOfDemo] + ')'
  if Pos('(demo', GameFN) = 1 then
  begin
    aPos := Pos(')', GameFN);
    if aPos = 0 then
    begin
      WriteLn(aGame.FileName + ': No ")" found at DEMO flag.');
      Exit;
    end;

    TempStr := AnsiReplaceText(Trim(Copy(GameFN, 2, aPos - 2)), '-', '/');
    if aGame.Tags.IndexOf(TempStr) = -1 then
      aGame.Tags.Add(TempStr);

    GameFN := Trim(Copy(GameFN, aPos + 1, 512)); // Next flags
  end;

  // Year (obl)
  // -----------------
  // '(' + YYYY [+ '-' + MM [+ '-' + DD]] + ')'
  if Pos('(', GameFN) <> 1 then
  begin
    WriteLn(aGame.FileName + ': No YEAR found.');
    Exit;
  end;

  aPos := Pos(')', GameFN);
  if aPos = 0 then
  begin
    WriteLn(aGame.FileName + ': No ")" found at YEAR flag.');
    Exit;
  end;

  TempStr := Trim(Copy(GameFN, 2, aPos - 2));

  // Some GoodXXX filenames had "(Year)(Publisher)" and can be confused...
  //   They can fail because "(Version)(Year)(Publisher)" but we try
  //   to read the flag...
  if Pos('.', TempStr) <> 0 then
  begin
    WriteLn(aGame.FileName + ': "." found at year flag... ' +
      'Maybe it''s GoodXXX Filename...');

    // Giving it a try... but some thing bad can result...
    // Skipping (Version) tag
    if (TempStr[1] <> 'v') and (TempStr[1] <> 'V') then
       TempStr := 'v' + TempStr;
    if (TempStr[1] = 'V') then
       TempStr := 'v' + Copy(TempStr, 2, 512);
    if aGame.Version <> '' then
      aGame.Version := aGame.Version + ', ' + TempStr
    else
      aGame.Version := TempStr;

    GameFN := Trim(Copy(GameFN, aPos + 1, 512)); // Next flags

    if Pos('(', GameFN) <> 1 then
    begin
      WriteLn(aGame.FileName + ': No YEAR found, at GoodXXX try.');
      Exit;
    end;

    aPos := Pos(')', GameFN);
    if aPos = 0 then
    begin
      WriteLn(aGame.FileName + ': No ")" found at YEAR flag in GoodXXX try.');
      Exit;
    end;

    TempStr := Trim(Copy(GameFN, 2, aPos - 2));
  end;

  // Trying to fix inconsistent dates... from GoodXXX :-/
  case Length(TempStr) of
    4: // YYYY No problem
    begin
      ;
    end;
    6: // YYYYMM -> YYYY-MM (if MMYYYY then it goes wrong...)
    begin
      TempStr:= Copy(TempStr, 1, 4) + '-' + Copy(TempStr, 5, 2);
    end;
    7: // YYYY-MM or MM-YYYY
    begin
      if TempStr[3] = '-' then
        TempStr:= Copy(TempStr, 4, 4) + '-' + Copy(TempStr, 1, 2);
    end;
    8: // YYYYMMDD  (YYYYDDMM or DDMMYYYY or MMDDYYYY will add it wrong)
    begin
      TempStr:= Copy(TempStr, 1, 4) + '-' + Copy(TempStr, 5, 2) + '-'
        + Copy(TempStr, 7, 2);
    end;
    10: // YYYY-MM-DD or DD-MM-YYYY  (MM-DD-YYYY...)
    begin
      if TempStr[3] = '-' then
        TempStr:= Copy(TempStr, 7, 4) + '-' + Copy(TempStr, 4, 2) + '-'
          + Copy(TempStr, 1, 2);
    end;
   end;

   case Length(TempStr) of
    4,7,10: // (YYYY) or (YYYY-MM) or (YYYY-MM-DD)
    begin
      // Simple check for "19XX" or "2XXX"
      if (TempStr[1] <> '1') and (TempStr[1] <> '2') then
      begin
        WriteLn(aGame.FileName + ': Error of YEAR flag, it doesn''t ' +
         'begin with "1" or "2".');
        Exit;
      end;

      aGame.Year := AnsiReplaceText(TempStr, '-', '/');
    end;
    else // Uhm "otherwise" don't work XD XD
    begin
      WriteLn(aGame.FileName + ': Wrong size of YEAR flag.');
      Exit;
    end;
  end;
  GameFN := Trim(Copy(GameFN, aPos + 1, 512)); // Next flags

  // Publisher (obl)
  // -----------------
  // '(' + PublisherName + ')'
  if Pos('(', GameFN) <> 1 then
  begin
    WriteLn(aGame.FileName + ': No PUBLISHER found.');
    Exit;
  end;

  aPos := Pos(')', GameFN);
  if aPos = 0 then
  begin
    WriteLn(aGame.FileName + ': No ")" found at PUBLISHER flag.');
    Exit;
  end;

  aGame.Publisher := Trim(Copy(GameFN, 2, aPos - 2)); // Publisher
  if aGame.Publisher = '-' then
    aGame.Publisher := '';

  GameFN := Trim(Copy(GameFN, aPos + 1, 512)); // Next flags

  // System (opt)
  // -----------------
  // '(' + System + ')'
  if Pos('(', GameFN) = 1 then
  begin
    aPos := Pos(')', GameFN);
    if aPos = 0 then
    begin
      WriteLn(aGame.FileName + ': No ")" found at SYSTEM flag.');
      Exit;
    end;

    TempStr := Copy(GameFN, 2, aPos - 2);

    i := Low(SystemStr);
    while i <= High(SystemStr) do
    begin
      if TempStr = SystemStr[i] then
      begin
        aGame.Version := aGame.Version + ' (' + TempStr + ')';
        GameFN := Trim(Copy(GameFN, aPos + 1, 512));
        i := High(SystemStr);
      end;
      Inc(i);
    end;
  end;

  // Video
  // -----------------
  // '(' + ('PAL'|'NTSC'|etc.) + ')'
  if Pos('(', GameFN) = 1 then
  begin
    aPos := Pos(')', GameFN);
    if aPos = 0 then
    begin
      WriteLn(aGame.FileName + ': No ")" found at VIDEO flag.');
      Exit;
    end;

    TempStr := Copy(GameFN, 2, aPos - 2);

    i := Low(VideoStr);
    while i <= High(VideoStr) do
    begin
      if TempStr = VideoStr[i] then
      begin
        aGame.Version := aGame.Version + ' (' + TempStr + ')';
        GameFN := Trim(Copy(GameFN, aPos + 1, 512));
        i := High(SystemStr);
      end;
      Inc(i);
    end;
  end;

  // Country
  // -----------------
  // '(' + Country + ')' -> US, EU, JP, ... US-EU
  if Pos('(', GameFN) = 1 then
  begin
    aPos := Pos(')', GameFN);
    if aPos = 0 then
    begin
      WriteLn(aGame.FileName + ': No ")" found at COUNTRY flag.');
      Exit;
    end;
        
    TempStr := Copy(GameFN, 2, aPos - 2);
    
    // Check if no its a Copyright tag...
    // GW = Guinea-Bisau and CW = Curaçao ... :-/
    i := Low(CopyrightStr);
    while (i <= High(CopyrightStr)) and (TempStr <> '') do
    begin
      if CopyrightStr[i] = TempStr then TempStr := '';
      Inc(i);
    end;    

    if ((Length(TempStr) + 1) mod 3 = 0) and (TempStr = AnsiUpperCase(TempStr))
    then
    begin
      // 'AS' = American Samoa
      TempStr := AnsiReplaceText(TempStr, 'as', 'asia');
            
      aGame.Zones.CommaText := AnsiReplaceText(TempStr, '-', ',');
      GameFN := Trim(Copy(GameFN, aPos + 1, 512));
    end;    
  end;

  // Language
  // -----------------
  // '(' + Language + ')' -> es, pt, fr, ... es-pt
  if Pos('(', GameFN) = 1 then
  begin
    aPos := Pos(')', GameFN);
    if aPos = 0 then
    begin
      WriteLn('"' + aGame.FileName + '": No ")" found at LANGUAGE flag.');
      Exit;
    end;
    
    TempStr := Copy(GameFN, 2, aPos - 2);
    
    if ((Length(TempStr) + 1) mod 3 = 0) and (TempStr = AnsiLowerCase(TempStr))
    then
    begin
      TempStr := '"' + AnsiReplaceText(TempStr, '-', '","') + '"';
      aGame.Languages.CommaText := TempStr;
      aGame.Languages.Sort;
      GameFN := Trim(Copy(GameFN, aPos + 1, 512)); 
    end;    
  end;
  // if no languages found...
  if (aGame.Languages.Count = 0) and (aGame.Zones.Count <> 0) then
  begin
    TempStr := ExtractTag(GameFN, '(M', ')');
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
  if Pos('(', GameFN) = 1 then
  begin
    aPos := Pos(')', GameFN);
    if aPos = 0 then
    begin
      WriteLn(aGame.FileName + ': No ")" found at COPYRIGHT flag.');
      Exit;
    end;
    
    TempStr := Copy(GameFN, 2, aPos - 2);

    i := Low(CopyrightStr);
    while i <= High(CopyrightStr) do
    begin
      if TempStr = CopyrightStr[i] then
      begin
        aGame.License := CopyrightEmu[i];
        GameFN := Trim(Copy(GameFN, aPos + 1, 512));
        i := High(CopyrightStr);
      end;
      Inc(i);
    end;
  end;
  if aGame.License = '' then aGame.License := CopyrightEmu[High(CopyrightEmu)];

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
  aGame.Cracked := ExtractTag(GameFN, '[cr', ']');

  // Fixed
  // -----------------
  // [f]
  aGame.Fixed := ExtractTag(GameFN, '[f', ']');

  // Hacked
  // -----------------
  // [h]
  aGame.Hack := ExtractTag(GameFN, '[h', ']');

  // Modified
  // -----------------
  // [m]
  aGame.Modified := ExtractTag(GameFN, '[m', ']');

  // Pirated
  // -----------------
  // [p]
  aGame.Pirate := ExtractTag(GameFN, '[p', ']');

  // Traslated
  // -----------------
  // It must be searched before [t]
  // [tr]
  aGame.Translation := ExtractTag(GameFN, '[tr', ']');

  // Trained
  // -----------------
  // [t]
  aGame.Trainer := ExtractTag(GameFN, '[t', ']');

  // Bad, overdump, underdump and virus
  // ----------------------------------
  // [b] [o] [u] [v]
  // Technically [v] will go in Modified property...
  //   but better here to warn about this 'modification by use' XD
  aGame.BadDump := '';
  TempStr := ExtractTag(GameFN, '[b', ']');
  if TempStr <> '' then
    aGame.BadDump := 'b ' + TempStr;

  TempStr := ExtractTag(GameFN, '[o', ']');
  if TempStr <> '' then
    if aGame.BadDump <> '' then
      aGame.BadDump := '+ ' + TempStr + ', ' + aGame.BadDump
    else
      aGame.BadDump := '+ ' + TempStr;

  TempStr := ExtractTag(GameFN, '[u', ']');
  if TempStr <> '' then
    if aGame.BadDump <> '' then
      aGame.BadDump := '- ' + TempStr + ', ' + aGame.BadDump
    else
      aGame.BadDump := '- ' + TempStr;

  TempStr := ExtractTag(GameFN, '[v', ']');
  if TempStr <> '' then
  begin
    // Crapy check, many games have '[vX.YY]', '[v.XXXX]', '[v Y.X]' for
    //   fan translation version...
    aPos := Pos('.', TempStr);
    if (aPos = 0) or (aPos > 3) then
    begin
      if aGame.BadDump <> '' then
        aGame.BadDump := 'v ' + TempStr + ', ' + aGame.BadDump
      else
        aGame.BadDump := 'v ' + TempStr;
    end
    else
      // Restoring other info tag
      GameFN := GameFN + '[v' + TempStr + ']';
  end;
  
  // Alternate
  // -----------------
  // [a]
  aGame.Alternate := ExtractTag(GameFN, '[a', ']');

  // Known Good Dump
  // -----------------
  // [!]
  if ExtractTag(GameFN, '[!', ']') <> '' then
    aGame.Verified :=  True
  else
    aGame.Verified :=  False;

  // Extra data
  // -----------------
  // [more info]
  // Unhandled flags...
  aGame.Version := Trim(Trim(aGame.Version) + ' ' + Trim(GameFN));
end;

var
  Game: cGame;
  i: integer;
begin
  VideoStr := ['CGA','EGA','HGC','MCGA','MDA','NTSC','NTSC-PAL',
    'PAL','PAL-60','PAL-NTSC','SVGA','VGA','XGA'];
  SystemStr := ['+2','+2a','+3','130XE','A1000','A1200','A1200-A4000','A2000',
    'A2000-A3000','A2024','A2500-A3000UX','A3000','A4000','A4000T','A500',
    'A500+','A500-A1000-A2000','A500-A1000-A2000-CDTV','A500-A1200',
    'A500-A1200-A2000-A4000','A500-A2000','A500-A600-A2000','A570','A600',
    'A600HD','AGA','AGA-CD32','Aladdin Deck Enhancer','CD32','CDTV',
    'Computrainer','Doctor PC Jr.','ECS','ECS-AGA','Executive','Mega ST',
    'Mega-STE','OCS','OCS-AGA','ORCH80','Osbourne 1','PIANO90','PlayChoice-10',
    'Plus4','Primo-A','Primo-A64','Primo-B','Primo-B64','Pro-Primo','ST','STE',
    'STE-Falcon','TT','TURBO-R GT','TURBO-R ST','VS DualSystem','VS UniSystem'];
  CopyrightStr := ['CW','CW-R','FW','GW','GW-R','LW','PD','SW','SW-R'];
  CopyrightEmu := ['Cardware','Cardware (registered)','Freeware','Giftware',
    'Giftware (registered)','Licenceware','Public Domain','Shareware',
    'Shareware (registered)',
    // Not in CopyrightStr, but used if none found
    'Commercial'];    

  WriteLn('Extracting TOSEC data from filenames:');
  i := 0;
  while i < GameManager.GameCount do
  begin
    Game := GameManager.GameAtPos(i);
    ExtractTOSEC(Game);
    Inc(i);
    if i mod 100 = 0 then
      WriteLn(IntToStr(i) + ' files parsed.');
  end;
  
  WriteLn('');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('Done.');
end.
