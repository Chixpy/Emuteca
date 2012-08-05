{
[SCRIPTDATA]
Author = Chixpy
Version = 0.1
Date = 20110121
Description =
~begin~
  Extracts data from filename cowered with GoodXXX
~end~
Changes =
~begin~
  0.1 - 20110121:
    + Initial version
~end~
[END]
}
program GoodXXXFilenames;

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

procedure ExtractGoodXXX(aGame: cGame);
var
  GameFN: String;
  TempStr: String;
  aPos, bPos: Integer;
begin
  GameFN := ChangeFileExt(aGame.FileName, '');

  // Title (and Version)
  // -----------------
  // Anything before a flag

  aPos := Pos('(', GameFN);
  bPos := Pos('[', GameFN);
  if aPos = 0 then
    aPos := 256;
  if bPos = 0 then
    bPos := 256;
  if bPos < aPos then
    aPos := bPos;

  aGame.Name := Trim(Copy(GameFN, 1, aPos - 1)); // Title [+ version]
  aGame.SortKey := aGame.Name;
  aGame.Version := '';
  GameFN := Trim(Copy(GameFN, aPos, 256)); // Flags

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
    // TODO: 'á', 'ñ', 'È', 'Ü', etc.
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
      else // No version found (only 1 space)
        aGame.Name := aGame.Name + ' ' + TempStr;
    end;    
  end;

  // Zone
  aGame.Zones.Clear;
  if Pos('(W)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('world');
    GameFN := AnsiReplaceText(GameFN, '(W)', '');
  end;
  if Pos('(S)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('es');
    GameFN := AnsiReplaceText(GameFN, '(S)', '');
  end;
  if Pos('(J)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('jp');
    GameFN := AnsiReplaceText(GameFN, '(J)', '');
  end;
  if Pos('(U)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('us');
    GameFN := AnsiReplaceText(GameFN, '(U)', '');
  end;
  if Pos('(JU)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('us');
    aGame.Zones.Add('jp');
    GameFN := AnsiReplaceText(GameFN, '(JU)', '');
  end;
  if Pos('(E)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('eu');
    GameFN := AnsiReplaceText(GameFN, '(E)', '');
  end;
  if Pos('(UE)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('eu');
    aGame.Zones.Add('us');
    GameFN := AnsiReplaceText(GameFN, '(UE)', '');
  end;
  if Pos('(1)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('ja');
    aGame.Zones.Add('kr');
    GameFN := AnsiReplaceText(GameFN, '(1)', '');
  end;
  if Pos('(C)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('cn');
    GameFN := AnsiReplaceText(GameFN, '(C)', '');
  end;
  if Pos('(F)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('fr');
    GameFN := AnsiReplaceText(GameFN, '(F)', '');
  end;
  if Pos('(FN)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('finland');
    GameFN := AnsiReplaceText(GameFN, '(FN)', '');
  end;
  if Pos('(G)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('gr');
    GameFN := AnsiReplaceText(GameFN, '(G)', '');
  end;
  if Pos('(HK)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('hk');
    GameFN := AnsiReplaceText(GameFN, '(HK)', '');
  end;
  if Pos('(4)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('us');
    aGame.Zones.Add('br');
    GameFN := AnsiReplaceText(GameFN, '(4)', '');
  end;
  if Pos('(B)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('br');
    GameFN := AnsiReplaceText(GameFN, '(B)', '');
  end;
  if Pos('(K)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('kr');
    GameFN := AnsiReplaceText(GameFN, '(K)', '');
  end;
  if Pos('(NL)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('nl');
    GameFN := AnsiReplaceText(GameFN, '(NL)', '');
  end;
  if Pos('(UK)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('gb');
    GameFN := AnsiReplaceText(GameFN, '(UK)', '');
  end;
  if Pos('(I)', GameFN) <> 0 then
  begin
    aGame.Zones.Add('it');
    GameFN := AnsiReplaceText(GameFN, '(I)', '');
  end;

  // Languages
  if (aGame.Zones.Count <> 0) then
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

  // Alternate
  // -----------------
  // [a??]
  aGame.Alternate := ExtractTag(GameFN, '[a', ']');

  // Bad dump
  // -----------------
  // [b??]
  aGame.BadDump := '';
  TempStr := ExtractTag(GameFN, '[b', ']');
  if TempStr <> '' then
    aGame.BadDump := 'b ' + TempStr;

  // Fixed
  // -----------------
  // [f??]
  aGame.Fixed := ExtractTag(GameFN, '[f', ']');

  // OverDump
  // -----------------
  // [o??]
  TempStr := ExtractTag(GameFN, '[o', ']');
  if TempStr <> '' then
    if aGame.BadDump <> '' then
      aGame.BadDump := '+ ' + TempStr + ', ' + aGame.BadDump
    else
      aGame.BadDump := '+ ' + TempStr;

  // Hacked
  // -----------------
  // [h??]
  aGame.Hack := ExtractTag(GameFN, '[h', ']');

  // Pirate
  // -----------------
  // [p??]
  aGame.Pirate := ExtractTag(GameFN, '[p', ']');

  // Trained
  // -----------------
  // [t??]
  aGame.Trainer := ExtractTag(GameFN, '[t', ']');

  // Traslated
  // -----------------
  // [T??]
  aGame.Translation := ExtractTag(GameFN, '[T', ']');

  // Verified Good Dump
  if ExtractTag(GameFN, '[!', ']') <> '' then
    aGame.Verified :=  True
  else
    aGame.Verified :=  False;

  aGame.Version := Trim(aGame.Version + ' ' + GameFN);
end;

var
  Game: cGame;
  i: integer;
begin
  WriteLn('Extracting GoodXXX data from filenames:');
  i := 0;
  while i < GameManager.GameCount do
  begin
    Game := GameManager.GameAtPos(i);
    ExtractGoodXXX(Game);
    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' games parsed.');
  end;

  WriteLn('');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('Done.');
end.
