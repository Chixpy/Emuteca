{
[SCRIPTDATA]
Author = Chixpy
Version = 0.1
Date = 20110121
Description =
~begin~
  Fix and extends zone and language acronyms.
~end~
Changes =
~begin~
  0.1 - 20111129:
    + Initial version
~end~
}
program TOSECFilenames;

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

var
  aGame: cGame;
  i, j, k, m: Integer;
  Found: Boolean;
  ZoneAcronyms: Array of Array of String;
  LangAcronyms: Array of Array of String;
begin
  // Zones...
  //   http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
  //   http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
  // I need to add more...
  SetLength(ZoneAcronyms, 22);
  ZoneAcronyms[0] := ['world','w'];
  ZoneAcronyms[1] := ['jp','jpn','japan','j','jap','ja'];
  ZoneAcronyms[2] := ['eu','eur','europe','e'];
  ZoneAcronyms[3] := ['us','usa','united states','u'];
  ZoneAcronyms[4] := ['asia','as','a']; // !!! 'as'= American Samoa
  ZoneAcronyms[5] := ['tw','twn','taiwan'];
  ZoneAcronyms[6] := ['kr','kor','south korea'];
  ZoneAcronyms[7] := ['br','bra','brazil'];
  ZoneAcronyms[8] := ['au','aus','australia'];
  ZoneAcronyms[9] := ['gb','gbr','united kingdom','uk'];
  ZoneAcronyms[10] := ['es','esp','spain','sp','spa'];
  ZoneAcronyms[11] := ['ru','rus','rusia'];
  ZoneAcronyms[12] := ['hk','hkg','hong kong'];
  ZoneAcronyms[13] := ['ca','can','canada'];
  ZoneAcronyms[14] := ['cn','chn','china'];
  ZoneAcronyms[15] := ['de','deu','germany'];
  ZoneAcronyms[16] := ['dk','dnk','denmark'];
  ZoneAcronyms[17] := ['fr','fra','france'];
  ZoneAcronyms[18] := ['gr','grc','greece'];
  ZoneAcronyms[19] := ['it','ita','italy'];
  ZoneAcronyms[20] := ['mx','mex','mexico'];
  ZoneAcronyms[21] := ['be','bel','belgium'];
  
  // Languages
  //   http://en.wikipedia.org/wiki/ISO_639-1
  SetLength(LangAcronyms, 10);
  LangAcronyms[0] := ['en','english',
    'eu','eur','europe','e',
    'us','usa', 'united states','u',
    'gb','gbr','united kingdom','uk',
    'au','aus','australia',
    'w','world' ];
  LangAcronyms[1] := ['ja','japanese','jp','jpn','japan','j','jap'];
  LangAcronyms[2] := ['es','spanish','esp','spain','sp','spa']; 
  LangAcronyms[3] := ['zh','chinese','cn','chn','china','tw','twn','taiwan'];
  LangAcronyms[4] := ['ko','korean','kr','kor','south korea'];
  LangAcronyms[5] := ['de','german','deu','germany'];
  LangAcronyms[6] := ['fr','french','fra','france'];
  LangAcronyms[7] := ['pt','portuguese','br','bra','brazil'];
  LangAcronyms[8] := ['ru','russian','rus','rusia'];
  LangAcronyms[9] := ['it','italian','ita','italy'];  

  WriteLn('Fixing zones and language acronyms:');
  i := 0;
  while i < GameManager.GameCount do
  begin
    aGame := GameManager.GameAtPos(i);

    // TODO 1: Extracct tags from version

    // Zones
    j := 0;
    while j < aGame.Zones.Count do
    begin
      Found := False;
      k := Low(ZoneAcronyms);
      while (k <= High(ZoneAcronyms)) and not Found do
      begin
        m := Low(ZoneAcronyms[k]);
        while (m <= High(ZoneAcronyms[k])) and not Found do
        begin
          if AnsiLowerCase(aGame.Zones[j]) = ZoneAcronyms[k][m] then
          begin
            aGame.Zones[j] := AnsiUpperCase(ZoneAcronyms[k][0]);
            Found := True;
          end;
          Inc(m);
        end;
        Inc(k);
      end;
      if (not Found) and (aGame.Zones[j] <> '') then
        WriteLn(aGame.Zones[j] + ': Zone not found.');
      Inc(j);
    end;

    // Languages
    j := 0;
    while j < aGame.Languages.Count do
    begin
      Found := False;
      k := Low(LangAcronyms);
      while (k <= High(LangAcronyms)) and not Found do
      begin
        m := Low(LangAcronyms[k]);
        while (m <= High(LangAcronyms[k])) and not Found do
        begin
          if AnsiLowerCase(aGame.Languages[j]) = LangAcronyms[k][m] then
          begin
            aGame.Languages[j] := AnsiUpperCase(LangAcronyms[k][0]);
            Found := True;
          end;
          Inc(m);
        end;
        Inc(k);
      end;
      if (not Found) and (aGame.Languages[j] <> '') then
        WriteLn(aGame.Languages[j] + ': Language not found.');
      Inc(j);
    end;

    Inc(i);
    if i mod 100 = 0 then
      WriteLn(IntToStr(i) + ' files parsed.');
  end;

  WriteLn('');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('Done.');
end.
