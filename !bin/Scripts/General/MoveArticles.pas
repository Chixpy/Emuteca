{ [SCRIPTDATA]
Author = Chixpy
Version = 0.2
Date = 20101118
Description =
~begin~
  Move artcles "The", "A", "El", "Las", etc. at the end of game sort key.
~end~
Changes =
~begin~
  0.4 - 20111116:
    m Adapting to changes in cGame and cGameGroup
  0.3 - 20110123:
    - It didn't move articles in group keys
  0.2 - 20101118:
    m It isn't needed todo it backguards
    + Now it takes care with subtitles (': 'or ' - ') and multigames(' / ')
  0.1 - 20101109:
    + Initial version
~end~
}
program GroupByFolder;
const
  Debug = False;

var
  // Yes, I know it's global variable...
  // But I don´t want to assign the values in every MoveArticle call
  // Yes, I know that can I pass the array as parameter, too
  Articles: array [1..11] of String;
  // var Articles: array [1..11] of UTF8String = ('The', 'A', 'An', ...); it seems don't work :/

function MoveNameArticle(aTitle: String): String;
var
  i: integer;
  ArticlePos: Integer;
begin
  Result := aTitle;
  i := Low(Articles);
  while i <= High(Articles) do
  begin
    ArticlePos := Pos(', ' + Articles[i], Result);
    if ArticlePos <> 0 then
    begin
      Result := AnsiReplaceStr(Result, ', ' + Articles[i], '');
      Result := Articles[i] + ' ' + Trim(Result);
      if Debug then WriteLn(aTitle + ' --> ' + Result);
      Exit;
    end;
    Inc(i);
  end;
end;

function MoveSortArticle(aTitle: String): String;
var
  i: integer;
  SeparatorPos, SeparatorPos2: Integer;
begin
  Result := aTitle;
  i := Low(Articles);
  while i <= High(Articles) do
  begin
    if Pos(LowerCase(Articles[i] + ' '), LowerCase(aTitle)) = 1 then
    begin
      SeparatorPos := Pos(': ', aTitle); // Has subtitle?
      SeparatorPos2 := Pos(' - ', aTitle);
      if (SeparatorPos = 0) then
      begin
        SeparatorPos := SeparatorPos2;
      end
      else
      begin
        // SeparatorPos <> 0
        if (SeparatorPos2 <> 0) and (SeparatorPos > SeparatorPos2) then
          SeparatorPos := SeparatorPos2;
      end;

      SeparatorPos2 := Pos(' / ', aTitle); // Is it multigame?
      if (SeparatorPos2 <> 0) and (SeparatorPos > SeparatorPos2) then
        SeparatorPos := SeparatorPos2;

      if SeparatorPos = 0 then
        Result := Copy(aTitle, Length(Articles[i]) + 2, 32000)
          + ', ' + Articles[i]
      else
        Result := Copy(aTitle, Length(Articles[i]) + 2,
            SeparatorPos - Length(Articles[i]) - 2)
          + ', ' + Articles[i]
          + Copy(aTitle, SeparatorPos, 32000);

      if Debug then WriteLn(aTitle + ' --> ' + Result);
      exit;
    end;
    Inc(i);
  end;
end;

var
  Game: cGame;
  Group: cGameGroup;
  i: integer;
begin
  Articles[1] := 'The';
  Articles[2] := 'A';
  Articles[3] := 'An';
  Articles[4] := 'El';
  Articles[5] := 'La';
  Articles[6] := 'Los';
  Articles[7] := 'Las';
  Articles[8] := 'Un';
  Articles[9] := 'Una';
  Articles[10] := 'Unas';
  Articles[11] := 'Unos';

  WriteLn('Changing articles in games:');
  i := 0;
  while i < GameManager.GameCount do
  begin
    Game := GameManager.GameAtPos(i)
    Game.Name := MoveNameArticle(Game.Name);
    Game.SortKey := MoveSortArticle(Game.SortKey);
    Game.GameGroup := MoveSortArticle(Game.GameGroup);
    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' games parsed.');
  end;

  WriteLn('');
  WriteLn('Changing articles in groups:');
  i := 0;
  while i < GameManager.GroupCount do
  begin
    Group := GameManager.GroupAtPos(i);
    Group.Name := MoveNameArticle(Group.Name);
    Group.Key := MoveSortArticle(Group.Key);
    Group.SortKey := MoveSortArticle(Group.SortKey);
    Group.MediaFileName := MoveSortArticle(ExtractFileNameOnly(
      Group.MediaFileName)) + ExtractFileExt(Group.MediaFileName);
    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' groups parsed.');
  end;

  WriteLn('');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('Done.');
end.
