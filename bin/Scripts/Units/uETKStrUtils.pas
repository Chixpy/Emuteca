{ Emuteca Script unit
[Info]
Some common functions for string handling.

* ETKCopyFrom: Copy a string from a position to the end.
* ETKExtractBetween: Copy a string between 2 delimiter strings. 
* ETKFixSortTitle and ETKFixGrpId: Fixes a Title, and returns its SortTitle
  or normalized GroupID (Remember, some systems use their own GroupID).
  * Title, The -> The Title
  * Title (The) -> The Title

[Data]
Name=Chixpy
Version=0.08
Date=20230810
[Changes]
0.08 20230810
  c ETKFixTitle: Splited in ETKFixSortTitle and ETKFixGrpId.
0.07 20221022
  + IsArticle: Added. 
  m Better ETKFixTitle adding multigame support
0.06 20220831
  m ETKFixTitle: Removing unused MediaFile and changed SortTitle (Remove article) as new standard
      in Emuteca.
0.05 20201108
  * ETKFixTitle: SortTitle parameter is cleared.
0.04 20201105
  + ETKExtractBetween: Added.
0.03
  + ETKCopyFrom: Added.
0.02
  * ETKFixTitle: Fixing articles.
0.01
  + ETKStrUtilInit.
  * ETKFixTitle: ' - ' -> ': '
[EndInfo]
}

var
  ETKArticles: Array of string;
  
function ETKCopyFrom(const aStr: string; aPosition: integer): string;
// Copies a string from a position to the end.
begin
  Result := Copy(aStr, aPosition, length(aStr));
end;

function ETKExtractBetween(const aString, aBegin, aEnd: string): string;
var
  aPos, bPos: Integer;
begin
  Result := '';
  aPos := Pos(aBegin, aString);
  if aPos > 0 then 
    begin
    aPos := aPos + Length(aBegin);
    bPos := PosEx(aEnd, aString, aPos);
    if bPos > 0 then 
     begin
      Result := Copy(aString, aPos, bPos - aPos);
    end;
  end;
end;

procedure ETKFixTitleInit;
// Inits arrays for ETKFixTitle (automatically called)
begin
  // Some articles are not added because is most likely to
  //   meaning other thing in another language
  //   - 'Die' in German -> 'Die by the Sword'
  ETKArticles := ['The', 'A', 
    'El', 'La', 'Los', 'Las', 'Un', 'Una', 'Unos', 'Unas',
    'L''', 'Le', 'Les', 'Une', 'Des'];
end;

function IsArticle(const aString: string): boolean;
var
  i: integer;
begin
  if Length(ETKArticles) = 0 then ETKFixTitleInit;
  
  Result := false;
  
  i := Low(ETKArticles);
  while i <= High(ETKArticles) do
  begin
    if aString = ETKArticles[i] then
    begin
      Result := true;
      i := High(ETKArticles); // Break while
    end;    
    
    Inc(i);
  end;
end;

// Padded procedures and functions are "private", not for use out of this
//   unit.
    procedure DoActualFixTitle(var aTitle: string; out SortTitle: string);
    var
     aPos: integer;
     aArticle: string;
    begin
      // Searching if aTitle has an article.
      //   - The Title -> (Must be in aTitle)
      //   - Title, The -> (Must be SortTitle, without ', The')
      //   - Title (The) -> Covert to previous syntax

      // Testing 'Title, The'
      aPos := RPos(',', aTitle);
      if aPos > 0 then
      begin
        aArticle := Trim(ETKCopyFrom(aTitle, aPos + 1));

        if IsArticle(aArticle) then
        begin
          SortTitle := Trim(LeftStr(aTitle, aPos - 1));
          aTitle := aArticle + ' ' + SortTitle;
          Exit;
        end;
      end;

      // Testing 'The Title'...
      aPos := Pos(' ', aTitle);

      // KitKat
      if aPos <= 0 then // TitleIsOneWord, so no article
      begin
        SortTitle := aTitle;
        Exit;
      end;

      // Testing 'The Title' 2
      aArticle := LeftStr(aTitle, aPos - 1);
      if IsArticle(aArticle) then
      begin
        SortTitle := Trim(ETKCopyFrom(aTitle, aPos));
        Exit;
      end;

      // Testing 'Title (The)'
      if aTitle[Length(aTitle)] = ')' then
      begin
        aPos := RPos('(', aTitle);

        if aPos > 0 then
        begin
          aArticle := Trim(Copy(aTitle, aPos + 1, Length(aTitle) -  aPos - 1));
          if IsArticle(aArticle) then
          begin
            SortTitle := Trim(LeftStr(aTitle, aPos - 1));
            aTitle := aArticle + ' ' + SortTitle;
            Exit;
          end;
        end;
      end;

      // No article found
      SortTitle := aTitle;
    end;


  procedure SplitSubTitles(var aTitle: string; out SortTitle: string);
  var
    slTitleList: TStringList;
    TempTitle, TempSort: string;
    i: integer;
  begin
    SortTitle := '';

    // Splitting multiple subtiltes
    // 'Game 1, The: Chapter 1: Subtitle, The'
    slTitleList := CreateStringList;
    TempTitle := AnsiReplaceText(aTitle, ': ', '|');
    slTitleList.AddDelimitedText(TempTitle, '|', true);

    aTitle := ''; // Reseting Title
    i := 0;
    while i < slTitleList.Count do
    begin
      TempTitle := Trim(slTitleList[i]);
      DoActualFixTitle(TempTitle, TempSort);

      // First letter uppercase
      if length(TempSort) > 0 then
        TempSort := UpperCase(AnsiLeftStr(TempSort, 1)) + 
          ETKCopyFrom(TempSort, 2);
      if length(TempTitle) > 0 then
        TempTitle := UpperCase(AnsiLeftStr(TempTitle, 1)) + 
          ETKCopyFrom(TempTitle, 2);
            
      if i > 0 then
      begin
        SortTitle := SortTitle + ': ' + TempSort;
        aTitle := aTitle + ': ' + TempTitle;
      end
      else
      begin
        SortTitle := TempSort;
        aTitle := TempTitle;
      end;
      
      Inc(i);
    end;

    slTitleList.Free;
  end;

  procedure DoFixTitle(var aTitle: string; out SortTitle: string);
  var
    slGameList: TStringList;
    TempTitle, TempSort: string;
    i: integer;
  begin
    aTitle := AnsiReplaceText(aTitle, ' - ', ': ');

    SortTitle := '';
 
    // Splitting multiple games
    // 'Game 1, The: Subtitle + Game 2: Subtitle, The + Game, A'
    slGameList := CreateStringList;
    aTitle := AnsiReplaceText(aTitle, ' + ', '|');
    slGameList.AddDelimitedText(aTitle, '|', true);
  
    aTitle := ''; // Reseting Title
    i := 0;
    while i < slGameList.Count do
    begin
      TempTitle := slGameList[i];
      SplitSubTitles(TempTitle, TempSort);
     
      if i > 0 then
      begin
        SortTitle := SortTitle + ' + ' + TempSort;
        aTitle := aTitle + ' + ' + TempTitle;
      end
      else
      begin
        SortTitle := TempSort;
        aTitle := TempTitle;
      end;
      
      Inc(i);
    end;

  slGameList.Free;
end;

procedure ETKFixGrpID(var aTitle: string; out GroupID: string);
begin
  DoFixTitle(aTitle, GroupID);
end;

procedure ETKFixSortTitle(var aTitle: string; out SortTitle: string);
begin
  DoFixTitle(aTitle, SortTitle);

  if SortTitle = aTitle then
    SortTitle := '';

  // Changing some Windows invalid characters, as EmutecaCore's
  //   uaEmutecaCustomSGItem, that are changed in diferent way than
  //   CleanFileName.

  // TODO: Maybe is better to make public and import the method
  //   caEmutecaCustomSGItem.FormatSortTitle?

  // Actually ': ' is changed to ' - ', but we want to sort with ' - ',
  //   because we want to sort...:
  // Game
  // Game - First Part  <- Go before Second Part
  // Game 2
  // Game 2 - Second Part
  // Game 2: Second Part
  // Game: First Part <- Go after Second Part

  SortTitle := AnsiReplaceText(SortTitle, ': ', ' - ');
  SortTitle := AnsiReplaceText(SortTitle, ' & ', ' and ');
end;

