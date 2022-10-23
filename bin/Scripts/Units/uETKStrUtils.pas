{ Emuteca Script unit
[Info]
Some common functions for string handling.

* ETKCopyFrom: Copy a string from a position to the end.
* ETKExtractBetween: Copy a string between 2 delimiter strings. 
* ETKFixTitle: Fixes a Title, and returns its SortTitle and MediaFile too.
  * Title, The -> The Title
  * Title (The) -> The Title

[Data]
Name=Chixpy
Version=0.07
Date=20221022
[Changes]
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
  if aPos > 0 then begin
    aPos := aPos + Length(aBegin);
    bPos := PosEx(aEnd, aString, aPos);
    if bPos > 0 then begin
      Result := Copy(aString, aPos, bPos - aPos);
    end;
  end;
end;

procedure ETKFixTitleInit;
// Inits arrays for ETKFixTitle (automatically called)
begin
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

    procedure DoFixTitle(var aTitle: string; out SortTitle: string);
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
      DoFixTitle(TempTitle, TempSort);

      // First letter uppercase
      if length(TempSort) > 0 then
        TempSort := UpperCase(AnsiLeftStr(TempSort, 1)) + 
          ETKCopyFrom(TempSort, 2);
      if length(TempTitle) > 0 then
        TempTitle := UpperCase(AnsiLeftStr(TempTitle, 1)) + 
          ETKCopyFrom(TempTitle, 2);
            
      if i > 0 then
      begin
        SortTitle := SortTitle + ' - ' + TempSort;
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

procedure ETKFixTitle(var aTitle: string; out SortTitle: string);
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

  if SortTitle = aTitle then SortTitle := '';
  
  slGameList.Free;
end;
