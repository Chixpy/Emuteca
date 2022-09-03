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
Version=0.05
Date=20201108
[Changes]
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
  
procedure ETKFixTitle(var aTitle: string; out SortTitle: string);
var
  i: integer;
  ArticleFound: string;
begin
  if Length(ETKArticles) = 0 then ETKFixTitleInit;

  SortTitle := '';

  // Replacing ' - ' with ': '
  aTitle := Trim(AnsiReplaceText(aTitle, ' - ', ': '));
 
 
  // Searching if aTitle has an article.
  //   - The Title -> (Must be in aTitle)
  //   - Title, The -> (Must be SortTitle, without ', The')
  //   - Title (The) -> Covert to previous syntax
  
  // To check if already changed and Keeping actual article
  ArticleFound := '';
  
  // (The)
  if ArticleFound = '' then
  begin
    i := Low(ETKArticles);
    while (ArticleFound = '') and (i <= High(ETKArticles)) do
    begin      
      if AnsiEndsStr(' (' + ETKArticles[i] + ')', aTitle) then
        ArticleFound := ETKArticles[i]
      else
        Inc(i);
    end;  
    if ArticleFound <> '' then
    begin
      aTitle := Trim(AnsiLeftStr(aTitle, Length(aTitle) - Length(ArticleFound)
        - 3));
      SortTitle := aTitle;
      aTitle := ArticleFound + ' ' + aTitle;
    end;
  end;
  
  // Title, The
  // TODO: Fix -> Title, The: Subtitle
  if ArticleFound = '' then
  begin  
    i := Low(ETKArticles);
    while (ArticleFound = '') and (i <= High(ETKArticles)) do
    begin
      if AnsiEndsStr(', ' + ETKArticles[i], aTitle) then
        ArticleFound := ETKArticles[i]
      else
        Inc(i);
    end;  
    if ArticleFound <> '' then
    begin
      SortTitle := Trim(AnsiLeftStr(aTitle, Length(aTitle)
        - Length(ArticleFound) - 2));
      aTitle := ArticleFound + ' ' + SortTitle;
    end; 
  end;  
  
  // The Title
  if ArticleFound = '' then
  begin  
    i := Low(ETKArticles);
    while (ArticleFound = '') and (i <= High(ETKArticles)) do
    begin
      if AnsiStartsStr(ETKArticles[i] + ' ', aTitle) then
        ArticleFound := ETKArticles[i]
      else
        Inc(i);
    end;  
    if ArticleFound <> '' then
    begin
      SortTitle := Trim(ETKCopyFrom(aTitle, Length(ArticleFound) + 1));
      // Don't change aTitle
    end; 
  end; 
  
  // Really not needed now...
  if CompareText(aTitle, SortTitle) = 0 then
    SortTitle := '';
    
  //// Setting MediaFile
  //if SortTitle <> '' then
  //  MediaFile := CleanFileName(SortTitle, true, false)
  //else    
  //  MediaFile := CleanFileName(aTitle, true, false);
end;
